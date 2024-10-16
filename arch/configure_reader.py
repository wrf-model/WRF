#!/usr/bin/env python3

import argparse
import sys
import os
import re
import inspect
import platform
from shutil import which

archBlock  = re.compile( r"(?:#[ ]*)(ARCH(?:.*\n)*?)(?:#{5,})", re.I )
kvPair     = re.compile( r"^(\w+)(?:[ \t]*=[ \t]*)(.*?)$",      re.I | re.M )
# Make this gnarly and complicated since configure.defaults has no standard formatting
#                            v start  v OS      V typical   v MACOS
osAndArch     = re.compile( r"^ARCH[ ]+(\w+)[ ]+((?:\w+.*?),|(?:[(].*?[)]))",         re.I )
# Just grab the first two words, thats what you get
osAndArchAlt  = re.compile( r"^ARCH[ ]+(\w+)[ ]+(\w+)",         re.I )

referenceVar  = re.compile( r"[$]([(])?(\w+)(?(1)[)])", re.I )
compileObject = re.compile( r"(\W|^)-c(\W|$)" )
configureRepl = re.compile( r"(\W|^)CONFIGURE_\w+(\W|$)" )

class Stanza():
  
  def __init__( self, lines ) :
    self.lines_   = lines
    self.os_      = None
    self.arch_    = None
    self.osArchLine_ = None
    self.archs_   = []
    self.kvPairs_ = {}
    self.crossPlatform_ = False
    self.skipCrossPlatform_ = True
    self.serialOpt_  = False
    self.smparOpt_   = False
    self.dmparOpt_   = False
    self.dmsmOpt_    = False

  def parse( self ) :
    self.osArchLine_ = self.lines_.partition("\n")[0]
    # First get os & archs
    osarchMatch = osAndArch.match( self.osArchLine_ )

    if osarchMatch is None :
      osarchMatch = osAndArchAlt.match( self.osArchLine_ )
      if osarchMatch is None :
        print( "Could not find OS and architecture info in " + self.osArchLine_ )
      
    self.os_    = osarchMatch.group(1)
    self.archs_ = osarchMatch.group(2).strip(",").split( " " )

    if ( self.os_.lower() != platform.system().lower() or
         platform.machine() not in self.archs_ ) :
      self.crossPlatform_ = True

    # Allow cross platform or must not be cross platform
    if not self.skipCrossPlatform_ or ( self.skipCrossPlatform_ and not self.crossPlatform_ ) :

      # Find OpenMP/MPI compilation options
      memOpts = self.osArchLine_.partition( "#" )[-1].split( " " )
      # print( memOpts )
      self.serialOpt_  = "serial" in memOpts
      self.smparOpt_   = "smpar"  in memOpts
      self.dmparOpt_   = "dmpar"  in memOpts
      self.dmsmOpt_    = "dm+sm"  in memOpts

      for kvPairMatch in kvPair.finditer( self.lines_ ) :
        self.kvPairs_[ kvPairMatch.group(1) ] = kvPairMatch.group(2)
        self.removeComments( kvPairMatch.group(1) )
      
      # Now sanitize
      self.sanitize()
  
  ######################################################################################################################
  ##
  ## search and replace $(<var>) and $<var> instances
  ##
  ######################################################################################################################
  def dereference( self, field, fatal=False ) :
    # print( "Dereferencing " + field )

    if field in self.kvPairs_ :
      prevField = self.kvPairs_[field]

      for refVarIter in referenceVar.finditer( prevField ) :
        envSub = None

        if refVarIter is not None :
          # Grab group 1 and check that it is in our kv pairs
          refVar = refVarIter.group(2)
          # print( "Found variable {0} in field {1}".format( refVar, field ) )
          if refVar not in self.kvPairs_ :
            # Try to use the environment variables
            if refVar in os.environ :
              envSub = os.environ[ refVar ]
            else:
              if fatal :
                # print( "Could not rereference : " + refVar )
                exit(1)
              else:
                continue
          

          # This is an environment variable
          if envSub is not None : 
            self.kvPairs_[field] = self.kvPairs_[field].replace(
                                                                "{var}".format( var=refVarIter.group(0) ),
                                                                envSub
                                                                )
          # This is a kv pair, recurse
          else :
            # Recursively deref
            self.dereference( refVar, fatal )

            # Replace in original
            self.kvPairs_[field] = self.kvPairs_[field].replace(
                                                                "{var}".format( var=refVarIter.group(0) ),
                                                                self.kvPairs_[refVar]
                                                                )

  def removeReferences( self, field, specifics=[] ) :
    if field in self.kvPairs_ :
      if specifics :
        for specific in specifics :
          self.kvPairs_[ field ] = self.kvPairs_[ field ].replace(
                                                                  "$({var})".format( var=specific ),
                                                                  ""
                                                                  )
      else :
        self.kvPairs_[ field ] = referenceVar.sub( "", self.kvPairs_[ field ] )


  def removeComments( self, field ) : 
    if field in self.kvPairs_ :
      self.kvPairs_[ field ] = self.kvPairs_[ field ].split( "#", 1 )[0]
  
  def splitIntoFieldAndFlags( self, field ) :
    # Fix flags being mixed with programs
    if field in self.kvPairs_ :
      fieldValue = self.kvPairs_[ field ]

      self.kvPairs_[field] = fieldValue.partition(" ")[0]
      self.kvPairs_[field + "_FLAGS"] = fieldValue.partition(" ")[1]

  ######################################################################################################################
  ##
  ## Clean up the stanza so kv pairs can be used as-is
  ##
  ######################################################################################################################
  def sanitize( self ) :
    # Fix problematic variables
    self.dereference( "DM_FC" )
    self.dereference( "DM_CC" )
    self.removeReferences( "FCBASEOPTS_NO_G" )
    # Get rid of all these mixed up flags, these are handled by cmake natively or 
    # just in the wrong place
    self.removeReferences( "FCBASEOPTS", [ "FCDEBUG", "FORMAT_FREE", "BYTESWAPIO", ] )
    self.removeReferences( "FFLAGS",   [ "FORMAT_FREE", "FORMAT_FIXED" ] )
    self.removeReferences( "F77FLAGS", [ "FORMAT_FREE", "FORMAT_FIXED" ] )
    # # Now deref
    self.dereference( "FCBASEOPTS" )

    # Remove rogue compile commands that should *NOT* even be here
    for keyToSan in self.kvPairs_.keys() :
      self.kvPairs_[ keyToSan ] = configureRepl.sub( r"\1\2", self.kvPairs_[ keyToSan ] ).strip()
      self.kvPairs_[ keyToSan ] = compileObject.sub( r"\1\2", self.kvPairs_[ keyToSan ] ).strip()


    # Now fix certain ones that are mixing programs with flags all mashed into one option
    self.splitIntoFieldAndFlags( "SFC" )
    self.splitIntoFieldAndFlags( "SCC" )
    self.splitIntoFieldAndFlags( "DM_FC" )
    self.splitIntoFieldAndFlags( "DM_CC" )
    self.splitIntoFieldAndFlags( "CPP" )
    self.splitIntoFieldAndFlags( "M4" )

    # Now deref all the rest
    for key in self.kvPairs_ :
      self.dereference( key )
      # And for final measure strip
      self.kvPairs_[ key ] = self.kvPairs_[ key ].strip()

  def serialCompilersAvailable( self ) :
    return which( self.kvPairs_["SFC"]   ) is not None and which( self.kvPairs_["SCC"]   ) is not None

  def dmCompilersAvailable( self ) :
    return which( self.kvPairs_["DM_FC"]   ) is not None and which( self.kvPairs_["DM_CC"]   ) is not None

  ######################################################################################################################
  ##
  ## string representation to view as option
  ##
  ######################################################################################################################
  def __str__( self ):
    # base = """OS {os:<8} ARCHITECTURES {archs:<20}
    #           >>  SFC    = {SFC:<12}
    #           >>  SCC    = {SCC:<12}
    #           >>  CCOMP  = {CCOMP:<12}
    #           >>  DM_FC  = {DM_FC:<12}
    #           >>  DM_CC  = {DM_CC:<12}
    #        """
    base = """  {os:<10} {recSFC} {SFC:<11} / {recSCC} {SCC:<11} / {recDM_FC} {DM_FC:<11} / {recDM_CC} {DM_CC:<11}"""
    text = inspect.cleandoc( base ).format( 
                                            os=str(self.os_),
                                            recSFC  =( "!!" if which( self.kvPairs_["SFC"]   ) is None else (" " * 2 ) ),
                                            recSCC  =( "!!" if which( self.kvPairs_["SCC"]   ) is None else (" " * 2 ) ),
                                            recDM_FC=( "!!" if which( self.kvPairs_["DM_FC"] ) is None else (" " * 2 ) ),
                                            recDM_CC=( "!!" if which( self.kvPairs_["DM_CC"] ) is None else (" " * 2 ) ),
                                            # archs=str(self.archs_),
                                            SFC=str( self.kvPairs_["SFC"] ),
                                            SCC=str( self.kvPairs_["SCC"] ),
                                            DM_FC=str( self.kvPairs_["DM_FC"] ),
                                            DM_CC=str( self.kvPairs_["DM_CC"] )
                                            )
    # text   += "\n" + "\n".join( [ "{key:<18} = {value}".format( key=key, value=value) for key, value in self.kvPairs_.items() ] ) 
    return text

  ######################################################################################################################
  ##
  ## Find first apparent difference between two stanzas
  ##
  ######################################################################################################################
  @staticmethod
  def findFirstDifference( rhStanza, lhStanza, maxLength=32 ) :
    diff  = False
    value = ""
    valuesToCheck = [
                      "ARCH_LOCAL",
                      "BYTESWAPIO",
                      "CFLAGS_LOCAL",
                      "CFLAGS",
                      "DM_CC_FLAGS",
                      "DM_CC",
                      "DM_FC_FLAGS",
                      "DM_FC",
                      "FCBASEOPTS",
                      "FCDEBUG",
                      "FCNOOPT",
                      "FCOPTIM",
                      "FFLAGS",
                      "M4_FLAGS",
                      "SCC",
                      "SFC"
                    ]
    for rhKey, rhValue in rhStanza.kvPairs_.items() :
      if rhKey in valuesToCheck and rhKey in lhStanza.kvPairs_ :
        # Qualifies for difference
        if rhValue != lhStanza.kvPairs_[rhKey] :
          diff  = True
          value = "{key:<12} = {value}".format( key=rhKey, value=lhStanza.kvPairs_[rhKey] )
          
          # Truncate
          value = ( value[:maxLength] + "..." ) if len( value ) > maxLength else value
    
    return diff, value

########################################################################################################################
##
## Option handling
##
########################################################################################################################
def getOptionsParser() :
  parser = argparse.ArgumentParser( )

  # https://stackoverflow.com/a/24181138
  requiredNamed = parser.add_argument_group( "required named arguments" )

  requiredNamed.add_argument( 
                              "-c", "--config",
                              dest="configFile",
                              help="configure.defaults file holding all stanza configurations",
                              type=str,
                              required=True
                              )
  requiredNamed.add_argument( 
                              "-t", "--template",
                              dest="cmakeTemplateFile",
                              help="cmake template file for configuring stanza into cmake syntax",
                              type=str,
                              required=True
                              )
  requiredNamed.add_argument( 
                              "-o", "--output",
                              dest="outputConfigFile",
                              help="cmake output toolchain config file for selected stanza",
                              type=str,
                              required=True
                              )

  parser.add_argument( 
                      "-p", "--preselect",
                      dest="preselect",
                      help="Use preselected stanza configuration, if multiple match grabs the first one",
                      type=str,
                      default=None
                      )

  parser.add_argument( 
                      "-x", "--skipCMakeOptions",
                      dest="skipCMakeOptions",
                      help="Skip query of available CMake options",
                      default=False,
                      const=True,
                      action='store_const'
                      )
  parser.add_argument( 
                      "-s", "--source",
                      dest="sourceCMakeFile",
                      help="Required unless -x/--skipCMakeOptions set, project cmake source file used to determine available options",
                      type=str,
                      default=None
                      )

  return parser


class Options(object):
  """Empty namespace"""
  pass

########################################################################################################################
##
## Select stanza to operate on
##
########################################################################################################################
def selectStanza( options ) :

  fp    = open( options.configFile, 'r' )
  lines = fp.read()
  fp.close()

  # Now grab the blocks and parse
  stanzas = []
  # Gather all stanzas available
  for stanzaBlock in archBlock.finditer( lines ) :
    stanza = Stanza( stanzaBlock.group(1) )
    stanza.parse()

    if not stanza.crossPlatform_ and stanza.serialCompilersAvailable() and ( stanza.dmCompilersAvailable() or ( stanza.serialOpt_ or stanza.smparOpt_ ) ) :
      if "DESCRIPTION" not in stanza.kvPairs_ :
        # Of course WPS configure.defaults is different than WRF so descriptions are embedded in the comments
        stanza.kvPairs_[ "DESCRIPTION" ] = stanza.osArchLine_.partition( "," )[ -1 ].partition( "#" )[0].strip()
      stanzas.append( stanza )

  idxSelection = 0
  if options.preselect is None :
    # Query for selected
    stanzaIdx = 0
    uniqueConfigs = {}
    for stanza in stanzas :
      stanzaConfig = str( stanza )
      stanzaId = "{idx:<3} ".format( idx=stanzaIdx )
      if stanzaConfig not in uniqueConfigs :
        uniqueConfigs[ stanzaConfig ] = { "stanza" : stanza, "idx" : stanzaIdx }

      print( stanzaId + stanzaConfig + stanza.kvPairs_[ "DESCRIPTION" ] )
      # else :
        # diff, value = Stanza.findFirstDifference( uniqueConfigs[ stanzaConfig ]["stanza"], stanza )
        # if diff :
        #   print( stanzaId + stanzaConfig + "@{idx} diff => {value}".format( idx=uniqueConfigs[ stanzaConfig ][ "idx" ], value=value ) )
        # else :
        #   print( stanzaId + stanzaConfig + "[no difference]" )
      stanzaIdx += 1
    print( "!! - Compiler not found, some configurations will not work and will be hidden" )
    stringSelection = input( "Select configuration [0-{stop}] Default [0] (note !!)  : ".format( stop=( stanzaIdx-1) ) )
    idxSelection = int( stringSelection if stringSelection.isdigit() else 0 )
    if idxSelection < 0 or idxSelection > stanzaIdx - 1 :
      print( "Invalid configuration selection!" )
      exit(1)
  else :
    for stanza in stanzas :
      if options.preselect.lower() in stanza.kvPairs_["DESCRIPTION"].lower() :
        print( str( stanza ) + stanza.kvPairs_[ "DESCRIPTION"] )
        break
      else :
        idxSelection += 1
    if idxSelection == len( stanzas ) :
      print( "Error: Stanza configuration with description '{0}' does not exist. Preselect failed.".format( options.preselect ) )
      exit(1)

  stanzaCfg         = stanzas[idxSelection]

  return stanzaCfg

########################################################################################################################
##
## Select enum-like string for string-based cmake options
##
########################################################################################################################
def getStringOptionSelection( topLevelCmake, searchString, destinationOption, defaultIndex=0 ) :
  topLevelCmakeFP    = open( topLevelCmake, "r" )
  topLevelCmakeLines = topLevelCmakeFP.read()
  topLevelCmakeFP.close()

  stringOptionsMatch = re.search(
                                  r"set\s*[(]\s*" + searchString + r"\s*(.*?)[)]",
                                  topLevelCmakeLines,
                                  re.I | re.S | re.M
                                  )
  if stringOptionsMatch is None :
    print( "Syntax error in parsing " + searchString + " from " + topLevelCmake )
    exit(1)
  
  options = [ option.split( "#", 1 )[0].strip() for option in stringOptionsMatch.group(1).split( "\n" ) ]
  # Weed out empties
  options = [ option for option in options if option ]

  optionsFmt = "\n\t" + "\n\t".join( [ "{idx} : {opt}".format( idx=options.index( opt ), opt=opt ) for opt in options ] )
  stringSelection = input( "Select option for {option} from {optionsSource} [0-{max}] {opts} \nDefault [{defIdx}] : ".format( 
                                                                                                                              option=destinationOption,
                                                                                                                              optionsSource=searchString,
                                                                                                                              max=len(options)-1,
                                                                                                                              opts=optionsFmt,
                                                                                                                              defIdx=defaultIndex
                                                                                                                              )
                          )
  selection  = int( stringSelection if stringSelection.isdigit() else defaultIndex )

  if selection < 0 or selection > len(options) :
    print( "Invalid option selection for " + searchString +  "!" )
    exit(1)
  
  return options[selection]

########################################################################################################################
##
## Aggregate and allow toggle of various suboptions in alternate menu
##
########################################################################################################################
def getSubOptions( topLevelCmake, ignoreOptions ) :
  topLevelCmakeFP    = open( topLevelCmake, "r" )
  topLevelCmakeLines = topLevelCmakeFP.read()
  topLevelCmakeFP.close()

  stringOptionsMatch = re.finditer(
                                  r"set\s*[(]\s*(\w+)\s*(ON|OFF)\s*CACHE\s*BOOL\s*\"(.*?)\"\s*[)]",
                                  topLevelCmakeLines,
                                  re.I | re.M
                                  )
  # Remove commented ones and ones that don't follow pattern set( <OPT> ON|OFF CACHE BOOL "<OPT>" )
  options = [ [ option.group( 1 ), option.group( 2 ) ] for option in stringOptionsMatch if option.group( 1 ) == option.group( 3 ) and option.group(0).split( "#", 1 )[0].strip() ]
  
  # Remove ignore options
  options = [ option for option in options if option[0] not in ignoreOptions ]
  subOptions      = {}
  
  if options :
    subOptionQuit = False
    optionToggleIdx = -1

    # Print menu
    optionStr = "{idx:<3} {option:<24} : {value:<5}"
    print( optionStr.format( idx="ID", option="Option", value="Default" ) )
    for opt in options :
      print( optionStr.format( idx=options.index(opt), option=opt[0], value=opt[1] ) )

    print( "Enter ID to toggle option on or off, q to quit : " )
    # Loop until q, toggle from default not current value
    while not subOptionQuit :
      optionToggleIdx = input()
      try:
        optionToggleIdx = int( optionToggleIdx )
        if optionToggleIdx < 0 or optionToggleIdx >= len( options ) :
          print( "Not a valid index" )
        else:
          subOptions[ options[optionToggleIdx][0] ] = "ON" if not ( options[optionToggleIdx][1] == "ON" ) else "OFF"
          print( "Set {option} to {value}".format( option=options[optionToggleIdx][0], value=subOptions[ options[optionToggleIdx][0] ] ) )
      except ValueError as err :
        subOptionQuit = optionToggleIdx.lower() == "q"

  return subOptions

def main() :
  
  parser  = getOptionsParser()
  options = Options()
  parser.parse_args( namespace=options )

  stanzaCfg        = selectStanza( options )

  additionalOptions = {}
  if not options.skipCMakeOptions :
    if options.sourceCMakeFile is None :
      print( "Error: Project source cmake file required for project specific options." )
      exit(1)
    else:
      additionalOptions = projectSpecificOptions( options, stanzaCfg )

  generateCMakeToolChainFile( options.cmakeTemplateFile, options.outputConfigFile, stanzaCfg, additionalOptions )

########################################################################################################################
########################################################################################################################
##
## ABOVE THIS BREAK THINGS ARE EXACTLY THE SAME AS WRF/WPS
## BELOW THIS BREAK THINGS DIFFER
##
########################################################################################################################
########################################################################################################################

def generateCMakeToolChainFile( cmakeToolChainTemplate, output, stanza, optionsDict={} ) :
  cmakeToolChainTemplateFP    = open( cmakeToolChainTemplate, "r" )
  cmakeToolChainTemplateLines = cmakeToolChainTemplateFP.read()
  cmakeToolChainTemplateFP.close()

  configStanza = cmakeToolChainTemplateLines.format( 
                                                    ARCH_LOCAL=stanza.kvPairs_["ARCH_LOCAL"],
                                                    LDFLAGS_LOCAL=stanza.kvPairs_["LDFLAGS_LOCAL"],
                                                    BYTESWAPIO=stanza.kvPairs_["BYTESWAPIO"],
                                                    CFLAGS_LOCAL=stanza.kvPairs_["CFLAGS_LOCAL"],
                                                    DM_CC=stanza.kvPairs_["DM_CC"],
                                                    DM_FC=stanza.kvPairs_["DM_FC"],
                                                    DM_FC_FLAGS=stanza.kvPairs_["DM_FC_FLAGS"],
                                                    DM_CC_FLAGS=stanza.kvPairs_["DM_CC_FLAGS"],
                                                    FCBASEOPTS=stanza.kvPairs_["FCBASEOPTS"],
                                                    FCDEBUG=stanza.kvPairs_["FCDEBUG"],
                                                    FCNOOPT=stanza.kvPairs_["FCNOOPT"],
                                                    FCOPTIM=stanza.kvPairs_["FCOPTIM"],
                                                    M4_FLAGS=stanza.kvPairs_["M4_FLAGS"],
                                                    SCC=stanza.kvPairs_["SCC"],
                                                    SFC=stanza.kvPairs_["SFC"],
                                                    SCC_FLAGS=stanza.kvPairs_["SCC_FLAGS"],
                                                    SFC_FLAGS=stanza.kvPairs_["SFC_FLAGS"],
                                                    CPP=stanza.kvPairs_["CPP"],
                                                    CPP_FLAGS=stanza.kvPairs_["CPP_FLAGS"],
                                                    )

  # Extra stufff not from stanza but options
  fmtOption = "set( {opt:<32} {value:<12} CACHE STRING \"Set by configuration\" FORCE )"
  configStanza += "\n" + "\n".join( [ fmtOption.format( opt=key, value=value ) for key, value in optionsDict.items() ] )

  outputFP = open( output, "w" )
  outputFP.write( configStanza )
  outputFP.close()

def projectSpecificOptions( options, stanzaCfg ) :
  coreOption       = getStringOptionSelection( options.sourceCMakeFile, "WRF_CORE_OPTIONS",    "WRF_CORE"    )
  if coreOption == "ARW" :
    nestingOption    = getStringOptionSelection( options.sourceCMakeFile, "WRF_NESTING_OPTIONS", "WRF_NESTING", 1 )
    caseOption       = getStringOptionSelection( options.sourceCMakeFile, "WRF_CASE_OPTIONS",    "WRF_CASE"    )
  else :
    nestingOption = "NONE"
    caseOption    = "NONE"
  
  # These are yes
  yesValues    = [ "yes", "y", "true", "1" ]
  # Acceptable no values
  noValues    = [ "no", "n", "false", "0" ]

  ##############################################################################
  # Decompose the weird way to write the logic for DM/SM 
  USE_MPI = False
  if ( stanzaCfg.serialOpt_ or stanzaCfg.smparOpt_ ) and ( stanzaCfg.dmparOpt_ or stanzaCfg.dmsmOpt_ ) :
    # togglable
    # we can safely check this since the user would not have been able to select this stanza if it couldn't be disabled
    if stanzaCfg.dmCompilersAvailable() :
      useMPI       = not( input( "[DM] Use MPI?    Default [Y] [Y/n] : " ).lower() in noValues )
    else :
      useMPI = False
  else:
    # User has no choice in the matter
    useMPI = ( stanzaCfg.dmparOpt_ or stanzaCfg.dmsmOpt_ )

  useOpenMP = False
  if ( stanzaCfg.serialOpt_ or stanzaCfg.dmparOpt_ ) and ( stanzaCfg.smparOpt_ or stanzaCfg.dmsmOpt_ ):
    # togglable
    useOpenMP    = input( "[SM] Use OpenMP? Default [N] [y/N] : " ).lower() in yesValues
  else:
    # User has no choice in the matter
    useOpenMP = ( stanzaCfg.smparOpt_ or stanzaCfg.dmsmOpt_ )

  ##############################################################################

  alreadyAsked = [ "USE_MPI", "USE_OPENMP" ]
  doSuboptionMenu = input( "Configure additional options? Default [N] [y/N] : " ).lower() in yesValues
  subOptions      = {}
  if doSuboptionMenu :
    subOptions = getSubOptions( options.sourceCMakeFile, alreadyAsked )

  additionalOptions = {
                        "WRF_CORE"    : coreOption,
                        "WRF_NESTING" : nestingOption,
                        "WRF_CASE"    : caseOption,
                        "USE_MPI"     : "ON" if useMPI    else "OFF",
                        "USE_OPENMP"  : "ON" if useOpenMP else "OFF",
                      }
  additionalOptions.update( subOptions )

  return additionalOptions

if __name__ == '__main__' :
  main()
