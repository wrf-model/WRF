#!/usr/bin/env python3

import sys
import re
import inspect
import platform

archBlock  = re.compile( r"(?:#[ ]*)(ARCH(?:.*\n)*?)(?:#{5,})", re.I )
kvPair     = re.compile( r"^(\w+)(?:[ \t]*=[ \t]*)(.*?)$",      re.I | re.M )
# Make this gnarly and complicated since configure.defaults has no standard formatting
#                            v start  v OS      V typical   v MACOS
osAndArch     = re.compile( r"^ARCH[ ]+(\w+)[ ]+((?:\w+.*?),|(?:[(].*?[)]))",         re.I )
# Just grab the first two words, thats what you get
osAndArchAlt  = re.compile( r"^ARCH[ ]+(\w+)[ ]+(\w+)",         re.I )

referenceVar  = re.compile( r"[$][(](\w+)[)]", re.I )

class Stanza():
  
  def __init__( self, lines ) :
    self.lines_   = lines
    self.os_      = None
    self.archs_   = []
    self.kvPairs_ = {}

  def parse( self ) :
    # First get os & archs
    osarchMatch = osAndArch.match( self.lines_ )

    if osarchMatch is None :
      osarchMatch = osAndArchAlt.match( self.lines_ )
      if osarchMatch is None :
        print( "Could not find OS and architecture info in " + self.lines_.partition("\n")[0] )
      
    self.os_    = osarchMatch.group(1)
    self.archs_ = osarchMatch.group(2).strip(",").split( " " )
    
    for kvPairMatch in kvPair.finditer( self.lines_ ) :
      self.kvPairs_[ kvPairMatch.group(1) ] = kvPairMatch.group(2)
      self.removeComments( kvPairMatch.group(1) )
    
    # Now sanitize
    self.sanitize()
  
  def dereference( self, field, fatal=False ) :
    if field in self.kvPairs_ :
      prevField = self.kvPairs_[field]

      for refVarIter in referenceVar.finditer( prevField ) :
        if refVarIter is not None :
          # Grab group 1 and check that it is in our kv pairs
          refVar = refVarIter.group(1)
          if refVar not in self.kvPairs_ :
            if fatal :
              print( "Could not rereference : " + refVar )
              exit(1)
            else:
              continue

          # Recursively deref
          self.dereference( refVar, fatal )

          # Replace in original
          self.kvPairs_[field] = self.kvPairs_[field].replace(
                                                              "$({var})".format( var=refVar ),
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

  def sanitize( self ) :
    # Fix problematic variables
    self.dereference( "DM_FC" )
    self.dereference( "DM_CC" )
    self.removeReferences( "FCBASEOPTS_NO_G" )
    # Get rid of all these mixed up flags, these are handled by cmake natively or 
    # just in the wrong place
    self.removeReferences( "FCBASEOPTS", [ "FCDEBUG", "FORMAT_FREE", "BYTESWAPIO", ] )
    # Now deref
    self.dereference( "FCBASEOPTS" )

    # Now fix certain ones that are mixing programs with flags all mashed into one option
    self.splitIntoFieldAndFlags( "DM_FC" )
    self.splitIntoFieldAndFlags( "DM_CC" )
    self.splitIntoFieldAndFlags( "M4" )

    # Remove rogue compile commands that should *NOT* even be here
    keysToSanitize = [ 
                      "ARFLAGS",
                      "CFLAGS_LOCAL",
                      "CPP",
                      "ESMF_LDFLAG",
                      "LDFLAGS_LOCAL",
                      "MODULE_SRCH_FLAG",
                      "RLFLAGS",
                      "TRADFLAG",
                      "FCBASEOPTS",
                      "FCBASEOPTS_NO_G",
                      "FCOPTIM",
                      "FORMAT_FIXED",
                      "FORMAT_FREE"
                      ]

    for keyToSan in keysToSanitize :
      if keyToSan in self.kvPairs_ :
        self.kvPairs_[ keyToSan ] = self.kvPairs_[ keyToSan ].replace( "-c", "" )
    


    # Now deref all the rest
    for key in self.kvPairs_ :
      self.dereference( key )
      # And for final measure strip
      self.kvPairs_[ key ] = self.kvPairs_[ key ].strip()

  def __str__( self ):
    # base = """OS {os:<8} ARCHITECTURES {archs:<20}
    #           >>  SFC    = {SFC:<12}
    #           >>  SCC    = {SCC:<12}
    #           >>  CCOMP  = {CCOMP:<12}
    #           >>  DM_FC  = {DM_FC:<12}
    #           >>  DM_CC  = {DM_CC:<12}
    #        """
    base = """{rec} {os:<10} {SFC:<11} / {SCC:<11} / {CCOMP:<11} / {DM_FC:<11} / {DM_CC:<11}"""
    text = inspect.cleandoc( base ).format( 
                                            os=str(self.os_),
                                            rec=( "!!" if platform.system().lower() != self.os_.lower() else "Ok" ),
                                            # archs=str(self.archs_),
                                            SFC=str( self.kvPairs_["SFC"].partition(" ")[0] ),
                                            SCC=str( self.kvPairs_["SCC"].partition(" ")[0] ),
                                            CCOMP=str( self.kvPairs_["CCOMP"].partition(" ")[0] ),
                                            DM_FC=str( self.kvPairs_["DM_FC"].partition(" ")[0] ),
                                            DM_CC=str( self.kvPairs_["DM_CC"].partition(" ")[0] )
                                            )
    # text   += "\n" + "\n".join( [ "{key:<18} = {value}".format( key=key, value=value) for key, value in self.kvPairs_.items() ] ) 
    return text

  @staticmethod
  def findFirstDifference( rhStanza, lhStanza, maxLength=32 ) :
    diff  = False
    value = ""
    valuesToCheck = [
                      "ARCH_LOCAL",
                      "BYTESWAPIO",
                      "CFLAGS_LOCAL",
                      "DM_CC",
                      "DM_FC",
                      "DM_FC_FLAGS",
                      "DM_CC_FLAGS",
                      "FCBASEOPTS",
                      "FCDEBUG",
                      "FCNOOPT",
                      "FCOPTIM",
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


def getStringOptionSelection( topLevelCmake, searchString ) :
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

  optionsFmt = ", ".join( [ "{idx} : {opt}".format( idx=options.index( opt ), opt=opt ) for opt in options ] )
  selection  = int( input( "Select option from {optionsStr} [0-{max}] ({opts}) : ".format( optionsStr=searchString, max=len(options)-1, opts=optionsFmt ) ) )

  if selection < 0 or selection > len(options) :
    print( "Invalid option selection for " + searchString +  "!" )
    exit(1)
  
  return options[selection]

def generateCMakeToolChainFile( cmakeToolChainTemplate, output, stanza, optionsDict={} ) :
  cmakeToolChainTemplateFP    = open( cmakeToolChainTemplate, "r" )
  cmakeToolChainTemplateLines = cmakeToolChainTemplateFP.read()
  cmakeToolChainTemplateFP.close()

  configStanza = cmakeToolChainTemplateLines.format( 
                                                    ARCH_LOCAL=stanza.kvPairs_["ARCH_LOCAL"],
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
                                                    SFC=stanza.kvPairs_["SFC"]
                                                    )

  # Extra stufff not from stanza but options
  fmtOption = "set( {opt:<32} {value:<12} CACHE STRING \"Set by configuration\" FORCE )"
  configStanza += "\n" + "\n".join( [ fmtOption.format( opt=key, value=value ) for key, value in optionsDict.items() ] )

  outputFP = open( output, "w" )
  outputFP.write( configStanza )
  outputFP.close()

def main() :
  configFile         = sys.argv[1]
  cmakeTemplateFile  = sys.argv[2]
  cmakeConfigFile    = sys.argv[3]
  cmakeFile          = sys.argv[4]

  coreOption       = getStringOptionSelection( cmakeFile, "WRF_CORE_OPTIONS"    )
  nestingOption    = getStringOptionSelection( cmakeFile, "WRF_NESTING_OPTIONS" )
  caseOption       = getStringOptionSelection( cmakeFile, "WRF_CASE_OPTIONS" )
  
  # These are yes
  yesValues    = [ "yes", "y", "true", "1" ]

  #!TODO Expand this for all wrf options
  useMPI       = input( "[DM] Use MPI?    [Y/n] : " ).lower() in yesValues
  useOpenMP    = input( "[SM] Use OpenMP? [Y/n] : " ).lower() in yesValues


  fp    = open( configFile, 'r' )
  lines = fp.read()
  fp.close()

  # Now grab the blocks and parse
  stanzas = []
  uniqueConfigs = {}
  stanzaIdx = 0
  for stanzaBlock in archBlock.finditer( lines ) :
    stanza = Stanza( stanzaBlock.group(1) )
    stanza.parse()

    stanzas.append( stanza )
    stanzaConfig = str( stanza )
    stanzaId = "{idx:<3} ".format( idx=stanzaIdx )
    if stanzaConfig not in uniqueConfigs :
      uniqueConfigs[ stanzaConfig ] = { "stanza" : stanza, "idx" : stanzaIdx }
      print( stanzaId + stanzaConfig + "[first entry]" )
    else :
      diff, value = Stanza.findFirstDifference( uniqueConfigs[ stanzaConfig ]["stanza"], stanza )
      if diff :
        print( stanzaId + stanzaConfig + "@{idx} diff => {value}".format( idx=uniqueConfigs[ stanzaConfig ][ "idx" ], value=value ) )
      else :
        print( stanzaId + stanzaConfig + "[no difference]" )
    stanzaIdx += 1
    
  print( "!! - Not recommended for your system" )

  idxSelection = int( input( "Select configuration [0-{stop}] (note !!)  : ".format( stop=( stanzaIdx-1) ) ) )
  if idxSelection < 0 or idxSelection > stanzaIdx - 1 :
    print( "Invalid configuration selection!" )
    exit(1)

  additionalOptions = {
                        "WRF_CORE"    : coreOption,
                        "WRF_NESTING" : nestingOption,
                        "WRF_CASE"    : caseOption,
                        "USE_MPI"     : "ON" if useMPI    else "OFF",
                        "USE_OPENMP"  : "ON" if useOpenMP else "OFF"
                        }
  generateCMakeToolChainFile( cmakeTemplateFile, cmakeConfigFile, stanzas[idxSelection], additionalOptions )




if __name__ == '__main__' :
  main()

