from collections import OrderedDict
import re

def load_nml( filename ):
  """Read a strict subset of Fortran 90 namelist
  This ONLY reads namelists in the format:
  &group
  key0 = value0,
  key1  =  value0, value1,
  key2='value0', 'value1',
  key3  =   "value0", "value1",
  /
  ...and so on...
  Index slices, multiple key-value pairs on the same line,
  and other features are NOT supported. For full support
  please utilize f90nml.
  Spaces between key and value and commas does not matter.
  Multivalue entries MUST be delimited by commas.
  """
  contents = ""
  with open( filename, "r" ) as f:
    contents = f.read()

  nml = OrderedDict()
  for match in re.finditer( r"&(?P<group>\w+)(?P<kv_pairs>.*?)\n*^[ ]*/", contents, re.S | re.M ):
    group = match.group( "group" )
    nml[group] = OrderedDict()
    for kv_match in re.finditer( r"^[ ]+(?P<key>\w+)[ ]*=[ ]*(?P<value>.*?)$", match.group("kv_pairs"), re.M ):
      key = kv_match.group( "key" )
      values = list(
                    filter(
                          None,
                          [
                            s.strip(" " ) # keep quotes for nml strings
                            for s in list( filter( None, kv_match.group( "value" ).split(",") ) )
                          ]
                          )
                    )
      nml[group][key] = values if len( values ) > 1 else values[0]
  return nml

def dump_nml( filename, nml ):
  contents = ""
  try:
    for group, kv in nml.items():
      contents += f"&{group}\n"
      for key, value in kv.items():
        contents += f" {key} = "
        if isinstance( value, list ):
          contents += ", ".join( [str(x) for x in value] )
        else:
          contents += str(value)
        contents += ",\n"
      contents += " /\n\n"
    with open( filename, "w+" ) as f:
      f.write( contents )
  except Exception as e:
    msg  = "Failed to write namelist:\n"
    msg += str(nml)
    print( msg )
    raise e
