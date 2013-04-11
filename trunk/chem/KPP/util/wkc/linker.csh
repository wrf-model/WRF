#!/bin/csh -f


if ( $#argv != 1 ) then
Error: Usage: linker.csh option 
exit
endif 

set files=( data.c data.h misc.c my_strtok.c  protos.h reg_parse.c registry.h  type.c sym.c sym.h symtab_gen.c )


foreach file ( $files )
if  ( $argv[1] == 'link' ) ln -s ../../../../tools/$file
if  ( $argv[1] == 'unlink' ) rm -f $file
end


exit
 
