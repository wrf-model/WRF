#!/bin/csh

/bin/rm -f good.* bad.*

foreach choyce ( 1 2 3 4 )
  unset subopt
  set subopt = ( 0 )
  if ( $choyce == 1 || $choyce == 2 ) then
    set subopt = ( y n )
  endif
  foreach subchoyce ( $subopt )
     clean -a
     if ( $subchoyce == 0 ) then
       echo $choyce | config_new -d
     else
       config_new -d << H1
$choyce
$subchoyce
H1
     endif
     compile wrf >& seeit.$choyce.$subchoyce
     if ( -x main/wrf.exe ) then
       touch good.$choyce.$subchoyce
     else
       touch bad.$choyce.$subchoyce
     endif
  end
end

