undef("change_date")
function change_date(ccyy:integer, mm:integer, dd:integer, delta:integer)
local mmday, newday
begin

   mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)
   if ( isleapyear(ccyy) ) then
      mmday(1) = 29
   end if
   dd = dd + delta
   if ( dd .eq. 0 ) then
      mm = mm - 1
      if ( mm .eq. 0 ) then
         mm = 12
         ccyy = ccyy - 1
      end if
      dd = mmday(mm-1)
   else
      if ( dd .gt. mmday(mm-1) ) then
         dd = 1
         mm = mm + 1
         if ( mm .gt. 12 ) then
            mm = 1
            ccyy = ccyy + 1
         end if
      end if
   end if
   newday = ccyy*10000 + mm*100 + dd
   ; newday = sprinti("%0.4i",ccyy)+sprinti("%0.2i",mm)+sprinti("%0.2i",dd)
   return(newday)

end 

undef("advance_cymdh")
function advance_cymdh(currentdatestr:string, dh:integer)
local ccyy, mm, dd, hh, newday, newdatestr
begin

   currentdate = stringtochar(currentdatestr)
   ccyy = stringtointeger((/currentdate(0:3)/))
   mm   = stringtointeger((/currentdate(4:5)/))
   dd   = stringtointeger((/currentdate(6:7)/))
   hh   = stringtointeger((/currentdate(8:9)/))
   hh = hh + dh
   newday = ccyy*10000 + mm*100 + dd
   ; newday = sprinti("%0.4i",ccyy)+sprinti("%0.2i",mm)+sprinti("%0.2i",dd)
   do while (hh .lt. 0)
      hh = hh + 24
      newday = change_date( ccyy, mm, dd, -1 )
   end do
   do while (hh .gt. 23)
      hh = hh - 24
      newday = change_date( ccyy, mm, dd, 1 )
   end do
   ;   newdate = newday*100 + hh
   newdatestr = sprinti("%0.8i",newday) + sprinti("%0.2i",hh)
   return(newdatestr)

end
