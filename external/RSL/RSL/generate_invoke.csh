# generates the case statement in invoke_pf.F
# Usage:  generate_invoke.csh n
#
# where n is the number of cases to generate.
# 
onintr cleanup

/bin/rm -f /tmp/foo1.$$

set n = $1

set i = 0 
while ( $i < $n )
  echo "," >> /tmp/foo1.$$
  @ i += 1
end

set i = 0
while ( $i < $n )
  @ ii = $i + 1
  sed "$ii,${n}s/.*,/&A($i),/" /tmp/foo1.$$ > /tmp/foo2.$$
  /bin/mv /tmp/foo2.$$ /tmp/foo1.$$
  @ i += 1
end

cat -n /tmp/foo1.$$ | \
sed -e 's/^/case/' -e 's/	/ : /' -e 's/,/(*f)(/' -e 's/,$/); break ;/' \
> /tmp/foo2.$$

echo 'case     0 : (*f)() ; break ;'
cat /tmp/foo2.$$


cleanup:

/bin/rm -f /tmp/foo[12].$$

