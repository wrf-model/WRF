 
#  ------------------------------------------------------------------------
#  This script will FORTRAN block or unblock BUFR files on a number of
#  standard computing platforms. Stictly speaking, real BUFR files are
#  unblocked, that is, a byte stream containing only allowable BUFR
#  constructs. On some platforms it is advantagous to use the FORTRAN
#  blocked structure for I/O efficiency, and on some platforms, when
#  using FORTRAN I/O, the unblocked structure is FORTRAN UN-readable.
#  NOTE: The script is set up to run in the Bourne shell. If you are a
#  C-shell user, enter 'sh ./cwordsh'.
#  ------------------------------------------------------------------------
#  cwordsh: <action> <inputfile> <outputfile>
#
#  where:
#
#  <action>     can be block or unblk
#  <inputfile>  [path/]filename of input file
#  <outputfile> [path/]filename of output file
#  ------------------------------------------------------------------------

[ $# -ne 3 ] && { echo; echo "$0: <action> <inputfile> <outputfile>";
                  echo;
                  echo "where:";
                  echo;
                  echo "<action>     can be block or unblk";
                  echo "<inputfile>  [path/]filename of input file";
                  echo "<outputfile> [path/]filename of output file";
                  echo; exit 99; }

CWRD=.

cat<<eof|$CWRD/cwordsh.x
$1
$2
$3
eof
