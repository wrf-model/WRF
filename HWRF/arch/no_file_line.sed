#### This sed script does not pass __FILE__ and __LINE__ to wrf_error_fatal() ####
#### Use this when the Fortran compiler complains about long source lines,    ####
#### usually due to cpp translating __FILE__ to a full pathname.              ####
#
# get rid of single quotes after comments
# WARNING:  This will break if a quoted string is followed by a comment that has 
#           a single quote.  
/\!.*'/s/'//g
# DO NOT Automatically add cpp __LINE__ and __FILE__ to calls to wrf_error_fatal().  
# s/[Cc][Aa][Ll][Ll] *[Ww][Rr][Ff]_[Ee][Rr][Rr][Oo][Rr]_[Ff][Aa][Tt][Aa][Ll] *(/& __FILE__ , __LINE__ , /
