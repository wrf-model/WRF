#### Standard sed script ####
#
# get rid of single quotes after comments
# WARNING:  This will break if a quoted string is followed by a comment that has 
#           a single quote.  
/\!.*'/s/'//g
# Automatically add cpp __LINE__ and __FILE__ to calls to wrf_error_fatal().  
s/[Cc][Aa][Ll][Ll] *[Ww][Rr][Ff]_[Ee][Rr][Rr][Oo][Rr]_[Ff][Aa][Tt][Aa][Ll] *(/CALL wrf_error_fatal3 ( __FILE__ , __LINE__ , /
