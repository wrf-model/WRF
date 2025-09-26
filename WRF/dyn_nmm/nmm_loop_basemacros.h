! these define the various loop range variables
! that were defined in module_MPP. Defined as macros
! here to allow thread-safety/tile callability

#define MY_IS(A,B) max(ids+(A),its-(B))
#define MY_IE(A,B) min(ide-(A),ite+(B))
#define MY_JS(A,B) max(jds+(A),jts-(B))
#define MY_JE(A,B) min(jde-(A),jte+(B))

