#ifndef KDATA_H
#include "registry.h"



typedef struct knode_struct {

  char          name[NAMELEN] ;
  struct knode_struct * members ;
  struct knode_struct * next ;


  char          wrf_name[NAMELEN] ;

  struct knode_struct * assoc_wrf_pack ;

  char       assoc_wrf_name[NAMELEN] ;

  /* flag whether a matching varname was found */
  int found_match ;

  /* flag whether variable is declared as radical species */
  int is_radical ;

  /* flag if M (third body conc) is found in .spc file */
  int got_air ;

  /* flag if N2 is found in .spc file */
  int got_n2 ;

  /* flag if O2 is found in .spc file */
  int got_o2 ;

  /* flag if CO2 is found in .spc file */
  int got_co2 ;

} knode_t ;

#ifndef DEFINE_GLOBALS
#  define EXTERN extern
#else
#  define EXTERN
#endif



/* store chemistry packages (mechanisms) from WRF in linked list rooted at WRFC_packs  
   - species variables will be stored as members of each package */
EXTERN knode_t * WRFC_packs ;



/* store chemistry packages (mechanisms) from KPP */
EXTERN knode_t * KPP_packs ;


/* non-transported radicals are not part of the packages */
EXTERN knode_t * WRFC_radicals ;


/* photolysis rates from Registry */
EXTERN knode_t * WRFC_jvals ;








#define KDATA_H
#endif
