#ifndef PROTOS_H_KPP
#include "kpp_data.h"

/* added for gen_kpp */
knode_t * new_knode ( ) ;

int add_knode_to_end ( knode_t * node , knode_t ** list ) ;
int gen_kpp (char * dirname1, char * dirname2);

int get_wrf_chem_specs ( ) ;
int get_wrf_radicals ( ) ;
int get_wrf_jvals ( );

int get_kpp_chem_specs (  char * kpp_dirname ) ;


int compare_kpp_to_species  ( char * kpp_dirname) ;


int run_kpp( char * dirname , char * kpp_version  );
void change_chem_Makefile( );


void gen_kpp_mechanism_driver ( );
void gen_kpp_call_to_mech_dr ( );
void gen_kpp_args_to_Update_Rconst ( );
void gen_kpp_interface( );


int debug_out( );

/* int copy_makefiles_kpp ( char * kpp_dirname ); */




/* added gen_kpp  utils */
void gen_kpp_warning( FILE * ofile, char * gen_by_name, char * cchar );
void gen_kpp_pass_down ( FILE * ofile, int is_driver );
void gen_kpp_decl ( FILE * ofile, int is_driver );
void gen_kpp_argl( FILE * ofile ,  knode_t * nl  );
void gen_kpp_argl_new( FILE * ofile ,  knode_t * nl  );
void gen_kpp_argd ( FILE * ofile, int is_driver );
void gen_kpp_decld ( FILE * ofile, int is_driver );
void gen_kpp_decl3d( FILE * ofile, knode_t * nl  );

/* added gen_kpp_interf  utils */
void decl_misc (  FILE * ofile );
void decl_jv (  FILE * ofile );
int count_members(  knode_t * nl );
void decl_jv_pointers (  FILE * ofile );
void decl_kwc_constants (  FILE * ofile );
void gen_map_jval(  FILE * ofile );
void gen_map_wrf_to_kpp (  FILE * ofile,  knode_t * nl );
void gen_map_kpp_to_wrf (  FILE * ofile,  knode_t * nl );
void gen_kpp_pargs( FILE * ofile, knode_t * nl  );
void gen_kpp_pdecl( FILE * ofile, knode_t * nl  );
void wki_prelim( FILE * ofile );
void wki_start_loop( FILE * ofile );
void wki_end_loop( FILE * ofile );
void wki_one_d_vars ( FILE * ofile, knode_t * pp );

#define PROTOS_H_KPP
#endif
