#ifndef PROTOS_H
#include "registry.h"
#include "data.h"

int init_dim_table()   ;
int make_lower( char * s1 ) ;
int reg_parse( FILE * infile ) ;
int set_dim_len ( char * dimspec , node_t * dim_entry ) ;
int set_dim_order ( char * dimorder , node_t * dim_entry ) ;
int set_dim_orient ( char * dimorient , node_t * dim_entry ) ;
int add_node_to_end ( node_t * node , node_t ** list ) ;
int add_node_to_end_4d ( node_t * node , node_t ** list ) ;
int init_type_table() ;
int set_state_type ( char * typename , node_t * node ) ;
int set_state_dims ( char * dims , node_t * node ) ;
int gen_state_struct ( char * fname ) ;

#if 0
int show_node( node_t * p ) ;
int show_node1( node_t * p, int indent ) ;
int show_nodelist( node_t * p ) ;
int show_nodelist1( node_t * p , int indent ) ;
#endif

int gen_state_struct ( char * fname ) ;
int gen_decls ( FILE * fp ,  node_t * node , int sw_ranges, int sw_point , int mask , int layer ) ;
int gen_state_subtypes ( char * fname ) ;
int gen_state_subtypes1 ( FILE * fp , node_t * node , int sw_ranges, int sw_point , int mask ) ;
int print_warning( FILE * fp , char * fname ) ;
int close_the_file( FILE * fp  ) ;
int make_entries_uniq ( char * fname ) ;
int add_warning ( char * fname ) ;

node_t * get_type_entry ( char * typename ) ;
node_t * get_rconfig_entry( char * name ) ;
node_t * get_entry ( char * name , node_t * node ) ;
node_t * get_entry_r ( char * name , char * use , node_t * node ) ;
node_t * get_dim_entry( char *s ) ;
node_t * new_node ( int kind ) ;

node_t * get_4d_entry ( char * name ) ;
node_t * get_dimnode_for_coord ( node_t * node , int coord_axis ) ;
int      get_index_for_coord ( node_t * node , int coord_axis ) ;

char * my_strtok( char * s1 ) ;
char * strtok_rentr( char * s1 , char * s2, char ** tokpos ) ;

char * bdy_indicator( int bdy ) ;

char * field_name( char * tmp, node_t * p , int tag ) ;
char * field_name_bdy( char * tmp, node_t * p , int tag, int bdy  ) ;
char * dimension_with_colons( char * pre, char * tmp, node_t * p, char * post) ;
char * dimension_with_ones( char * pre, char * tmp, node_t * p, char * post) ;
char * dimension_with_ranges( char * ref , char * pre, int bdy , char * tmp, node_t * p, char * post, char * nlstructname ) ;
char * arrray_size_expression( char * refarg , char * pre , int bdy , char * tmp , node_t * p , char * post , char * nlstructname  ) ;
char * index_with_firstelem( char * pre , char * dref , int bdy , char * tmp , node_t * p , char * post ) ;

char * declare_array_as_pointer( char * tmp, node_t * p ) ;
char * field_type( char * tmp , node_t * p ) ;

/* For typedef history -ajb */
int init_typedef_history() ;
int add_typedef_name ( char * name ) ;
int get_num_typedefs() ;
char * get_typedef_name ( char * name ) ;
char * get_typename_i(int i) ;

int gen_alloc ( char * dirname ) ;
int gen_alloc1 ( char * dirname ) ;
int gen_alloc2 ( FILE * fp , char * structname , node_t * node , int sw ) ;

int gen_module_state_description ( char * dirname ) ;
int gen_module_state_description1 ( FILE * fp , node_t * node ) ;

int gen_scalar_indices ( char * dirname ) ;
int gen_scalar_indices1 ( FILE * fp, FILE * fp2 ) ;

int gen_actual_args ( char * dirname ) ;
int gen_dummy_args ( char * dirname ) ;
int gen_dummy_decls ( char * dn ) ;
int gen_args ( char * dirname , int sw ) ;
int gen_args1 ( FILE * fp , char * outstr, char * structname , node_t * node , int *linelen , int sw , int deep ) ;

int gen_scalar_derefs ( char * dirname ) ;
int scalar_derefs ( char * dirname ) ;
int scalar_derefs1 ( FILE * fp , node_t * node, int direction ) ;

int set_mark ( int val , node_t * lst ) ;
int set_mark_4d ( int val , node_t * lst ) ;

int gen_i1_decls ( char * dn ) ;
int gen_get_nl_config ( char * dirname ) ;

int gen_config_assigns ( char * dirname ) ;
int gen_config_reads ( char * dirname ) ;

char * set_mem_order( node_t * node , char * str , int n  ) ;

int gen_wrf_io ( char * dirname ) ;
int set_dim_strs ( node_t *node , char ddim[3][2][NAMELEN], char mdim[3][2][NAMELEN], char pdim[3][2][NAMELEN] , char * prepend, int sw_allow_stagger ) ;
int gen_wrf_io2 ( FILE * fp , char * fname , char * structname , char * fourdname , node_t * node , int io_mask , int sw_io ) ;

int gen_namelist_defines ( char * dirname , int sw_dimension ) ;
int gen_namelist_defaults ( char * dirname ) ;
int gen_namelist_script ( char * dirname ) ;

int gen_model_data_ord ( char * dirname ) ;

int get_elem ( char * structname , char * nlstructname , char * tx , int i , node_t * p , int first_last ) ;

int associated_with_4d_array( node_t * p ) ;



#define PROTOS_H
#endif

