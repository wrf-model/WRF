#ifndef CRAY
# ifdef NOUNDERSCORE
#      define RSL_LITE_ERROR_DUP1 rsl_error_dup1
#      define BYTE_BCAST byte_bcast
#      define BYTE_BCAST_FROM_ROOT byte_bcast_from_root
#      define RSL_LITE_INIT_EXCH rsl_lite_init_exch
#      define RSL_LITE_EXCH_Y rsl_lite_exch_y
#      define RSL_LITE_EXCH_X rsl_lite_exch_x
#      define RSL_LITE_PACK  rsl_lite_pack
#if ( WRFPLUS == 1 )
#      define RSL_LITE_PACK_AD  rsl_lite_pack_ad
#endif
#      define RSL_LITE_BCAST_MSGS rsl_lite_bcast_msgs
#      define RSL_LITE_TO_CHILD_MSG rsl_lite_to_child_msg
#      define RSL_LITE_TO_CHILD_INFO rsl_lite_to_child_info
#      define RSL_LITE_FROM_PARENT_MSG rsl_lite_from_parent_msg
#      define RSL_LITE_FROM_PARENT_INFO rsl_lite_from_parent_info
#      define RSL_LITE_MERGE_MSGS rsl_lite_merge_msgs
#      define RSL_LITE_TO_PARENT_MSG rsl_lite_to_parent_msg
#      define RSL_LITE_TO_PARENT_INFO rsl_lite_to_parent_info
#      define RSL_LITE_FROM_CHILD_MSG rsl_lite_from_child_msg
#      define RSL_LITE_FROM_CHILD_INFO rsl_lite_from_child_info
#      define RSL_INTERNAL_MILLICLOCK rsl_internal_milliclock
#      define RSL_INTERNAL_MICROCLOCK rsl_internal_microclock
#      define TASK_FOR_POINT task_for_point
#      define TASK_FOR_POINT_MESSAGE task_for_point_message
#      define RSL_LITE_INIT_PERIOD rsl_lite_init_period
#      define RSL_LITE_EXCH_PERIOD_Y rsl_lite_exch_period_y
#      define RSL_LITE_EXCH_PERIOD_X rsl_lite_exch_period_x
#      define RSL_LITE_PACK_PERIOD  rsl_lite_pack_period
#      define RSL_LITE_INIT_SWAP rsl_lite_init_swap
#      define RSL_LITE_SWAP rsl_lite_swap
#      define RSL_LITE_PACK_SWAP  rsl_lite_pack_swap
#      define RSL_LITE_INIT_CYCLE rsl_lite_init_cycle
#      define RSL_LITE_CYCLE rsl_lite_cycle
#      define RSL_LITE_PACK_CYCLE rsl_lite_pack_cycle
#      define F_PACK_LINT f_pack_lint
#      define F_PACK_INT f_pack_int
#      define F_UNPACK_LINT f_unpack_lint
#      define F_UNPACK_INT f_unpack_int
#if ( WRFPLUS == 1 )
#      define F_PACK_LINT_AD f_pack_lint_ad
#      define F_PACK_INT_AD f_pack_int_ad
#      define F_UNPACK_LINT_AD f_unpack_lint_ad
#      define F_UNPACK_INT_AD f_unpack_int_ad
#endif
#      define RSL_LITE_GET_HOSTNAME rsl_lite_get_hostname
#      define RSL_LITE_NESTING_RESET rsl_lite_nesting_reset
# else
#   ifdef F2CSTYLE
#      define RSL_LITE_ERROR_DUP1 rsl_error_dup1__
#      define BYTE_BCAST byte_bcast__
#      define BYTE_BCAST_FROM_ROOT byte_bcast_from_root__
#      define RSL_LITE_INIT_EXCH rsl_lite_init_exch__
#      define RSL_LITE_EXCH_Y rsl_lite_exch_y__
#      define RSL_LITE_EXCH_X rsl_lite_exch_x__
#      define RSL_LITE_PACK  rsl_lite_pack__
#if ( WRFPLUS == 1 )
#      define RSL_LITE_PACK_AD  rsl_lite_pack_ad__
#endif
#      define RSL_LITE_BCAST_MSGS rsl_lite_bcast_msgs__
#      define RSL_LITE_TO_CHILD_MSG rsl_lite_to_child_msg__
#      define RSL_LITE_TO_CHILD_INFO rsl_lite_to_child_info__
#      define RSL_LITE_FROM_PARENT_MSG rsl_lite_from_parent_msg__
#      define RSL_LITE_FROM_PARENT_INFO rsl_lite_from_parent_info__
#      define RSL_LITE_MERGE_MSGS rsl_lite_merge_msgs__
#      define RSL_LITE_TO_PARENT_MSG rsl_lite_to_parent_msg__
#      define RSL_LITE_TO_PARENT_INFO rsl_lite_to_parent_info__
#      define RSL_LITE_FROM_CHILD_MSG rsl_lite_from_child_msg__
#      define RSL_LITE_FROM_CHILD_INFO rsl_lite_from_child_info__
#      define RSL_INTERNAL_MILLICLOCK rsl_internal_milliclock__
#      define RSL_INTERNAL_MICROCLOCK rsl_internal_microclock__
#      define TASK_FOR_POINT task_for_point__
#      define TASK_FOR_POINT_MESSAGE task_for_point_message__
#      define RSL_LITE_INIT_PERIOD rsl_lite_init_period__
#      define RSL_LITE_EXCH_PERIOD_Y rsl_lite_exch_period_y__
#      define RSL_LITE_EXCH_PERIOD_X rsl_lite_exch_period_x__
#      define RSL_LITE_PACK_PERIOD  rsl_lite_pack_period__
#      define RSL_LITE_INIT_SWAP rsl_lite_init_swap__
#      define RSL_LITE_SWAP rsl_lite_swap__
#      define RSL_LITE_PACK_SWAP  rsl_lite_pack_swap__
#      define RSL_LITE_INIT_CYCLE rsl_lite_init_cycle__
#      define RSL_LITE_CYCLE rsl_lite_cycle__
#      define RSL_LITE_PACK_CYCLE rsl_lite_pack_cycle__
#      define F_PACK_LINT f_pack_lint__
#      define F_PACK_INT f_pack_int__
#      define F_UNPACK_LINT f_unpack_lint__
#      define F_UNPACK_INT f_unpack_int__
#if ( WRFPLUS == 1 )
#      define F_PACK_LINT_AD f_pack_lint_ad__
#      define F_PACK_INT_AD f_pack_int_ad__
#      define F_UNPACK_LINT_AD f_unpack_lint_ad__
#      define F_UNPACK_INT_AD f_unpack_int_ad__
#endif
#      define RSL_LITE_GET_HOSTNAME rsl_lite_get_hostname__
#      define RSL_LITE_NESTING_RESET rsl_lite_nesting_reset__
#   else
#      define RSL_LITE_ERROR_DUP1 rsl_error_dup1_
#      define BYTE_BCAST byte_bcast_
#      define BYTE_BCAST_FROM_ROOT byte_bcast_from_root_
#      define RSL_LITE_INIT_EXCH rsl_lite_init_exch_
#      define RSL_LITE_EXCH_Y rsl_lite_exch_y_
#      define RSL_LITE_EXCH_X rsl_lite_exch_x_
#      define RSL_LITE_PACK  rsl_lite_pack_
#if ( WRFPLUS == 1 )
#      define RSL_LITE_PACK_AD  rsl_lite_pack_ad_
#endif
#      define RSL_LITE_BCAST_MSGS rsl_lite_bcast_msgs_
#      define RSL_LITE_TO_CHILD_MSG rsl_lite_to_child_msg_
#      define RSL_LITE_TO_CHILD_INFO rsl_lite_to_child_info_
#      define RSL_LITE_FROM_PARENT_MSG rsl_lite_from_parent_msg_
#      define RSL_LITE_FROM_PARENT_INFO rsl_lite_from_parent_info_
#      define RSL_LITE_MERGE_MSGS rsl_lite_merge_msgs_
#      define RSL_LITE_TO_PARENT_MSG rsl_lite_to_parent_msg_
#      define RSL_LITE_TO_PARENT_INFO rsl_lite_to_parent_info_
#      define RSL_LITE_FROM_CHILD_MSG rsl_lite_from_child_msg_
#      define RSL_LITE_FROM_CHILD_INFO rsl_lite_from_child_info_
#      define RSL_INTERNAL_MILLICLOCK rsl_internal_milliclock_
#      define RSL_INTERNAL_MICROCLOCK rsl_internal_microclock_
#      define TASK_FOR_POINT task_for_point_
#      define TASK_FOR_POINT_MESSAGE task_for_point_message_
#      define RSL_LITE_INIT_PERIOD rsl_lite_init_period_
#      define RSL_LITE_EXCH_PERIOD_Y rsl_lite_exch_period_y_
#      define RSL_LITE_EXCH_PERIOD_X rsl_lite_exch_period_x_
#      define RSL_LITE_PACK_PERIOD  rsl_lite_pack_period_
#      define RSL_LITE_INIT_SWAP rsl_lite_init_swap_
#      define RSL_LITE_SWAP rsl_lite_swap_
#      define RSL_LITE_PACK_SWAP rsl_lite_pack_swap_
#      define RSL_LITE_INIT_CYCLE rsl_lite_init_cycle_
#      define RSL_LITE_CYCLE rsl_lite_cycle_
#      define RSL_LITE_PACK_CYCLE rsl_lite_pack_cycle_
#      define F_PACK_LINT f_pack_lint_
#      define F_PACK_INT f_pack_int_
#      define F_UNPACK_LINT f_unpack_lint_
#      define F_UNPACK_INT f_unpack_int_
#if ( WRFPLUS == 1 )
#      define F_PACK_LINT_AD f_pack_lint_ad_
#      define F_PACK_INT_AD f_pack_int_ad_
#      define F_UNPACK_LINT_AD f_unpack_lint_ad_
#      define F_UNPACK_INT_AD f_unpack_int_ad_
#endif
#      define RSL_LITE_GET_HOSTNAME rsl_lite_get_hostname_
#      define RSL_LITE_NESTING_RESET rsl_lite_nesting_reset_
#   endif
# endif
#endif

#define RSL_SENDBUF 0
#define RSL_RECVBUF 1
#define RSL_FREEBUF 3
#define RSL_MAXPROC 100001
#define RSL_INVALID -1

/* this must be the same as defined in frame/module_driver_constants.F */
#define   DATA_ORDER_XYZ   1
#define   DATA_ORDER_YXZ   2
#define   DATA_ORDER_ZXY   3
#define   DATA_ORDER_ZYX   4
#define   DATA_ORDER_XZY   5
#define   DATA_ORDER_YZX   6


#define RSL_MALLOC(T,N)  (T *)rsl_malloc(__FILE__,__LINE__,(sizeof(T))*(N))
#define RSL_FREE(P)      rsl_free((char **)&(P))

char * buffer_for_proc ( int P, int size, int code ) ;
int buffer_size_for_proc( int P, int code );

void * rsl_malloc( char * f, int l, int s ) ;
void rsl_free( char ** p ) ;
typedef int * int_p ;

#define INDEX_2(A,B,NB)       ( (B) + (A)*(NB) )
#define INDEX_3(A,B,NB,C,NC)  INDEX_2( (A), INDEX_2( (B), (C), (NC) ), (NB)*(NC) )

#ifndef STUBMPI
# define RSL_FATAL(N)     MPI_Abort(MPI_COMM_WORLD, 9)
#else
# define RSL_FATAL(N)     exit(9) ;
#endif
#ifndef MS_SUA
# define RSL_TEST_ERR(T,M) {if(T){fprintf(stderr,"rsl_lite error (\"%s\":%d) %s\n",__FILE__,__LINE__,M);RSL_FATAL(5);}}
#else
# define RSL_TEST_ERR(T,M) {if(T){RSL_FATAL(5);}}
#endif

typedef struct rsl_list {
  struct rsl_list * next ;
  void * data ;                 /* pointer to some node */
#ifdef crayx1
  int info1 ;                   /* blank info field */
  int info2 ;                   /* blank info field */
#else
  short info1 ;                 /* blank info field */
  short info2 ;                 /* blank info field */
#endif
} rsl_list_t ;


void F_PACK_LINT (long *inbuf, long *outbuf, int* memorder, int* js, int* je, int* ks, int* ke, int* is, int* ie, int* jms, int* jme, int* kms, int* kme, int* ims, int* ime, int* curs);
void F_PACK_INT (int *inbuf, int *outbuf, int* memorder, int* js, int* je, int* ks, int* ke, int* is, int* ie, int* jms, int* jme, int* kms, int* kme, int* ims, int* ime, int* curs);
void F_UNPACK_LINT (long *inbuf, long *outbuf, int* memorder, int* js, int* je, int* ks, int* ke, int* is, int* ie, int* jms, int* jme, int* kms, int* kme, int* ims, int* ime, int* curs);
void F_UNPACK_INT (int *inbuf, int *outbuf, int* memorder, int* js, int* je, int* ks, int* ke, int* is, int* ie, int* jms, int* jme, int* kms, int* kme, int* ims, int* ime, int* curs);

#if ( WRFPLUS == 1 )
void F_PACK_LINT_AD (long *inbuf, long *outbuf, int* memorder, int* js, int* je, int* ks, int* ke, int* is, int* ie, int* jms, int* jme, int* kms, int* kme, int* ims, int* ime, int* curs);
void F_PACK_INT_AD (int *inbuf, int *outbuf, int* memorder, int* js, int* je, int* ks, int* ke, int* is, int* ie, int* jms, int* jme, int* kms, int* kme, int* ims, int* ime, int* curs);
void F_UNPACK_LINT_AD (long *inbuf, long *outbuf, int* memorder, int* js, int* je, int* ks, int* ke, int* is, int* ie, int* jms, int* jme, int* kms, int* kme, int* ims, int* ime, int* curs);
void F_UNPACK_INT_AD (int *inbuf, int *outbuf, int* memorder, int* js, int* je, int* ks, int* ke, int* is, int* ie, int* jms, int* jme, int* kms, int* kme, int* ims, int* ime, int* curs);
#endif

int TASK_FOR_POINT ( int* i_p , int* j_p , int* ids_p, int* ide_p , int* jds_p, int* jde_p , int* npx_p , int* npy_p , int* Px_p, int* Py_p , int* minx_p, int* miny_p, int* ierr_p );
void TASK_FOR_POINT_MESSAGE();
