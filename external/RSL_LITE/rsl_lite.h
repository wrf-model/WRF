#ifndef CRAY
# ifdef NOUNDERSCORE
#      define RSL_LITE_ERROR_DUP1 rsl_error_dup1
#      define BYTE_BCAST byte_bcast
#      define RSL_LITE_INIT_EXCH rsl_lite_init_exch
#      define RSL_LITE_EXCH_Y rsl_lite_exch_y
#      define RSL_LITE_EXCH_X rsl_lite_exch_x
#      define RSL_LITE_PACK  rsl_lite_pack
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
# else
#   ifdef F2CSTYLE
#      define RSL_LITE_ERROR_DUP1 rsl_error_dup1__
#      define BYTE_BCAST byte_bcast__
#      define RSL_LITE_INIT_EXCH rsl_lite_init_exch__
#      define RSL_LITE_EXCH_Y rsl_lite_exch_y__
#      define RSL_LITE_EXCH_X rsl_lite_exch_x__
#      define RSL_LITE_PACK  rsl_lite_pack__
#      define RSL_LITE_BCAST_MSGS rsl_lite_bcast_msgs__
#      define RSL_LITE_TO_CHILD_MSG rsl_lite_to_child_msg__
#      define RSL_LITE_TO_CHILD_INFO rsl_lite_to_child_info__
#      define RSL_LITE_FROM_PARENT_MSG rsl_lite_from_parent_msg__
#      define RSL_LITE_FROM_PARENT_INFO rsl_lite_from_parent_info__
#      define RSL_LITE_MERGE_MSGS rsl_lite_merge_msgs__
#      define RSL_LITE_TO_PARENT_MSG rsl_lite_to_parent_msg__
#      define RSL_LITE_TO_PARENT_INFO rsl_lite_to_parent_info__
#      define RSL_LITE_FROM_CHILD_MSG rsl_lite_from_child_msg__
#      define RSL_LITE_FROM_CHILD_INFO rsl_lite_from_child_info
#      define RSL_INTERNAL_MILLICLOCK rsl_internal_milliclock__
#      define RSL_INTERNAL_MICROCLOCK rsl_internal_microclock__
#      define TASK_FOR_POINT task_for_point__
#   else
#      define RSL_LITE_ERROR_DUP1 rsl_error_dup1_
#      define BYTE_BCAST byte_bcast_
#      define RSL_LITE_INIT_EXCH rsl_lite_init_exch_
#      define RSL_LITE_EXCH_Y rsl_lite_exch_y_
#      define RSL_LITE_EXCH_X rsl_lite_exch_x_
#      define RSL_LITE_PACK  rsl_lite_pack_
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
#   endif
# endif
#endif

#define RSL_SENDBUF 0
#define RSL_RECVBUF 1
#define RSL_FREEBUF 3
#define RSL_MAXPROC 10000
#define RSL_INVALID -1

#define RSL_MALLOC(T,N)  (T *)rsl_malloc(__FILE__,__LINE__,(sizeof(T))*(N))
#define RSL_FREE(P)      rsl_free(P)

char * buffer_for_proc ( int P, int size, int code ) ;
void * rsl_malloc( char * f, int l, int s ) ;
typedef int * int_p ;

#define INDEX_2(A,B,NB)       ( (B) + (A)*(NB) )
#define INDEX_3(A,B,NB,C,NC)  INDEX_2( (A), INDEX_2( (B), (C), (NC) ), (NB)*(NC) )

#define RSL_FATAL(N)     MPI_Abort(MPI_COMM_WORLD, 9)
#define RSL_TEST_ERR(T,M) {if(T){fprintf(stderr,"rsl_lite error (\"%s\":%d) %s\n",__FILE__,__LINE__,M);RSL_FATAL(5);}}

#ifndef MPI2_SUPPORT
typedef int MPI_Fint;
#  define MPI_Comm_c2f(comm) (MPI_Fint)(comm)
#  define MPI_Comm_f2c(comm) (MPI_Comm)(comm)
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

