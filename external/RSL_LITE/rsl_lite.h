#define RSL_SENDBUF 0
#define RSL_RECVBUF 1
#define RSL_FREEBUF 3
#define RSL_MAXPROC 10000

#define RSL_MALLOC(T,N)  (T *)rsl_malloc(__FILE__,__LINE__,(sizeof(T))*(N))
#define RSL_FREE(P)      rsl_free(P)

char * buffer_for_proc ( int P, int size, int code ) ;
void * rsl_malloc( char * f, int l, int s ) ;

#define RSL_FATAL(N)     MPI_Abort(MPI_COMM_WORLD, 9)
#define RSL_TEST_ERR(T,M) {if(T){fprintf(stderr,"rsl_lite error (\"%s\":%d) %s\n",__FILE__,__LINE__,M);RSL_FATAL(5);}}

