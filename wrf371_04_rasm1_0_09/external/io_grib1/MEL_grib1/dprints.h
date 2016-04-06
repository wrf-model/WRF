/* file:  dprints.h				12/30/96 Nakajima/SAIC/MRY
*/
#include <stdio.h>

#define p_char(expr)    fprintf(stdout," (char) " #expr "= %c\n",expr)
#define p_string(expr)  fprintf(stdout," (char *) " #expr "= %s\n",expr)
#define p_ushort(expr)  fprintf(stdout," (uns. short) " #expr "= %u\n",expr)
#define p_short(expr)   fprintf(stdout," (short) " #expr "= %d\n",expr)
#define p_int(expr)     fprintf(stdout," (int) " #expr "= %d\n",expr)
#define p_long(expr)    fprintf(stdout," (long) " #expr "= %ld\n",expr)
#define p_ulong(expr)   fprintf(stdout," (uns.long) " #expr "= %u\n",expr)
#define p_float(expr)   fprintf(stdout," (float) " #expr "= %.5f\n",expr)
#define p_double(expr)  fprintf(stdout," (double) " #expr "= %.5lf\n",expr)


#ifdef  VERBOSE	
/************************************************ 
 *    DEBUG IS DESIRED (compiled with -DVERBOSE)  
 ************************************************/
#define VERB_ON          	1
#define LIB_VERSION             	"verbose"
#define DISPLAY_GRIBHDR(gh)    	  	display_gribhdr(gh)
#define HDR_PRINT(str,addr,sz)  	hdr_print(str,addr,sz)
#define PRT_INP_STRUCT(a,b,c,d,e)	 prt_inp_struct(a,b,c,d,e)
#define DPRINT0(fmt)  			fprintf(stdout,(fmt))
#define DPRINT1(fmt,a)  		fprintf(stdout,(fmt),(a))
#define DPRINT2(fmt,a,b)  		fprintf(stdout,(fmt),(a),(b))
#define DPRINT3(fmt,a,b,c)  		fprintf(stdout,(fmt),(a),(b),(c))
#define DPRINT4(fmt,a,b,c,d)  		fprintf(stdout,(fmt),(a),(b),(c),(d))
#define DPRINT5(fmt,a,b,c,d,e)  	fprintf(stdout,(fmt),\
					(a),(b),(c),(d),(e))
#define DPRINT6(fmt,a,b,c,d,e,f)   	fprintf(stdout,(fmt),\
					(a),(b),(c),(d),(e),(f))
#define DPRINT7(fmt,a,b,c,d,e,f,g) 	fprintf(stdout,(fmt), \
					(a),(b),(c),(d),(e), (f),(g))
#define DPRINT8(fmt,a,b,c,d,e,f,g,h) 	fprintf(stdout,(fmt),\
					(a),(b),(c),(d),(e),(f),(g),(h))
#define DPRINT9(fmt,a,b,c,d,e,f,g,h,i)  fprintf(stdout,(fmt),\
					(a),(b),(c),(d),(e),(f),(g),(h),(i))
#define P_CHAR(x)    			p_char(x)
#define P_STRING(x)  			p_string(x)
#define P_USHORT(x) 			p_ushort(x)
#define P_SHORT(x)   			p_short(x)
#define P_INT(x)    			p_int(x)
#define P_LONG(x)    			p_long(x)
#define P_ULONG(x)   			p_ulong(x)
#define P_FLOAT(x)   			p_float(x)
#define P_DOUBLE(x)  			p_double(x)

#else
/***********************************************
*    ELSE TURN ALL DEBUG PRINTING OFF                 
*    null out function calls
************************************************/
#define VERB_ON        0
#define LIB_VERSION             "non-verbose"
#define DISPLAY_GRIBHDR(gh)            	{}
#define HDR_PRINT(title,addr,sz)   	{}
#define PRT_INP_STRUCT(a,b,c,d,e)	{}
#define DPRINT0(fmt)                    {}
#define DPRINT1(fmt,s)                  {}
#define DPRINT2(fmt,a,b)                {}
#define DPRINT3(fmt,a,b,c)              {}
#define DPRINT4(fmt,a,b,c,d)            {}
#define DPRINT5(fmt,a,b,c,d,e)          {}
#define DPRINT6(fmt,a,b,c,d,e,f)        {}
#define DPRINT7(fmt,a,b,c,d,e,f,g)      {}
#define DPRINT8(fmt,a,b,c,d,e,f,g,h)    {}
#define DPRINT9(fmt,a,b,c,d,e,f,g,h,i)  {}
#define P_CHAR(expr)    		{}
#define P_STRING(expr)  		{}
#define P_USHORT(expr)  		{}
#define P_SHORT(expr)   		{}
#define P_INT(expr)     		{}
#define P_LONG(expr)    		{}
#define P_ULONG(expr)   		{}
#define P_FLOAT(expr)   		{}
#define P_DOUBLE(expr)  		{}
#endif
