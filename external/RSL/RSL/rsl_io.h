/***********************************************************************
     
                              COPYRIGHT
     
     The following is a notice of limited availability of the code and 
     Government license and disclaimer which must be included in the 
     prologue of the code and in all source listings of the code.
     
     Copyright notice
       (c) 1977  University of Chicago
     
     Permission is hereby granted to use, reproduce, prepare 
     derivative works, and to redistribute to others at no charge.  If 
     you distribute a copy or copies of the Software, or you modify a 
     copy or copies of the Software or any portion of it, thus forming 
     a work based on the Software and make and/or distribute copies of 
     such work, you must meet the following conditions:
     
          a) If you make a copy of the Software (modified or verbatim) 
             it must include the copyright notice and Government       
             license and disclaimer.
     
          b) You must cause the modified Software to carry prominent   
             notices stating that you changed specified portions of    
             the Software.
     
     This software was authored by:
     
     Argonne National Laboratory
     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
     Mathematics and Computer Science Division
     Argonne National Laboratory, Argonne, IL  60439
     
     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
     CONTRACT WITH THE DEPARTMENT OF ENERGY.
     
                      GOVERNMENT LICENSE AND DISCLAIMER
     
     This computer code material was prepared, in part, as an account 
     of work sponsored by an agency of the United States Government.
     The Government is granted for itself and others acting on its 
     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
     this data to reproduce, prepare derivative works, distribute 
     copies to the public, perform publicly and display publicly, and 
     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
     NOT INFRINGE PRIVATELY OWNED RIGHTS.

***************************************************************************/



#ifndef RSL_IO_H
#define RSL_IO_H

typedef struct rsl_read_resp {
  int response_type ;
  int sequence ;
  int tofollow ;
} rsl_read_resp_t ;

typedef struct rsl_read_req {
  int request_type  ;

  int request_mode  ;	/* FORTRAN or SOCKET   added 9/30/94 */
  int request_mode2 ;	/* RAW or PORTAL       added 9/30/94 */

  rsl_processor_t myproc ;
  rsl_index_t domain ;
  void * base ;
  int sequence ;
  int iotag ;
  int unit ;
  int_p unit_p ;
  int internal ;
  int ndim ;
  int type ;
  int speciala ;		/* extra information */
  int specialb ;		/* extra information */
  int specialc ;		/* extra information */
  int glen[RSL_MAXDIM] ;
  int llen[RSL_MAXDIM] ;
#ifdef crayx1
  int is_write, ie_write ;
  int js_write, je_write ;
  int is_read , ie_read  ;
  int js_read , je_read  ;
#else
  short is_write, ie_write ;
  short js_write, je_write ;
  short is_read , ie_read  ;
  short js_read , je_read  ;
#endif
} rsl_read_req_t ;

typedef rsl_read_req_t rsl_write_req_t ;


/* start 981228 AFWA IO */

typedef struct rsl_write_buffer_struct {
  rsl_write_req_t req ;
  int nelem ;
  char * buf ;
  struct rsl_write_buffer_struct *next ;
} rsl_write_buffer_struct_t ;

#ifdef DEFINE_GLOBAL
rsl_write_buffer_struct_t * write_buffer_head = NULL ;
rsl_write_buffer_struct_t * write_buffer_tail = NULL ;
int rsl_buffer_output = 0 ;
int rsl_io_node = 0 ;
#else
extern rsl_write_buffer_struct_t * write_buffer_head ;
extern rsl_write_buffer_struct_t * write_buffer_tail ;
extern int rsl_buffer_output ;
extern int rsl_io_node ;
#endif


/* end 981228 AFWA IO */


#endif /* nothing after this line */
