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

#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"
#include <sys/stat.h>


static int show_domain_first = 1 ;

SHOW_DOMAIN_DECOMP ( d_p )
  int_p d_p ;
{
#ifndef STUBS
  rsl_index_t d ;
  int Phist[RSL_MAXPROC] ;
  char fname[50] ;
  FILE * fp ;
  rsl_index_t i, j, k ;
  char * code ;
  int i_am_monitor ;

  RSL_C_IAMMONITOR ( &i_am_monitor ) ;

 if ( i_am_monitor )
 {
  d = *d_p ;

  RSL_TEST_ERR(d < 0 || d >= RSL_MAXDOMAINS,
          "bad domain descriptor" ) ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
        "descriptor for invalid domain" ) ;

  if ( show_domain_first )
  {
    code = "w" ;
    show_domain_first = 0 ;
  }
  else
  {
    code = "r+" ;
  }
  sprintf(fname,"show_domain_%04d",rsl_myproc) ;
  if (( fp = fopen ( fname, code )) == NULL )
  {
     perror(fname) ;
     exit(2) ;
  }
  fseek(fp,0L,2) ;
  fprintf(fp,"domain=%d, len_n=%d, len_m=%d\n", 
	   d, domain_info[d].len_n, domain_info[d].len_m ) ;
  if ( domain_info[d].decomposed != 1 )
  {
    fprintf(fp,"not decomposed at this point in program") ;
    return ;
  }
  for ( i = 0 ; i < RSL_MAXPROC ; i++ ) Phist[i] = 0 ;
  for ( i = domain_info[d].len_m-1 ; i >= 0 ; i-- )
  {
    for ( j = 0 ; j < domain_info[d].len_n ; j++ )
    {
      if ( domain_info[d].domain[INDEX_2(j,i,domain_info[d].len_m)].children_p != NULL )
        fprintf(fp, "%2d ",domain_info[d].domain[INDEX_2(j,i,domain_info[d].len_m)].P+90) ;
      else
        fprintf(fp, "%2d ",domain_info[d].domain[INDEX_2(j,i,domain_info[d].len_m)].P) ;
      Phist[domain_info[d].domain[INDEX_2(j,i,domain_info[d].len_m)].P]++ ;
    }
    fprintf(fp,"\n") ;
  }
  for ( i = 0 ; i < rsl_nproc_all ; i++ )
  {
    fprintf(fp,"%5d  %7d\n",i,Phist[i]) ;
  }

/* added 20010222 */
  if ( domain_info[d].len_z > 1 )
  {
    fprintf(fp,"\nMZ decoomposition len_m = %d len_z = %d\n", domain_info[d].len_m, domain_info[d].len_z) ;
    for ( k = domain_info[d].len_z-1 ; k >= 0 ; k-- ) {
      for ( i = 0 ; i < domain_info[d].len_m ; i++ ) {
        fprintf(fp, "%2d ",domain_info[d].domain_mz[INDEX_2(k,i,domain_info[d].len_m)].P) ;
      }
      fprintf(fp,"\n") ;
    }
    fprintf(fp,"\nNZ decoomposition len_n = %d len_z = %d\n", domain_info[d].len_n, domain_info[d].len_z ) ;
    for ( k = domain_info[d].len_z-1 ; k >= 0 ; k-- ) {
      for ( j = 0 ; j < domain_info[d].len_n ; j++ ) {
        fprintf(fp, "%2d ",domain_info[d].domain_nz[INDEX_2(k,j,domain_info[d].len_n)].P) ;
      }
      fprintf(fp,"\n") ;
    }
  }

  fclose(fp) ;
 }
#endif
  return(0) ;
}


READ_DOMAIN_DECOMP ( d_p, wrk,  m, n )
  int_p d_p ;
  int * wrk ;
  int m, n ;
{
  int P ;
  int d ;
  char fname[50] ;
  FILE * fp ;
  rsl_index_t i, j, k ;
  char * code ;
  int i_am_monitor ;
  int in_d, in_len_n, in_len_m ;

  code = "r+" ;

  d = *d_p ;

  sprintf(fname,"read_domain_%04d",0) ;

  if (( fp = fopen ( fname, code )) == NULL )
  {
     perror(fname) ;
     return(1) ;
  }

  fscanf(fp,"domain=%d, len_n=%d, len_m=%d\n", 
	     &in_d, &in_len_n, &in_len_m ) ;
  fprintf(stderr,"READ_DOMAIN_DECOMP: domain=%d, len_n=%d, len_m=%d\n", in_d,in_len_n,in_len_m ) ;

  if ( in_len_n != domain_info[d].len_n ) {
    fprintf(stderr,"in_len_n != domain_info[d].len_n (%d != %d)\n",in_len_n,domain_info[d].len_n) ;
    RSL_TEST_ERR(1,"" ) ;
  }
  if ( in_len_m != domain_info[d].len_m ) {
    fprintf(stderr,"in_len_m != domain_info[d].len_m (%d != %d)\n",in_len_m,domain_info[d].len_m) ;
    RSL_TEST_ERR(1,"" ) ;
  }

  for ( i = domain_info[d].len_m-1 ; i >= 0 ; i-- )
  {
    for ( j = 0 ; j < domain_info[d].len_n ; j++ )
    {
      fscanf(fp, "%2d ",&P) ;
      wrk[INDEX_2(j,i,domain_info[d].len_m)] = P ;
#if 0
      fprintf(stderr,"%2d ",wrk[INDEX_2(j,i,domain_info[d].len_m)]) ;
#endif
    }
    fscanf(fp,"\n") ;
#if 0
    fprintf(stderr,"\n") ;
#endif
  }

  fclose(fp) ;
  return(0) ;
}


GET_DOMAIN_DECOMP ( d_p, wk, nwk_p )
  int_p d_p ;
  int_p wk ;
  int_p nwk_p ;
{
  rsl_index_t d ;
  char fname[50] ;
  FILE * fp ;
  int nwk ;
  rsl_index_t i, j, m ;
  char * code ;

  d = *d_p ;
  nwk = *nwk_p ;

  RSL_TEST_ERR(d < 0 || d >= RSL_MAXDOMAINS,
          "bad domain descriptor" ) ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
        "descriptor for invalid domain" ) ;
  RSL_TEST_ERR(wk == NULL, "GET_DOMAIN_DECOMP: NULL argument wk" ) ;
  if ( domain_info[d].len_m * domain_info[d].len_n > nwk )
  {
    sprintf(mess,"%d ints would overwrite input array (size=%d ints)\n",
	    domain_info[d].len_m * domain_info[d].len_n,
	    nwk);
    RSL_TEST_ERR(1,mess) ;
  }

  if ( domain_info[d].decomposed != 1 )
  {
    return ;
  }
  m = domain_info[d].len_m ;

  for ( j = 0 ; j < domain_info[d].len_n ; j++ )
  {
    for ( i = 0 ; i < domain_info[d].len_m ; i++ )
    {
      wk[INDEX_2(j,i,m)] = domain_info[d].domain[INDEX_2(j,i,m)].P ;
    }
  }
  return(0) ;
}
