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

#define INTXFER
#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

#define FWD(A,B,C)	my_bcopy(A,B,C)
#define BWD(A,B,C)	my_bcopy(B,A,C)

extern int debuggal_pack ;

static int first_1 = 1 ;
static int first_2 = 1 ;

static FILE * xfp = NULL ;

pack_message( msg, buf, cursor_p, d, ig, jg)
  message_desc_t *msg  ;
  char * buf ;
  int * cursor_p ;
  rsl_index_t d, ig, jg;
{
  rsl_fldspec_t *fld ;
  int stride ;
  register int *ips, *ipd ;
  char * dd, * ss ;
  unsigned int i, j, k, elemsz, t0, t1, t2, t3 ;
  char * base ;
  int cursor ;

  cursor = *cursor_p ;

#if 0
if ( xfp == NULL )
{
  sprintf(mess,"xfp.%03d",rsl_myproc);
  if(( xfp = fopen(mess,"w")) == NULL ) perror(mess)  ;
}
#endif

  if ( msg == NULL ) return(-1) ;

  i = ig - domain_info[d].ilocaloffset ;	/* this must not go neg */
  j = jg - domain_info[d].jlocaloffset ;        /* this must not go neg */

#if 0
  if ( debuggal_pack )
  {
    fprintf(stderr,"pack_message: %16x, d %d i %d,j %d,ig %d,jg %d, ioff %d, joff %d\n",
    msg->fldspecs->base, d, i,j,ig,jg,domain_info[d].ilocaloffset,domain_info[d].jlocaloffset ) ;
    if ( first_2 == 1 )
    {
      first_2 = 0 ;
      for ( fld = msg->fldspecs ; fld != NULL ;  fld = fld->next )
      {
        base = fld->base ;
        fprintf(stderr,"  : %16x\n",base) ;
      }
    }
    fflush(stderr) ;
  }
#endif

  for ( fld = msg->fldspecs ; fld != NULL ;  fld = fld->next )
  {
    elemsz = fld->elemsz ;
    if ( fld->type >= 100 )
    {
      base = (void *)get_base_for_index( fld->f90_table_index ) ;
    }
    else
    {
      base = fld->base ;
    }
    switch (fld->strategy)
    {
    case MINNS_MAJEW_2D :		/* <MM> eg: psa(i,j) */
      t0 = fld->llen[0] ;
      FWD( base+(i+j*t0)*elemsz,&(buf[cursor]),elemsz ) ; cursor+=elemsz ;
      break ;
    case MINEW_MAJNS_2D :		/*      eg: xxx(j,i) */
      t0 = fld->llen[0] ;
      FWD( base+(j+i*t0)*elemsz,&(buf[cursor]),elemsz ) ; cursor+=elemsz ;
      break ;
    case MINNS_MAJEW_K_3D :		/* <MM> eg: ua(i,j,k) */
      t0 = fld->llen[0] ;
      t1 = fld->llen[1]*t0 ;
      switch (elemsz)
      {
#ifdef INTXFER
      case sizeof(int) :
	stride = t1 ;
        ipd = (int *)(&buf[cursor]) ;
	ips = (int *)(base + (i + j*t0)*elemsz) ;
	/* ipd must be aligned on 4 byte boundary on some machines
	   for this to work -- a symptom of it not working would be
	   a bus error, for example. */
	for ( k = 0 ; k < fld->llen[2] ; k++ )
	{
           *ipd = *ips ;
	   ips += stride ;
	   ipd ++ ;
	}
	cursor += fld->llen[2] * elemsz ;
	break ;
#endif
      default :
        for( k = 0 ; k < fld->llen[2] ; k++ )
        {
          FWD( base+(i+j*t0+k*t1)*elemsz,&(buf[cursor]),elemsz ) ;
	  cursor+=elemsz ;
        }
	break ;
      }
      break ;
    case MINEW_MAJNS_K_3D :		/* <MM> eg: u(j,i,k) */
      t0 = fld->llen[0] ;
      t1 = fld->llen[1]*t0 ;
      switch (elemsz)
      {
#ifdef INTXFER
      case sizeof(int) :
        stride = t1 ;
        ipd = (int *)(&buf[cursor]) ;
        ips = (int *)(base + (j + i*t0)*elemsz) ;
        /* ipd must be aligned on 4 byte boundary on some machines
           for this to work -- a symptom of it not working would be
           a bus error, for example. */
        for ( k = 0 ; k < fld->llen[2] ; k++ )
        {
           *ipd = *ips ;
           ips += stride ;
           ipd ++ ;
        }
        cursor += fld->llen[2] * elemsz ;
        break ;
#endif
      default :
        for( k = 0 ; k < fld->llen[2] ; k++ )
        {
          FWD( base+(j+i*t0+k*t1)*elemsz,&(buf[cursor]),elemsz ) ;
	  cursor+=elemsz ;
        }
	break ;
      }
      break ;

    case K_MIDNS_MAJEW_3D :             /* <MM> eg: u(k,i,j) */
      t0 = fld->llen[0] ;
      t1 = fld->llen[1]*t0 ;
      switch (elemsz)
      {
#ifdef INTXFER
      case sizeof(int) :
        ipd = (int *)(&buf[cursor]) ;
        ips = (int *)(base + (i*t0+j*t1)*elemsz) ;
        /* ipd must be aligned on 4 byte boundary on some machines
           for this to work -- a symptom of it not working would be
           a bus error, for example. */
        for ( k = 0 ; k < t0 ; k++ )
        {
           *ipd++ = *ips++ ;
        }
        cursor += t0*elemsz ;
        break ;
#endif
      default :
        FWD( base+(i*t0+j*t1)*elemsz,&(buf[cursor]),t0*elemsz ) ;
        cursor+=t0*elemsz ;
        break ;
      }
      break ;


    default:
      RSL_TEST_ERR(1,"pack_message: strategy not supported" ) ;
      break ;
    }
  }
  *cursor_p = cursor ;
}

unpack_message( msg, buf, cursor_p, d, ig, jg)
  message_desc_t *msg  ;
  char * buf ;
  int * cursor_p ;
  rsl_index_t d, ig, jg;
{
  rsl_fldspec_t *fld ;
  register int * ips, * ipd ;
  char * dd, * ss ;
  unsigned int i, j, k, elemsz, t0, t1, t2, t3 ;
  char * base ;
  int cursor ;
  int stride ;

  cursor = *cursor_p ;

#if 0
if ( xfp == NULL )
{
  sprintf(mess,"xfp.%03d",rsl_myproc);
  xfp = fopen(mess,"w") ;
}
#endif


  if ( msg == NULL ) return(-1) ;

  i = ig - domain_info[d].ilocaloffset ;	/* this must not go neg */
  j = jg - domain_info[d].jlocaloffset ;        /* this must not go neg */

#if 0
  if ( debuggal_pack )
  {
    fprintf(stderr,"unpack_message: %16x, i %d,j %d,ig %d,jg %d, ioff %d, joff %d\n",
    msg->fldspecs->base,i,j,ig,jg,domain_info[d].ilocaloffset,domain_info[d].jlocaloffset ) ;
    if ( first_1 == 1 )
    {
      first_1 = 0 ;
      for ( fld = msg->fldspecs ; fld != NULL ;  fld = fld->next )
      {
        base = fld->base ;
        fprintf(stderr, "  : base=%16x, elemsz=%d, strategy=%d \n"
                           , base, fld->elemsz, fld->strategy
               ) ;
      }
    }

    fflush(stderr) ;
  }
#endif

  for ( fld = msg->fldspecs ; fld != NULL ;  fld = fld->next )
  {
    elemsz = fld->elemsz ;
    if ( fld->type >= 100 )
    {
      base = (void *)get_base_for_index( fld->f90_table_index ) ;
    }
    else
    {
      base = fld->base ;
    }
    switch (fld->strategy)
    {
    case MINNS_MAJEW_2D :		/* <MM> eg: psa(i,j) */
      t0 = fld->llen[0] ;
      BWD( base+(i+j*t0)*elemsz,&(buf[cursor]),elemsz ) ; cursor+=elemsz ;
      break ;
    case MINEW_MAJNS_2D :		/*      eg: xxx(j,i) */
      t0 = fld->llen[0] ;
      BWD( base+(j+i*t0)*elemsz,&(buf[cursor]),elemsz ) ; cursor+=elemsz ;
      break ;
    case MINNS_MAJEW_K_3D :		/* <MM> eg: ua(i,j,k) */
      t0 = fld->llen[0] ;
      t1 = fld->llen[1]*t0 ;
      switch (elemsz)
      {
#ifdef INTXFER
      case sizeof(int) :
        stride = t1 ;
        ips = (int *)(&buf[cursor]) ;
        ipd = (int *)(base + (i + j*t0)*elemsz) ;
        /* ipd must be aligned on 4 byte boundary on some machines
           for this to work -- a symptom of it not working would be
           a bus error, for example. */
        for ( k = 0 ; k < fld->llen[2] ; k++ )
        {
           *ipd = *ips ;
           ips ++ ;
           ipd += stride ;
        }
        cursor += fld->llen[2] * elemsz ;
        break ;
#endif
      default :
        for( k = 0 ; k < fld->llen[2] ; k++ )
        {
          BWD( base+(i+j*t0+k*t1)*elemsz,&(buf[cursor]),elemsz ) ;
          cursor+=elemsz ;
        }
        break ;
      }
      break ;
    case MINEW_MAJNS_K_3D :		/* <MM> eg: u(j,i,k) */
      t0 = fld->llen[0] ;
      t1 = fld->llen[1]*t0 ;
      switch (elemsz)
      {
#ifdef INTXFER
      case sizeof(int) :
        stride = t1 ;
        ips = (int *)(&buf[cursor]) ;
        ipd = (int *)(base + (j + i*t0)*elemsz) ;
        /* ipd must be aligned on 4 byte boundary on some machines
           for this to work -- a symptom of it not working would be
           a bus error, for example. */
        for ( k = 0 ; k < fld->llen[2] ; k++ )
        {
           *ipd = *ips ;
           ips ++ ;
           ipd += stride ;
        }
        cursor += fld->llen[2] * elemsz ;
        break ;
#endif
      default :
        for( k = 0 ; k < fld->llen[2] ; k++ )
        {
          BWD( base+(j+i*t0+k*t1)*elemsz,&(buf[cursor]),elemsz ) ;
          cursor+=elemsz ;
        }
        break ;
      }
      break ;

    case K_MIDNS_MAJEW_3D :             /* <MM> eg: u(k,i,j) */
      t0 = fld->llen[0] ;
      t1 = fld->llen[1]*t0 ;
      switch (elemsz)
      {
#ifdef INTXFER
      case sizeof(int) :
        ips = (int *)(&buf[cursor]) ;
        ipd = (int *)(base + (i*t0+j*t1)*elemsz) ;
        /* ipd must be aligned on 4 byte boundary on some machines
           for this to work -- a symptom of it not working would be
           a bus error, for example. */
        for ( k = 0 ; k < t0 ; k++ )
        {
           *ipd++ = *ips++ ;
        }
        cursor += t0*elemsz ;
        break ;
#endif
      default :
        BWD( base+(i*t0+j*t1)*elemsz,&(buf[cursor]),t0*elemsz ) ;
        cursor+=t0*elemsz ;
        break ;
      }
      break ;

    default:
      RSL_TEST_ERR(1,"unpack_message: strategy not supported" ) ;
      break ;
    }
  }
  *cursor_p = cursor ;
}

my_bcopy( A, B, C )
  char * A ;
  char * B ;
  int C ;
{
  int i ;
  for ( i = 0 ; i < C ; i++ ) *(B+i) = *(A+i) ;
}

