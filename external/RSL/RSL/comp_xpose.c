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

/* The mechanism here is similar to and patterned after that used by the
   stencil mechanism: see comp_sten.c */


/* this is used internally only -- this will be called automatically
   whenever a xpose is attempted that has not
   yet been compiled */
rsl_compile_xpose( d_p, x_p )
  int_p d_p, x_p ;
{
  int d, x ;
  xpose_desc_t * xp ;
  int i, j, ig, jg, kg, k, js, je, is, ie, ks, ke  ;
  int len_plist ;
  rsl_domain_info_t *dp ;
  rsl_point_t  *pt ;
  rsl_dimlen_t mlen, nlen, zlen ;
  rsl_fldspec_t * fld ;
  message_desc_t *msg_from, *msg_to ;
  rsl_procrec_t *procrec ;
  int m, n, dir ;
  rsl_processor_t P, Plist[RSL_MAXPROC], sendP, recvP, prevP ;
  int elemsz, t0, t1 ;
  int ipack ;
  void *base ;

  d = *d_p ;
  x = *x_p ;

  RSL_TEST_ERR(d < 0 || d >= RSL_MAXDOMAINS,
          "rsl_compile_xpose: bad domain descriptor" ) ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID, 
        "rsl_compile_xpose: descriptor for invalid domain" ) ;

  mlen = domain_info[d].len_m ;
  nlen = domain_info[d].len_n ;
  zlen = domain_info[d].len_z ;

  RSL_TEST_ERR( x <= 0 || x > RSL_MAXDESCRIPTORS,
        "rsl_compile_xpose: bad xpose descriptor" ) ;
  RSL_TEST_ERR((xp = (xpose_desc_t *)xp_descriptors[x]) == NULL,
        "rsl_compile_xpose: null xpose descriptor" ) ;
  RSL_TEST_ERR( xp->tag != XPOSE_DESC,
        "rsl_compile_xpose: bad xpose descriptor" ) ;
  RSL_TEST_ERR( xp->compiled[d] != 0,
        "rsl_compile_xpose: xpose has already been compiled for this domain") ;

  xp->compiled[d] = 1 ;

  dp = &(domain_info[d]) ;
  if ( dp->decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }

/************* MN to MZ *************/

  msg_from = xp->msgs_mn[d] ; 
  msg_to   = xp->msgs_mz[d] ;


/* first pass builds the procrec list, second pass traverses it */
/* this is necessary because the process_refs mechanism can only */
/* build one set of pack or unpack lists at a time */
/* ipack = 0 packing on sender ; ipack = 1, unpacking on receiver */

    xp->procs[d][XPOSE_MN_MZ] = NULL ;
    ipack = 0 ;
    for ( P = 0 ; P < rsl_nproc_all ; P++ )
    {
      procrec = RSL_MALLOC( rsl_procrec_t, 1 ) ;
      procrec->P = P ;

      init_process_refs() ;

/***/ for ( k = 0 ; k < zlen ; k++ )
      {
/***/   for ( ig = 0 ; ig < mlen ; ig++ )
        {
         recvP = domain_info[*d_p].domain_mz[INDEX_2(k,ig,mlen)].P ;
         if ( ipack == 0 || rsl_c_comp2phys_proc(recvP) != rsl_myproc )
         {
          js = 0 - domain_info[d].jlocaloffset ; je = -1 ;
          prevP = domain_info[*d_p].domain[INDEX_2(0,ig,mlen)].P ;
#if 0
fprintf(stderr,"set js to %d\n",js) ;
fprintf(stderr,"set prevP to %d\n",prevP) ;
#endif
/***/     for ( jg = 0 ; jg < nlen ; jg++ ) 
          {
	    sendP = domain_info[*d_p].domain[INDEX_2(jg,ig,mlen)].P ;
	    if ( jg == nlen-1 ) 
              { sendP = -1 ; je++ ;}
            i = ig - domain_info[d].ilocaloffset ;
            j = jg - domain_info[d].jlocaloffset ;
#if 0
if ( k==0) fprintf(stderr,"P %d sendP %d prevP %d js %d je %d\n",P,sendP,prevP, js,je) ;
#endif
	    if ((ipack == 0) && (sendP != prevP ))
	    {
	      if ( rsl_c_comp2phys_proc(prevP) == rsl_myproc && recvP == P )
	      {
	        if ( jg > 0 )
	        {
	          /* store the pencil (ig,k,js:je) as being sent 
	             from sendP and received by recvP */
  
                  if ( ipack == 0 ) { fld = msg_from->fldspecs ; }
                  else              { fld = msg_to->fldspecs ; }
  
                  for ( ; fld != NULL ;  fld = fld->next )
                  {
                    if ( fld->type >= 100 ) xp->has_f90_fields = 1 ;
                    base = fld->base ;
                    elemsz = fld->elemsz ;
                    switch (fld->strategy)
                    {
                      case MINNS_MAJEW_K_3D :             /* <MM> eg: ua(i,j,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"s_p_r^: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d js %2d je %2d je-js+1 %3d\n",P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,js,je,je-js+1) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (i+js*t0+k*t1)*elemsz, elemsz,
                                                  je-js+1 ,
                                                  -t0*elemsz) ;
                        break ;
                      case MINEW_MAJNS_K_3D :             /* <MM> eg: u(j,i,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (js+i*t0+k*t1)*elemsz, (je-js+1)*elemsz,
                                                  1 ,
                                                  -elemsz) ;
                        break ;
                      case K_MIDNS_MAJEW_3D :             /* <MM> eg: u(k,i,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (k+i*t0+js*t1)*elemsz, elemsz,
                                                  je-js+1, 
						  -t1*elemsz) ;
                        break ;
                      case MINNS_K_MAJEW_3D :             /* <MM> eg: u(i,k,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"s_p_r^: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d js %2d je %2d je-js+1 %3d\n",P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,js,je,je-js+1) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (i+k*t0+js*t1)*elemsz, elemsz,
                                                  je-js+1,
						  -t1*elemsz) ;  /* don't need to suppress packing optimization on MN grid because of pads */
                        break ;
                      default:
                        RSL_TEST_ERR(1,"new pack comp: strategy not supported" ) ;
                        break ;
                    }
                  }
	        }
	      }
#if 0
fprintf(stderr,"resetting js to %d\n",j) ;
#endif
	      js = j ;
	    }
	    je = j ;
#if 0
fprintf(stderr,"resetting je to %d\n",je) ;
#endif
	    prevP = sendP ;
#if 0
fprintf(stderr,"resset prevP to %d\n",prevP) ;
#endif
          }
         }
        }
      }
        process_refs( &(procrec->pack_table),
                      &(procrec->pack_table_size),
                      &(procrec->pack_table_nbytes), 1 ) ;

#if 0
        fprintf(stderr,"pack P = %3d:\n",procrec->P ) ;
        show_pack_table( procrec->pack_table,
                         procrec->pack_table_size,
                         procrec->pack_table_nbytes ) ;
#endif

        procrec->next = xp->procs[d][XPOSE_MN_MZ] ;
        xp->procs[d][XPOSE_MN_MZ] = procrec ;
    }

/* unpacking loop */

    ipack = 1 ;
    for ( procrec = xp->procs[d][XPOSE_MN_MZ] ; procrec != NULL ; procrec = procrec->next )
    {
      P = procrec->P ;
      init_process_refs() ;

/***/ for ( kg = 0 ; kg < zlen ; kg++ )
      {
/***/   for ( ig = 0 ; ig < mlen ; ig++ )
        {
         recvP = domain_info[*d_p].domain_mz[INDEX_2(kg,ig,mlen)].P ;

          js = 0 - domain_info[d].jlocaloffset_mz ; je = -1 ;

          prevP = domain_info[*d_p].domain[INDEX_2(0,ig,mlen)].P ;
/***/     for ( jg = 0 ; jg < nlen ; jg++ ) 
          {
	    sendP = domain_info[*d_p].domain[INDEX_2(jg,ig,mlen)].P ;
	    if ( jg == nlen-1 ) 
              { sendP = -1 ; je++ ;}

            i = ig - domain_info[d].ilocaloffset_mz ;
            j = jg - domain_info[d].jlocaloffset_mz ;
            k = kg - domain_info[d].klocaloffset_mz ;

	    if (sendP != prevP )
	    {
#if 0
fprintf(stderr,"sendP (%2d) != prevP (%d) =? P (%d) ||  recvP (%d) =? rsl_myproc (%d)\n",
sendP,prevP,P,recvP,rsl_myproc) ;
#endif
 	      if ( rsl_c_comp2phys_proc(prevP) == P && recvP == rsl_myproc )
	      {
	        if ( jg > 0 )
	        {
	          /* store the pencil (ig,k,js:je) as being sent 
	             from sendP and received by recvP */
    
                  if ( ipack == 0 ) { fld = msg_from->fldspecs ; }
                  else              { fld = msg_to->fldspecs ; }
    
                  for ( ; fld != NULL ;  fld = fld->next )
                  {
                    if ( fld->type >= 100 ) xp->has_f90_fields = 1 ;
                    base = fld->base ;
                    elemsz = fld->elemsz ;
                    switch (fld->strategy)
                    {
                      case MINNS_MAJEW_K_3D :             /* <MM> eg: ua(i,j,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"s_p_rv: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d js %2d je %2d je-js+1 %3d\n",
P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,js,je,je-js+1) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (i+js*t0+k*t1)*elemsz, elemsz,
                                                  je-js+1 ,
                                                  -t0*elemsz) ;
                        break ;
                      case MINEW_MAJNS_K_3D :             /* <MM> eg: u(j,i,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (js+i*t0+k*t1)*elemsz, (je-js+1)*elemsz,
                                                  1 ,
                                                  -elemsz) ;
                        break ;
                      case K_MIDNS_MAJEW_3D :             /* <MM> eg: u(k,i,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (k+i*t0+js*t1)*elemsz, elemsz,
                                                  je-js+1, 
						  -t1*elemsz) ;
                        break ;
                      case MINNS_K_MAJEW_3D :             /* <MM> eg: u(i,k,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"s_p_rv: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d js %2d je %2d je-js+1 %3d ofst %3d\n",
P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,js,je,je-js+1, (i+k*t0+js*t1)*elemsz) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (i+k*t0+js*t1)*elemsz, elemsz,
                                                  je-js+1, 
						  -t1*elemsz) ;   /* negative stride suppresses some unpacking collapses */
                        break ;
                      default:
                        RSL_TEST_ERR(1,"new pack comp: strategy not supported" ) ;
                        break ;
                    }
                  }
	        }
	      }
#if 0
fprintf(stderr,"resetting js to %d\n",j) ;
#endif
	      js = j ;
	    }
	    je = j ;
#if 0
fprintf(stderr,"resetting je to %d\n",je) ;
#endif
	    prevP = sendP ;
#if 0
fprintf(stderr,"resset prevP to %d\n",prevP) ;
#endif
          }
        }
      }
        process_refs( &(procrec->unpack_table),
                      &(procrec->unpack_table_size),
                      &(procrec->unpack_table_nbytes), 1 ) ;
#if 0
        fprintf(stderr,"unpack P = %3d:\n",procrec->P ) ;
        show_pack_table( procrec->unpack_table,
                         procrec->unpack_table_size,
                         procrec->unpack_table_nbytes ) ;
#endif

    }

/************* MZ to NZ *************/

  msg_from = xp->msgs_mz[d] ; 
  msg_to   = xp->msgs_nz[d] ;


/* first pass builds the procrec list, second pass traverses it */
/* this is necessary because the process_refs mechanism can only */
/* build one set of pack or unpack lists at a time */
/* ipack = 0 packing on sender ; ipack = 1, unpacking on receiver */

   ipack = 0  ;
    xp->procs[d][XPOSE_MZ_NZ] = NULL ;
    ipack = 0 ;
    for ( P = 0 ; P < rsl_nproc_all ; P++ )
    {
      procrec = RSL_MALLOC( rsl_procrec_t, 1 ) ;
      procrec->P = P ;

      init_process_refs() ;

/***/ for ( jg = 0 ; jg < nlen ; jg++ )
      {
/***/   for ( kg = 0 ; kg < zlen ; kg++ )
        {
         recvP = domain_info[*d_p].domain_nz[INDEX_2(kg,jg,nlen)].P ;
         if ( ipack == 0 || rsl_c_comp2phys_proc(recvP) != rsl_myproc )
         {
          is = 0 - domain_info[d].ilocaloffset_mz ; ie = -1 ;
          prevP = domain_info[*d_p].domain_mz[INDEX_2(kg,0,mlen)].P ;
#if 0
fprintf(stderr,"set is to %d\n",is) ;
fprintf(stderr,"set prevP to %d\n",prevP) ;
#endif
/***/     for ( ig = 0 ; ig < mlen ; ig++ ) 
          {
	    sendP = domain_info[*d_p].domain_mz[INDEX_2(kg,ig,mlen)].P ;
	    if ( ig == mlen-1 ) 
              { sendP = -1 ; ie++ ;}
            i = ig - domain_info[d].ilocaloffset_mz ;
            j = jg - domain_info[d].jlocaloffset_mz ;
            k = kg - domain_info[d].klocaloffset_mz ;
#if 0
if ( k==0) fprintf(stderr,"P %d sendP %d prevP %d js %d je %d\n",P,sendP,prevP, is,ie) ;
#endif
	    if (sendP != prevP )
	    {
	      if ( rsl_c_comp2phys_proc(prevP) == rsl_myproc && recvP == P )
	      {
	        if ( ig > 0 )
	        {
	          /* store the pencil (is:ie,k,jg) as being sent 
	             from sendP and received by recvP */
  
                  fld = msg_from->fldspecs ;
  
                  for ( ; fld != NULL ;  fld = fld->next )
                  {
                    if ( fld->type >= 100 ) xp->has_f90_fields = 1 ;
                    base = fld->base ;
                    elemsz = fld->elemsz ;
                    switch (fld->strategy)
                    {
                      case MINNS_MAJEW_K_3D :             /* <MM> eg: ua(i,j,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"s_p_r^: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d is %2d ie %2d ie-is+1 %3d\n",P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,is,ie,ie-is+1) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (is+j*t0+k*t1)*elemsz, (ie-is+1)*elemsz,
                                                  1 ,
                                                  -elemsz) ;
                        break ;
                      case MINEW_MAJNS_K_3D :             /* <MM> eg: u(j,i,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (j+is*t0+k*t1)*elemsz, elemsz,
                                                  (ie-is+1) ,
                                                  -t0*elemsz) ;
                        break ;
                      case K_MIDNS_MAJEW_3D :             /* <MM> eg: u(k,i,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (k+is*t0+j*t1)*elemsz, elemsz,
                                                  ie-is+1,
                                                  -t0*elemsz) ;
                        break ;
                      case MINNS_K_MAJEW_3D :             /* <MM> eg: u(i,k,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"s_p_r^: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d is %2d ie %2d ie-is+1 %3d\n",P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,is,ie,ie-is+1) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (is+k*t0+j*t1)*elemsz, (ie-is+1)*elemsz,
                                                  1,
						  -elemsz) ;   /* negative stride suppresses some packing optimzation in process_refs */
                        break ;
                      default:
                        RSL_TEST_ERR(1,"new pack comp: strategy not supported" ) ;
                        break ;
                    }
                  }
	        }
	      }
#if 0
fprintf(stderr,"resetting is to %d\n",j) ;
#endif
	      is = i ;
	    }
	    ie = i ;
#if 0
fprintf(stderr,"resetting ie to %d\n",ie) ;
#endif
	    prevP = sendP ;
#if 0
fprintf(stderr,"resset prevP to %d\n",prevP) ;
#endif
          }
         }
        }
      }
        process_refs( &(procrec->pack_table),
                      &(procrec->pack_table_size),
                      &(procrec->pack_table_nbytes), 1 ) ;

#if 0
        fprintf(stderr,"pack P = %3d:\n",procrec->P ) ;
        show_pack_table( procrec->pack_table,
                         procrec->pack_table_size,
                         procrec->pack_table_nbytes ) ;
#endif

        procrec->next = xp->procs[d][XPOSE_MZ_NZ] ;
        xp->procs[d][XPOSE_MZ_NZ] = procrec ;
    }

/* unpacking loop */

    ipack = 1 ;
    for ( procrec = xp->procs[d][XPOSE_MZ_NZ] ; procrec != NULL ; procrec = procrec->next )
    {
      P = procrec->P ;
      init_process_refs() ;

/***/ for ( jg = 0 ; jg < nlen ; jg++ )
      {
/***/   for ( kg = 0 ; kg < zlen ; kg++ )
        {
         recvP = domain_info[*d_p].domain_nz[INDEX_2(kg,jg,nlen)].P ;
          is = 0 - domain_info[d].ilocaloffset_nz ; ie = -1 ;
          prevP = domain_info[*d_p].domain_mz[INDEX_2(kg,0,mlen)].P ;
/***/     for ( ig = 0 ; ig < mlen ; ig++ ) 
          {
	    sendP = domain_info[*d_p].domain_mz[INDEX_2(kg,ig,mlen)].P ;
	    if ( ig == mlen-1 ) 
              { sendP = -1 ; ie++ ;}

            i = ig - domain_info[d].ilocaloffset_nz ;
            j = jg - domain_info[d].jlocaloffset_nz ;
            k = kg - domain_info[d].klocaloffset_nz ;

	    if (sendP != prevP )
	    {
#if 0
fprintf(stderr,"sendP (%2d) != prevP (%d) =? P (%d) ||  recvP (%d) =? rsl_myproc (%d)\n",
sendP,prevP,P,recvP,rsl_myproc) ;
#endif
 	      if ( rsl_c_comp2phys_proc(prevP) == P && recvP == rsl_myproc )
	      {
	        if ( ig > 0 )
	        {
	          /* store the pencil (is:ie,k,jg) as being sent 
	             from sendP and received by recvP */
    
                  fld = msg_to->fldspecs ;
    
                  for ( ; fld != NULL ;  fld = fld->next )
                  {
                    if ( fld->type >= 100 ) xp->has_f90_fields = 1 ;
                    base = fld->base ;
                    elemsz = fld->elemsz ;
                    switch (fld->strategy)
                    {
                      case MINNS_MAJEW_K_3D :             /* <MM> eg: ua(i,j,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"s_p_rv: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d is %2d ie %2d ie-is+1 %3d\n",
P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,is,ie,ie-is+1) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (is+j*t0+k*t1)*elemsz, (ie-is+1)*elemsz,
                                                  1 ,
                                                  -elemsz) ;
                        break ;
                      case MINEW_MAJNS_K_3D :             /* <MM> eg: u(j,i,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (j+is*t0+k*t1)*elemsz, elemsz,
                                                  ie-is+1 ,
                                                  -t0*elemsz) ;
                        break ;
                      case K_MIDNS_MAJEW_3D :             /* <MM> eg: u(k,i,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (k+is*t0+j*t1)*elemsz, elemsz,
                                                  ie-is+1,
						  -t0*elemsz) ;
                        break ;
                      case MINNS_K_MAJEW_3D :             /* <MM> eg: u(i,k,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"s_p_rv: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d is %2d ie %2d ie-is+1 %3d\n",
P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,is,ie,ie-is+1) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (is+k*t0+j*t1)*elemsz, (ie-is+1)*elemsz,
                                                  1,
						  -elemsz) ;  /* negative stride suppresses some unpacking optimization in process_refs */
                        break ;
                      default:
                        RSL_TEST_ERR(1,"new pack comp: strategy not supported" ) ;
                        break ;
                    }
                  }
	        }
	      }
#if 0
fprintf(stderr,"resetting is to %d\n",i) ;
#endif
	      is = i ;
	    }
	    ie = i ;
#if 0
fprintf(stderr,"resetting je to %d\n",je) ;
#endif
	    prevP = sendP ;
#if 0
fprintf(stderr,"resset prevP to %d\n",prevP) ;
#endif
          }
        }
      }
        process_refs( &(procrec->unpack_table),
                      &(procrec->unpack_table_size),
                      &(procrec->unpack_table_nbytes), 1 ) ;
#if 0
        fprintf(stderr,"unpack P = %3d:\n",procrec->P ) ;
        show_pack_table( procrec->unpack_table,
                         procrec->unpack_table_size,
                         procrec->unpack_table_nbytes ) ;
#endif

    }

/************* NZ to MN *************/
 /* (may the circle be unbroken) */

  msg_from = xp->msgs_nz[d] ; 
  msg_to   = xp->msgs_mn[d] ;

/* first pass builds the procrec list, second pass traverses it */
/* this is necessary because the process_refs mechanism can only */
/* build one set of pack or unpack lists at a time */
/* ipack = 0 packing on sender ; ipack = 1, unpacking on receiver */

   ipack = 0  ;
    xp->procs[d][XPOSE_NZ_MN] = NULL ;
    ipack = 0 ;
    for ( P = 0 ; P < rsl_nproc_all ; P++ )
    {
      procrec = RSL_MALLOC( rsl_procrec_t, 1 ) ;
      procrec->P = P ;

      init_process_refs() ;

/***/ for ( jg = 0 ; jg < nlen ; jg++ )
      {
/***/   for ( ig = 0 ; ig < mlen ; ig++ ) 
        {
         recvP = domain_info[*d_p].domain[INDEX_2(jg,ig,mlen)].P ;
         if ( ipack == 0 || rsl_c_comp2phys_proc(recvP) != rsl_myproc )
         {
          ks = 0 - domain_info[d].klocaloffset_nz ; ke = -1 ;
          prevP = domain_info[*d_p].domain_nz[INDEX_2(0,jg,nlen)].P ;
#if 0
fprintf(stderr,"set ks to %d\n",ks) ;
fprintf(stderr,"set prevP to %d\n",prevP) ;
#endif
/***/     for ( kg = 0 ; kg < zlen ; kg++ )
          {
	    sendP = domain_info[*d_p].domain_nz[INDEX_2(kg,jg,nlen)].P ;
	    if ( kg == zlen-1 ) 
              { sendP = -1 ; ke++ ;}
            i = ig - domain_info[d].ilocaloffset_nz ;
            j = jg - domain_info[d].jlocaloffset_nz ;
            k = kg - domain_info[d].klocaloffset_nz ;
#if 0
if ( k==0) fprintf(stderr,"P %d sendP %d prevP %d ks %d ke %d\n",P,sendP,prevP, ks,ke) ;
#endif
	    if (sendP != prevP )
	    {
	      if ( rsl_c_comp2phys_proc(prevP) == rsl_myproc && recvP == P )
	      {
	        if ( kg > 0 )
	        {
	          /* store the pencil (ig,ks:ke,jg) as being sent 
	             from sendP and received by recvP */
  
                  fld = msg_from->fldspecs ;
  
                  for ( ; fld != NULL ;  fld = fld->next )
                  {
                    if ( fld->type >= 100 ) xp->has_f90_fields = 1 ;
                    base = fld->base ;
                    elemsz = fld->elemsz ;
                    switch (fld->strategy)
                    {
                      case MINNS_MAJEW_K_3D :             /* <MM> eg: ua(i,j,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"s_p_r^: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d ks %2d ke %2d ke-ks+1 %3d\n",P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,ks,ke,ke-ks+1) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (i+j*t0+ks*t1)*elemsz, elemsz,
                                                  ke-ks+1 ,
                                                  -t1*elemsz) ;
                        break ;
                      case MINEW_MAJNS_K_3D :             /* <MM> eg: u(j,i,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (j+i*t0+ks*t1)*elemsz, elemsz,
                                                  ke-ks+1 ,
                                                  -t1*elemsz) ;
                        break ;
                      case K_MIDNS_MAJEW_3D :             /* <MM> eg: u(k,i,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (ks+i*t0+j*t1)*elemsz, (ke-ks+1)*elemsz,
                                                  1, 
						  -elemsz) ;
                        break ;
                      case MINNS_K_MAJEW_3D :             /* <MM> eg: u(i,k,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"MZ to MN s_p_r^: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d ks %2d ke %2d ke-ks+1 %3d\n",P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,ks,ke,ke-ks+1) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (i+ks*t0+j*t1)*elemsz, elemsz,
                                                  ke-ks+1,
						  -t0*elemsz) ;  /* negative stride suppresses some packing optimizationin process_refs */
                        break ;
                      default:
                        RSL_TEST_ERR(1,"new pack comp: strategy not supported" ) ;
                        break ;
                    }
                  }
	        }
	      }
#if 0
fprintf(stderr,"resetting ks to %d\n",k) ;
#endif
	      ks = k ;
	    }
	    ke = k ;
#if 0
fprintf(stderr,"resetting ke to %d\n",ke) ;
#endif
	    prevP = sendP ;
#if 0
fprintf(stderr,"resset prevP to %d\n",prevP) ;
#endif
          }
         }
        }
      }
        process_refs( &(procrec->pack_table),
                      &(procrec->pack_table_size),
                      &(procrec->pack_table_nbytes), 1 ) ;

#if 0
        fprintf(stderr,"pack P = %3d:\n",procrec->P ) ;
        show_pack_table( procrec->pack_table,
                         procrec->pack_table_size,
                         procrec->pack_table_nbytes ) ;
#endif

        procrec->next = xp->procs[d][XPOSE_NZ_MN] ;
        xp->procs[d][XPOSE_NZ_MN] = procrec ;
    }

/* unpacking loop */

    ipack = 1 ;
    for ( procrec = xp->procs[d][XPOSE_NZ_MN] ; procrec != NULL ; procrec = procrec->next )
    {
      P = procrec->P ;
      init_process_refs() ;

/***/ for ( jg = 0 ; jg < nlen ; jg++ )
      {
/***/   for ( ig = 0 ; ig < mlen ; ig++ ) 
        {
         recvP = domain_info[*d_p].domain[INDEX_2(jg,ig,mlen)].P ;
          ks = 0                               ; ke = -1 ;
          prevP = domain_info[*d_p].domain_nz[INDEX_2(0,jg,nlen)].P ;
/***/     for ( kg = 0 ; kg < zlen ; kg++ )
          {
	    sendP = domain_info[*d_p].domain_nz[INDEX_2(kg,jg,nlen)].P ;
	    if ( kg == zlen-1 ) 
              { sendP = -1 ; ke++ ;}

            i = ig - domain_info[d].ilocaloffset ;
            j = jg - domain_info[d].jlocaloffset ;
            k = kg ;

	    if (sendP != prevP )
	    {
#if 0
fprintf(stderr,"sendP (%2d) != prevP (%d) =? P (%d) ||  recvP (%d) =? rsl_myproc (%d)\n",
sendP,prevP,P,recvP,rsl_myproc) ;
#endif
 	      if ( rsl_c_comp2phys_proc(prevP) == P && recvP == rsl_myproc )
	      {
	        if ( kg > 0 )
	        {
	          /* store the pencil (i,ks:ke,jg) as being sent 
	             from sendP and received by recvP */
    
                  fld = msg_to->fldspecs ;
    
                  for ( ; fld != NULL ;  fld = fld->next )
                  {
                    if ( fld->type >= 100 ) xp->has_f90_fields = 1 ;
                    base = fld->base ;
                    elemsz = fld->elemsz ;
                    switch (fld->strategy)
                    {
                      case MINNS_MAJEW_K_3D :             /* <MM> eg: ua(i,j,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"s_p_rv: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d ks %2d ke %2d ke-ks+1 %3d\n",
P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,ks,ke,ke-ks+1) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (i+j*t0+ks*t1)*elemsz, elemsz,
                                                  ke-ks+1 ,
                                                  -t1*elemsz) ;
                        break ;
                      case MINEW_MAJNS_K_3D :             /* <MM> eg: u(j,i,k) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (j+i*t0+ks*t1)*elemsz, elemsz,
                                                  ke-ks+1 ,
                                                  -t1*elemsz) ;
                        break ;
                      case K_MIDNS_MAJEW_3D :             /* <MM> eg: u(k,i,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
                        store_process_refs( base, fld->f90_table_index, (ks+i*t0+j*t1)*elemsz, (ke-ks+1)*elemsz,
                                                  1, 
						  -elemsz) ;
                        break ;
                      case MINNS_K_MAJEW_3D :             /* <MM> eg: u(i,k,j) */
                        t0 = fld->llen[0] ; t1 = fld->llen[1]*t0 ;
#if 0
if (1)fprintf(stderr,"MZ to MN s_p_rv: P %2d prevP %2d recvP %2d ipack %d i %2d j %2d k %2d ig %2d jg %2d t0 %3d t1 %3d ks %2d ke %2d ke-ks+1 %3d\n",
P,prevP,recvP,ipack,i,j,k,ig,jg,t0,t1,ks,ke,ke-ks+1) ;
#endif
                        store_process_refs( base, fld->f90_table_index, (i+ks*t0+j*t1)*elemsz, elemsz,
                                                  ke-ks+1,
						  -t0*elemsz) ;   /* don't need to suppress optimizations unpacking onto MN grid because of pads */
                        break ;
                      default:
                        RSL_TEST_ERR(1,"new pack comp: strategy not supported" ) ;
                        break ;
                    }
                  }
	        }
	      }
#if 0
fprintf(stderr,"resetting ks to %d\n",k) ;
#endif
	      ks = k ;
	    }
	    ke = k ;
#if 0
fprintf(stderr,"resetting ke to %d\n",ke) ;
#endif
	    prevP = sendP ;
#if 0
fprintf(stderr,"resset prevP to %d\n",prevP) ;
#endif
          }
        }
      }
        process_refs( &(procrec->unpack_table),
                      &(procrec->unpack_table_size),
                      &(procrec->unpack_table_nbytes), 1 ) ;
#if 0
        fprintf(stderr,"unpack P = %3d:\n",procrec->P ) ;
        show_pack_table( procrec->unpack_table,
                         procrec->unpack_table_size,
                         procrec->unpack_table_nbytes ) ;
#endif

    }
}

