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

#undef AP1000
#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

/*@
  RSL_INITIALIZE - initialize the RSL package.

  Notes:
  This routine initializes the RSL package and must be called on each
  processor before any other RSL routine.  Once RSL_INITIALIZE is called,
  RSL_MON_BCAST and several other low-level informational RSL routines 
  (e.g. RSL_IAMMONITOR)
  may be used to broadcast configuration data read in on
  processor zero to other processors.  Other RSL routines may not be
  used until RSL_MESH has been called.

  Example:

$  program model
$  integer intsize                     ! Size in bytes of an integer.
$  parameter (intsize = 4)
$  integer nproc_m, nproc_n            ! Number of processors in m, n.
$  integer retval
$  namelist /config/ nproc_m, nproc_n  ! Will be read in from namelist.

$  call RSL_INITIALIZE                 ! Initialize RSL
$  call RSL_IAMMONITOR( retval )       ! Read namelist on processor zero
$  if ( retval .eq. 1 ) then
$    read(10,config)
$  endif
$  call RSL_MON_BCAST( nproc_m, intsize )   ! Broadcast config to other procs.
$  call RSL_MON_BCAST( nproc_n, intsize )   ! Broadcast config to other procs.
$  call RSL_MESH( nproc_m, nproc_n )   ! All processors define processor mesh.
$  ...                                 ! Rest of model.
$  call RSL_SHUTDOWN                   ! Shutdown.
$  stop
$  end
  
BREAKTHEEXAMPLECODE

  See also:
  RSL_SHUTDOWN, RSL_MESH, RSL_MON_BCAST

@*/
RSL_INITIALIZE ()
{
  int s, o ;
#ifndef STUBS
  rslMPIInit() ;
  rsl_mpi_communicator = MPI_COMM_WORLD ;
#endif
  rsl_initialize_internal() ;
  s = 1 ; o = 0 ;
  RSL_DEBUG( &s , &o ) ;
  
}

#ifndef STUBS
RSL_INITIALIZE1 ( MPI_Fint * comm )
{
  rsl_mpi_communicator = MPI_Comm_f2c( *comm ) ;
  rsl_initialize_internal() ;
}
#else
RSL_INITIALIZE1 ( int * comm )
{
  rsl_initialize_internal() ;
}
#endif

rsl_initialize_internal()
{
  char name[256] ;
  int d ;
  int i ;
  int rsl_default_decomp() ;		/* defined in cd.c */

  for ( d = 0 ; d < RSL_MAXDOMAINS ; d++ )
  {
    domain_info[d].valid = RSL_INVALID ;
  }
  for ( i = 0 ; i < RSL_MAXDESCRIPTORS ; i++ ) mh_descriptors[i] = NULL ;
  for ( i = 0 ; i < RSL_MAXDESCRIPTORS ; i++ ) sh_descriptors[i] = NULL ;
  for ( i = 0 ; i < RSL_MAXDESCRIPTORS ; i++ ) xp_descriptors[i] = NULL ;
  for ( i = 0 ; i < RSL_MAXDESCRIPTORS ; i++ ) pr_descriptors[i] = NULL ;
  mh_descriptors[0] = (void*)1 ;  /* leave 0th one alone -- never use.
				   this means that a message handle of
				   zero is always an error */
  rsl_ndomains = 0 ;
  old_offsets = 0 ;

#ifndef STUBS
  rslMPIInit() ;

  MPI_Comm_size( rsl_mpi_communicator , &rsl_nproc_all ) ;
  MPI_Comm_rank( rsl_mpi_communicator , &rsl_myproc ) ;
#else
  rsl_nproc_all = 1 ;
  rsl_myproc = 0 ;
#endif

/* John's patented brain substitute ; 5/3/2002 */
  if ( rsl_nproc_all > RSL_MAXPROC )
  {
    sprintf(mess,"rsl_nproc_all (%d) > RSL_MAXPROC (%d). Recompile RSL with larger value.\n%s\n",rsl_nproc_all,RSL_MAXPROC,
                 "(For WRF, change value of MAX_PROC in configure.wrf)"
    ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }

  rsl_nproc = rsl_nproc_all ; 	/* this may be reset by RSL_MESH */
  rsl_padarea = RSL_DEFAULT_PADAREA ;
  io_seq_monitor = 0 ;  /* OBS */
  io_seq_compute = 1 ;  /* OBS */

  regular_decomp = 0 ;
  sw_allow_dynpad = 0 ;



  RSL_INIT_FORTRAN ( &rsl_nproc_all, &rsl_nproc, &rsl_myproc,
                     &rsl_nproc_m, &rsl_nproc_n, &rsl_ndomains ) ;

#ifndef T3D
  gethostname(name,255) ;
  fprintf(stderr,"%s -- rsl_nproc_all %d, rsl_myproc %d\n",name,
                  rsl_nproc_all, rsl_myproc ) ;
#endif

  RSL_F_SET_PADAREA ( &rsl_padarea ) ;

  rsl_noprobe = (char *)getenv( "RSL_NOPROBE" ) ;
  if ( rsl_noprobe != NULL && rsl_myproc == 0 )
  {
    if ( rsl_myproc == 0 ) 
      fprintf(stderr,"Advisory: RSL_NOPROBE defined.  Won't probe.\n") ;
  }

  for (  d = 0 ; d < RSL_MAXDOMAINS ; d++ )
  {
    domain_info[d].valid = RSL_INVALID ;
    domain_info[d].iruns = NULL ;
    domain_info[d].domain = NULL ;
    domain_info[d].bcast_Xlist = NULL ;
    domain_info[d].merge_Xlist = NULL ;
    { int p ;
      for ( p = 0 ; p < MAX_KINDPAD ; p++ )
      {
        domain_info[d].js[p] = NULL ;
        domain_info[d].is[p] = NULL ;
        domain_info[d].ie[p] = NULL ;
        domain_info[d].jg2n[p] = NULL ;
        domain_info[d].is2[p] = NULL ;
        domain_info[d].js2[p] = NULL ;
        domain_info[d].je2[p] = NULL ;
        domain_info[d].ig2n[p] = NULL ;
      }
    }
    domain_info[d].is_write = RSL_INVALID ;
    domain_info[d].ie_write = RSL_INVALID ;
    domain_info[d].js_write = RSL_INVALID ;
    domain_info[d].je_write = RSL_INVALID ;
    domain_info[d].is_read = RSL_INVALID ;
    domain_info[d].ie_read = RSL_INVALID ;
    domain_info[d].js_read = RSL_INVALID ;
    domain_info[d].je_read = RSL_INVALID ;

  }

  /* set up default decomposition fuctions */
  SET_DEF_DECOMP_FCN ( rsl_default_decomp ) ;  /* defined in cd.c */
  for (  d = 0 ; d < RSL_MAXDOMAINS ; d++ )
  {
    SET_DEF_DECOMP_INFO ( &d, NULL ) ;
  }
}

/*@
  RSL_SHUTDOWN - shut down the RSL package.

  Notes:
  This routine shuts down the RSL package at the end of the program
  and should be called before program termination.

  See also:
  RSL_INITIALIZE, RSL_MESH

@*/
RSL_SHUTDOWN ()
{
  RSL_CLOSE0 () ;
}

/*@
  RSL_MESH - specify a 2-dimensional mesh of processors for the RSL package.

  Notes:
  This routine is used to specify the two-dimensional mesh of processors.
  RSL_INITIALIZE must have already been called.  RSL_MESH
  must be called before any RSL domain has been defined.  Only RSL_MON_BCAST
  will work prior to the call to RSL_MESH; this allows node zero to read
  and broadcast to the other processors configuration information.
  RSL_MESH must be called on all processors.

  Example:
$  program model
$  integer intsize                     ! Size in bytes of an integer.
$  parameter (intsize = 4)
$  integer nproc_m, nproc_n            ! Number of processors in m, n.
$  integer retval
$  namelist /config/ nproc_m, nproc_n  ! Will be read in from namelist.
$
$  call RSL_INITIALIZE                 ! Initialize RSL
$  call RSL_IAMMONITOR( retval )       ! Read namelist on processor zero
$  if ( retval .eq. 1 ) then
$    read(10,config)
$  endif
$  call RSL_MON_BCAST( nproc_m, intsize )   ! Broadcast config to other procs.
$  call RSL_MON_BCAST( nproc_n, intsize )   ! Broadcast config to other procs.
$  call RSL_MESH( nproc_m, nproc_n )   ! All processors define processor mesh.
$  ...                                 ! Rest of model.
$  call RSL_SHUTDOWN                   ! Shutdown.
$  stop
$  end
BREAKTHEEXAMPLECODE

  See also:
  RSL_INITIALIZE, RSL_MON_BCAST, RSL_SHUTDOWN
@*/
RSL_MESH (nproc_m_p, nproc_n_p )
  int_p 
    nproc_m_p   /* (I) Number of processors decomposing M dimension. */
   ,nproc_n_p   /* (I) Number of processors decomposing N dimension. */
   ;
{
  char name[256] ;
  int d ;
  int i ;
  int rsl_default_decomp() ;		/* defined in cd.c */

  rsl_nproc_m = *nproc_m_p ;
  rsl_nproc_n = *nproc_n_p ;
  rsl_nproc = rsl_nproc_n * rsl_nproc_m ;

  rsl_padarea = RSL_DEFAULT_PADAREA ;
  io_seq_monitor = 0 ;  /* OBS */
  io_seq_compute = 1 ;  /* OBS */

  RSL_INIT_FORTRAN ( &rsl_nproc_all, &rsl_nproc, &rsl_myproc,
                     &rsl_nproc_m, &rsl_nproc_n, &rsl_ndomains ) ;
  RSL_F_SET_PADAREA ( &rsl_padarea ) ;
  if ( rsl_nproc_all < rsl_nproc )
  {
    sprintf(mess,"RSL_MESH: %d is too few processors (need px*py=%d)",
                 rsl_nproc_all, rsl_nproc ) ;
    RSL_TEST_ERR( rsl_nproc_all < rsl_nproc, mess ) ;
  }

}

/*@
  RSL_OLD_OFFSETS -- Calculate local indices using old F77 MPMM strategy.

  Notes:
  This routine is provided for backward compatibility with the F77
  parallel implementation of MM5, MPMM, which was developed
  using an earlier version of RSL.  Call this routine after
  RSL_INITIALIZE in the model to use this version of RSL with MPMM.

  See also:
  RSL_INITIALIZE

@*/
RSL_OLD_OFFSETS ()
{
  old_offsets = 1 ;
}

RSL_SET_REGULAR_DECOMP ()
{
  regular_decomp = 1 ;
}

RSL_GET_COMMUNICATOR ( communicator )
  int_p  communicator ;  /* (O) return value with communicator from underlying mp layer (mpi probably) */
{
#ifdef MPI
  *communicator = MPI_Comm_c2f( rsl_mpi_communicator ) ;
#else
  *communicator = 0 ;
#endif
}

RSL_SET_COMMUNICATOR ( communicator )
  int_p  communicator ;  /* (O) return value with communicator from underlying mp layer (mpi probably) */
{
#ifdef MPI
  rsl_mpi_communicator = MPI_Comm_f2c( *communicator ) ;
#endif
}


