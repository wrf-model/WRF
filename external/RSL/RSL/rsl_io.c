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

/*@
  RSL_CLOSE - close a fortran unit

  Synopsis:
  RSL_CLOSE ( unit )
  integer unit

  Input parameters:
. unit - unit number

  Notes:
  Used to close down a fortran unit number.  At present, RSL does
  not provide a way of opening a named file, so RSL_CLOSE does
  no do much except flush the identifier.

@*/
RSL_CLOSE ( unit_p )
  int_p unit_p ;
{
/* this was added 7/27/94 to the monitor-less version.  It won't
   work, as is, in the monitor/compute version. */
  int i_am_monitor ;

  RSL_C_IAMMONITOR ( &i_am_monitor ) ;
  if ( i_am_monitor )
  {
    FORT_CLOSE ( unit_p ) ;
  }
}

int io_debug = 0 ;

enable_rsl_debug() { io_debug = 1 ; }
disable_rsl_debug() { io_debug = 0 ; }

/*@
  

  Notes:
  Used to read in one record from an unformatted (binary) Fortran
  file or from a globally dimensioned data structure in the memory
  of the monitor processor (see RSL_IAMMONITOR).  The record should
  contain an undecomposed two- or three-dimensional array.  As a result
  of the read, the record is distributed according to the decomposition
  in effect for the domain whose RSL descriptor is given as argument
  Arg4.


  The layout of the
  array in memory is specified by Arg2, which may be

  Verbatim:
$     IO2D_IJ,
$     IO2D_JI,
$     IO3D_IJK, or
$     IO3D_JIK
BREAKTHEEXAMPLECODE

  for reads from a file, or

  Verbatim:
$     IO2D_IJ_INTERNAL,
$     IO2D_JI_INTERNAL,
$     IO3D_IJK_INTERNAL, or
$     IO3D_JIK_INTERNAL
BREAKTHEEXAMPLECODE

  for reads from the memory of the monitor processor.  The constants
  are defined in the header file rsl.inc.


  Internal reads are useful 
  for distributing the results
  from global calculations performed on the monitor node.
  Although this is not a scalable technique --- communication from
  the monitor processor becomes a bottleneck --- it is often practical
  and much simpler to implement for operations that are performed
  only once during initialization, or infrequently.

  For internal reads, instead of a unit number,
  the first argument is the global (undecomposed)
  data structure from which the data is to be read and distributed.
  The data must actually populated the data structure only on the
  monitor processor (the other processors treat the first argument
  as a dummy).  The monitor distributes the data and, on return
  from the read, the local (decomposed) portion of the array 
  is returned as Arg3 on each processor.

  IO2D_IJ specifies a two-dimensional array
  whose minor dimension is M.  IO3D_JI specifies a two-dimensional array
  whose minor dimension is N.  (This is as
  as specified in the call to the routine that created or spawned
  the domain; e.g., RSL_MOTHER_DOMAIN).

  The type argument specifies the data type of an array element and
  may be one of
  Verbatim:
$     RSL_REAL,
$     RSL_DOUBLE,
$     RSL_COMPLEX,
$     RSL_INTEGER, or
$     RSL_CHARACTER.  
BREAKTHEEXAMPLECODE

  The Arg6 array should contain the global (undecomposed) size of
  each dimension in order from minor to major.  The first element of
  glen is the size of the minor dimension.  The Arg7 array should
  contain the size of each dimension as statically declared on the
  processor.

  Example:
$ C Example 1, reading from a file
$ C
$    #include "rsl.inc"
$       real ua(mix,mjx,mkx)
$       integer glen(3), llen(3)
$       glen(1) = il
$       glen(2) = jl
$       glen(3) = mkx
$       llen(1) = mix
$       llen(2) = mjx
$       llen(3) = mkx
$ C
$       call rsl_read( 10, IO3D_IJK, ua, did,
$     +               RSL_REAL, glen, llen )
$ C
BREAKTHEEXAMPLECODE

  In the example a three-dimensional field, ua, is read in
  from Fortran unit 10 on 
  domain DID.
  GLEN(1) and GLEN(2) are set to the global sizes of the two
  horizontal dimensions.
  LLEN(1) and LLEN(2) are set to the
  static sizes of the local array (mix, mjx, and mkx are 
  Fortran parameters).

  See also:
  RSL_WRITE

@*/
RSL_READ ( unit_p, iotag_p, base, d_p, type_p, glen, llen  )
  int_p
    unit_p              /* (I) Fortran I/O unit number. */
   ,iotag_p ;           /* (I) RSL I/O code. */
  char *
    base ;              /* (O) Buffer. */
  int_p
    d_p                 /* (I) RSL domain descriptor */
   ,type_p ;            /* (I) RSL data type code. */
  int
    glen[]              /* (I) Global dimension information. */
   ,llen[] ;            /* (I) Local dimension information. */
{
  rsl_read_req_t request ;
  rsl_read_resp_t resp ;
  rsl_processor_t me ;
  int cursor, mdest, mtag, msglen, dim, d ;
  int mlen, nlen, minelems, majelems ;
  unsigned long ig, jg, min, maj, ioffset, joffset, tlen, k ;
  void *dex ;
  char *pbuf ;
  int i_am_monitor ;
  rsl_point_t *domain ;
  int iotag ;


  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_read: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_read: invalid domain") ;
  if ( domain_info[d].decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }
  mlen = domain_info[d].len_m ;
  nlen = domain_info[d].len_n ;
  domain = domain_info[d].domain ;
  iotag = *iotag_p ;

  RSL_C_IAMMONITOR( &i_am_monitor ) ;
  me = rsl_c_phys2comp_proc( rsl_myproc ) ;
  ioffset = domain_info[*d_p].ilocaloffset ;
  joffset = domain_info[*d_p].jlocaloffset ;
  tlen = elemsize( *type_p ) ;

  switch( iotag )
  {
  case IO2D_IJ_INTERNAL :
    iotag = IO2D_IJ ;
    request.internal = 1 ;
    break ;
  case IO2D_JI_INTERNAL :
    iotag = IO2D_JI ;
    request.internal = 1 ;
    break ;
  case IO3D_IJK_INTERNAL :
    iotag = IO3D_IJK ;
    request.internal = 1 ;
    break ;
  case IO3D_JIK_INTERNAL :
    iotag = IO3D_JIK ;
    request.internal = 1 ;
    break ;
  case IO3D_IKJ_INTERNAL :
    iotag = IO3D_IKJ ;
    request.internal = 1 ;
    break ;
  default :
    request.internal = 0 ;
    break ;
  }

  request.request_type = RSL_READ_REQUEST ;
  request.request_mode = MSG_IO_FORTRAN ;
  request.myproc = rsl_myproc ;
  request.base = base ;
  request.domain = *d_p ;
  request.unit = *unit_p ;
  request.unit_p = unit_p ;
  request.type = *type_p ;
  request.iotag = iotag ;
  request.sequence = io_seq_compute++ ;

  switch( iotag )
  {
  case IO2D_IJ :
    request.ndim = 2 ;
    minelems = request.glen[0] ;
    majelems = request.glen[1] ;
    break ;
  case IO2D_JI :
    request.ndim = 2 ;
    minelems = request.glen[1] ;
    majelems = request.glen[0] ;
    break ;
  case IO3D_IJK :
    RSL_TEST_ERR(glen[2] > llen[2],
       "rsl_write: global len of K dim is greater than local len") ;
    request.ndim = 3 ;
    minelems = request.glen[0] ;
    majelems = request.glen[1] ;
    break ;
  case IO3D_JIK :
    RSL_TEST_ERR(glen[2] > llen[2],
       "rsl_write: global len of K dim is greater than local len") ;
    request.ndim = 3 ;
    minelems = request.glen[1] ;
    majelems = request.glen[0] ;
    break ;
  case IO3D_KIJ :
    RSL_TEST_ERR(glen[0] > llen[0],
       "rsl_write: global len of K dim is greater than local len") ;
    request.ndim = 3 ;
    minelems = request.glen[1] ;
    majelems = request.glen[2] ;
    break ;
  case IO3D_IKJ :
    RSL_TEST_ERR(glen[1] > llen[1],
       "rsl_write: global len of K dim is greater than local len") ;
    request.ndim = 3 ;
    minelems = request.glen[0] ;
    majelems = request.glen[2] ;
    break ;

  default:
    RSL_TEST_ERR(1,"rsl_read: unknown data tag") ;
  }

  for ( dim = 0 ; dim < request.ndim ; dim++ )
  {
    request.glen[dim] = glen[dim] ;
    request.llen[dim] = llen[dim] ;
  }

  pbuf = NULL ;
  if ( i_am_monitor )
  {
    /* note ! this routine allocates pbuf */
    handle_read_request( &request, &resp, &pbuf ) ;
  }
  else
  {
    mdest = RSL_C_MONITOR_PROC () ;
    mtag = MTYPE_FROMTO( MSG_READ_RESPONSE, mdest, rsl_myproc ) ;
    msglen = sizeof(resp) ;
    RSL_RECV( &resp, msglen, mtag ) ;

    pbuf = RSL_MALLOC( char, resp.tofollow ) ;
    msglen = resp.tofollow ;
    RSL_RECV( pbuf, msglen, mtag ) ;
  }

  if ( pbuf != NULL ) 
  {
  /* we do it this way to ensure that we unpack in the same
     order that the data were packed on the monitor */
  cursor = 0 ;
#ifndef vpp

  for ( jg = 0 ; jg < nlen ; jg++ )
  {
    for ( ig = 0 ; ig < mlen ; ig++ )
    {
      if ( me == domain[INDEX_2(jg,ig,mlen)].P )
      {
        switch( iotag )
        {
        case IO2D_IJ :
          min = ig - ioffset ;
          maj = jg - joffset ;
          dex = base+tlen*(min+maj*llen[0]) ;
          bcopy(&(pbuf[cursor]),dex,tlen) ;
          cursor += tlen ;
          break ;
        case IO2D_JI :
          min = jg - joffset ;
          maj = ig - ioffset ;
          dex = base+tlen*(min+maj*llen[0]) ;
          bcopy(&(pbuf[cursor]),dex,tlen) ;
          cursor += tlen ;
          break ;
        case IO3D_IJK :
          min = ig - ioffset ;
          maj = jg - joffset ;
          for ( k = 0 ; k < glen[2] ; k++ )
          {
            dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
            bcopy(&(pbuf[cursor]),dex,tlen) ;
            cursor += tlen ;
          }
          break ;
        case IO3D_JIK :
          min = jg - joffset ;
          maj = ig - ioffset ;
          for ( k = 0 ; k < glen[2] ; k++ )
          {
            dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
            bcopy(&(pbuf[cursor]),dex,tlen) ;
            cursor += tlen ;
          }
          break ;
        case IO3D_KIJ :
          min = ig - ioffset ;
          maj = jg - joffset ;
          for ( k = 0 ; k < glen[0] ; k++ )
          {
            dex = base+tlen*(k+llen[0]*(min+maj*llen[1])) ;
            bcopy(&(pbuf[cursor]),dex,tlen) ;
            cursor += tlen ;
          }
          break ;
        case IO3D_IKJ :
          min = ig - ioffset ;
          maj = jg - joffset ;
          for ( k = 0 ; k < glen[1] ; k++ )
          {
            dex = base+tlen*(min+llen[0]*(k+maj*llen[1])) ;
            bcopy(&(pbuf[cursor]),dex,tlen) ;
            cursor += tlen ;
          }
          break ;

        }
      }
    }
  }
#else
/* assumes 1 d decomp in j only */
  for ( jg = 0 ; jg < nlen ; jg++ )
  {
    if ( me == domain[INDEX_2(jg,0,mlen)].P )
    {
        switch( iotag )
        {
        case IO2D_IJ :
          if ( request.type == RSL_REAL )
          {
            min = 0 - ioffset ;
            maj = jg - joffset ;
            dex = base+tlen*(min+maj*llen[0]) ;
            VRCOPY (&(pbuf[cursor]),dex,&mlen) ;
            cursor += tlen*mlen ;
          }
          else
          {
            for ( ig = 0 ; ig < mlen ; ig++ )
            {
              min = ig - ioffset ;
              maj = jg - joffset ;
              dex = base+tlen*(min+maj*llen[0]) ;
              bcopy(&(pbuf[cursor]),dex,tlen) ;
              cursor += tlen ;
            }
          }
          break ;
        case IO2D_JI :
          for ( ig = 0 ; ig < mlen ; ig++ )
          {
            min = jg - joffset ;
            maj = ig - ioffset ;
            dex = base+tlen*(min+maj*llen[0]) ;
            bcopy(&(pbuf[cursor]),dex,tlen) ;
            cursor += tlen ;
          }
          break ;
        case IO3D_IJK :
          maj = jg - joffset ;
          if ( request.type == RSL_REAL )
          {
            for ( k = 0 ; k < glen[2] ; k++ )                 /* note reversal of k and i packing order for vpp */
            {
              min = 0 - ioffset ;
              dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
              VRCOPY ( &(pbuf[cursor]),dex,&mlen) ;
              cursor += tlen*mlen ;
            }
          }
          else
          {
            for ( k = 0 ; k < glen[2] ; k++ )                 /* note reversal of k and i packing order for vpp */
            {
              for ( ig = 0 ; ig < mlen ; ig++ )
              {
                min = ig - ioffset ;
                dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
                bcopy(&(pbuf[cursor]),dex,tlen) ;
                cursor += tlen ;
              }
            }
          }
          break ;
        case IO3D_JIK :
          for ( ig = 0 ; ig < mlen ; ig++ )
          {
            min = jg - joffset ;
            maj = ig - ioffset ;
            for ( k = 0 ; k < glen[2] ; k++ )
            {
              dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
              bcopy(&(pbuf[cursor]),dex,tlen) ;
              cursor += tlen ;
            }
          }
          break ;
        case IO3D_KIJ :
          for ( ig = 0 ; ig < mlen ; ig++ )
          {
            min = ig - ioffset ;
            maj = jg - joffset ;
            for ( k = 0 ; k < glen[0] ; k++ )
            {
              dex = base+tlen*(k+llen[0]*(min+maj*llen[1])) ;
              bcopy(&(pbuf[cursor]),dex,tlen) ;
              cursor += tlen ;
            }
          }
          break ;
        case IO3D_IKJ :
          for ( ig = 0 ; ig < mlen ; ig++ )
          {
            min = ig - ioffset ;
            maj = jg - joffset ;
            for ( k = 0 ; k < glen[1] ; k++ )
            {
              dex = base+tlen*(min+llen[0]*(k+maj*llen[1])) ;
              bcopy(&(pbuf[cursor]),dex,tlen) ;
              cursor += tlen ;
            }
          }
          break ;

        }
    }
  }
#endif

  RSL_FREE( pbuf ) ;
  }
}

/************/

/*@
  RSL_WRITE - Collect and write a distributed array to a file or memory.

  Notes:
  Used to write one record to an unformatted (binary) Fortran
  file or to a globally dimensioned data structure in the memory
  of the monitor processor (see RSL_IAMMONITOR).  After the write,
  the record will
  contain an undecomposed two- or three-dimensional array.

  The layout of the
  array to be written is specified by Arg2, which may be

  Verbatim:
$     IO2D_IJ,
$     IO2D_JI,
$     IO3D_IJK, or
$     IO3D_JIK
BREAKTHEEXAMPLECODE

  for writes to a file, or

  Verbatim:
$     IO2D_IJ_INTERNAL,
$     IO2D_JI_INTERNAL,
$     IO3D_IJK_INTERNAL, or
$     IO3D_JIK_INTERNAL
BREAKTHEEXAMPLECODE

  for writes to the memory of the monitor processor.  The constants
  are defined in the header file rsl.inc.
  
  For internal writes, instead of a unit number,
  the first argument is the global (undecomposed)
  data structure into which the distributed data is written.
  All processors send their portion of the distributed array
  to the monitor processor.  On that processor, the write 
  returns with the global data structure.

  Internal writes are useful for collecting distributed data onto
  one processor to perform global operations (RSL_READ may then
  be used to redistribute the results with an internal read, or
  if the operation is a reduction, the result can be broadcast
  using RSL_MON_BCAST).
  Although this is not a scalable technique --- communication to
  the monitor processor becomes a bottleneck --- it is often practical
  and much simpler to implement for operations that are performed
  only once during initialization, or infrequently (see Example 2 below).
     
  IO2D_IJ specifies a two-dimensional array
  whose minor dimension is M.  IO3D_JI specifies a two-dimensional array
  whose minor dimension is N.  (This is as
  as specified in the call to the routine that created or spawned
  the domain; e.g., RSL_MOTHER_DOMAIN).

  The type argument specifies the data type of an array element and
  may be one of
  Verbatim:
$     RSL_REAL,
$     RSL_DOUBLE,
$     RSL_COMPLEX,
$     RSL_INTEGER, or
$     RSL_CHARACTER.  
BREAKTHEEXAMPLECODE

  The Arg6 array should contain the global (undecomposed) size of
  each dimension in order from minor to major.  The first element of
  glen is the size of the minor dimension.  The Arg7 array should
  contain the size of each dimension as statically declared on the
  processor.

  Example:

$ C
$ C Example 1, writing a distributed array to unit 11.
$ C
$    #include "rsl.inc"
$       real ua(mix,mjx,mkx)
$       integer glen(3), llen(3)
$       glen(1) = il
$       glen(2) = jl
$       glen(3) = mkx
$       llen(1) = mix
$       llen(2) = mjx
$       llen(3) = mkx
$       call rsl_write( 11, IO3D_IJK, ua, domains(inest),
$     +                 RSL_REAL, glen, llen )
$ C
$ C Example 2, part of a global initialization in MM90.
$ C
$       ALLOCATE( ASTORE_G(IL,JL) )
$       ...
$       CALL RSL_WRITE(ASTORE_G, IO2D_IJ_INTERNAL,
$     +                ASTORE,DID,RSL_REAL,GLEN,LLEN)
$       ...
$       CALL RSL_IAMMONITOR(RETVAL)
$       IF(RETVAL.EQ.1)THEN
$         DO J=1,JL
$           DO I=1,IL
$             ATOT=ATOT+ASTORE_G(I,J)
$           ENDDO
$       ENDDO
$         NPTS=IL*JL
$         ABAR=ATOT/NPTS
$       ENDIF
$       CALL RSL_MON_BCAST(  ABAR,    WORDSIZE )
BREAKTHEEXAMPLECODE

  In the example a three-dimensional field, ua, is written to
  Fortran unit 11 from
  domain DID.
  GLEN(1) and GLEN(2) are set to the global sizes of the two
  horizontal dimensions.
  LLEN(1) and LLEN(2) are set to the
  static sizes of the local array (mix, mjx, and mkx are
  Fortran parameters).

  In example two, a reduction is used to compute an average
  that will be used to initialize a calculation in a weather
  model.  RSL_WRITE is used to write into the globally dimensioned
  array ASTORE_G; RSL_IAMMONITOR is used to limit the calculation
  to the monitor processor; RSL_MON_BCAST is used to broadcast
  back the result.


  See also:
  RSL_READ

@*/
RSL_WRITE ( unit_p, iotag_p, base, d_p, type_p, glen, llen  )
  int_p
    unit_p              /* (I) Fortran unit number. */
   ,iotag_p ;           /* (I) RSL I/O code. */
  char *
    base ;              /* (I) Buffer. */
  int_p
    d_p                 /* (I) RSL domain descriptor */
   ,type_p ;            /* (I) RSL data type code. */
  int
    glen[]              /* (I) Global dimension information. */
   ,llen[] ;            /* (I) Local dimension information. */
{
  rsl_read_req_t request ;
  rsl_read_resp_t resp ;
  rsl_processor_t me ;
  int cursor, mdest, mtag, msglen, dim, d ;
  int mlen, nlen ;
  int minelems, majelems ;
  unsigned long min, maj, ioffset, joffset, tlen, k ;
  int ig, jg ;
  void *dex ;
  char *pbuf ;
  int i_am_monitor ;
  int psize, nelem, typelen, nbytes, columnelems  ;
  rsl_point_t *domain ;
  int iotag ;
  int *is_write, *ie_write, *js_write, *je_write ;
  int in_write ;
  int dummy ;

  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_write: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_write: invalid domain") ;
  if ( domain_info[d].decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }
  mlen = domain_info[d].len_m ;
  nlen = domain_info[d].len_n ;
  domain = domain_info[d].domain ;

  is_write = &( domain_info[d].is_write ) ;
  js_write = &( domain_info[d].js_write ) ;
  ie_write = &( domain_info[d].ie_write ) ;
  je_write = &( domain_info[d].je_write ) ;

/* reset and recompute each time; otherwise smaller fields will truncate bigger fields later on . JM 20030417.  */
  *is_write = RSL_INVALID ;
  *ie_write = RSL_INVALID ;
  *js_write = RSL_INVALID ;
  *je_write = RSL_INVALID ;

  iotag = *iotag_p ;

  RSL_C_IAMMONITOR( &i_am_monitor ) ;

  me = rsl_c_phys2comp_proc( rsl_myproc ) ;
  ioffset = domain_info[*d_p].ilocaloffset ;
  joffset = domain_info[*d_p].jlocaloffset ;
  tlen = elemsize( *type_p ) ;

  switch( iotag )
  {
  case IO2D_IJ_INTERNAL :
    iotag = IO2D_IJ ;
    request.internal = 1 ;
    break ;
  case IO2D_JI_INTERNAL :
    iotag = IO2D_JI ;
    request.internal = 1 ;
    break ;
  case IO3D_IJK_INTERNAL :
    iotag = IO3D_IJK ;
    request.internal = 1 ;
    break ;
  case IO3D_JIK_INTERNAL :
    iotag = IO3D_JIK ;
    request.internal = 1 ;
    break ;
  case IO3D_IKJ_INTERNAL :
    iotag = IO3D_IKJ ;
    request.internal = 1 ;
    break ;
  default :
    request.internal = 0 ;
    break ;
  }

  request.request_type = RSL_WRITE_REQUEST ;
  request.request_mode = MSG_IO_FORTRAN ;
  request.myproc = rsl_myproc ;
  request.base = base ;
  request.domain = *d_p ;
  request.unit = *unit_p ;
  request.unit_p = unit_p ;
  request.type = *type_p ;
  request.iotag = iotag ;
  request.sequence = io_seq_compute++ ;

  switch( iotag )
  {
  case IO2D_IJ :
    request.ndim = 2 ;
    break ;
  case IO2D_JI :
    request.ndim = 2 ;
    break ;
  case IO3D_IJK :
    request.ndim = 3 ;
    RSL_TEST_ERR(glen[2] > llen[2],
       "rsl_write: global len of K dim is greater than local len") ;
    break ;
  case IO3D_JIK :
    RSL_TEST_ERR(glen[2] > llen[2],
       "rsl_write: global len of K dim is greater than local len") ;
    request.ndim = 3 ;
    break ;
  case IO3D_KIJ :
    RSL_TEST_ERR(glen[0] > llen[0],
       "rsl_write: global len of K dim is greater than local len") ;
    request.ndim = 3 ;
    break ;
  case IO3D_IKJ :
    RSL_TEST_ERR(glen[1] > llen[1],
       "rsl_write: global len of K dim is greater than local len") ;
    request.ndim = 3 ;
    break ;
  default:
    RSL_TEST_ERR(1,"rsl_write: unknown data tag") ;
  }
  for ( dim = 0 ; dim < request.ndim ; dim++ )
  {
    request.glen[dim] = glen[dim] ;
    request.llen[dim] = llen[dim] ;
  }

  /* figure out size of buffer needed */
  nelem = 1 ;
  for  ( dim = 0 ; dim < request.ndim ; dim++ )
  {
    nelem *= request.glen[dim] ;
  }
  typelen = elemsize( request.type ) ;
  nbytes = nelem * typelen ;

  switch ( request.iotag )
  {
  case IO2D_IJ :
    columnelems = 1 ;
    minelems = request.glen[0] ;
    majelems = request.glen[1] ;
    break ;
  case IO2D_JI :
    columnelems = 1 ;
    minelems = request.glen[1] ;
    majelems = request.glen[0] ;
    break ;
  case IO3D_IJK :
    columnelems = request.glen[2] ;
    minelems = request.glen[0] ;
    majelems = request.glen[1] ;
    break ;
  case IO3D_JIK :
    columnelems = request.glen[2] ;
    minelems = request.glen[1] ;
    majelems = request.glen[0] ;
    break ;
  case IO3D_KIJ :
    columnelems = request.glen[0] ;
    minelems = request.glen[1] ;
    majelems = request.glen[2] ;
    break ;
  case IO3D_IKJ :
    columnelems = request.glen[1] ;
    minelems = request.glen[0] ;
    majelems = request.glen[2] ;
    break ;
  default:
    RSL_TEST_ERR(1,"handle_write_request: unknown data tag") ;
  }

  /*  figure out size for this processor */
  pbuf = NULL ;
  psize = (regular_decomp)?(4*sizeof(int)):0 ;

  RSL_TEST_ERR( majelems <= 0, "Major dim spec on write is zero or less.") ;
  RSL_TEST_ERR( minelems <= 0, "Minor dim spec on write is zero or less.") ;
  if ( majelems > domain_info[request.domain].len_n )
  { sprintf(mess,"Major dim spec on write (%d) greater than global domain definition in that dimension (%d)\n",majelems,domain_info[request.domain].len_n) ;
    RSL_TEST_ERR(1,mess) ; }
  if ( minelems > domain_info[request.domain].len_m )
  { sprintf(mess,"Minor dim spec on write (%d) greater than global domain definition in that dimension (%d)\n",minelems,domain_info[request.domain].len_m) ;
    RSL_TEST_ERR(1,mess) ; }

  for ( jg = 0 ; jg < majelems ; jg++ )
  {
    for ( ig = 0 ; ig < minelems ; ig++ )
    {
      if ( me == domain[INDEX_2(jg,ig,mlen)].P )
        psize += columnelems * typelen ;
    }
  }

  pbuf = RSL_MALLOC( char, psize ) ;

  cursor = 0 ;

  if ( regular_decomp )
  {
    if ( *is_write == RSL_INVALID )
    {
      for ( jg = 0 ; jg < majelems ; jg++ )
      {
        for ( ig = 0 ; ig < minelems ; ig++ )
        {
          if ( me == domain[INDEX_2(jg,ig,mlen)].P )
          {
            if ( *is_write == RSL_INVALID ) { *is_write = ig ; }
            if ( *js_write == RSL_INVALID ) { *js_write = jg ; }
            *ie_write = ig ;
            *je_write = jg ;
          }
        }
      }
      if ( *is_write == RSL_INVALID )   /* nothing set */
      {
        *is_write = 0 ;      /* set so no iterations occur */
        *js_write = 0 ;
        *ie_write = -1 ;
        *je_write = -1 ;
      }
    }

    bcopy( is_write, &(pbuf[cursor]), sizeof(int) ) ; cursor += sizeof(int) ;
    bcopy( ie_write, &(pbuf[cursor]), sizeof(int) ) ; cursor += sizeof(int) ;
    bcopy( js_write, &(pbuf[cursor]), sizeof(int) ) ; cursor += sizeof(int) ;
    bcopy( je_write, &(pbuf[cursor]), sizeof(int) ) ; cursor += sizeof(int) ;

    if ( *ie_write == -1 )
    {
      in_write = 0 ;
    }
    else
    {
      in_write = *ie_write - *is_write + 1 ;
    }

    for ( jg = *js_write ; jg <= *je_write ; jg++ )
    {
        switch( iotag )
        {
        case IO2D_IJ :
	  if ( request.type == RSL_REAL ) 
	  {
            min = *is_write - ioffset ;
            maj = jg - joffset ;
            dex = base+tlen*(min+maj*llen[0]) ;
            VRCOPY (dex,&(pbuf[cursor]),&in_write) ;
            cursor += tlen*in_write ;
	  }
	  else
	  {
            for ( ig = *is_write ; ig <= *ie_write ; ig++ )
            {
              min = ig - ioffset ;
              maj = jg - joffset ;
              dex = base+tlen*(min+maj*llen[0]) ;
              bcopy(dex,&(pbuf[cursor]),tlen) ;
              cursor += tlen ;
            }
          }
          break ;
        case IO2D_JI :
          for ( ig = *is_write ; ig <= *ie_write ; ig++ )
          {
            min = jg - joffset ;
            maj = ig - ioffset ;
            dex = base+tlen*(min+maj*llen[0]) ;
            bcopy(dex,&(pbuf[cursor]),tlen) ;
            cursor += tlen ;
	  }
          break ;
        case IO3D_IJK :
          maj = jg - joffset ;
	  if ( request.type == RSL_REAL )
	  {
            for ( k = 0 ; k < glen[2] ; k++ )                 /* note reversal of k and i packing order for vpp */
            {
              min = *is_write - ioffset ;
              dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
	      VRCOPY ( dex,&(pbuf[cursor]),&in_write) ;
              cursor += tlen*in_write ;
	    }
	  }
	  else
	  {
            for ( k = 0 ; k < glen[2] ; k++ )                 /* note reversal of k and i packing order for vpp */
            {
	      for ( ig = *is_write ; ig <= *ie_write ; ig++ )
	      {
                min = ig - ioffset ;
                dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
                bcopy(dex,&(pbuf[cursor]),tlen) ;
                cursor += tlen ;
	      }
            }
          }
          break ;
        case IO3D_JIK :
          for ( ig = *is_write ; ig <= *ie_write ; ig++ )
          {
            min = jg - joffset ;
            maj = ig - ioffset ;
            for ( k = 0 ; k < glen[2] ; k++ )
            {
              dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
              bcopy(dex,&(pbuf[cursor]),tlen) ;
              cursor += tlen ;
            }
	  }
          break ;

        case IO3D_KIJ :
          for ( ig = *is_write ; ig <= *ie_write ; ig++ )
          {
            min = ig - ioffset ;
            maj = jg - joffset ;
            for ( k = 0 ; k < glen[0] ; k++ )
            {
              dex = base+tlen*(k+llen[0]*(min+maj*llen[1])) ;
              bcopy(dex,&(pbuf[cursor]),tlen) ;
              cursor += tlen ;
            }
          }
          break ;

        case IO3D_IKJ :
          for ( ig = *is_write ; ig <= *ie_write ; ig++ )
          {
            min = ig - ioffset ;
            maj = jg - joffset ;
            for ( k = 0 ; k < glen[1] ; k++ )
            {
              dex = base+tlen*(min+llen[0]*(k+maj*llen[1])) ;
              bcopy(dex,&(pbuf[cursor]),tlen) ;
              cursor += tlen ;
            }
          }
          break ;

        }
    }
  }
  else
  {
    for ( jg = 0 ; jg < majelems ; jg++ )
    {
      for ( ig = 0 ; ig < minelems ; ig++ )
      {
        if ( me == domain[INDEX_2(jg,ig,mlen)].P )
        {
          switch( iotag )
          {
          case IO2D_IJ :
            min = ig - ioffset ;
            maj = jg - joffset ;
            dex = base+tlen*(min+maj*llen[0]) ;
            bcopy(dex,&(pbuf[cursor]),tlen) ;
            cursor += tlen ;
            break ;
          case IO2D_JI :
            min = jg - joffset ;
            maj = ig - ioffset ;
            dex = base+tlen*(min+maj*llen[0]) ;
            bcopy(dex,&(pbuf[cursor]),tlen) ;
            cursor += tlen ;
            break ;
          case IO3D_IJK :
            min = ig - ioffset ;
            maj = jg - joffset ;
            for ( k = 0 ; k < glen[2] ; k++ )
            {
              dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
              bcopy(dex,&(pbuf[cursor]),tlen) ;
              cursor += tlen ;
            }
            break ;
          case IO3D_JIK :
            min = jg - joffset ;
            maj = ig - ioffset ;
            for ( k = 0 ; k < glen[2] ; k++ )
            {
              dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
              bcopy(dex,&(pbuf[cursor]),tlen) ;
              cursor += tlen ;
            }
            break ;

          case IO3D_KIJ :
            min = ig - ioffset ;
            maj = jg - joffset ;
            for ( k = 0 ; k < glen[0] ; k++ )
            {
              dex = base+tlen*(k+llen[0]*(min+maj*llen[1])) ;
              bcopy(dex,&(pbuf[cursor]),tlen) ;
              cursor += tlen ;
            }
            break ;
          case IO3D_IKJ :
            min = ig - ioffset ;
            maj = jg - joffset ;
            for ( k = 0 ; k < glen[1] ; k++ )
            {
              dex = base+tlen*(min+llen[0]*(k+maj*llen[1])) ;
              bcopy(dex,&(pbuf[cursor]),tlen) ;
              cursor += tlen ;
            }
            break ;
          }
        }
      }
    }
  }

  if ( pbuf != NULL )
  {
  if ( i_am_monitor )
  {
    handle_write_request( &request, nelem, psize, pbuf ) ;
  }
  else
  {
#ifdef RSL_SYNCIO
    mdest = RSL_C_MONITOR_PROC () ;
    msglen = 1 ;
    mtag = MTYPE_FROMTO( MSG_WRITE_COMPUTE_RESPONSE, mdest, rsl_myproc ) ;
    RSL_RECV( &dummy, msglen, mtag ) ;
#endif
    mdest = RSL_C_MONITOR_PROC () ;
    msglen = psize ;
    mtag = MTYPE_FROMTO( MSG_WRITE_COMPUTE_RESPONSE, rsl_myproc, mdest ) ;
    RSL_SEND( pbuf, msglen, mtag, mdest ) ;
  }

  RSL_FREE( pbuf ) ;
  }
}



RSL_IO_SHUTDOWN ()
{ 
  rsl_read_req_t request ;
  int mdest, mtag, msglen ;

  request.request_type = RSL_SHUTDOWN_REQUEST ;
  request.sequence = io_seq_compute++ ;
  mdest = RSL_C_MONITOR_PROC () ;
  mtag = MSG_MONITOR_REQUEST ;
  msglen = sizeof( request ) ;

  RSL_SEND( &request, msglen, mtag, mdest ) ;

  return ;
}

/* this is collective over all processors, but not every processor will
   necessarily have data at input or output. Map the patch from the input 
   array onto the patch of the output array, communicating as necessary.
   code assumes (for now) anyway, that each patch is disjoint on each side. */

RSL_REMAP_ARRAY ( inbuf, ndim_p, type_p,
                          is_dimd, ie_dimd, 
                          is_dimp, ie_dimp, is_dimm, ie_dimm,
                  outbuf, os_dimp, oe_dimp, os_dimm, oe_dimm )
  char inbuf[], outbuf[] ;
  int_p ndim_p, type_p ;
  int is_dimd[], is_dimp[], is_dimm[], os_dimp[], os_dimm[] ;
  int ie_dimd[], ie_dimp[], ie_dimm[], oe_dimp[], oe_dimm[] ;
{

#ifndef STUBS

#define STRT 0
#define ENDD 1
#define PCH  0
#define MEM  1
#define DOM  2

  int group_i[2][3][RSL_MAXPROC][3] ;
  int group_o[2][3][RSL_MAXPROC][3] ;
  MPI_Request reqlist[RSL_MAXPROC] ;
  MPI_Comm rsl_mpi_communicator=MPI_COMM_WORLD ;
  char * rcvbuf, *sndbuf ;
  int outstanding = 0 ; 
  int msglen ;
  int P, good, ps0[RSL_MAXPROC], ps1[RSL_MAXPROC] ;
  int          pe0[RSL_MAXPROC], pe1[RSL_MAXPROC] ;
  int          ds0, ds1 ;
  int          de0, de1 ;
  int i,j,k,l,curs,dex,w, outer ;
  int ndim, type, mtag ;
  MPI_Status Stat ;

  ndim = *ndim_p ;
  type = *type_p ;

  MPI_Allgather( is_dimp, 3, MPI_INT, group_i[STRT][PCH], 3, MPI_INT, rsl_mpi_communicator )  ;
  MPI_Allgather( is_dimm, 3, MPI_INT, group_i[STRT][MEM], 3, MPI_INT, rsl_mpi_communicator )  ;
  MPI_Allgather( is_dimd, 3, MPI_INT, group_i[STRT][DOM], 3, MPI_INT, rsl_mpi_communicator )  ;
  MPI_Allgather( ie_dimp, 3, MPI_INT, group_i[ENDD][PCH], 3, MPI_INT, rsl_mpi_communicator )  ;
  MPI_Allgather( ie_dimm, 3, MPI_INT, group_i[ENDD][MEM], 3, MPI_INT, rsl_mpi_communicator )  ;
  MPI_Allgather( ie_dimd, 3, MPI_INT, group_i[ENDD][DOM], 3, MPI_INT, rsl_mpi_communicator )  ;

  MPI_Allgather( os_dimp, 3, MPI_INT, group_o[STRT][PCH], 3, MPI_INT, rsl_mpi_communicator )  ;
  MPI_Allgather( os_dimm, 3, MPI_INT, group_o[STRT][MEM], 3, MPI_INT, rsl_mpi_communicator )  ;
  MPI_Allgather( oe_dimp, 3, MPI_INT, group_o[ENDD][PCH], 3, MPI_INT, rsl_mpi_communicator )  ;
  MPI_Allgather( oe_dimm, 3, MPI_INT, group_o[ENDD][MEM], 3, MPI_INT, rsl_mpi_communicator )  ;

/* now everybody knows about everybody, figure out what I need from where */
/* assume ijk for now */

  /* loop over possible senders so we can post receives */
  /* is any point I need on P? */
  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    good = 0 ;
    /* cases where there definitely are not... */
    if ( os_dimp[0] > group_i[ENDD][PCH][P][0] || oe_dimp[0] < group_i[STRT][PCH][P][0] )
    { }
    else
    {
      ps0[outstanding] = ( os_dimp[0] < group_i[STRT][PCH][P][0] )? group_i[STRT][PCH][P][0] : os_dimp[0] ;
      pe0[outstanding] = ( oe_dimp[0] > group_i[ENDD][PCH][P][0] )? group_i[ENDD][PCH][P][0] : oe_dimp[0] ;
      if ( os_dimp[1] > group_i[ENDD][PCH][P][1] || oe_dimp[1] < group_i[STRT][PCH][P][1] )
      { }
      else
      {
        ps1[outstanding] = ( os_dimp[1] < group_i[STRT][PCH][P][1] )? group_i[STRT][PCH][P][1] : os_dimp[1] ;
        pe1[outstanding] = ( oe_dimp[1] > group_i[ENDD][PCH][P][1] )? group_i[ENDD][PCH][P][1] : oe_dimp[1] ;
        good = 1 ;
      }
    }
    if ( good )
    {
      msglen = (oe_dimp[2]-os_dimp[2]+1)*(pe1[outstanding]-ps1[outstanding]+1)*(pe0[outstanding]-ps0[outstanding]+1)*elemsize(type) ;
      rcvbuf = (char *) buffer_for_proc(P, msglen, RSL_RECVBUF) ;
      mtag = MTYPE_FROMTO( MSG_READ_RESPONSE, P, rsl_myproc ) ;

      MPI_Irecv( rcvbuf , msglen, MPI_CHAR, P, 
                 mtag , rsl_mpi_communicator, &reqlist[outstanding] ) ;  /* posted receive */
      outstanding++ ;
    }
  }
  /* loop over possible receivers so we can pack and send stuff */
  /* are points I have needed by P */
  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    good = 0 ;
    /* cases where they definitely are not */
    if ( is_dimp[0] > group_o[ENDD][PCH][P][0] || ie_dimp[0] < group_o[STRT][PCH][P][0] )
    { }
    else
    {
      ds0 = ( is_dimp[0] < group_o[STRT][PCH][P][0] )? group_o[STRT][PCH][P][0] : is_dimp[0] ;
      de0 = ( ie_dimp[0] > group_o[ENDD][PCH][P][0] )? group_o[ENDD][PCH][P][0] : ie_dimp[0] ;
      if ( is_dimp[1] > group_o[ENDD][PCH][P][1] || ie_dimp[1] < group_o[STRT][PCH][P][1] )
      { }
      else
      {
        ds1 = ( is_dimp[1] < group_o[STRT][PCH][P][1] )? group_o[STRT][PCH][P][1] : is_dimp[1] ;
        de1 = ( ie_dimp[1] > group_o[ENDD][PCH][P][1] )? group_o[ENDD][PCH][P][1] : ie_dimp[1] ;
        good = 1 ;
      }
    }
    if ( good )
    {
      /* pack and send */
      msglen = (oe_dimp[2]-os_dimp[2]+1)*(de1-ds1+1)*(de0-ds0+1)*elemsize(type) ;
      sndbuf = (char *) buffer_for_proc(P, msglen, RSL_SENDBUF) ;
      curs = 0 ;
      for ( k = is_dimm[2] ; k <= ie_dimm[2] ; k++ )
        for ( j = ds1 ; j <= de1 ; j++ )
          for ( i = ds0 ; i <= de0 ; i++ )
            for ( l = 0 ; l < elemsize(type) ; l++ )
            {
              dex = elemsize(type)*
                    ((i-is_dimm[0])
                    +(j-is_dimm[1])*(ie_dimm[0]-is_dimm[0]+1)
                    +(k-is_dimm[2])*(ie_dimm[0]-is_dimm[0]+1)*(ie_dimm[1]-is_dimm[1]+1))  ;
              sndbuf[curs++] = inbuf[l+dex] ;
            }
       mtag = MTYPE_FROMTO( MSG_READ_RESPONSE, rsl_myproc, P ) ;
#if 0
fprintf(stderr,"MPI_Send to %d %d\n",P,msglen) ;
#endif
       MPI_Send( sndbuf, msglen, MPI_CHAR, P, mtag, rsl_mpi_communicator ) ;
    }
  }

  for ( outer = 0 ; outer < outstanding ; outer++ )
  {
    MPI_Waitany ( outstanding, reqlist, &w, &Stat ) ;
    P = Stat.MPI_SOURCE ;
    rcvbuf = (char *) buffer_for_proc(P, 0, RSL_RECVBUF) ;
    curs = 0 ;
    for ( k = os_dimm[2] ; k <= oe_dimm[2] ; k++ )
      for ( j = ps1[w] ; j <= pe1[w] ; j++ )
        for ( i = ps0[w] ; i <= pe0[w] ; i++ )
          for ( l = 0 ; l < elemsize(type) ; l++ )
	  {
            dex = elemsize(type)*
                  ((i-os_dimm[0])
                  +(j-os_dimm[1])*(oe_dimm[0]-os_dimm[0]+1)
                  +(k-os_dimm[2])*(oe_dimm[0]-os_dimm[0]+1)*(oe_dimm[1]-os_dimm[1]+1))  ;
            outbuf[l+dex] = rcvbuf[curs++] ;
	  }
  }
#else
  RSL_TEST_ERR (1,"RSL_REMAP_ARRAY STUBBED") ; 
#endif
}

