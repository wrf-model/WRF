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

/*
 *  rsl_socket.c  -- added 9/30/94
 *
 *               RSL_SOCKOPEN
 *               RSL_SOCKREAD   <-- not yet
 *               RSL_SOCKWRITE
 *               RSL_SOCKCLOSE
 */

#include "stdio.h"
#include "stdlib.h"
#include "rsl.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/uio.h>


/*@
  RSL_SOCKCLOSE - close a socket

  Synopsis:
  subroutine RSL_SOCKCLOSE ( sid )

  Input parameters:
. sid  close a socket opened by RSL_SOCKOPEN

  See also:
  RSL_SOCKOPEN

@*/
RSL_SOCKCLOSE ( sid0 )
  int_p sid0 ;          /* socket id -- set by this routine */
{
  int retval ;

  RSL_C_IAMMONITOR ( &retval ) ;
  if ( retval == 1 ) 
  {
    close(*sid0) ;
  }
}

static int first = 1 ;


/*@
  RSL_SOCKOPEN - open a TCP/IP socket connection for reading or writing

  Synopsis:
  subroutine RSL_SOCKOPEN ( sid, portnum, hostname, namelen )
  integer sid
  integer portnum
  character*(*) hostname
  integer namelen

  Input parameters:
. portnum  port to connect to on hostname
. hostname string containing the name of the remote host
. namelen  length in characters of hostname

  Output parameters:
. sid	socket descriptor for use in subsequent operations on socket

  Notes:
  Open a TCP/IP stream socket to a host for later use by RSL_SOCKWRITE and
  RSL_SOCKREAD.  Portnum must be specified and it is the number of the
  port on the remote host to connect to.   The name of the host
  (e.g., xyz.abc.com) is passed to the routine as a string whose length
  is passed as namelen.
  
  On return, sid argument contains the socket descriptor.

  Example:

$ call rsl_sockopen( sid, 5550, 'xyz.abc.com', 11 )

BREAKTHEEXAMPLECODE

  Bugs:

  The routine prints a warning message if it fails to open
  a socket and returns.  It would be better if this returned
  an error code.

  See also:
  socket(2), RSL_SOCKWRITE, RSL_SOCKREAD, RSL_SOCKCLOSE


@*/
RSL_SOCKOPEN ( sid0, portnum0, hstname0, namelen0 )
  int_p sid0 ;		/* socket id -- set by this routine */
  int_p portnum0 ;	/* port number input to this routine */
  char *hstname0 ;	/* name of host */
  int_p namelen0 ;	/* number of characters in hstname0 */
{
  int i, retval  ;
  char * p, * q, c ;
  int portnum ;
  char hstname[128] ;

  struct sockaddr_in name;
  struct hostent *hp, *gethostbyname();



  RSL_C_IAMMONITOR ( &retval ) ;
  if ( retval == 1 )
  {

    /* process input args from fortran */
    portnum = *portnum0 ;
    RSL_TEST_WRN( hstname0 == NULL, "Null hstname argument" ) ;
    if ( *namelen0 < 0 || *namelen0 > 64 )
    {
      sprintf(mess,"Invalid hstname length %d.",*namelen0) ;
      RSL_TEST_WRN(1,mess) ;
    }
    strncpy( hstname, hstname0, *namelen0 ) ;
    hstname[*namelen0] = '\0' ;
/* get rid of any white space */
    {
      char *p, *q;
      for ( p = hstname, q = p ; *p ; p++ )
	if ( *p != ' ' && *p != '\t' && *p != '\n' ) *q++ = *p ; 
      *q = '\0' ;
    }
/* end mod for removing white space */

    /* create socket */
    if ( (*sid0 = socket(AF_INET,SOCK_STREAM,0)) < 0 ) 
    {
      perror("opening socket") ;
      RSL_TEST_WRN(1,"") ;
    }

    /* connect socket */
    name.sin_family = AF_INET;
    if((hp = gethostbyname(hstname)) == NULL )
    {
      sprintf(mess,"%s: unknown host", hstname);
      RSL_TEST_WRN(1,mess) ;
    }

    bcopy((char *)hp -> h_addr, (char *)&name.sin_addr, hp-> h_length);
    name.sin_port = htons(portnum);

    if(connect(*sid0, (struct sockaddr *)&name, sizeof(name)) < 0)
    {
      perror("connecting stream socket");
      RSL_TEST_WRN(1,"") ;
    }
    if ( first == 1 )
    {
      first = 0 ;
      setup_socket(*sid0) ;
    }
  }

fprintf(stderr,"RSL_SOCKOPEN returns *sid0 = %d\n",*sid0) ;
  return ;
}

/************/

/*@
  RSL_SOCKWRITE - write a distributed two- or three-dimensional array to a socket

  Synopsis:
  subroutine RSL_SOCKWRITE ( sid, iotag, var, domain, type, glen, llen )

  Input parameters:
. socket descriptor
. iotag  tag describing array dimensions
. var    distributed array being written
. domain  domain descriptor
. type   type of an array element
. glen   integer array of global (undecomposed) dimensions of array
. llen  integer array of local static dimensions of array

  Notes:
  A distributed two- or three-dimensional array will be written to
  a socket, previously opened with RSL_SOCKOPEN.  Except as noted below,
  the semantics are similar to RSL_READ and RSL_WRITE (which read and
  write Fortran files).  The reader should become familiar with these
  routines first.

  A number of different output options are available, depending on the
  value of iotag.  The tags IO2D_IJ, IO2D_JI, IO3D_IJK, or IO3D_JIK specify
  Fortran-style record blocking (that is, with Fortran record blocking 
  information encoded in the data stream).   The data is written to the
  socket, but a Fortran record-blocking control word is added to the
  beginning and end of each record written to the socket.  (Each 4-byte
  control word is a byte
  count for the record that follows/preceeds it, and for each n-byte
  record written, n+8 bytes will actually be written to the socket).

  The tags IO2D_IJ_RAW, IO2D_JI_RAW, IO3D_IJK_RAW, and IO3D_JIK_RAW specify
  that the data is to be streamed to the socket as-is, with no additional
  record blocking information.

  The tags IO2D_IJ_PORTAL, IO2D_JI_PORTAL, IO3D_IJK_PORTAL, and
  IO3D_JIK_PORTAL specify that special header packets understood by the
  Portal communication library (see reference below) are added to the
  beginning of each record.  This header data describes the dimensionality
  of the data that will follow and the size of the dimensions; Portal
  should be used on the receiving process to properly handle data written
  in this mode.

  Example:

$ glen(1) = il
$ glen(2) = jl
$ glen(3) = mkx
$ llen(1) = mix
$ llen(2) = mjx
$ llen(3) = mkx

$ m2 = IO2D_IJ_RAW
$ m3 = IO3D_IJK_RAW

$ call rsl_sockwrite(sock,m2,ht,domains(inest),RSL_REAL,glen,llen)
$ call rsl_sockwrite(sock,m3,ta,domains(inest),RSL_REAL,glen,llen)

BREAKTHEEXAMPLECODE

  The 2-dimensional distributed array ht and the three-dimensional
  distributed array ta are written in raw mode to the socket specified
  by sock.  Sock has been opened using RSL_SOCKOPEN.

  See also:
  ``Portal Communication Library for Run-Time Visualization of Distributed,
  Asynchronous Data,'' J.S. Rowlan, B.T. Wightman, Mathematics and Computer
  Science Division, Argonne National Laboratory, 1994.  Preprint MCS-P395-1193.

  RSL_SOCKOPEN, RSL_SOCKCLOSE, RSL_READ, RSL_WRITE


@*/
RSL_SOCKWRITE ( unit_p, iotag_p, base, d_p, type_p, glen, llen  )
  int_p unit_p ;
  int_p iotag_p ;
  int_p type_p ;
  int_p d_p ;
  char * base ;
  int glen[], llen[] ;
{
  rsl_read_req_t request ;
  rsl_read_resp_t resp ;
  rsl_processor_t me ;
  int cursor, mdest, mtag, msglen, dim ;
  int mlen, nlen ;
  int minelems, majelems ;
  unsigned long ig, jg, min, maj, ioffset, joffset, tlen, k ;
  void * dex ;
  char *pbuf ;
  int i_am_monitor ;
  int psize, nelem, typelen, nbytes, columnelems  ;
  rsl_point_t *domain ;

  RSL_TEST_ERR( *d_p < 0 || *d_p >= RSL_MAXDOMAINS,
		"rsl_sockwrite: bad domain descriptor") ;
  RSL_TEST_ERR( domain_info[*d_p].valid != RSL_VALID,
		"rsl_sockwrite: invalid domain descriptor" ) ;

  mlen = domain_info[*d_p].len_m ;
  nlen = domain_info[*d_p].len_n ;
  domain = domain_info[*d_p].domain ;

  RSL_C_IAMMONITOR ( &i_am_monitor ) ;

  me = rsl_c_phys2comp_proc( rsl_myproc ) ;
  ioffset = domain_info[*d_p].ilocaloffset ;
  joffset = domain_info[*d_p].jlocaloffset ;
  tlen = elemsize( *type_p ) ;

  request.request_type = RSL_WRITE_REQUEST ;
  request.request_mode = MSG_IO_SOCKET ;
  request.myproc = rsl_myproc ;
  request.base = base ;
  request.domain = *d_p ;
  request.unit = *unit_p ;
  request.type = *type_p ;
  request.iotag = *iotag_p ;
  request.sequence = io_seq_compute++ ;
  switch( *iotag_p )
  {

  case IO2D_IJ :
    request.ndim = 2 ;
    request.request_mode2 = MSG_MODE2_FORTRAN ;
    break ;
  case IO2D_JI :
    request.ndim = 2 ;
    request.request_mode2 = MSG_MODE2_FORTRAN ;
    break ;
  case IO3D_IJK :
    request.ndim = 3 ;
    request.request_mode2 = MSG_MODE2_FORTRAN ;
    break ;
  case IO3D_JIK :
    request.ndim = 3 ;
    request.request_mode2 = MSG_MODE2_FORTRAN ;
    break ;

  case IO2D_IJ_RAW :
    request.ndim = 2 ;
    request.request_mode2 = MSG_MODE2_RAW ;
    break ;
  case IO2D_JI_RAW :
    request.ndim = 2 ;
    request.request_mode2 = MSG_MODE2_RAW ;
    break ;
  case IO3D_IJK_RAW :
    request.ndim = 3 ;
    request.request_mode2 = MSG_MODE2_RAW ;
    break ;
  case IO3D_JIK_RAW :
    request.ndim = 3 ;
    request.request_mode2 = MSG_MODE2_RAW ;
    break ;

  case IO2D_IJ_PORTAL :
    request.ndim = 2 ;
    request.request_mode2 = MSG_MODE2_PORTAL ;
    break ;
  case IO2D_JI_PORTAL :
    request.ndim = 2 ;
    request.request_mode2 = MSG_MODE2_PORTAL ;
    break ;
  case IO3D_IJK_PORTAL :
    request.ndim = 3 ;
    request.request_mode2 = MSG_MODE2_PORTAL ;
    break ;
  case IO3D_JIK_PORTAL :
    request.ndim = 3 ;
    request.request_mode2 = MSG_MODE2_PORTAL ;
    break ;

  case IO2D_IJ_88 :
    request.ndim = 2 ;
    request.request_mode2 = MSG_MODE2_88 ;
    break ;
  case IO2D_JI_88 :
    request.ndim = 2 ;
    request.request_mode2 = MSG_MODE2_88 ;
    break ;
  case IO3D_IJK_88 :
    request.ndim = 3 ;
    request.request_mode2 = MSG_MODE2_88 ;
    break ;
  case IO3D_JIK_88 :
    request.ndim = 3 ;
    request.request_mode2 = MSG_MODE2_88 ;
    break ;

  default:
    RSL_TEST_ERR(1,"rsl_read: unknown data tag") ;
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
  case IO2D_IJ_RAW :
  case IO2D_IJ_PORTAL :
  case IO2D_IJ_88 :
    columnelems = 1 ;
    minelems = request.glen[0] ;
    majelems = request.glen[1] ;
    break ;
  case IO2D_JI :
  case IO2D_JI_RAW :
  case IO2D_JI_PORTAL :
  case IO2D_JI_88 :
    columnelems = 1 ;
    minelems = request.glen[1] ;
    majelems = request.glen[0] ;
    break ;
  case IO3D_IJK :
  case IO3D_IJK_RAW :
  case IO3D_IJK_PORTAL :
  case IO3D_IJK_88 :
    columnelems = request.llen[2] ;
    minelems = request.glen[0] ;
    majelems = request.glen[1] ;
    break ;
  case IO3D_JIK :
  case IO3D_JIK_RAW :
  case IO3D_JIK_PORTAL :
  case IO3D_JIK_88 :
    columnelems = request.llen[2] ;
    minelems = request.glen[1] ;
    majelems = request.glen[0] ;
    break ;

  default:
    RSL_TEST_ERR(1,"handle_write_request: unknown data tag") ;
  }


  /*  figure out size for this processor */
  pbuf = NULL ;
  psize = 0 ;

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

#if 0
  for ( jg = 0 ; jg < domain_info[*d_p].len_n ; jg++ )
  {
    for ( ig = 0 ; ig < domain_info[*d_p].len_m ; ig++ )
    {
#else
  for ( jg = 0 ; jg < majelems ; jg++ )
  {
    for ( ig = 0 ; ig < minelems ; ig++ )
    {
#endif
      if ( me == domain[INDEX_2(jg,ig,mlen)].P )
      {
        switch( *iotag_p )
        {
        case IO2D_IJ :
        case IO2D_IJ_RAW :
        case IO2D_IJ_PORTAL :
        case IO2D_IJ_88 :
	  min = ig - ioffset ;
	  maj = jg - joffset ;
	  dex = base+tlen*(min+maj*llen[0]) ;
	  bcopy(dex,&(pbuf[cursor]),tlen) ;
	  cursor += tlen ;
          break ;
        case IO2D_JI :
        case IO2D_JI_RAW :
        case IO2D_JI_PORTAL :
        case IO2D_JI_88 :
	  min = jg - joffset ;
	  maj = ig - ioffset ;
	  dex = base+tlen*(min+maj*llen[0]) ;
	  bcopy(dex,&(pbuf[cursor]),tlen) ;
	  cursor += tlen ;
          break ;
        case IO3D_IJK :
        case IO3D_IJK_RAW :
        case IO3D_IJK_PORTAL :
        case IO3D_IJK_88 :
	  min = ig - ioffset ;
	  maj = jg - joffset ;
	  for ( k = 0 ; k < llen[2] ; k++ )
	  {
	    dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
	    bcopy(dex,&(pbuf[cursor]),tlen) ;
	    cursor += tlen ;
	  }
          break ;
        case IO3D_JIK :
        case IO3D_JIK_RAW :
        case IO3D_JIK_PORTAL :
        case IO3D_JIK_88 :
	  min = jg - ioffset ;
	  maj = ig - joffset ;
	  for ( k = 0 ; k < llen[2] ; k++ )
	  {
	    dex = base+tlen*(min+llen[0]*(maj+k*llen[1])) ;
	    bcopy(dex,&(pbuf[cursor]),tlen) ;
	    cursor += tlen ;
	  }
          break ;
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
    mdest = RSL_C_MONITOR_PROC () ;
    msglen = psize ;
    mtag = MTYPE_FROMTO( MSG_WRITE_COMPUTE_RESPONSE, rsl_myproc, mdest ) ;
    RSL_SEND( pbuf, msglen, mtag, mdest ) ;
  }

  RSL_FREE( pbuf ) ;
  }
}

