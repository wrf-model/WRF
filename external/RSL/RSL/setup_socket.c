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

#if 0
From olson@mcs.anl.gov Fri Sep 30 16:02:19 1994
Received: from mcs.anl.gov (dalek.mcs.anl.gov [140.221.1.2]) by antares.mcs.anl.gov (8.6.4/8.6.4) with ESMTP id QAA01471 for <michalak>; Fri, 30 Sep 1994 16:02:18 -0500
Message-Id: <199409302102.QAA01471@antares.mcs.anl.gov>
To: michalak
Subject: socket setup stuff
Date: Fri, 30 Sep 1994 16:02:17 -0500
From: Bob Olson <olson@mcs.anl.gov>
Status: RO
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <netinet/tcp.h>

static void setup_socket(s)
  int s ;
{
  int on = 1;

#ifdef TCP_RFC1323
  if (setsockopt(s,IPPROTO_TCP,TCP_RFC1323,&on,sizeof(on)) < 0)
    perror("setsockopt RFC1323");
#endif
  if (setsockopt(s,IPPROTO_TCP,TCP_NODELAY,&on,sizeof(on)) < 0)
    perror("setsockopt NODELAY");
  if (setsockopt(s,SOL_SOCKET,SO_REUSEADDR,&on,sizeof(on)) < 0)
    perror("setsockopt REUSEADDR");
}

