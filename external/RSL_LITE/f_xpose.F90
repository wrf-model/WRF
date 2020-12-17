      subroutine trans_z2x ( np, comm, dir, r_wordsize, i_wordsize, memorder, &
                               a, &
                               sd1, ed1, sd2, ed2, sd3, ed3, & 
                               sp1, ep1, sp2, ep2, sp3, ep3, & 
                               sm1, em1, sm2, em2, sm3, em3, & 
                               ax, &
                               sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, & 
                               sm1x, em1x, sm2x, em2x, sm3x, em3x )
         USE duplicate_of_driver_constants
         implicit none
         integer, intent(in) :: sd1, ed1, sd2, ed2, sd3, ed3, & 
                                sp1, ep1, sp2, ep2, sp3, ep3, & 
                                sm1, em1, sm2, em2, sm3, em3, & 
                                sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, & 
                                sm1x, em1x, sm2x, em2x, sm3x, em3x
         integer, intent(in) :: np, comm, r_wordsize, i_wordsize
         integer, intent(in) :: dir ! 1 is a->ax, otherwise ax->a
         integer, intent(in) :: memorder
         integer, dimension((ep1-sp1+1)*(ep2-sp2+1)*(ep3-sp3+1)*max(1,(r_wordsize/i_wordsize)))         :: a
         integer, dimension((ep1x-sp1x+1)*(ep2x-ep2x+1)*(ep3x-sp3x+1)*max(1,(r_wordsize/i_wordsize)))   :: ax
#ifndef STUBMPI
         include 'mpif.h'

!local
         integer   ::         ids, ide, jds, jde, kds, kde, & 
                              ips, ipe, jps, jpe, kps, kpe, & 
                              ims, ime, jms, jme, kms, kme, & 
                              ipsx, ipex, jpsx, jpex, kpsx, kpex, & 
                              imsx, imex, jmsx, jmex, kmsx, kmex

         integer, dimension(0:(ep1-sp1+1)*(ep2-sp2+1)*(ep3-sp3+1)*max(1,(r_wordsize/i_wordsize)))       :: zbuf
         integer, dimension(0:(ep1x-sp1x+1)*(ep2x-sp2x+1)*(ep3x-sp3x+1)*max(1,(r_wordsize/i_wordsize))) :: xbuf

         integer pencil(4), allpencils(4,np)
         integer sendcnts(np), sdispls(np), recvcnts(np), rdispls(np)
         integer allsendcnts(np+2,np), is(np), ie(np), ks(np),ke(np)
         integer sendcurs(np), recvcurs(np)
         integer i,j,k,p,sc,sp,rp,yp,zp,curs,zbufsz,cells,nkcells,ivectype,ierr

         SELECT CASE ( memorder )
          CASE ( DATA_ORDER_XYZ )
            ids  = sd1  ; ide  = ed1  ; jds  = sd2  ; jde  = ed2  ; kds  = sd3  ; kde  = ed3
            ips  = sp1  ; ipe  = ep1  ; jps  = sp2  ; jpe  = ep2  ; kps  = sp3  ; kpe  = ep3
            ims  = sm1  ; ime  = em1  ; jms  = sm2  ; jme  = em2  ; kms  = sm3  ; kme  = em3
            ipsx = sp1x ; ipex = ep1x ; jpsx = sp2x ; jpex = ep2x ; kpsx = sp3x ; kpex = ep3x
            imsx = sm1x ; imex = em1x ; jmsx = sm2x ; jmex = em2x ; kmsx = sm3x ; kmex = em3x
          CASE ( DATA_ORDER_YXZ )
            ids  = sd2  ; ide  = ed2  ; jds  = sd1  ; jde  = ed1  ; kds  = sd3  ; kde  = ed3
            ips  = sp2  ; ipe  = ep2  ; jps  = sp1  ; jpe  = ep1  ; kps  = sp3  ; kpe  = ep3
            ims  = sm2  ; ime  = em2  ; jms  = sm1  ; jme  = em1  ; kms  = sm3  ; kme  = em3
            ipsx = sp2x ; ipex = ep2x ; jpsx = sp1x ; jpex = ep1x ; kpsx = sp3x ; kpex = ep3x
            imsx = sm2x ; imex = em2x ; jmsx = sm1x ; jmex = em1x ; kmsx = sm3x ; kmex = em3x
          CASE ( DATA_ORDER_XZY )
            ids  = sd1  ; ide  = ed1  ; jds  = sd3  ; jde  = ed3  ; kds  = sd2  ; kde  = ed2
            ips  = sp1  ; ipe  = ep1  ; jps  = sp3  ; jpe  = ep3  ; kps  = sp2  ; kpe  = ep2
            ims  = sm1  ; ime  = em1  ; jms  = sm3  ; jme  = em3  ; kms  = sm2  ; kme  = em2
            ipsx = sp1x ; ipex = ep1x ; jpsx = sp3x ; jpex = ep3x ; kpsx = sp2x ; kpex = ep2x
            imsx = sm1x ; imex = em1x ; jmsx = sm3x ; jmex = em3x ; kmsx = sm2x ; kmex = em2x
          CASE ( DATA_ORDER_YZX )
            ids  = sd3  ; ide  = ed3  ; jds  = sd1  ; jde  = ed1  ; kds  = sd2  ; kde  = ed2
            ips  = sp3  ; ipe  = ep3  ; jps  = sp1  ; jpe  = ep1  ; kps  = sp2  ; kpe  = ep2
            ims  = sm3  ; ime  = em3  ; jms  = sm1  ; jme  = em1  ; kms  = sm2  ; kme  = em2
            ipsx = sp3x ; ipex = ep3x ; jpsx = sp1x ; jpex = ep1x ; kpsx = sp2x ; kpex = ep2x
            imsx = sm3x ; imex = em3x ; jmsx = sm1x ; jmex = em1x ; kmsx = sm2x ; kmex = em2x
          CASE ( DATA_ORDER_ZXY )
            ids  = sd2  ; ide  = ed2  ; jds  = sd3  ; jde  = ed3  ; kds  = sd1  ; kde  = ed1
            ips  = sp2  ; ipe  = ep2  ; jps  = sp3  ; jpe  = ep3  ; kps  = sp1  ; kpe  = ep1
            ims  = sm2  ; ime  = em2  ; jms  = sm3  ; jme  = em3  ; kms  = sm1  ; kme  = em1
            ipsx = sp2x ; ipex = ep2x ; jpsx = sp3x ; jpex = ep3x ; kpsx = sp1x ; kpex = ep1x
            imsx = sm2x ; imex = em2x ; jmsx = sm3x ; jmex = em3x ; kmsx = sm1x ; kmex = em1x
          CASE ( DATA_ORDER_ZYX )
            ids  = sd3  ; ide  = ed3  ; jds  = sd2  ; jde  = ed2  ; kds  = sd1  ; kde  = ed1
            ips  = sp3  ; ipe  = ep3  ; jps  = sp2  ; jpe  = ep2  ; kps  = sp1  ; kpe  = ep1
            ims  = sm3  ; ime  = em3  ; jms  = sm2  ; jme  = em2  ; kms  = sm1  ; kme  = em1
            ipsx = sp3x ; ipex = ep3x ; jpsx = sp2x ; jpex = ep2x ; kpsx = sp1x ; kpex = ep1x
            imsx = sm3x ; imex = em3x ; jmsx = sm2x ; jmex = em2x ; kmsx = sm1x ; kmex = em1x
         END SELECT

         sendcnts = 0 ; recvcnts = 0

         xbuf = 0 
         zbuf = 0 

! work out send/recv sizes to each processor in X dimension
         pencil(1) = ips 
         pencil(2) = ipe 
         pencil(3) = kpsx
         pencil(4) = kpex
         call mpi_allgather( pencil, 4, MPI_INTEGER, allpencils, 4, MPI_INTEGER, comm, ierr )
         do p = 1, np
           is(p) = allpencils(1,p)
           ie(p) = allpencils(2,p)
           ks(p) = allpencils(3,p)
           ke(p) = allpencils(4,p)
         enddo
! pack send buffer
         sendcurs = 0 
         sdispls = 0
         sc = 0
         do p = 1, np
           if ( r_wordsize .eq. i_wordsize ) then
             if ( dir .eq. 1 ) then
               call f_pack_int ( a, zbuf(sc), memorder,                                     &
     &                                        jps, jpe, ks(p), ke(p), ips, ipe,             &
     &                                        jms, jme, kms, kme, ims, ime, sendcurs(p) )
             else
               call f_pack_int ( ax, xbuf(sc), memorder,                                    &
     &                                        jpsx, jpex, kpsx, kpex, is(p), ie(p),         &
     &                                        jmsx, jmex, kmsx, kmex, imsx, imex, sendcurs(p) )
             endif
           else if ( r_wordsize .eq. 8 ) THEN
             if ( dir .eq. 1 ) then
               call f_pack_lint ( a, zbuf(sc), memorder,                                    &
     &                                        jps, jpe, ks(p), ke(p), ips, ipe,             &
     &                                        jms, jme, kms, kme, ims, ime, sendcurs(p) )
             else
               call f_pack_lint ( ax, xbuf(sc), memorder,                                   &
     &                                        jpsx, jpex, kpsx, kpex, is(p), ie(p),         &
     &                                        jmsx, jmex, kmsx, kmex, imsx, imex, sendcurs(p) )
             endif
             sendcurs(p) = sendcurs(p) * max(1,(r_wordsize/i_wordsize)) 
           else
             write(0,*)'RSL_LITE internal error: type size mismatch ',__FILE__,__LINE__
             call mpi_abort(ierr)
           endif
           sc = sc + sendcurs(p)
           sendcnts(p) = sendcurs(p)
           if ( p .GT. 1 ) sdispls(p) = sdispls(p-1) + sendcnts(p-1)
         enddo
! work out receive counts and displs
         rdispls = 0
         recvcnts = 0
         do p = 1, np
           if ( dir .eq. 1 ) then
             recvcnts(p) = (ie(p)-is(p)+1)*(kpex-kpsx+1)*(jpex-jpsx+1) * max(1,(r_wordsize/i_wordsize))
           else
             recvcnts(p) = (ke(p)-ks(p)+1)*(ipe-ips+1)*(jpe-jps+1) * max(1,(r_wordsize/i_wordsize))
           endif
           if ( p .GT. 1 ) rdispls(p) = rdispls(p-1) + recvcnts(p-1)
         enddo
! do the transpose
         if ( dir .eq. 1 ) then
           call mpi_alltoallv(zbuf, sendcnts, sdispls, MPI_INTEGER,              &
                              xbuf, recvcnts, rdispls, MPI_INTEGER, comm, ierr )
         else
           call mpi_alltoallv(xbuf, sendcnts, sdispls, MPI_INTEGER,              &
                              zbuf, recvcnts, rdispls, MPI_INTEGER, comm, ierr )
         endif
! unpack
         do p = 1, np
           if ( r_wordsize .eq. i_wordsize ) then
             if ( dir .eq. 1 ) then
               call f_unpack_int ( xbuf(rdispls(p)), ax, memorder,                        &
     &                                        jpsx, jpex, kpsx, kpex, is(p), ie(p),       &
     &                                        jmsx, jmex, kmsx, kmex, imsx, imex, curs )
             else
               call f_unpack_int ( zbuf(rdispls(p)), a, memorder,                         &
     &                                        jps, jpe, ks(p), ke(p), ips, ipe,           &
     &                                        jms, jme, kms, kme, ims, ime, curs )
             endif
           else if ( r_wordsize .eq. 8 ) THEN
             if ( dir .eq. 1 ) then
               call f_unpack_lint ( xbuf(rdispls(p)), ax, memorder,                       &
     &                                        jpsx, jpex, kpsx, kpex, is(p), ie(p),       &
     &                                        jmsx, jmex, kmsx, kmex, imsx, imex, curs )
             else
               call f_unpack_lint ( zbuf(rdispls(p)), a, memorder,                        &
     &                                        jps, jpe, ks(p), ke(p), ips, ipe,           &
     &                                        jms, jme, kms, kme, ims, ime, curs )
             endif
           else
             write(0,*)'RSL_LITE internal error: type size mismatch ',__FILE__,__LINE__
             call mpi_abort(ierr)
           endif
         enddo
#endif
         return
      end subroutine trans_z2x

      subroutine trans_x2y ( np, comm, dir, r_wordsize, i_wordsize, memorder, &
                               ax, &
                               sd1, ed1, sd2, ed2, sd3, ed3, &
                               sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, &
                               sm1x, em1x, sm2x, em2x, sm3x, em3x, &
                               ay, &
                               sp1y, ep1y, sp2y, ep2y, sp3y, ep3y, &
                               sm1y, em1y, sm2y, em2y, sm3y, em3y )
         USE duplicate_of_driver_constants
         implicit none
         integer, intent(in) :: memorder
         integer, intent(in) ::  sd1, ed1, sd2, ed2, sd3, ed3, &
                                 sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, &
                                 sm1x, em1x, sm2x, em2x, sm3x, em3x, &
                                 sp1y, ep1y, sp2y, ep2y, sp3y, ep3y, &
                                 sm1y, em1y, sm2y, em2y, sm3y, em3y

         integer, intent(in) :: np, comm, r_wordsize, i_wordsize
         integer, intent(in) :: dir ! 1 is a->ax, otherwise ax->a
         integer, dimension((ep1x-sp1x+1)*(ep2x-ep2x+1)*(ep3x-sp3x+1)*max(1,(r_wordsize/i_wordsize)))   :: ax
         integer, dimension((ep1y-sp1y+1)*(ep2y-sp2y+1)*(ep3y-sp3y+1)*max(1,(r_wordsize/i_wordsize)))   :: ay
#ifndef STUBMPI
         include 'mpif.h'

         integer, dimension(0:(ep1x-sp1x+1)*(ep2x-sp2x+1)*(ep3x-sp3x+1)*max(1,(r_wordsize/i_wordsize))) :: xbuf
         integer, dimension(0:(ep1y-sp1y+1)*(ep2y-sp2y+1)*(ep3y-sp3y+1)*max(1,(r_wordsize/i_wordsize))) :: ybuf

!local
         integer              ids, ide, jds, jde, kds, kde, & 
                              ipsx, ipex, jpsx, jpex, kpsx, kpex, & 
                              imsx, imex, jmsx, jmex, kmsx, kmex, &
                              ipsy, ipey, jpsy, jpey, kpsy, kpey, & 
                              imsy, imey, jmsy, jmey, kmsy, kmey
         integer pencil(4), allpencils(4,np)
         integer sendcnts(np), sdispls(np), recvcnts(np), rdispls(np)
         integer allsendcnts(np+2,np), is(np), ie(np), js(np), je(np)
         integer sendcurs(np), recvcurs(np)
         integer i,j,k,p,sc,sp,rp,yp,zp,curs,xbufsz,cells,nkcells,ivectype,ierr

         SELECT CASE ( memorder )
          CASE ( DATA_ORDER_XYZ )
            ids  = sd1  ; ide  = ed1  ; jds  = sd2  ; jde  = ed2  ; kds  = sd3  ; kde  = ed3
            ipsx = sp1x ; ipex = ep1x ; jpsx = sp2x ; jpex = ep2x ; kpsx = sp3x ; kpex = ep3x
            imsx = sm1x ; imex = em1x ; jmsx = sm2x ; jmex = em2x ; kmsx = sm3x ; kmex = em3x
            ipsy = sp1y ; ipey = ep1y ; jpsy = sp2y ; jpey = ep2y ; kpsy = sp3y ; kpey = ep3y
            imsy = sm1y ; imey = em1y ; jmsy = sm2y ; jmey = em2y ; kmsy = sm3y ; kmey = em3y
          CASE ( DATA_ORDER_YXZ )
            ids  = sd2  ; ide  = ed2  ; jds  = sd1  ; jde  = ed1  ; kds  = sd3  ; kde  = ed3
            ipsx = sp2x ; ipex = ep2x ; jpsx = sp1x ; jpex = ep1x ; kpsx = sp3x ; kpex = ep3x
            imsx = sm2x ; imex = em2x ; jmsx = sm1x ; jmex = em1x ; kmsx = sm3x ; kmex = em3x
            ipsy = sp2y ; ipey = ep2y ; jpsy = sp1y ; jpey = ep1y ; kpsy = sp3y ; kpey = ep3y
            imsy = sm2y ; imey = em2y ; jmsy = sm1y ; jmey = em1y ; kmsy = sm3y ; kmey = em3y
          CASE ( DATA_ORDER_XZY )
            ids  = sd1  ; ide  = ed1  ; jds  = sd3  ; jde  = ed3  ; kds  = sd2  ; kde  = ed2
            ipsx = sp1x ; ipex = ep1x ; jpsx = sp3x ; jpex = ep3x ; kpsx = sp2x ; kpex = ep2x
            imsx = sm1x ; imex = em1x ; jmsx = sm3x ; jmex = em3x ; kmsx = sm2x ; kmex = em2x
            ipsy = sp1y ; ipey = ep1y ; jpsy = sp3y ; jpey = ep3y ; kpsy = sp2y ; kpey = ep2y
            imsy = sm1y ; imey = em1y ; jmsy = sm3y ; jmey = em3y ; kmsy = sm2y ; kmey = em2y
          CASE ( DATA_ORDER_YZX )
            ids  = sd3  ; ide  = ed3  ; jds  = sd1  ; jde  = ed1  ; kds  = sd2  ; kde  = ed2
            ipsx = sp3x ; ipex = ep3x ; jpsx = sp1x ; jpex = ep1x ; kpsx = sp2x ; kpex = ep2x
            imsx = sm3x ; imex = em3x ; jmsx = sm1x ; jmex = em1x ; kmsx = sm2x ; kmex = em2x
            ipsy = sp3y ; ipey = ep3y ; jpsy = sp1y ; jpey = ep1y ; kpsy = sp2y ; kpey = ep2y
            imsy = sm3y ; imey = em3y ; jmsy = sm1y ; jmey = em1y ; kmsy = sm2y ; kmey = em2y
          CASE ( DATA_ORDER_ZXY )
            ids  = sd2  ; ide  = ed2  ; jds  = sd3  ; jde  = ed3  ; kds  = sd1  ; kde  = ed1
            ipsx = sp2x ; ipex = ep2x ; jpsx = sp3x ; jpex = ep3x ; kpsx = sp1x ; kpex = ep1x
            imsx = sm2x ; imex = em2x ; jmsx = sm3x ; jmex = em3x ; kmsx = sm1x ; kmex = em1x
            ipsy = sp2y ; ipey = ep2y ; jpsy = sp3y ; jpey = ep3y ; kpsy = sp1y ; kpey = ep1y
            imsy = sm2y ; imey = em2y ; jmsy = sm3y ; jmey = em3y ; kmsy = sm1y ; kmey = em1y
          CASE ( DATA_ORDER_ZYX )
            ids  = sd3  ; ide  = ed3  ; jds  = sd2  ; jde  = ed2  ; kds  = sd1  ; kde  = ed1
            ipsx = sp3x ; ipex = ep3x ; jpsx = sp2x ; jpex = ep2x ; kpsx = sp1x ; kpex = ep1x
            imsx = sm3x ; imex = em3x ; jmsx = sm2x ; jmex = em2x ; kmsx = sm1x ; kmex = em1x
            ipsy = sp3y ; ipey = ep3y ; jpsy = sp2y ; jpey = ep2y ; kpsy = sp1y ; kpey = ep1y
            imsy = sm3y ; imey = em3y ; jmsy = sm2y ; jmey = em2y ; kmsy = sm1y ; kmey = em1y
         END SELECT

         sendcnts = 0 ; recvcnts = 0

         xbuf = 0 
         ybuf = 0 

! work out send/recv sizes to each processor in X dimension
         pencil(1) = jpsx
         pencil(2) = jpex
         pencil(3) = ipsy
         pencil(4) = ipey

         call mpi_allgather( pencil, 4, MPI_INTEGER, allpencils, 4, MPI_INTEGER, comm, ierr )
         do p = 1, np
           js(p) = allpencils(1,p)
           je(p) = allpencils(2,p)
           is(p) = allpencils(3,p)
           ie(p) = allpencils(4,p)
         enddo


! pack send buffer
         sendcurs = 0 
         sdispls = 0
         sc = 0
         do p = 1, np
           if ( r_wordsize .eq. i_wordsize ) then
             if ( dir .eq. 1 ) then
               call f_pack_int ( ax, xbuf(sc), memorder,                                    &
     &                                         jpsx, jpex, kpsx, kpex, is(p), ie(p),        &
     &                                         jmsx, jmex, kmsx, kmex, imsx, imex, sendcurs(p) )
             else
               call f_pack_int ( ay, ybuf(sc), memorder,                                    &
     &                                         js(p), je(p), kpsy, kpey, ipsy, ipey,        &
     &                                         jmsy, jmey, kmsy, kmey, imsy, imey, sendcurs(p) )
             endif
           else if ( r_wordsize .eq. 8 ) THEN
             if ( dir .eq. 1 ) then
               call f_pack_lint ( ax, xbuf(sc), memorder,                                    &
     &                                          jpsx, jpex, kpsx, kpex, is(p), ie(p),        &
     &                                          jmsx, jmex, kmsx, kmex, imsx, imex, sendcurs(p) )
             else
               call f_pack_lint ( ay, ybuf(sc), memorder,                                    &
     &                                          js(p), je(p), kpsy, kpey, ipsy, ipey,        &
     &                                          jmsy, jmey, kmsy, kmey, imsy, imey, sendcurs(p) )
             endif
             sendcurs(p) = sendcurs(p) * max(1,(r_wordsize/i_wordsize)) 
           else
             write(0,*)'RSL_LITE internal error: type size mismatch ',__FILE__,__LINE__
             call mpi_abort(ierr)
           endif
           sc = sc + sendcurs(p)
           sendcnts(p) = sendcurs(p)
           if ( p .GT. 1 ) sdispls(p) = sdispls(p-1) + sendcnts(p-1)
         enddo

! work out receive counts and displs
         rdispls = 0
         recvcnts = 0
         do p = 1, np
           if ( dir .eq. 1 ) then
             recvcnts(p) = (je(p)-js(p)+1)*(kpey-kpsy+1)*(ipey-ipsy+1) * max(1,(r_wordsize/i_wordsize))
           else
             recvcnts(p) = (ie(p)-is(p)+1)*(kpex-kpsx+1)*(jpex-jpsx+1) * max(1,(r_wordsize/i_wordsize))
           endif
           if ( p .GT. 1 ) rdispls(p) = rdispls(p-1) + recvcnts(p-1)
         enddo

! do the transpose
         if ( dir .eq. 1 ) then
           call mpi_alltoallv(xbuf, sendcnts, sdispls, MPI_INTEGER,              &
                              ybuf, recvcnts, rdispls, MPI_INTEGER, comm, ierr )
         else
           call mpi_alltoallv(ybuf, sendcnts, sdispls, MPI_INTEGER,              &
                              xbuf, recvcnts, rdispls, MPI_INTEGER, comm, ierr )
         endif
! unpack
         do p = 1, np
           if ( r_wordsize .eq. i_wordsize ) then
             if ( dir .eq. 1 ) then
               call f_unpack_int ( ybuf(rdispls(p)), ay, memorder,                                  &
     &                                                   js(p), je(p), kpsy, kpey, ipsy, ipey,      &
     &                                                   jmsy, jmey, kmsy, kmey, imsy, imey, curs )
             else
               call f_unpack_int ( xbuf(rdispls(p)), ax, memorder,                                  &
     &                                                   jpsx, jpex, kpsx, kpex, is(p), ie(p),      &
     &                                                   jmsx, jmex, kmsx, kmex, imsx, imex, curs )
             endif
           else if ( r_wordsize .eq. 8 ) THEN
             if ( dir .eq. 1 ) then
               call f_unpack_lint ( ybuf(rdispls(p)), ay, memorder,                                 &
     &                                                    js(p), je(p), kpsy, kpey, ipsy, ipey,     &
     &                                                    jmsy, jmey, kmsy, kmey, imsy, imey, curs )
             else
               call f_unpack_lint ( xbuf(rdispls(p)), ax, memorder,                                 &
     &                                                    jpsx, jpex, kpsx, kpex, is(p), ie(p),     &
     &                                                    jmsx, jmex, kmsx, kmex, imsx, imex, curs )
             endif
           else
             write(0,*)'RSL_LITE internal error: type size mismatch ',__FILE__,__LINE__
             call mpi_abort(ierr)
           endif
         enddo
#endif
         return
      end subroutine trans_x2y

