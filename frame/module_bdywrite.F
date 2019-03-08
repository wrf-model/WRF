module module_bdywrite
  implicit none
#if (NMM_CORE==1 && NMM_NEST==1)
#ifdef EXTRA_HWRF_DEBUG_STUFF
  private
  public :: bdywrite

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  include 'mpif.h'
#else
  integer, parameter :: MPI_UNDEFINED = -1
#endif

  CHACARCTER (LEN=256) , PRIVATE :: a_message
contains
  subroutine bdywrite(grid,filename)
    use module_domain_type, only: domain
    use module_domain, only: get_ijk_from_grid
    use module_state_description, only: &
         p_qv, p_qc, p_qs, p_qi, p_qr, p_qg, &
         f_qv, f_qc, f_qs, f_qi, f_qr, f_qg, &
         p_qnr, p_qni, f_qnr, f_qni,         &
         PARAM_FIRST_SCALAR,num_moist,num_scalar
    implicit none

    type(domain), intent(in) :: grid
    character(*), intent(in) :: filename

    include 'netcdf.inc'

    integer :: fid,vid
    integer :: dimi,dimj,dimk
    integer :: ni,nj,nk
    integer :: jk(2),ik(2)

! Some temporary definitions to reduce code complexity:

#define paste(x,y) x/**/y

#define check(ST) call bdy_nf_assert(ST,filename,__FILE__,__LINE__,.false.)
#define assert(ST) call bdy_nf_assert(ST,filename,__FILE__,__LINE__,.true.)

#define declare_bdy(X) integer :: paste(X,_bxs), paste(X,_bxe), paste(X,_bys), paste(X,_bye)

! NOTE: we should be able to do #X instead of using an additional "s"
! argument in define_vars2d and define_vars1d, but some GNU cpp bug is
! breaking that (and also breaking ##)

#define define_vars2d(X,s) \
    assert(nf_def_var(fid, s // '_bxs',NF_FLOAT,2,jk,paste(X,_bxs))) ; \
    assert(nf_def_var(fid, s // '_bxe',NF_FLOAT,2,jk,paste(X,_bxe))) ; \
    assert(nf_def_var(fid, s // '_bys',NF_FLOAT,2,ik,paste(X,_bys))) ; \
    assert(nf_def_var(fid, s // '_bye',NF_FLOAT,2,ik,paste(X,_bye)))

#define define_vars1d(X,s) \
    assert(nf_def_var(fid, s // '_bxs',NF_FLOAT,1,dimj,paste(X,_bxs))) ; \
    assert(nf_def_var(fid, s // '_bxe',NF_FLOAT,1,dimj,paste(X,_bxe))) ; \
    assert(nf_def_var(fid, s // '_bys',NF_FLOAT,1,dimi,paste(X,_bys))) ; \
    assert(nf_def_var(fid, s // '_bye',NF_FLOAT,1,dimi,paste(X,_bye)))

#define copy_data2d(to,from,d1s,d1e,d2s,d2e)     \
    do i2=d2s,d2e                              ; \
       do i1=d1s,d1e                           ; \
          to(i1+i2*(d1e-d1s+1))=from(i1,i2,1)  ; \
       end do                                  ; \
    end do

#define copy_data1d(to,from,d1s,d1e)     \
       do i1=d1s,d1e                   ; \
          to(i1)=from(i1,1,1)          ; \
       end do

#define write_vars3d_part(L,G,type,I) \
    call paste(bdy_write_,type)(fid,filename,                           \
          paste(L,_bxs),paste(L,_bxe),paste(L,_bys),paste(L,_bye),      \
          ids,ide,jds,jde,kds,kde,                                      \
          ims,ime,jms,jme,kms,kme,                                      \
          ips,ipe,jps,jpe,kps,kpe,                                      \
          grid%paste(G,_bxs)(:,:,1,I), grid%paste(G,_bxe)(:,:,1,I),     \
          grid%paste(G,_bys)(:,:,1,I), grid%paste(G,_bye)(:,:,1,I))

#define write_vars2d(X,type) \
    call paste(bdy_write_,type)(fid,filename,                           \
          paste(X,_bxs),paste(X,_bxe),paste(X,_bys),paste(X,_bye),      \
          ids,ide,jds,jde,kds,kde,                                      \
          ims,ime,jms,jme,kms,kme,                                      \
          ips,ipe,jps,jpe,kps,kpe,                                      \
          grid%paste(X,_bxs), grid%paste(X,_bxe),                       \
          grid%paste(X,_bys), grid%paste(X,_bye))

#define write_vars1d(X,type) \
    call paste(bdy_write_,type)(fid,filename,                           \
          paste(X,_bxs),paste(X,_bxe),paste(X,_bys),paste(X,_bye),      \
          ids,ide,jds,jde, 1 , 1 ,                                      \
          ims,ime,jms,jme, 1 , 1 ,                                      \
          ips,ipe,jps,jpe, 1 , 1 ,                                      \
          grid%paste(X,_bxs), grid%paste(X,_bxe),                       \
          grid%paste(X,_bys), grid%paste(X,_bye))

! End of temporary definitions.

    declare_bdy(u)
    declare_bdy(v)
    declare_bdy(t)
    declare_bdy(q)
    declare_bdy(cwm)
    declare_bdy(pd)
    declare_bdy(winfo)
    declare_bdy(iinfo)
    declare_bdy(qv)
    declare_bdy(qc)
    declare_bdy(qr)
    declare_bdy(qi)
    declare_bdy(qg)
    declare_bdy(qs)
    declare_bdy(qnr)
    declare_bdy(qni)
!    declare_bdy(utemp)
!    declare_bdy(vtemp)
!    declare_bdy(ttemp)
!    declare_bdy(qtemp)
!    declare_bdy(pdtemp)
!    declare_bdy(cwmtemp)

    INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE, &
               IMS,IME,JMS,JME,KMS,KME, &
               IPS,IPE,JPS,JPE,KPS,KPE

    ! ---------- end of definitions ----------

    IDS=-1; IDE=-1; JDS=-1; JDE=-1; KDS=-1; KDE=-1
    IMS=-1; IME=-1; JMS=-1; JME=-1; KMS=-1; KME=-1
    IPS=-1; IPE=-1; JPS=-1; JPE=-1; KPS=-1; KPE=-1
    CALL GET_IJK_FROM_GRID(GRID                                       &
                          ,IDS,IDE,JDS,JDE,KDS,KDE                    &
                          ,IMS,IME,JMS,JME,KMS,KME                    &
                          ,IPS,IPE,JPS,JPE,KPS,KPE )

    write(a_message,*) "Sam's special boundary output (TM) for domain ",grid%id ; CALL wrf_message ( a_message )
    ! call CHECK_MOIST('MOIST',grid%moist,PARAM_FIRST_SCALAR, num_moist, &
    !        grid%moist_bxs,grid%moist_bxe, &
    !        grid%moist_bys,grid%moist_bye, &
    !        ids,ide, jds,jde, kds,kde, &
    !        ims,ime, jms,jme, kms,kme, &
    !        ips,ipe, jps,jpe, kps,kpe)
    ! call CHECK_MOIST('SCALAR',grid%scalar,PARAM_FIRST_SCALAR, num_scalar, &
    !        grid%scalar_bxs,grid%scalar_bxe, &
    !        grid%scalar_bys,grid%scalar_bye, &
    !        ids,ide, jds,jde, kds,kde, &
    !        ims,ime, jms,jme, kms,kme, &
    !        ips,ipe, jps,jpe, kps,kpe)

    assert(nf_create(trim(filename),NF_64BIT_OFFSET,fid))
    assert(nf_def_dim(fid,'i',ide-ids+1,dimi))
    assert(nf_def_dim(fid,'j',jde-jds+1,dimj))
    assert(nf_def_dim(fid,'k',kde-kds+1,dimk))
    
    jk=(/ dimj,dimk /)
    ik=(/ dimi,dimk /)

    define_vars2d(u,'u')
    define_vars2d(v,'v')
    define_vars2d(t,'t')
    define_vars2d(q,'q')
    define_vars2d(cwm,'cwm')
    define_vars1d(pd,'pd')
    define_vars2d(winfo,'winfo')
    define_vars2d(iinfo,'iinfo')
    if(f_qv) define_vars2d(qv,'qv')
    if(f_qc) define_vars2d(qc,'qc')
    if(f_qr) define_vars2d(qr,'qr')
    if(f_qi) define_vars2d(qi,'qi')
    if(f_qg) define_vars2d(qg,'qg')
    if(f_qs) define_vars2d(qs,'qs')
    if(f_qnr) define_vars2d(qnr,'qnr')
    if(f_qni) define_vars2d(qni,'qni')
!    define_vars2d(utemp,'utemp')
!    define_vars2d(vtemp,'vtemp')
!    define_vars2d(ttemp,'ttemp')
!    define_vars2d(qtemp,'qtemp')
!    define_vars1d(pdtemp,'pdtemp')
!    define_vars2d(cwmtemp,'cwmtemp')

    assert(nf_enddef(fid))

    write_vars2d(u,real)
    write_vars2d(v,real)
    write_vars2d(t,real)
    write_vars2d(q,real)
    write_vars2d(cwm,real)
    write_vars1d(pd,real)
    write_vars2d(winfo,real)
    write_vars2d(iinfo,int)
    if(f_qv) write_vars3d_part(qv,moist,real,p_qv)
    if(f_qc) write_vars3d_part(qc,moist,real,p_qc)
    if(f_qr) write_vars3d_part(qr,moist,real,p_qr)
    if(f_qi) write_vars3d_part(qi,moist,real,p_qi)
    if(f_qg) write_vars3d_part(qg,moist,real,p_qg)
    if(f_qs) write_vars3d_part(qs,moist,real,p_qs)
    if(f_qnr) write_vars3d_part(qnr,scalar,real,p_qnr)
    if(f_qni) write_vars3d_part(qni,scalar,real,p_qni)

    if(f_qv) then
       write(a_message,*) f_qv,p_qv ; CALL wrf_message ( a_message )
    endif
    if(f_qc) then
       write(a_message,*) f_qc,p_qc ; CALL wrf_message ( a_message )
    endif
    if(f_qr) then
       write(a_message,*) f_qr,p_qr ; CALL wrf_message ( a_message )
    endif
    if(f_qi) then
       write(a_message,*) f_qi,p_qi ; CALL wrf_message ( a_message )
    endif
    if(f_qg) then
       write(a_message,*) f_qg,p_qg ; CALL wrf_message ( a_message )
    endif
    if(f_qs) then
       write(a_message,*) f_qs,p_qs ; CALL wrf_message ( a_message )
    endif
    if(f_qnr) then
       write(a_message,*) f_qnr,p_qnr ; CALL wrf_message ( a_message )
    endif
    if(f_qni) then
       write(a_message,*) f_qni,p_qni ; CALL wrf_message ( a_message )
    endif
!    write_vars2d(utemp,real)
!    write_vars2d(vtemp,real)
!    write_vars2d(ttemp,real)
!    write_vars2d(qtemp,real)
!    write_vars1d(pdtemp,real)
!    write_vars2d(cwmtemp,real)

    assert(nf_close(fid))
    write(a_message,*) "Finished Sam's special boundary output (TM) for domain ",grid%id ; CALL wrf_message ( a_message )

  end subroutine bdywrite

  subroutine bdy_write_real(fid,filename,            &
                            xsid,xeid,ysid,yeid,     &
                            ids,ide,jds,jde,kds,kde, &
                            ims,ime,jms,jme,kms,kme, &
                            ips,ipe,jps,jpe,kps,kpe, &
                            bxs,bxe,bys,bye)
    use module_dm, only: ntasks, local_communicator
    implicit none
    logical, external :: wrf_dm_on_monitor
    integer, parameter :: bdyw=1
    logical :: monitor
    character(*), intent(in) :: filename
    integer, intent(in) :: fid,xsid,xeid,ysid,yeid
    integer, intent(in) :: ids,ide,jds,jde,kds,kde, &
                           ims,ime,jms,jme,kms,kme, &
                           ips,ipe,jps,jpe,kps,kpe
    real, intent(in) :: bxs(jms:jme,kms:kme,bdyw)
    real, intent(in) :: bxe(jms:jme,kms:kme,bdyw)
    real, intent(in) :: bys(ims:ime,kms:kme,bdyw)
    real, intent(in) :: bye(ims:ime,kms:kme,bdyw)

    integer :: j1,j2,i1,i2,k1,k2, ni,nj,nk

!    write(0,*) 'DUDE, THIS IS LIKE SOOOOOOO REAL'

    monitor=wrf_dm_on_monitor()

    j1=max(jps,jds)
    j2=min(jpe,jde)
    i1=max(ips,ids)
    i2=min(ipe,jde)
30  format('ips=',I0,' ipe=',I0,' jps=',I0,' jpe=',I0,' kps=',I0,' kpe=',I0)
31  format('i1=',I0,' i2=',I0,' j1=',I0,' j2=',I0,' k1=',I0,' k2=',I0)
32  format('ni=',I0,' nj=',I0,' nk=',I0)
    k1=kds
    k2=kde
    ni=ide-ids+1
    nj=jde-jds+1
    nk=kde-kds+1
!    write(0,*) ips,ipe,jps,jpe,kps,kpe
!    write(0,*) i1,i2,j1,j2,k1,k2
!    write(0,*) ni,nj,nk

    if(ni<=0) call wrf_error_fatal('ni<=0')
    if(nj<=0) call wrf_error_fatal('nj<=0')
    if(nk<=0) call wrf_error_fatal('nk<=0')
    
    if(monitor) then
       call bdy_recv()
    else
       call bdy_send()
    endif

!    write(0,*) 'BUT DUDE, NO MORE REAL'

  contains
    subroutine bdy_send
      implicit none

      integer :: idum1(2),idum2(2),idum3(2),idum4(2), nreceive
      real :: rdum(2)

      ! West boundary:
      call bdy_send_one(ips==1,j1,j2,idum1,idum2,idum3,idum4,rdum, &
           bxs, jms,jme,kms,kme, nreceive,2)

      ! East boundary:
      call bdy_send_one(ipe>=ide-1,j1,j2,idum1,idum2,idum3,idum4,rdum, &
           bxe, jms,jme,kms,kme, nreceive,2)
      
      ! South boundary:
      call bdy_send_one(jps==1,i1,i2,idum1,idum2,idum3,idum4,rdum, &
           bys, ims,ime,kms,kme, nreceive,2)

      ! North boundary:
      call bdy_send_one(jpe>=jde-1,i1,i2,idum1,idum2,idum3,idum4,rdum, &
           bye, ims,ime,kms,kme, nreceive,2)
    end subroutine bdy_send

    subroutine bdy_recv
      implicit none

      integer :: istore(ni*nk+ntasks), ikstore(ni*nk+ntasks)
      integer :: jstore(nj*nk+ntasks), jkstore(nj*nk+ntasks)
      integer :: counts(ntasks), disp(ntasks)
      real :: irstore(ni*nk+ntasks), jrstore(nj*nk+ntasks)
      integer :: storesize, nreceive

33    format('ni=',I0,' nj=',I0,' nk=',I0,' ntasks=',I0,' storesize=',I0,' or ',I0)
!      write(0,33) ni,nj,nk,ntasks,ni*nk+ntasks,nj*nk+ntasks

      ! West boundary:
      call bdy_send_one(ips==1,j1,j2, counts,disp, jstore,jkstore,jrstore, &
           bxs, jms,jme,kms,kme, nreceive, nj*nk+ntasks)
      call bdy_process_one(jstore,jkstore,jrstore,jds,jde,kds,kde,xsid, nreceive)

      ! East boundary:
      call bdy_send_one(ipe>=ide-1,j1,j2, counts,disp, jstore,jkstore,jrstore, &
           bxe, jms,jme,kms,kme, nreceive, nj*nk+ntasks)
      call bdy_process_one(jstore,jkstore,jrstore,jds,jde,kds,kde,xeid, nreceive)

      ! South boundary:
      call bdy_send_one(jps==1,i1,i2, counts,disp, istore,ikstore,irstore, &
           bys, ims,ime,kms,kme, nreceive, ni*nk+ntasks)
      call bdy_process_one(istore,ikstore,irstore,ids,ide,kds,kde,ysid, nreceive)

      ! North boundary:
      call bdy_send_one(jpe>=jde-1,i1,i2, counts,disp, istore,ikstore,irstore, &
           bye, ims,ime,kms,kme, nreceive, ni*nk+ntasks)
      call bdy_process_one(istore,ikstore,irstore,ids,ide,kds,kde,yeid, nreceive)

    end subroutine bdy_recv

    subroutine bdy_process_one(mstore,kstore,rstore,mds,mde,kds,kde,varid, nreceive)
      implicit none
      include 'netcdf.inc'
      integer :: m,k,n,nm
      integer, intent(in) :: mds,mde,kds,kde
      integer, intent(inout) :: nreceive
      integer, intent(in) :: mstore((mde-mds+1)*nk+ntasks)
      integer, intent(in) :: kstore((mde-mds+1)*nk+ntasks), varid
      real, intent(in) :: rstore((mde-mds+1)*nk+ntasks)

      real :: writeme(mds:mde,kds:kde)

      writeme=0
      nm=mde-mds+1
      do n=1,nreceive
         if(mstore(n)<0) cycle
 23      format('store writeme(',I0,' : ',I0,',',I0,') = ',F0.3)
!         write(0,23) n,mstore(n),kstore(n),rstore(n)
         writeme(mstore(n),kstore(n))=rstore(n)
      enddo
      assert(nf_put_var_real(fid,varid,writeme))
    end subroutine bdy_process_one

    subroutine bdy_send_one(bdyactive,m1,m2, &
         counts,disp,mstore,kstore,rstore, &
         bdy, bm1,bm2, bk1,bk2, nreceive, storesize)
      implicit none
      integer, intent(in) :: storesize
      integer, intent(in) :: bm1,bm2,bk1,bk2
      logical, intent(in) :: bdyactive
      integer, intent(inout), dimension(ntasks) :: counts,disp
      integer, intent(inout), dimension(storesize) :: mstore,kstore
      real, intent(inout), dimension(storesize) :: rstore
      integer, intent(in) :: m1,m2
      integer, intent(inout) :: nreceive
      real, intent(in) :: bdy(bm1:bm2,bk1:bk2,1)

      real :: values((m2-m1+1)*nk)
      integer :: mindex((m2-m1+1)*nk),kindex((m2-m1+1)*nk),csize,crank
      integer :: mycount(2),m,k,nm,task,ierr,shift

      nm=m2-m1+1

      mycount(1)=0
      if(monitor) then
         do task=1,ntasks
            counts(task)=87654321
            disp(task)=12345678
         enddo
      endif
      if(bdyactive) then
         mycount(1)=(m2-m1+1)*nk
      else
         mycount(1)=1
         mindex(1)=-99
         kindex(1)=-99
         values(1)=-99.
      endif
      
      call mpi_comm_size(local_communicator,csize,ierr)
      call mpi_comm_rank(local_communicator,crank,ierr)
9     format('I am rank ',I0,' of ',I0,' processes in communicator ',I0)
!      write(0,9) crank,csize,local_communicator

!      write(0,*) 'ntasks=',ntasks
!      write(0,*) 'send count=',mycount(1)
      ierr=-999
      call mpi_gather(mycount,1,MPI_INTEGER, counts,1,MPI_INTEGER, &
                      0,local_communicator, ierr)
!      write(0,*) 'result from mpi_gather is ',ierr
      if(monitor) then
         disp(1)=0
12       format(' rank ',I0,': count=',I0,' and disp=',I0)
!         write(0,12) 0,counts(1),disp(1)
         if(counts(1)<0 .or. counts(1)>storesize) then
            call wrf_error_fatal('Invalid count')
         endif
         do task=2,ntasks
            disp(task)=disp(task-1)+counts(task-1)
!            write(0,12) task-1,counts(task),disp(task)
            if(counts(task)<0 .or. counts(task)>storesize) then
               call wrf_error_fatal('Invalid count')
            endif
         enddo
      endif
      if(bdyactive) then
         do k=k1,k2
            do m=m1,m2
11             format('copy value(m=',I0,',k=',I0,' : shift=',I0,') = ',F0.3)
               shift = (k-k1)*nm + m-m1 + 1
!               write(0,11) m,k,shift,bdy(m,k,1)
               values(shift)=bdy(m,k,1)
               mindex(shift)=m
               kindex(shift)=k
!               write(0,*) '    ... copied.'
            enddo
         enddo
      endif
      if(monitor) then
!         write(0,12) 0,counts(1),disp(1)
         if(counts(1)<0 .or. counts(1)>storesize) then
            call wrf_error_fatal('Invalid count')
         endif
         do task=2,ntasks
!            write(0,12) task-1,counts(task),disp(task)
            if(counts(task)<0 .or. counts(task)>storesize) then
               call wrf_error_fatal('Invalid count')
            endif
         enddo
         nreceive=counts(ntasks)+disp(ntasks)
!         write(0,*) 'total received: ',nreceive
      endif
!      write(0,*) 'barrier before gathervs'
!      call mpi_barrier(local_communicator,ierr)
20 format('gatherv #',I0,' with mycount=',I0,'...')
!      write(0,20) 1,mycount(1)
!      flush(0)
!      if(monitor) then
!         write(0,*) 'counts(1)=',counts(1)
!         write(0,*) 'disp(1)=',disp(1)
!      endif
!      write(0,*) 'mycount(1)=',mycount(1)
!      write(0,*) 'mindex(1)=',mindex(1)
!      flush(0)

      call mpi_gatherv(values,mycount(1),MPI_REAL, rstore,counts,disp, &
                       MPI_REAL,0,local_communicator,ierr)

!      write(0,*) 'barrier before gathervs'
!      call mpi_barrier(local_communicator,ierr)
!      write(0,20) 2,mycount(1)
!      flush(0)

      call mpi_gatherv(kindex,mycount(1),MPI_INTEGER, kstore,counts,disp, &
                       MPI_INTEGER,0,local_communicator,ierr)

!      write(0,*) 'barrier before gathervs'
!      call mpi_barrier(local_communicator,ierr)
!      write(0,20) 3,mycount(1)
!      flush(0)

      call mpi_gatherv(mindex,mycount(1),MPI_INTEGER, mstore,counts,disp, &
                       MPI_INTEGER,0,local_communicator,ierr)
!      write(0,*) 'back.'
!      call mpi_barrier(local_communicator,ierr)
    end subroutine bdy_send_one
  end subroutine bdy_write_real

  subroutine bdy_write_int (fid,filename,            &
                            xsid,xeid,ysid,yeid,     &
                            ids,ide,jds,jde,kds,kde, &
                            ims,ime,jms,jme,kms,kme, &
                            ips,ipe,jps,jpe,kps,kpe, &
                            bxs,bxe,bys,bye)
    use module_dm, only: ntasks, local_communicator
    implicit none
    logical, external :: wrf_dm_on_monitor
    integer, parameter :: bdyw=1
    logical :: monitor
    character(*), intent(in) :: filename
    integer, intent(in) :: fid,xsid,xeid,ysid,yeid
    integer, intent(in) :: ids,ide,jds,jde,kds,kde, &
                           ims,ime,jms,jme,kms,kme, &
                           ips,ipe,jps,jpe,kps,kpe
    integer, intent(in) :: bxs(jms:jme,kms:kme,bdyw)
    integer, intent(in) :: bxe(jms:jme,kms:kme,bdyw)
    integer, intent(in) :: bys(ims:ime,kms:kme,bdyw)
    integer, intent(in) :: bye(ims:ime,kms:kme,bdyw)

    integer :: j1,j2,i1,i2,k1,k2, ni,nj,nk

!    write(0,*) 'DUDE, THIS IS LIKE SOOOOOOO INTEGER'

    monitor=wrf_dm_on_monitor()

    j1=max(jps,jds)
    j2=min(jpe,jde)
    i1=max(ips,ids)
    i2=min(ipe,jde)
30  format('ips=',I0,' ipe=',I0,' jps=',I0,' jpe=',I0,' kps=',I0,' kpe=',I0)
31  format('i1=',I0,' i2=',I0,' j1=',I0,' j2=',I0,' k1=',I0,' k2=',I0)
32  format('ni=',I0,' nj=',I0,' nk=',I0)
    k1=kds
    k2=kde
    ni=ide-ids+1
    nj=jde-jds+1
    nk=kde-kds+1
!    write(0,*) ips,ipe,jps,jpe,kps,kpe
!    write(0,*) i1,i2,j1,j2,k1,k2
!    write(0,*) ni,nj,nk

    if(ni<=0) call wrf_error_fatal('ni<=0')
    if(nj<=0) call wrf_error_fatal('nj<=0')
    if(nk<=0) call wrf_error_fatal('nk<=0')
    
    if(monitor) then
       call bdy_recv()
    else
       call bdy_send()
    endif

!    write(0,*) 'BUT DUDE, NO MORE INTEGER'

  contains
    subroutine bdy_send
      implicit none

      integer :: idum1(2),idum2(2),idum3(2),idum4(2), nreceive
      integer :: rdum(2)

      ! West boundary:
      call bdy_send_one(ips==1,j1,j2,idum1,idum2,idum3,idum4,rdum, &
           bxs, jms,jme,kms,kme, nreceive,2)

      ! East boundary:
      call bdy_send_one(ipe>=ide-1,j1,j2,idum1,idum2,idum3,idum4,rdum, &
           bxe, jms,jme,kms,kme, nreceive,2)
      
      ! South boundary:
      call bdy_send_one(jps==1,i1,i2,idum1,idum2,idum3,idum4,rdum, &
           bys, ims,ime,kms,kme, nreceive,2)

      ! North boundary:
      call bdy_send_one(jpe>=jde-1,i1,i2,idum1,idum2,idum3,idum4,rdum, &
           bye, ims,ime,kms,kme, nreceive,2)
    end subroutine bdy_send

    subroutine bdy_recv
      implicit none

      integer :: istore(ni*nk+ntasks), ikstore(ni*nk+ntasks)
      integer :: jstore(nj*nk+ntasks), jkstore(nj*nk+ntasks)
      integer :: counts(ntasks), disp(ntasks)
      integer :: irstore(ni*nk+ntasks), jrstore(nj*nk+ntasks)
      integer :: storesize, nreceive

33    format('ni=',I0,' nj=',I0,' nk=',I0,' ntasks=',I0,' storesize=',I0,' or ',I0)
!      write(0,33) ni,nj,nk,ntasks,ni*nk+ntasks,nj*nk+ntasks

      ! West boundary:
      call bdy_send_one(ips==1,j1,j2, counts,disp, jstore,jkstore,jrstore, &
           bxs, jms,jme,kms,kme, nreceive, nj*nk+ntasks)
      call bdy_process_one(jstore,jkstore,jrstore,jds,jde,kds,kde,xsid, nreceive)

      ! East boundary:
      call bdy_send_one(ipe>=ide-1,j1,j2, counts,disp, jstore,jkstore,jrstore, &
           bxe, jms,jme,kms,kme, nreceive, nj*nk+ntasks)
      call bdy_process_one(jstore,jkstore,jrstore,jds,jde,kds,kde,xeid, nreceive)

      ! South boundary:
      call bdy_send_one(jps==1,i1,i2, counts,disp, istore,ikstore,irstore, &
           bys, ims,ime,kms,kme, nreceive, ni*nk+ntasks)
      call bdy_process_one(istore,ikstore,irstore,ids,ide,kds,kde,ysid, nreceive)

      ! North boundary:
      call bdy_send_one(jpe>=jde-1,i1,i2, counts,disp, istore,ikstore,irstore, &
           bye, ims,ime,kms,kme, nreceive, ni*nk+ntasks)
      call bdy_process_one(istore,ikstore,irstore,ids,ide,kds,kde,yeid, nreceive)

    end subroutine bdy_recv

    subroutine bdy_process_one(mstore,kstore,rstore,mds,mde,kds,kde,varid, nreceive)
      implicit none
      include 'netcdf.inc'
      integer :: m,k,n,nm
      integer, intent(in) :: mds,mde,kds,kde
      integer, intent(inout) :: nreceive
      integer, intent(in) :: mstore((mde-mds+1)*nk+ntasks)
      integer, intent(in) :: kstore((mde-mds+1)*nk+ntasks), varid
      integer, intent(in) :: rstore((mde-mds+1)*nk+ntasks)

      integer :: writeme(mds:mde,kds:kde)

      writeme=0
      nm=mde-mds+1
      do n=1,nreceive
         if(mstore(n)<0) cycle
 23      format('store writeme(',I0,' : ',I0,',',I0,') = ',I0)
!         write(0,23) n,mstore(n),kstore(n),rstore(n)
         writeme(mstore(n),kstore(n))=rstore(n)
      enddo
      assert(nf_put_var_int(fid,varid,writeme))
    end subroutine bdy_process_one

    subroutine bdy_send_one(bdyactive,m1,m2, &
         counts,disp,mstore,kstore,rstore, &
         bdy, bm1,bm2, bk1,bk2, nreceive, storesize)
      implicit none
      integer, intent(in) :: storesize
      integer, intent(in) :: bm1,bm2,bk1,bk2
      logical, intent(in) :: bdyactive
      integer, intent(inout), dimension(ntasks) :: counts,disp
      integer, intent(inout), dimension(storesize) :: mstore,kstore
      integer, intent(inout), dimension(storesize) :: rstore
      integer, intent(in) :: m1,m2
      integer, intent(inout) :: nreceive
      integer, intent(in) :: bdy(bm1:bm2,bk1:bk2,1)

      integer :: values((m2-m1+1)*nk)
      integer :: mindex((m2-m1+1)*nk),kindex((m2-m1+1)*nk),csize,crank
      integer :: mycount(2),m,k,nm,task,ierr,shift

      nm=m2-m1+1

      mycount(1)=0
      if(monitor) then
         do task=1,ntasks
            counts(task)=87654321
            disp(task)=12345678
         enddo
      endif
      if(bdyactive) then
         mycount(1)=(m2-m1+1)*nk
      else
         mycount(1)=1
         mindex(1)=-99
         kindex(1)=-99
         values(1)=-99.
      endif
      
      call mpi_comm_size(local_communicator,csize,ierr)
      call mpi_comm_rank(local_communicator,crank,ierr)
9     format('I am rank ',I0,' of ',I0,' processes in communicator ',I0)
!      write(0,9) crank,csize,local_communicator

!      write(0,*) 'ntasks=',ntasks
!     write(0,*) 'send count=',mycount(1)
      ierr=-999
      call mpi_gather(mycount,1,MPI_INTEGER, counts,1,MPI_INTEGER, &
                      0,local_communicator, ierr)
!      write(0,*) 'result from mpi_gather is ',ierr
      if(monitor) then
         disp(1)=0
12       format(' rank ',I0,': count=',I0,' and disp=',I0)
!         write(0,12) 0,counts(1),disp(1)
         if(counts(1)<0 .or. counts(1)>storesize) then
            call wrf_error_fatal('Invalid count')
         endif
         do task=2,ntasks
            disp(task)=disp(task-1)+counts(task-1)
!            write(0,12) task-1,counts(task),disp(task)
            if(counts(task)<0 .or. counts(task)>storesize) then
               call wrf_error_fatal('Invalid count')
            endif
         enddo
      endif
      if(bdyactive) then
         do k=k1,k2
            do m=m1,m2
11             format('copy value(m=',I0,',k=',I0,' : shift=',I0,') = ',I0)
               shift = (k-k1)*nm + m-m1 + 1
!               write(0,11) m,k,shift,bdy(m,k,1)
               values(shift)=bdy(m,k,1)
               mindex(shift)=m
               kindex(shift)=k
!               write(0,*) '    ... copied.'
            enddo
         enddo
      endif
      if(monitor) then
!         write(0,12) 0,counts(1),disp(1)
         if(counts(1)<0 .or. counts(1)>storesize) then
            call wrf_error_fatal('Invalid count')
         endif
         do task=2,ntasks
!            write(0,12) task-1,counts(task),disp(task)
            if(counts(task)<0 .or. counts(task)>storesize) then
               call wrf_error_fatal('Invalid count')
            endif
         enddo
         nreceive=counts(ntasks)+disp(ntasks)
!         write(0,*) 'total received: ',nreceive
      endif
!      write(0,*) 'barrier before gathervs'
!      call mpi_barrier(local_communicator,ierr)
20 format('gatherv #',I0,' with mycount=',I0,'...')
!      write(0,20) 1,mycount(1)
!      flush(0)
!      if(monitor) then
!         write(0,*) 'counts(1)=',counts(1)
!         write(0,*) 'disp(1)=',disp(1)
!      endif
!      write(0,*) 'mycount(1)=',mycount(1)
!      write(0,*) 'mindex(1)=',mindex(1)
!      flush(0)

      call mpi_gatherv(values,mycount(1),MPI_INTEGER, rstore,counts,disp, &
                       MPI_INTEGER,0,local_communicator,ierr)

!      write(0,*) 'barrier before gathervs'
!      call mpi_barrier(local_communicator,ierr)
!      write(0,20) 2,mycount(1)
!      flush(0)

      call mpi_gatherv(kindex,mycount(1),MPI_INTEGER, kstore,counts,disp, &
                       MPI_INTEGER,0,local_communicator,ierr)

!      write(0,*) 'barrier before gathervs'
!      call mpi_barrier(local_communicator,ierr)
!      write(0,20) 3,mycount(1)
!      flush(0)

      call mpi_gatherv(mindex,mycount(1),MPI_INTEGER, mstore,counts,disp, &
                       MPI_INTEGER,0,local_communicator,ierr)
!      write(0,*) 'back.'
!      call mpi_barrier(local_communicator,ierr)
    end subroutine bdy_send_one
  end subroutine bdy_write_int

  subroutine bdy_nf_assert(status,filename,srcfile,srcline,fatal)
    implicit none
    character(*),intent(in) :: filename,srcfile
    integer, intent(in) :: status,srcline
    logical,intent(in), optional :: fatal
    character*80 strerr
    character*700 message

    include 'netcdf.inc'

    if(status/=NF_NOERR) then
       strerr=nf_strerror(status)
450    format(A,':',I0,': netcdf error ',I0,' (',A,') writing to "',A,'".')
       write(message,450) trim(srcfile),srcline,status,trim(strerr),trim(filename)
       if(present(fatal)) then
          if(fatal) then
             call wrf_error_fatal3(srcfile,srcline,message)
          endif
       endif
       call wrf_message(message)
    endif
  end subroutine bdy_nf_assert
#endif
#endif
end module module_bdywrite
