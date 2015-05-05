      MODULE ATM_cc

      USE CMP_COMM, ONLY: &

     &   MPI_COMM_Atmos => COMM_local, &

     &   Coupler_id, &
     &   component_master_rank_local, &
     &   process_rank_local, &
     &   component_nprocs, &
     &   ibuffer, &

     &   MPI_INTEGER,MPI_STATUS_SIZE, &
     &   kind_REAL,kind_alt_REAL, &
     &   MPI_kind_REAL,MPI_kind_alt_REAL

      implicit none

      integer,parameter:: ND=3
      integer Ocean_spec /-1/, WM_id /-10/
      integer NSF
      integer NSF_WM
      real dtc,              &  !<- Coupling period
     &     dta,              &  !<- AM time step ("physical")
     &     dta2dtc              !<- AM time step / Coupling period
      integer i_dtc2dta /100/   !<- Coupling period / AM time step
      integer & !,dimension(ND)::
     &ims,ime,jms,jme,its,ite,jts,jte,ids,idf,jds,jdf,  NGP
      integer kms,kme,kts,kte,kds,kde
      integer,parameter:: kind_R=kind_alt_REAL
!c     integer,parameter:: kind_tiling=kind_R
      integer,parameter:: kind_sfcflux=kind_R, &
     &                    kind_SST=kind_R, &
     &                    kind_SLM=kind_R, &
     &                    kind_lonlat=kind_R
      integer MPI_kind_R, &
     &MPI_kind_sfcflux,MPI_kind_SST,MPI_kind_SLM,MPI_kind_lonlat
      integer n_ts(ND) /0,0,0/, gid
      integer rc /5/
      real,parameter:: &
     &   SLM_OS_value=1., & !<-must be real open sea mask value in AM
     &   unrealistically_low_SST=0.01, &  ! <- must be unreal low but >=0.,
                                       ! see interp. --- check!
     &   unrealistically_low_SV=-1.E30, &
                     ! <- must be negative unreal low surface flux
                     ! or other surface value to be sent
                     ! to Coupler, see Coupler code
     &   unrealistically_low_SF=unrealistically_low_SV, & !<- same thing
     &   unrealistically_low_SVp=0.99*unrealistically_low_SV

      logical initialized /.false./
      logical PHYS,zeroSF,nrmSF,sendSF,getSST

      TYPE SST_ARRAY
        real(kind=kind_SST),dimension(:,:),pointer:: a 
      END TYPE SST_ARRAY
      TYPE SF_ARRAY
        real(kind=kind_sfcflux),dimension(:,:,:),pointer:: a
      END TYPE SF_ARRAY

      TYPE (SST_ARRAY), dimension(ND):: SST_cc
      TYPE (SF_ARRAY), dimension(min(ND,2)):: sf

      character*12 sgid

!Controls:
      integer nunit_announce /6/, VerbLev /3/

      SAVE

      END MODULE ATM_cc
!C
!C***********************************************************************
!C
      SUBROUTINE ATM_SET_COMM(new_atm_comm)
        USE ATM_cc
        integer, intent(in) :: new_atm_comm

!       This routine is called when the atmospheric model wants to
!       remove processors from taking part in coupling so that they
!       can perform I/O or diagnostics.  Any processors that will
!       continue to be in MPI_COMM_Atmos must call this routine, and
!       any processors that will leave MPI_COMM_Atmos must call
!       ATM_LEAVE_COUPLING.

        MPI_COMM_Atmos=new_atm_comm
      end
!C
!C***********************************************************************
!C
      SUBROUTINE ATM_LEAVE_COUPLING()
        USE ATM_cc

!       This routine is called when the atmospheric model wants to
!       remove processors from taking part in coupling so that they
!       can perform I/O or diagnostics.  Any processors that will
!       continue to be in MPI_COMM_Atmos must call ATM_SET_COMM, and
!       any processors that will leave MPI_COMM_Atmos must call
!       this routine.

!       Currently, there is nothing we have to do here.
        return

      end
!C
!C***********************************************************************
!C
      SUBROUTINE ATM_CMP_START(atm_comm)

      USE ATM_cc

      implicit none

      integer atm_comm

      integer Atmos_id /1/, Atmos_master_rank_local /0/, Atmos_spec /1/
      integer ibuf(1),ierr
      character*20 s
!C

                      !<-id of OM as a component of the coupled system
      call CMP_INIT(Atmos_id,1)
                             !<-"flexibility level"
      if (Coupler_id.ge.0) VerbLev=min(VerbLev,ibuffer(4))
      write(s,'(i2)') VerbLev

      call CMP_INTRO(Atmos_master_rank_local)
      call ATM_ANNOUNCE('back from CMP_INTRO, VerbLev='//s,2)

      initialized=.true.

      call CMP_INTEGER_SEND(Atmos_spec,1)

      call CMP_gnr_RECV(Ocean_spec,1,MPI_INTEGER)
      write(s,'(i2)') Ocean_spec
      call ATM_ANNOUNCE('back from CMP_INTEGER_RECV, OM spec is '//s,2)
      call MPI_BCAST(Ocean_spec,1,MPI_INTEGER, &
     &component_master_rank_local,MPI_COMM_Atmos,ierr)
      call ATM_ANNOUNCE('ATM_CMP_START: Ocean_spec broadcast',2)

      call CMP_gnr_RECV(WM_id,1,MPI_INTEGER)
      write(s,'(i4)') WM_id
      call ATM_ANNOUNCE('back from CMP_INTEGER_RECV, WM id is '//s,2)
      call MPI_BCAST(WM_id,1,MPI_INTEGER, &
     &component_master_rank_local,MPI_COMM_Atmos,ierr)
      call ATM_ANNOUNCE('ATM_CMP_START: WM_id broadcast',2)
      if (WM_id.gt.0) then
        NSF_WM=2
      else
        NSF_WM=0
      end if

      if (Ocean_spec.eq.1) then
        NSF=4+NSF_WM
      else if (Ocean_spec.eq.2) then
        NSF=8+NSF_WM
      else if (Ocean_spec.eq.0) then
        NSF=NSF_WM
      else if (Coupler_id.ge.0) then
        call GLOB_ABORT(Ocean_spec-1, &
     &  'ATM_CMP_START received wrong Ocean_spec value, aborted',rc)
      else
        Ocean_spec=1
        NSF=4
        call ATM_ANNOUNCE('AM is standalone: Ocean_spec=1, NSF=4'// &
     &  ' assigned (as if for POM coupling)',2)
      end if

      if (kind_R.eq.kind_REAL) then
        MPI_kind_R=MPI_kind_REAL
      else 
        MPI_kind_R=MPI_kind_alt_REAL
      end if
      if (kind_sfcflux.eq.kind_REAL) then
        MPI_kind_sfcflux=MPI_kind_REAL
      else 
        MPI_kind_sfcflux=MPI_kind_alt_REAL
      end if
      if (kind_SST.eq.kind_REAL) then
        MPI_kind_SST=MPI_kind_REAL
      else 
        MPI_kind_SST=MPI_kind_alt_REAL
      end if
      if (kind_SLM.eq.kind_REAL) then
        MPI_kind_SLM=MPI_kind_REAL
      else 
        MPI_kind_SLM=MPI_kind_alt_REAL
      end if
      if (kind_lonlat.eq.kind_REAL) then
        MPI_kind_lonlat=MPI_kind_REAL
      else 
        MPI_kind_lonlat=MPI_kind_alt_REAL
      end if

      atm_comm=MPI_COMM_Atmos

      return
      END
!C
!C***********************************************************************
!C
      SUBROUTINE ATM_INIT_CHECK(s)

      USE ATM_cc, ONLY: initialized,rc

      implicit none

      character*(*) s

      if (.not. initialized) call GLOB_ABORT(1,s,rc)

      return
      END
!C
!C***********************************************************************
!C
      subroutine ATM_TSTEP_INIT(NTSD,NPHS,gid_,dta_, &
     &ids_,idf_,jds_,jdf_,its_,ite_,jts_,jte_,ims_,ime_,jms_,jme_, &
       !<-"domain"         !<-"tile"           !<-"memory" (tile+halo)
     &kds_,kde_,kts_,kte_,kms_,kme_,&
     &HLON,HLAT,VLON,VLAT, &
     &SLM, &
     &i_parent_start,j_parent_start,&
     &guessdtc,dtc_)

      USE ATM_cc

      implicit none

      real, intent(in) :: guessdtc
      real, intent(out) :: dtc_

      integer NTSD,NPHS,gid_
      real dta_
      integer ids_,idf_,jds_,jdf_,its_,ite_,jts_,jte_, &
     &ims_,ime_,jms_,jme_,kds_,kde_,kts_,kte_,kms_,kme_
      real(kind=kind_lonlat),dimension(ims_:ime_,jms_:jme_):: &
     &HLON,HLAT,VLON,VLAT
      real(kind=kind_SLM),dimension(ims_:ime_,jms_:jme_):: SLM
      integer i_parent_start,j_parent_start

      integer KDT,buf(2) /0,0/
      character*24 s
      character*80 s1
      character*255 message
      SAVE
!C

      gid=gid_
      call GLOB_ABORT((gid-1)*(gid-2)*(gid-3), &
     &'Abort: in ATM_TSTEP_INIT gid is neither 1 nor 2 nor 3',rc)
      KDT=NTSD/NPHS+1
      PHYS=MOD(NTSD,NPHS).eq.0 ! .and. gid.eq.1 <-removed to bring MG in
      dta=dta_ 

      write(s1,'("gid=",i1," NTSD=",i5," NPHS=",i3," KDT=",i5,'// &
     &'" PHYS=",L1)') gid,NTSD,NPHS,KDT,PHYS
      call ATM_ANNOUNCE('ATM_TSTEP_INIT entered: '//trim(s1),3)

!c     IF (n_ts.eq.-1 .and. PHYS) THEN
!c       PHYS=.false.
!c       n_ts=0   ! init. value must be -1 . But if PHYS does not need
!c                ! this correction, init. value must be 0 (whereas this
!c                ! IF statement may stay)
!c     END IF
      if (.not.PHYS) then
        zeroSF=.false.
        nrmSF=.false.
        sendSF=.false.
        RETURN
      end if

      n_ts(gid)=n_ts(gid)+1  ! init. value must be 0   ***0***
      write(s,'(2i8)') KDT,n_ts(gid)
      write(sgid,'(" grid id = ",i1)') gid
      call ATM_ANNOUNCE('ATM_TSTEP_INIT working:'// &
     &sgid//'; KDT, n_ts: '//s,3)
      call GLOB_ABORT(KDT-n_ts(gid), &
     &'Abort: in ATM_TSTEP_INIT KDT, n_ts(gid) differ '//s,rc)

      call ATM_RECVdtc(guessdtc)
      dtc_=dtc

      zeroSF=((n_ts(gid)-1)/i_dtc2dta)*i_dtc2dta .eq. n_ts(gid)-1
      nrmSF=(n_ts(gid)/i_dtc2dta)*i_dtc2dta .eq. n_ts(gid)
      sendSF=(n_ts(gid)/i_dtc2dta)*i_dtc2dta .eq. n_ts(gid)
                                    !<-check, this depends
                                    ! on where ATM_SENDFLUXES is called.
                                    ! MOD(n_ts,i_dtc2dta).eq.0 should
                                    ! be good for calling it after
                                    ! ATM_DOFLUXES at the same t.s.

      ids=ids_
      idf=idf_
      jds=jds_
      jdf=jdf_
      its=its_
      ite=ite_
      jts=jts_
      jte=jte_
      ims=ims_
      ime=ime_
      jms=jms_
      jme=jme_

      kds=kds_
      kde=kde_
      kts=kts_
      kms=kms_
      kme=kme_
      kte=kte_

      NGP=(idf-ids+1)*(jdf-jds+1)

      IF (n_ts(gid).eq.1) THEN

      call ATM_ANNOUNCE('ATM_TSTEP_INIT to allocate sf, SST_cc',3)

      IF (gid.le.2) THEN !** innermost grid not active in coupling **
        allocate(sf(gid)%a(ims:ime,jms:jme,NSF))
      END IF !** innermost grid not active in coupling **
        allocate(SST_cc(gid)%a(ims:ime,jms:jme))

      END IF

      if (gid.eq.2) then
        write(s,'(2i8)') i_parent_start,j_parent_start
        if (zeroSF) then
          buf(1)=i_parent_start
          buf(2)=j_parent_start
          call CMP_INTEGER_SEND(buf,2)
          call ATM_ANNOUNCE( &
     &    'ATM_TSTEP_INIT: i_parent_start, j_parent_start sent '//s,3)
        else
          call GLOB_ABORT(abs(i_parent_start-buf(1))+abs(j_parent_start- &
     &    buf(2)),'NESTED GRID MOVED DURING C TIME STEP: ABORTED '// &
     &    s,rc)
        end if
      end if

      IF (gid.le.2) THEN !** innermost grid not active in coupling **

      CALL ATM_SENDGRIDS(HLON,HLAT,VLON,VLAT)

      CALL ATM_SENDSLM(SLM)

      END IF !** innermost grid not active in coupling **

      if (VerbLev.ge.2) then
         write(message,*) 'AM: ATM_TSTEP_INIT: returning ',gid,         &
     &n_ts(gid),ids,idf,jds,jdf,its,ite,jts,jte,ims,ime,jms,jme,NGP,NSF
         call wrf_message(message)
      endif

      RETURN
      end
!C
!C***********************************************************************
!C
      SUBROUTINE ATM_RECVdtc(guessdtc)

      USE ATM_cc

      implicit none

      real,intent(in) :: guessdtc
      real(kind=kind_R) buf(1),dtc2dta
      integer ierr,i
      logical first/.true./
      character*20 s
      SAVE
!C

      write(s,'(1pe20.12)') dta
      call ATM_ANNOUNCE('ATM_RECVdtc: AM time step dta='//s,3)

      IF (first) THEN
        call ATM_ANNOUNCE( &
     &  'ATM_RECVdtc: to receive C time step; AM time step dta='//s,2)

        call CMP_gnr_RECV(buf,1,MPI_kind_R)

        call MPI_BCAST(buf,1,MPI_kind_R, &
     &  component_master_rank_local,MPI_COMM_Atmos,ierr)
        call ATM_ANNOUNCE('ATM_RECVdtc: C time step broadcast',2)
        dtc=buf(1)

        if (Coupler_id.lt.0) then
          dtc=guessdtc
          write(s,'(1pe20.12)') dtc
          call ATM_ANNOUNCE('ATM_RECVdtc: C time step assigned '// &
     &    trim(s)//' : standalone mode',2)
        else
          write(s,'(1pe20.12)') buf
          call ATM_ANNOUNCE( &
     &    'ATM_RECVdtc: C time step dtc='//s//' received',2)
        end if
      END IF

      dtc2dta=dtc/dta
      i_dtc2dta=nint(dtc2dta)
      if (abs(i_dtc2dta-dtc2dta).gt.1.E-5) call GLOB_ABORT(1, &
     &'AM: ABORTED: dtc is not a multiple of dta',1)

      i=3
      if (n_ts(gid).eq.1) i=2
      if (i_dtc2dta.eq.0) then
        i_dtc2dta=4
        call ATM_ANNOUNCE('ratio of C/AM time steps =0, assigned 4 .'// &
     &  ' This should only occur in standalone mode and ONLY IF dtc '// &
     &  'HAS NOT BEEN ASSIGNED A POSITIVE VALUE: ** ATTENTION **',i)
      else
        write(s,'(i2)') i_dtc2dta
        call ATM_ANNOUNCE('ratio of C/AM time steps: '//trim(s),i)
      end if

      dta2dtc=1./i_dtc2dta

      first=.false.

      RETURN
      END
!C
!C***********************************************************************
!C
      SUBROUTINE ATM_SENDGRIDS(HLON,HLAT,VLON,VLAT)

      USE ATM_cc

      implicit none

      real(kind=kind_lonlat),dimension(ims:ime,jms:jme):: &
     &HLON,HLAT,VLON,VLAT 

      real(kind=kind_lonlat),dimension(ids:idf,jds:jdf):: &
     &ALONt,ALATt,ALONv,ALATv

      integer buf(2)
!C

!c     IF (gid.ne.1) RETURN ! only "parent grid" dim. and coor. are sent

      IF (.not.PHYS .or. n_ts(gid).ne.1) RETURN

      IF (gid.gt.2) RETURN ! innermost grid's dim. / coor. are not sent
      
!temporarily excluded      if (Coupler_id.lt.0) return    !   <- standalone mode

      buf(1)=idf-ids+1
      buf(2)=jdf-jds+1
      call ATM_ANNOUNCE('to send grid dimensions,'//sgid,1)
      call CMP_INTEGER_SEND(buf,2)
      call ATM_ANNOUNCE('grid dimensions sent,'//sgid,1)

!c     IF (gid.eq.1) THEN    !  only "parent grid" coordinates are sent

        call ASSEMBLE(ALONt,HLON,kind_lonlat)
        call ASSEMBLE(ALATt,HLAT,kind_lonlat)
        call ASSEMBLE(ALONv,VLON,kind_lonlat)
        call ASSEMBLE(ALATv,VLAT,kind_lonlat)

        call ATM_ANNOUNCE('(BP) to send grid arrays (4 MPI calls)',2)

        call CMP_gnr_SEND(ALONt,NGP,MPI_kind_lonlat)
        call CMP_gnr_SEND(ALATt,NGP,MPI_kind_lonlat)
        call CMP_gnr_SEND(ALONv,NGP,MPI_kind_lonlat)
        call CMP_gnr_SEND(ALATv,NGP,MPI_kind_lonlat)

        call ATM_ANNOUNCE('the 4 grid arrays sent',1)

!c     END IF

      call ATM_ANNOUNCE('(BP) ATM_SENDGRIDS: returning',2)

      return
      END
!C
!C***********************************************************************
!C
      SUBROUTINE ATM_SENDSLM(SLM)

      USE ATM_cc

      implicit none

      real(kind=kind_SLM),dimension(ims:ime,jms:jme):: SLM

      real(kind=kind_SLM),dimension(ids:idf,jds:jdf):: SLM_g
      integer buf(2)
!C

!c     IF (gid.ne.1) RETURN  !  only "parent grid" mask is sent
      IF (.not.PHYS .or. n_ts(gid).ne.1) RETURN

      IF (gid.gt.2) RETURN ! innermost grid's mask is not sent
      
!temporarily excluded      if (Coupler_id.lt.0) return    !   <- standalone mode

      call ASSEMBLE(SLM_g,SLM,kind_SLM)

      call ATM_ANNOUNCE('(BP) to send SLM',2)

      call CMP_gnr_SEND(SLM_g,NGP,MPI_kind_SLM)
      call CMP_gnr_SEND(SLM_g,NGP,MPI_kind_SLM)
           ! Coupler requires as many copies of mask as there are grids
           ! [and mask array is the same for H- (=t-) and V- grids]

      call ATM_ANNOUNCE('(BP) ATM_SENDSLM: returning',2)

      return
      END
!C
!C***********************************************************************
!C
      SUBROUTINE ATM_GETSST(SST,SLM)

      USE ATM_cc

      implicit none

      real(kind=kind_SST) SST(ims:ime,jms:jme)
      real(kind=kind_SLM) SLM(ims:ime,jms:jme)

      integer i,j
      real(kind=kind_SST) SST_g(ids:idf,jds:jdf)
!C

      IF (.not.PHYS) RETURN

      IF (gid.gt.2) RETURN ! nothing is done to get innermost grid's
                           ! SST ** IN THIS PRELIMINARY VERSION **

      call ATM_ANNOUNCE('ATM_GETSST entered (PHYS=.true.)',3)

      getSST=((n_ts(gid)-1)/i_dtc2dta)*i_dtc2dta .eq. n_ts(gid)-1
      if (getSST.neqv.zeroSF) then
        call GLOB_ABORT(1,'getSST differs from zeroSF, which screws'// &
     &  ' up the design for exchanges with C',rc)
      end if

      if (getSST) then
        if (n_ts(gid).eq.1 .and. gid.eq.1) then
          call ATM_ANNOUNCE('ATM_GETSST: to send ref. SST'//sgid,2)
          call ASSEMBLE(SST_g,SST,kind_SST)
          call CMP_gnr_SEND(SST_g,NGP,MPI_kind_SST)
          call ATM_ANNOUNCE('ATM_GETSST: ref. SST sent'//sgid,2)
        end if
        call ATM_ANNOUNCE('ATM_GETSST: to receive SST',3)
        call CMP_gnr_RECV(SST_g,NGP,MPI_kind_SST)
        call DISASSEMBLE(SST_g,SST_cc(gid)%a,kind_SST)
        call ATM_ANNOUNCE('ATM_GETSST: SST received',3)
      end if
      
      if (Coupler_id.lt.0) return    !   <- standalone mode

      do j=jts,jte
      do i=its,ite
        if (abs(SLM(i,j)-SLM_OS_value).lt.0.01) then 
                                  ! i.e. if it is OS (open sea) AMGP
                                  !
          if (SST_cc(gid)%a(i,j).gt.unrealistically_low_SST) &
                                          ! i.e. if there is a valid
                                          ! result of interpolation from
                                          ! OMG for this AMGP
     &       SST(i,j)=SST_cc(gid)%a(i,j)
        end if
      end do
      end do

      return
      END
!C
!C***********************************************************************
!C
      SUBROUTINE ATM_DOFLUXES(TWBS,QWBS,RLWIN,RSWIN,RADOT,RSWOUT, &
!c    &USTAR,U10,V10,PINT,PREC)
     &TX,TY,PINT,PREC,U10,V10)

      USE ATM_cc

      implicit none

      real(kind=kind_sfcflux),dimension(ims:ime,jms:jme,kms:kme):: PINT

      real(kind=kind_sfcflux),dimension(ims:ime,jms:jme):: &
     &TWBS,QWBS,RLWIN,RSWIN,RADOT,RSWOUT,TX,TY,PREC,U10,V10
!c    &TWBS,QWBS,RLWIN,RSWIN,RADOT,RSWOUT,USTAR,U10,V10,PINT,PREC
!       Act. arg. for PINT is a 3d array - so this only is OK if
!       Ps=Act.arg.(:,:.1) - actually, Ps=PINT(:,1,:)

      real(kind=kind_sfcflux),dimension(ims:ime,jms:jme):: SWR,R
      real dtainv
!C

      IF (.not.PHYS) RETURN

      IF (gid.gt.2) RETURN

! Debug insertion:->
!c     if (PREC(ims+3,jms+3).ne.0 .or. PREC(ims+5,jms+5).ne.0) then
!c       print*,'ATM_DOFLUXES,gid,n_ts(gid),PREC(3,3),PREC(5,5): ',
!c    &  gid,n_ts(gid),PREC(ims+3,jms+3),PREC(ims+5,jms+5)
!c     end if
! <-:Debug insertion

      call ATM_ANNOUNCE('ATM_DOFLUXES entered',3)

      dtainv=1./dta

      if (zeroSF) sf(gid)%a=0.

      SWR(its:ite,jts:jte)=-RSWIN(its:ite,jts:jte)+RSWOUT(its:ite,jts:jte)          ! Check sign! here SWR is meant to be
                                 ! positive upward
!c     sf(gid)%a(:,:,NSF-1)=sf(gid)%a(:,:,NSF-1)-TX
!c     sf(gid)%a(:,:,NSF)=sf(gid)%a(:,:,NSF)-TY
!c                    ! <- signs for stress components are     changed
!c                    ! so it is -stress

!c     R=SWR+RADOT-RLWIN          ! Check sign! here R (net radiation)
                                 ! is meant to be positive upward

!oooooooooooooooooooooooooooooo
      IF (Ocean_spec.eq.1) THEN
!oooooooooooooooooooooooooooooo
        sf(gid)%a(its:ite,jts:jte,1)=sf(gid)%a(its:ite,jts:jte,1)-TWBS(its:ite,jts:jte)-QWBS(its:ite,jts:jte)+RADOT(its:ite,jts:jte)-RLWIN(its:ite,jts:jte)
                                       ! -TWBS (-QWBS) is supposed to
                                       ! be sensible (latent) heat flux,
                                       ! positive upward
        sf(gid)%a(its:ite,jts:jte,2)=sf(gid)%a(its:ite,jts:jte,2)+SWR(its:ite,jts:jte)
        sf(gid)%a(its:ite,jts:jte,NSF-NSF_WM-1)=sf(gid)%a(its:ite,jts:jte,NSF-NSF_WM-1)-TX(its:ite,jts:jte)
        sf(gid)%a(its:ite,jts:jte,NSF-NSF_WM)=sf(gid)%a(its:ite,jts:jte,NSF-NSF_WM)-TY(its:ite,jts:jte)
                     ! <- signs for stress components are changed
!ooooooooooooooooooooooooooooooooooo
      ELSE IF (Ocean_spec.eq.2) THEN
!ooooooooooooooooooooooooooooooooooo
        sf(gid)%a(its:ite,jts:jte,1)=sf(gid)%a(its:ite,jts:jte,1)+PREC(its:ite,jts:jte)
        sf(gid)%a(its:ite,jts:jte,2)=sf(gid)%a(its:ite,jts:jte,2)-TWBS(its:ite,jts:jte)
        sf(gid)%a(its:ite,jts:jte,3)=sf(gid)%a(its:ite,jts:jte,3)-QWBS(its:ite,jts:jte)
        sf(gid)%a(its:ite,jts:jte,4)=sf(gid)%a(its:ite,jts:jte,4)+PINT(its:ite,jts:jte,1)-101300.
        sf(gid)%a(its:ite,jts:jte,5)=sf(gid)%a(its:ite,jts:jte,5)-SWR(its:ite,jts:jte)-RADOT(its:ite,jts:jte)+RLWIN(its:ite,jts:jte)
        sf(gid)%a(its:ite,jts:jte,6)=sf(gid)%a(its:ite,jts:jte,6)-SWR(its:ite,jts:jte)

        sf(gid)%a(its:ite,jts:jte,NSF-NSF_WM-1)=sf(gid)%a(its:ite,jts:jte,NSF-NSF_WM-1)+TX(its:ite,jts:jte)
        sf(gid)%a(its:ite,jts:jte,NSF-NSF_WM)=sf(gid)%a(its:ite,jts:jte,NSF-NSF_WM)+TY(its:ite,jts:jte)
                     ! <- signs for stress components are NOT changed
        if (nrmSF) then
          sf(gid)%a(its:ite,jts:jte,1)=sf(gid)%a(its:ite,jts:jte,1)*dtainv
                        ! so this will be m/s; check what OM wants
        end if
!ooooooooooo
      END IF
!ooooooooooo


!wwwwwwwwwwwwwwwwwwwwwwwww
      IF (WM_id.gt.0) THEN
!wwwwwwwwwwwwwwwwwwwwwwwww
        sf(gid)%a(its:ite,jts:jte,NSF-1)=sf(gid)%a(its:ite,jts:jte,NSF-1)+U10(its:ite,jts:jte)
        sf(gid)%a(its:ite,jts:jte,NSF)=sf(gid)%a(its:ite,jts:jte,NSF)+V10(its:ite,jts:jte)
!wwwwwwwwwww
      END IF
!wwwwwwwwwww

      if (nrmSF) then
        sf(gid)%a=sf(gid)%a*dta2dtc
      end if

      call ATM_ANNOUNCE('ATM_DOFLUXES to return',3)

      return
      END
!C
!C***********************************************************************
!C
      SUBROUTINE ATM_SENDFLUXES

      USE ATM_cc

      implicit none

      real(kind=kind_sfcflux) F(ids:idf,jds:jdf)
      integer n
!C

      if (.not.PHYS) RETURN

      IF (gid.gt.2) RETURN

      if (.not.sendSF) then
        call ATM_ANNOUNCE( &
     &  'ATM_SENDLUXES entered with PHYS but not sendSF: returning'// &
     &  sgid,3)
        RETURN
      end if

      call ATM_ANNOUNCE('In ATM_SENDLUXES'//sgid,3)

      do n=1,NSF
        call ASSEMBLE(F,sf(gid)%a(:,:,n),kind_sfcflux)
        call CMP_gnr_SEND(F,NGP,MPI_kind_sfcflux)
      end do

      call ATM_ANNOUNCE('ATM_SENDFLUXES to return'//sgid,3)

      return
      END
!C
!C***********************************************************************
!C
      SUBROUTINE ATM_ANNOUNCE(s,DbgLev)

      USE ATM_cc, ONLY: nunit_announce,VerbLev,MPI_COMM_Atmos

      implicit none

      character*(*) s
      integer DbgLev

      integer ierr
!C
      if (DbgLev.le.VerbLev) then
        if (s(1:5).eq.'(BP) ') then
          call MPI_BARRIER(MPI_COMM_Atmos,ierr)
        end if
        CALL CMP_ANNOUNCE(nunit_announce,'AM: '//s)
      end if

      return
      END
