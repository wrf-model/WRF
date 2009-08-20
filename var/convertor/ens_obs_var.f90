
!xlf90 -g -qrealsize=8 ens_obs_var.f90 -o ens_obs_var.exe

program ens_obs_var

     parameter (ne=4)
     character(len=40) :: platform, date, name, id

     CHARACTER (LEN = 80)          :: filename1, filename2
     CHARACTER (LEN = 120)         :: fmt_info, &
                                      fmt_srfc, &
                                      fmt_srfc2, &
                                      fmt_each, fmt_each_iv
     character (len = 160)         :: header_string, eheader_string

     integer                       :: n, iunit(ne),ens_size
     real                          :: data, error
     real                          :: missing_v, missing_qc, missing_e
     integer                       :: iqc, iost
     real                :: xlon,xlat
     real                :: data1,error1, data2,error2, &
                            data3,error3, data4,error4, data5,error5, &
                            data6,error6, data7,error7, data8,error8, &
                            data9,error9
     integer             :: iqc1, iqc2, iqc3, iqc4, iqc5, iqc6, iqc7, iqc8, iqc9
     real, dimension(ne) :: edata1,eerror1, edata2,eerror2, &
                            edata3,eerror3, edata4,eerror4, edata5,eerror5, &
                            edata6,eerror6, edata7,eerror7, edata8,eerror8, &
                            edata9,eerror9, euinv,evinv,etinv,eqinv
     integer,dimension(ne):: eiqc1, eiqc2, eiqc3, eiqc4, eiqc5, eiqc6, eiqc7, eiqc8, eiqc9
     real                :: utotsp, vtotsp, ttotsp, qtotsp

     fmt_info = '(A12,1X,A19,1X,A40,1X,I6,3(F12.3,11X),6X,A5)'
     fmt_srfc = '(F12.3,I4,F7.2,F12.3,I4,F7.3)'
     fmt_srfc2 = '(7(:,f12.3,i4,f7.2))'
     fmt_each = '(3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2),11X,1(F12.3,I4,F7.2)))'
     fmt_each_iv = '(3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2),11X,1(F12.3,I4,F7.2),4f17.7)'

     missing_v  = -888888.000
     missing_qc = -88.0
     missing_e  = -888.0

     open(12,file='filtered_obs',status='old')
     open(13,file='filtered_obs.var')

     do n=1,ne
        iunit(n)=100+n
        write(filename1,'(a,i2.2)') 'filtered_obs.e',n
        open(iunit(n),file=filename1,status='old')
     enddo

     do
       read(unit=12,fmt='(a)',iostat=iost) header_string
       do n=1,ne
          read(unit=iunit(n),fmt='(a)',iostat=iost) eheader_string
       enddo
       if (iost /= 0) then
          print*, 'Problem reading obs header, exit'
          stop
       endif
       write(unit=13,fmt='(a)') header_string
       if (header_string(1:6) == '#-----' .or. header_string(1:21) == 'MODIFIED FILTERED OBS') exit
     enddo

     istamax=250000

     do ista=1,istamax
        read(12,fmt=trim(fmt_info),end=999) platform,date,name,ilev,xlat,xlon,elevation,id
        write(13,fmt=trim(fmt_info))        platform,date,name,ilev,xlat,xlon,elevation,id

        ! print*,'ista=',ista
        ! print*,platform,date,name,ilev,xlat,xlon,elevation,id
        read(12,fmt=trim(fmt_srfc),end=999) data1,iqc1,error1,data2,iqc2,error2
        write(13,fmt=trim(fmt_srfc2))        data1,iqc1,error1,data2,iqc2,error2

        do n=1,ne
           read(iunit(n),fmt=trim(fmt_info),end=999)platform,date,name,ilev,xlat,xlon,elevation,id
           read(iunit(n),fmt=trim(fmt_srfc),end=999)edata1(n),eiqc1(n),eerror1(n),edata2(n),eiqc2(n),eerror2(n)
           if(edata1(n).ne.data1 .or. eiqc1(n).ne.iqc1 .or. eerror1(n).ne.error1) then
             print*,'Observation mismatch!, ensemble#',n
             print*,platform,date,name,ilev,xlat,xlon,elevation,id
             print*,data1,iqc1,error1,data2,iqc2,error2
             print*,edata1(n),eiqc1(n),eerror1(n),edata2(n),eiqc2(n),eerror2(n)
             stop
           endif
        enddo

        do k=1,ilev
           read(12,fmt=trim(fmt_each),end=999) data3,iqc3,error3, data4,iqc4,error4, data5,iqc5,error5, &
                                               data6,iqc6,error6, data7,iqc7,error7, data8,iqc8,error8, &
                                               data9,iqc9,error9
           do n=1,ne
              read(iunit(n),fmt=trim(fmt_each_iv),end=999)   &
                  edata3(n),eiqc3(n),eerror3(n), edata4(n),eiqc4(n),eerror4(n), edata5(n),eiqc5(n),eerror5(n), &
                  edata6(n),eiqc6(n),eerror6(n), edata7(n),eiqc7(n),eerror7(n), edata8(n),eiqc8(n),eerror8(n), &
                  edata9(n),eiqc9(n),eerror9(n), euinv(n),evinv(n),etinv(n),eqinv(n)
           enddo
           call compute_total_spread(edata4,eiqc4,eerror4,euinv,ne,utotsp)
           call compute_total_spread(edata5,eiqc5,eerror5,evinv,ne,vtotsp)
           call compute_total_spread(edata7,eiqc7,eerror7,etinv,ne,ttotsp)
           call compute_total_spread(edata9,eiqc9,eerror9,eqinv,ne,qtotsp)

           if(qtotsp .ge. 0) qtotsp=max(0.01,qtotsp)
           write(13,fmt=trim(fmt_each))data3,iqc3,error3, data4,iqc4,utotsp, data5,iqc5,vtotsp, &
                                       data6,iqc6,error6, data7,iqc7,ttotsp, data8,iqc8,error8, &
                                       data9,iqc9,qtotsp
        enddo
     enddo
999  print*, ista-1
     if(ista-1.eq.istamax)then
     print*,'you should give larger istamax'

     endif

contains

subroutine compute_total_spread(edata,eqc,eerr,einv,esize,etotsp)
    integer :: esize, i
    real    :: edata(esize), eerr(esize), einv(esize), etotsp
    integer :: eqc(esize)
    real    :: emean, evar

    do i=1,esize
       if (edata(i)==missing_r .or. eqc(i)==missing_qc .or. eerr(i)==missing_e) then
          etotsp=missing_e
          return
       endif
       if (eerr(i).ne.eerr(1)) then
          print*,'Ensemble obs error mismatch: ',eerr(1), eerr(i)
          stop
       endif
    enddo
    call compute_mean_var(einv,esize,emean,evar)
    etotsp=sqrt(eerr(1)**2+evar)
    return
end subroutine compute_total_spread

subroutine compute_mean_var(evalue,esize,emean,evar)
    integer :: esize
    real    :: evalue(esize), emean, evar
    emean=sum(evalue)/esize 
    evar=sum((evalue-emean)**2)/(esize-1)
end subroutine compute_mean_var

end program 


