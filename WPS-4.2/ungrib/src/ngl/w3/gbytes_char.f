      SUBROUTINE GBYTEC(IN,IOUT,ISKIP,NBYTE)
      character*1 in(*)
      integer iout(*)
      CALL GBYTESC(IN,IOUT,ISKIP,NBYTE,0,1)
      RETURN
      END

      SUBROUTINE SBYTEC(OUT,IN,ISKIP,NBYTE)
      character*1 out(*)
      integer in(*)
      CALL SBYTESC(OUT,IN,ISKIP,NBYTE,0,1)
      RETURN
      END

      SUBROUTINE GBYTESC(IN,IOUT,ISKIP,NBYTE,NSKIP,N)
C          Get bytes - unpack bits:  Extract arbitrary size values from a
C          packed bit string, right justifying each value in the unpacked
C          array.
C            IN    = character*1 array input
C            IOUT  = unpacked array output
C            ISKIP = initial number of bits to skip
C            NBYTE = number of bits to take
C            NSKIP = additional number of bits to skip on each iteration
C            N     = number of iterations
C v1.1
C
      character*1 in(*)
      integer iout(*)
      integer ones(8), tbit, bitcnt
      save ones
      data ones/1,3,7,15,31,63,127,255/

c     nbit is the start position of the field in bits
      nbit = iskip
      do i = 1, n
         bitcnt = nbyte
         index=nbit/8+1
         ibit=mod(nbit,8)
         nbit = nbit + nbyte + nskip

c        first byte
         tbit = min(bitcnt,8-ibit)
         itmp = iand(mov_a2i(in(index)),ones(8-ibit))
         if (tbit.ne.8-ibit) itmp = ishft(itmp,tbit-8+ibit)
         index = index + 1
         bitcnt = bitcnt - tbit

c        now transfer whole bytes
         do while (bitcnt.ge.8)
             itmp = ior(ishft(itmp,8),mov_a2i(in(index)))
             bitcnt = bitcnt - 8
             index = index + 1
         enddo

c        get data from last byte
         if (bitcnt.gt.0) then
            itmp = ior(ishft(itmp,bitcnt),iand(ishft(mov_a2i(in(index)),
     1          -(8-bitcnt)),ones(bitcnt)))
         endif

         iout(i) = itmp
      enddo

      RETURN
      END                                                                  

      SUBROUTINE SBYTESC(OUT,IN,ISKIP,NBYTE,NSKIP,N)
C          Store bytes - pack bits:  Put arbitrary size values into a
C          packed bit string, taking the low order bits from each value
C          in the unpacked array.
C            IOUT  = packed array output
C            IN    = unpacked array input
C            ISKIP = initial number of bits to skip
C            NBYTE = number of bits to pack
C            NSKIP = additional number of bits to skip on each iteration
C            N     = number of iterations
C v1.1
C
      character*1 out(*)
      integer in(N), bitcnt, ones(8), tbit
      save ones
      data ones/    1,  3,  7, 15, 31, 63,127,255/

c     number bits from zero to ...
c     nbit is the last bit of the field to be filled

      nbit = iskip + nbyte - 1
      do i = 1, n
         itmp = in(i)
         bitcnt = nbyte
         index=nbit/8+1
         ibit=mod(nbit,8)
         nbit = nbit + nbyte + nskip

c        make byte aligned 
         if (ibit.ne.7) then
             tbit = min(bitcnt,ibit+1)
             imask = ishft(ones(tbit),7-ibit)
             itmp2 = iand(ishft(itmp,7-ibit),imask)
             itmp3 = iand(mov_a2i(out(index)), 255-imask)
             out(index) = char(ior(itmp2,itmp3))
             bitcnt = bitcnt - tbit
             itmp = ishft(itmp, -tbit)
             index = index - 1
         endif

c        now byte aligned

c        do by bytes
         do while (bitcnt.ge.8)
             out(index) = char(iand(itmp,255))
             itmp = ishft(itmp,-8)
             bitcnt = bitcnt - 8
             index = index - 1
         enddo

c        do last byte

         if (bitcnt.gt.0) then
             itmp2 = iand(itmp,ones(bitcnt))
             itmp3 = iand(mov_a2i(out(index)), 255-ones(bitcnt))
             out(index) = char(ior(itmp2,itmp3))
         endif
      enddo

      return
      end
