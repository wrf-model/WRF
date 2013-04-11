.SUFFIXES: .c .f .F .o

OBJ =  closbf.o   digit.o     icopysb.o   ireadns.o  makestab.o  nenuaa.o  ovrbs1.o    rbytes.o    readlc.o  strnum.o   ufbmns.o  upc.o \
       closmg.o   drfini.o    idn30.o     ireadsb.o  maxout.o    nenubd.o  pad.o       rcstpl.o    readmg.o  strsuc.o   ufbovr.o  upds3.o \
       addate.o   cmpmsg.o   drstpl.o    ifbget.o    irev.o     mesgbc.o    nevn.o    padmsg.o    rdbfdx.o    readmm.o  subupd.o   ufbpos.o  upftbv.o \
       adn30.o    cmsgini.o  dumpbf.o    ifxy.o      istdesc.o  mesgbf.o    newwin.o  parseq.o    rdcmps.o    readns.o  tabent.o   ufbqcd.o  uptdd.o \
       bfrini.o   cnved4.o   dxdump.o    igetdate.o  iupb.o     minimg.o    nmbyt.o   parstr.o    rdmemm.o    readsb.o  tabsub.o   ufbqcp.o  usrtpl.o \
       bort.o     cobfl.o    dxinit.o    igetfxy.o   iupbs01.o  mova2i.o    nmsub.o   parusr.o    rdmems.o    restd.o   trybump.o  ufbrep.o  valx.o \
       bort2.o    conwin.o   dxmini.o    igetntbl.o  iupbs1.o   mrginv.o    nmwrd.o   parutg.o    rdmgsb.o    rewnbf.o  ufbcnt.o   ufbrms.o  wrcmps.o \
       bort_exit.o     copybf.o   elemdx.o    inctab.o    iupm.o     msgini.o    numbck.o  pkb.o       rdmsgb.o    rjust.o   ufbcpy.o   ufbrp.o   wrdesc.o \
       copymg.o   getabdb.o   invcon.o   iupvs01.o   msgupd.o   numtab.o    pkbs1.o     rdmsgw.o    rsvfvm.o  ufbcup.o   ufbrw.o   wrdlen.o \
       copysb.o   getlens.o   invmrg.o   iupvs1.o    msgwrt.o   numtbd.o    pkc.o       rdmtbb.o    seqsdx.o  ufbdmp.o   ufbseq.o  writcp.o \
       cadn30.o   cpbfdx.o   getntbe.o   invtag.o    jstchr.o   mvb.o       nvnwin.o  pkftbv.o    rdmtbd.o    sntbbe.o  ufbevn.o   ufbsp.o   writdx.o \
       capit.o    cpymem.o   gets1loc.o  invwin.o    jstnum.o   nemock.o    nwords.o  pktdd.o     rdtree.o    sntbde.o  ufbget.o   ufbstp.o  writlc.o \
       ccbfl.o    cpyupd.o   gettbh.o    ipkm.o      ljust.o    nemtab.o    nxtwin.o  pkvs01.o    rdusdx.o    status.o  ufbin3.o   ufbtab.o  writsa.o \
       chekstab.o crbmg.o    getwin.o    ireadft.o   lmsg.o     nemtba.o    openbf.o  pkvs1.o     readdx.o    stdmsg.o  ufbint.o   ufbtam.o  writsb.o \
       chrtrn.o   cwbmg.o    i4dy.o      ireadibm.o  lstjpb.o   nemtbax.o   openbt.o  posapn.o    readerme.o  stndrd.o  ufbinx.o   ufdump.o  wrtree.o \
       chrtrna.o  datebf.o   ibfms.o     ireadmg.o   lstrpc.o   nemtbb.o    openmb.o  posapx.o    readft.o    strcln.o  ufbmem.o   upb.o     wtstat.o \
       cktaba.o   datelen.o  ichkstr.o   ireadmm.o   lstrps.o   nemtbd.o    openmg.o  readibm.o   string.o  ufbmms.o   upbb.o

AR = ar
ARFLAGS = cr
TARGET = libbufr.a

library:$(OBJ)
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)
	$(RANLIB) $(TARGET)

.c.o:
	$(RM) $@
	$(CC) -o $@ -c $(CFLAGS) $*.c

.f.o:
	$(FC) -c $(FFLAGS) $< 

.F.o:
	./preproc.sh $*.F
	$(FC) -c $(FFLAGS) $*.f

clean:
	rm -f $(OBJ) $(TARGET) irev.f wrdlen.f

superclean: clean
