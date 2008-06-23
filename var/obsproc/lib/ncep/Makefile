#makefile to build NCEP bufr lib
#  2/1/06  JFB

# Compilation macros

include ../../compiler_macros

.SUFFIXES:	.f .c .o 

##########
COBJS   =	bort_exit.o		\
		ccbfl.o			\
		cobfl.o			\
		crbmg.o			\
		cwbmg.o			\
		rbytes.o		\
		restd.o			\
		wrdesc.o

OBJS	=	addate.o		\
		adn30.o			\
		bfrini.o		\
		bort.o			\
		bort2.o			\
		cadn30.o		\
		capit.o			\
		chekstab.o		\
		chrtrna.o		\
		chrtrn.o		\
		cktaba.o		\
		closbf.o		\
		closmg.o		\
		cmpmsg.o		\
		cmsgini.o		\
		cnved4.o		\
		conwin.o		\
		copybf.o		\
		copymg.o		\
		copysb.o		\
		cpbfdx.o		\
		cpymem.o		\
		cpyupd.o		\
		datebf.o		\
		datelen.o		\
		digit.o			\
		drfini.o		\
		drstpl.o		\
		dumpbf.o		\
		dxdump.o		\
		dxinit.o		\
		dxmini.o		\
		elemdx.o		\
		getabdb.o		\
		getlens.o		\
		gets1loc.o		\
		getwin.o		\
		i4dy.o			\
		ichkstr.o		\
		icopysb.o		\
		idn30.o			\
		ifbget.o		\
		ifxy.o			\
		igetdate.o		\
		inctab.o		\
		invcon.o		\
		invmrg.o		\
		invtag.o		\
		invwin.o		\
		ipkm.o			\
		ireadft.o		\
		ireadibm.o		\
		ireadmg.o		\
		ireadmm.o		\
		ireadns.o		\
		ireadsb.o		\
		irev.o			\
		istdesc.o		\
		iupb.o			\
		iupbs01.o		\
		iupbs1.o		\
		iupm.o			\
		iupvs01.o		\
		iupvs1.o		\
		jstchr.o		\
		jstnum.o		\
		ljust.o			\
		lmsg.o			\
		lstjpb.o		\
		lstrpc.o		\
		lstrps.o		\
		makestab.o		\
		maxout.o		\
		mesgbc.o		\
		mesgbf.o		\
		minimg.o		\
		mova2i.o		\
		mrginv.o		\
		msgini.o		\
		msgupd.o		\
		msgwrt.o		\
		mvb.o			\
		nemock.o		\
		nemtab.o		\
		nemtba.o		\
		nemtbax.o		\
		nemtbb.o		\
		nemtbd.o		\
		nenuaa.o		\
		nenubd.o		\
		nevn.o			\
		newwin.o		\
		nmbyt.o			\
		nmsub.o			\
		nmwrd.o			\
		numbck.o		\
		numtab.o		\
		numtbd.o		\
		nvnwin.o		\
		nwords.o		\
		nxtwin.o		\
		openbf.o		\
		openbt.o		\
		openmb.o		\
		openmg.o		\
		ovrbs1.o		\
		pad.o			\
		padmsg.o		\
		parseq.o		\
		parusr.o		\
		parutg.o		\
		pkb.o			\
		pkbs1.o			\
		pkc.o			\
		pkftbv.o		\
		pktdd.o			\
		pkvs01.o		\
		pkvs1.o			\
		posapn.o		\
		posapx.o		\
		rcstpl.o		\
		rdbfdx.o		\
		rdcmps.o		\
		rdmemm.o		\
		rdmems.o		\
		rdmgsb.o		\
		rdmsgb.o		\
		rdmsgw.o		\
		rdtree.o		\
		rdusdx.o		\
		readdx.o		\
		readerme.o		\
		readft.o		\
		readibm.o		\
		readlc.o		\
		readmg.o		\
		readmm.o		\
		readns.o		\
		readsb.o		\
		rewnbf.o		\
		rjust.o			\
		rsvfvm.o		\
		seqsdx.o		\
		status.o		\
		stdmsg.o		\
		stndrd.o		\
		strcln.o		\
		string.o		\
		strnum.o		\
		strsuc.o		\
		subupd.o		\
		tabent.o		\
		tabsub.o		\
		trybump.o		\
		ufbcnt.o		\
		ufbcpy.o		\
		ufbcup.o		\
		ufbdmp.o		\
		ufbevn.o		\
		ufbget.o		\
		ufbin3.o		\
		ufbint.o		\
		ufbinx.o		\
		ufbmem.o		\
		ufbmms.o		\
		ufbmns.o		\
		ufbovr.o		\
		ufbpos.o		\
		ufbqcd.o		\
		ufbqcp.o		\
		ufbrep.o		\
		ufbrms.o		\
		ufbrp.o			\
		ufbrw.o			\
		ufbseq.o		\
		ufbsp.o			\
		ufbstp.o		\
		ufbtab.o		\
		ufbtam.o		\
		ufdump.o		\
		upbb.o			\
		upb.o			\
		upc.o			\
		upds3.o			\
		uptdd.o			\
		usrtpl.o		\
		valx.o			\
		wrcmps.o		\
		wrdlen.o		\
		writca.o		\
		writcp.o		\
		writdx.o		\
		writlc.o		\
		writsa.o		\
		writsb.o		\
		wrtree.o		\
		wtstat.o	

################

lib_bufr:	$(OBJS) $(COBJS) 
		/bin/rm -f lib_bufr.a
		$(AR) lib_bufr.a $(OBJS) $(COBJS)
		$(RANLIB) lib_bufr.a

clean:
		$(RM) *.o *.a *.lst *.prm

bufrlib.prm:
	$(CPP) $(CPPFLAGS) bufrlib.PRM > bufrlib.prm

$(OBJS): bufrlib.prm
	$(FC) -c $(FFLAGS) $*.f

$(COBJS): bufrlib.h
	$(CC) -c $(CCFLAGS) $*.c

