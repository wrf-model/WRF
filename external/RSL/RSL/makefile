#########################################################################
#     
#                              COPYRIGHT
#     
#     The following is a notice of limited availability of the code and 
#     Government license and disclaimer which must be included in the 
#     prologue of the code and in all source listings of the code.
#     
#     Copyright notice
#       (c) 1977  University of Chicago
#     
#     Permission is hereby granted to use, reproduce, prepare 
#     derivative works, and to redistribute to others at no charge.  If 
#     you distribute a copy or copies of the Software, or you modify a 
#     copy or copies of the Software or any portion of it, thus forming 
#     a work based on the Software and make and/or distribute copies of 
#     such work, you must meet the following conditions:
#     
#          a) If you make a copy of the Software (modified or verbatim) 
#             it must include the copyright notice and Government       
#             license and disclaimer.
#     
#          b) You must cause the modified Software to carry prominent   
#             notices stating that you changed specified portions of    
#             the Software.
#     
#     This software was authored by:
#     
#     Argonne National Laboratory
#     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
#     Mathematics and Computer Science Division
#     Argonne National Laboratory, Argonne, IL  60439
#     
#     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
#     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
#     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
#     CONTRACT WITH THE DEPARTMENT OF ENERGY.
#     
#                      GOVERNMENT LICENSE AND DISCLAIMER
#     
#     This computer code material was prepared, in part, as an account 
#     of work sponsored by an agency of the United States Government.
#     The Government is granted for itself and others acting on its 
#     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
#     this data to reproduce, prepare derivative works, distribute 
#     copies to the public, perform publicly and display publicly, and 
#     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
#     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
#     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
#     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
#     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
#     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
#     NOT INFRINGE PRIVATELY OWNED RIGHTS.
#
#########################################################################

# Top level makefile for RSL.
#
#

###################################################
# User settable compile time constants.
###################################################

# largest number of domains
MAX_DOMAINS  = 6

# largest number of processors
MAX_PROC     = 256

###################################################
# Install time constants
###################################################

# 0 is hostless (monitor is a compute node);
# 1 (monitor is an extra node)
HOST_NODE = 0
#HOST_NODE = 1

# 1, monitor is the low node; 0, monitor is high node
# (set this for all hosts)
MON_LOW = 1
#MON_LOW = 0

###################################################
# Generally not modified
###################################################

CONFIG_OPTS = -DIMAX_MAKE=$(IMAX) -DJMAX_MAKE=$(JMAX) \
	      -DMAXDOM_MAKE=$(MAX_DOMAINS) \
	      -DMAXPROC_MAKE=$(MAX_PROC) \
	      -DHOST_NODE=$(HOST_NODE) -DMON_LOW=$(MON_LOW) \
	      -DALLOW_RSL_168PT=1 $(LEARN_BCAST)

MAKE_OPTS=SED_LINE="-e s/IMAX_MAKE/$(IMAX)/ \
		     -e s/JMAX_MAKE/$(JMAX)/ \
		     -e s/MAXDOM_MAKE/$(MAX_DOMAINS)/ \
	             -e s/MAXPROC_MAKE/$(MAX_PROC)/" \
          CONFIG_OPTS="$(CONFIG_OPTS)"


include makefile.core
OBJ = $(CORE_OBJ)
HDR = $(CORE_HDR)
TAR = $(CORE_TAR)
MINITAR = $(CORE_MINITAR)

.SUFFIXES: .o .c
MAKE = make -r

help :
	@ echo "Must specify target (see makefile)."

mpl :
	@ echo "Do not compile for MPL.  Use MPI."

sp2 :
	@ if [ -f LAST_MADE ] ; then /bin/mv -f LAST_MADE LAST_MADE.bak ; fi
	@ echo Last made as: make sp2 > LAST_MADE
	@ make >> LAST_MADE
	$(MAKE) -f makefile.sp2 $(MAKE_OPTS) all

sp2ch :
	@ if [ -f LAST_MADE ] ; then /bin/mv -f LAST_MADE LAST_MADE.bak ; fi
	@ echo Last made as: make sp2ch > LAST_MADE
	@ make >> LAST_MADE
	$(MAKE) -f makefile.sp2ch $(MAKE_OPTS) all

sp2.1 :
	@ if [ -f LAST_MADE ] ; then /bin/mv -f LAST_MADE LAST_MADE.bak ; fi
	@ echo Last made as: make sp2.1 > LAST_MADE
	@ make >> LAST_MADE
	$(MAKE) -f makefile.sp2.1 $(MAKE_OPTS) all

t3e :
	@ if [ -f LAST_MADE ] ; then /bin/mv -f LAST_MADE LAST_MADE.bak ; fi
	@ echo Last made as: make t3e > LAST_MADE
	@ make >> LAST_MADE
	$(MAKE) -f makefile.t3e $(MAKE_OPTS) all

crayx1 :
	@ if [ -f LAST_MADE ] ; then /bin/mv -f LAST_MADE LAST_MADE.bak ; fi
	@ echo Last made as: make x1 > LAST_MADE
	@ make >> LAST_MADE
	$(MAKE) -f makefile.x1 $(MAKE_OPTS) all

o2k :
	$(MAKE) -f makefile.o2k $(MAKE_OPTS) all

hp :
	$(MAKE) -f makefile.hp $(MAKE_OPTS) all

alpha :
	$(MAKE) -f makefile.alpha $(MAKE_OPTS) all

alphavector :
	$(MAKE) -f makefile.alphavector $(MAKE_OPTS) all

vpp :
	$(MAKE) -f makefile.vpp $(MAKE_OPTS) all

sunmpi :
	$(MAKE) -f makefile.sunmpi $(MAKE_OPTS) all

altix :
	$(MAKE) -f makefile.altix $(MAKE_OPTS) all

linux :
	$(MAKE) -f makefile.linux LINUX_MPIHOME=$(LINUX_MPIHOME) $(MAKE_OPTS) all

sx :
	$(MAKE) -f makefile.sx $(MAKE_OPTS) all

stub :
	@ if [ -f LAST_MADE ] ; then /bin/mv -f LAST_MADE LAST_MADE.bak ; fi
	@ echo Last made as: make stub > LAST_MADE
	@ make >> LAST_MADE
	$(MAKE) -f makefile.stub $(MAKE_OPTS) all

showtar :
	@ echo these files will be included in the tar
	@ echo $(TAR) | tr '\040' '\012'

tar :
	tar cvf rsltar $(TAR)
	compress rsltar
	ls -l rsltar.Z

minitar :
	tar cvf rslminitar $(MINITAR)
	compress rslminitar
	ls -l rslminitar.Z

man :
	$(MAKE) -f Makefile.man
	cd docs ; make man

manclean :
	/bin/rm -fr man
	cd docs ; make manclean

superclean : 
	/bin/rm -f *.o librsl.a rsl.inc

###

$(OBJ) : $(HDR)
