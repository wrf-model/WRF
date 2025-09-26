#
#       Top-level Makefile for 3DVAR

#       Macros, these should be generic for all machines

.IGNORE:

#
# General commands
#-----------------------------------------------------------------------------
MAKE = make -i
AR = ar ru
RM = rm -f
RM_CMD = $(RM) *.CKP *.ln *.BAK *.bak *.o *.i core errs ,* *~ *.a \
.emacs_* tags TAGS make.log MakeOut *.f *.mod CONDRV.SCR
#-----------------------------------------------------------------------------
DEVTOP = .


all:	make_rules
	($(RM_CMD))   ;
	(cd Dir_map   ;  $(MAKE));

little_f:make_rules
	Map.f

#
clean:	make_rules
	(cd Dir_map   					; \
	$(MAKE) clean)					; \
	$(RM_CMD)					; \
	$(RM) Map.out.*  Map.log gmeta* fort.* *.exe

make_rules:
	uname -a > .tmpfile
	@grep OSF .tmpfile 						; \
	if [ $$? = 0 ] ; then echo "Compiling for Compaq"         	; \
	cp configure.user.dec       configure.user                 	; \
	else grep AIX .tmpfile                                     	; \
	if [ $$? = 0 ] ; then echo "Compiling for IBM"			; \
	cp configure.user.ibm       configure.user			; \
	else grep Darwin .tmpfile                                     	; \
	if [ $$? = 0 ] ; then echo "Compiling for Mac"			; \
	cp configure.user.mac       configure.user			; \
        cp mac_Makefile1            Dir_map/Makefile                    ; \
        cp mac_Makefile2            Dir_map/Skewt/Makefile              ; \
	else grep IRIX64 .tmpfile                                     	; \
	if [ $$? = 0 ] ; then echo "Compiling for SGI"			; \
	cp configure.user.sgi       configure.user			; \
	else grep Linux .tmpfile					; \
	if [ $$? = 0 ] ; then echo "Compiling for PC Linux"		; \
	cp  configure.user.linux    configure.user			; \
	else echo "Not Compaq, ibm, and PC-Linux, please edit configure.user" ; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi 
	       








