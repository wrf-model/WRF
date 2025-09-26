#This is the embryo Makefile for the JNI interface to MeteoIO.
#When the build system will be unified (ie: when using cmake for ALL functionnality)
#this Makefile should be integrated in the global build system.

CXX		= g++ -D_METEOIO_JNI
LINKER		= g++ -DGNU

DEBUG		= -g -O0 -D__DEBUG #-D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC #-dr
WARNING		=  -Wall  -Wextra #-Weffc++ #-pedantic-errors #-Werror
ARCH		=  -march=native
OPTIM		=  -fomit-frame-pointer -O3
MAKE_OPTIM	=  -combine -pipe

ifeq ($(MODE),debug)		#for debug
	CFLAGS   = $(MAKE_OPTIM) $(ARCH) $(DEBUG) $(WARNING)
	CCFLAGS  = $(MAKE_OPTIM) $(ARCH) $(DEBUG) $(WARNING)
	FFLAGS	 = $(MAKE_OPTIM) $(DEBUG) $(WARNING)
else				#for release
	CFLAGS   = $(MAKE_OPTIM) $(ARCH) $(OPTIM) $(WARNING)
	CCFLAGS  = $(MAKE_OPTIM) $(ARCH) $(OPTIM) $(WARNING)
	FFLAGS	 = $(MAKE_OPTIM) $(FFOPTIM) $(WARNING)
endif

#PATHS
METEOIO_ROOT	= ../..
LIBDIR		= $(METEOIO_ROOT)/lib
SRCDIR		= $(METEOIO_ROOT)
JAVAHOME	= ${JAVA_HOME}
JNI_INCLUDE	= -I$(JAVAHOME)/include -I$(JAVAHOME)/include/win32 -I$(JAVAHOME)/include/linux

LIBS		= -lmeteoio
LDFLAGS		= -L$(LIBDIR) $(LIBS) -shared -Xlinker --add-stdcall-alias
INCLUDE		= -I$(SRCDIR) $(JNI_INCLUDE)

#OBJECTS
METEOIO_JNI_OBJ = 	ch_slf_gin_jnative_MeteoIOJNIInterface.o \
			jnative.o \
			DEMLoader.o

#generic rules
.cc.o: $*.cc $*.h
	$(CXX) $(CCFLAGS) -fPIC -c $< $(INCLUDE) -o $@


#TARGETS
all: $(METEOIO_JNI_OBJ)

link:
	#g++ -DGNU -rdynamic -lstdc++ -ldl *.o $(LIBDIR)/libmeteoio.so  -oliblibmeteoio.so
	g++ -DGNU -rdynamic -shared -lstdc++ -ldl *.o -L$(LIBDIR) -lmeteoio -L$(LIBDIR)/plugins -larcio -lgrassio -olibmeteoio_jni.so

clean:
	-rm *.o
