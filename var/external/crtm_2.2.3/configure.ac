## Put version control id tag in output configure script
AC_REVISION($Id: $)


## Initialise autoconf
AC_INIT([CRTM], m4_esyscmd([tr -d "\n'" < libsrc/CRTM_Version.inc]), [ncep.list.emc.jcsda_crtm.support@noaa.gov], [crtm])


# Output info for user
AC_MSG_NOTICE([AC_PACKAGE_NAME AC_PACKAGE_VERSION])


# Check for existence of unique file before proceeding
AC_CONFIG_SRCDIR([libsrc/CRTM_Module.fpp])


# Define the configuration files
AC_CONFIG_FILES([Makefile libsrc/Makefile libsrc/test/Makefile])


# Check for programs
AC_PROG_FC(ifort gfortran xlf2003 pgf95 g95)
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_CHECK_TOOL([AR], ar)
# ...Set defaults for ARFLAGS since there is no autoconf macro
test -n "$ARFLAGS" || ARFLAGS="crvs"
AC_SUBST(ARFLAGS)


# Set the programming language
AC_LANG(Fortran)
AC_FC_FREEFORM
AC_FC_SRCEXT(f90)


# Check platform endian-ness
AC_MSG_CHECKING([if platform is little-endian])
if test 1 -eq `printf '\1' | od -dAn`; then
  AC_MSG_RESULT(yes)
  ENDIAN_PLATFORM="little"
else
  AC_MSG_RESULT(no)
  ENDIAN_PLATFORM="big"
fi


# Determine the I/O endian-type to use.
AC_ARG_ENABLE([big-endian],
  [AS_HELP_STRING([--disable-big-endian],[Native I/O on little-endian machines. @<:@default: big-endian@:>@])],
  [ENDIAN_IO=little],[ENDIAN_IO=big])

# ...All little-endian I/O requires the following for the check program
if test "${ENDIAN_IO}" = little; then
  case ${FC} in
    xlf2003) FPPFLAGS="-WF,-DLITTLE_ENDIAN $FPPFLAGS" ;;
    *)       FPPFLAGS="-DLITTLE_ENDIAN $FPPFLAGS" ;;
  esac
fi

# ...Big-endian I/O on little-endian platforms
if test "${ENDIAN_IO}" = big -a "${ENDIAN_PLATFORM}" = little; then
  case ${FC} in
    ifort)    FCFLAGS="-convert big_endian $FCFLAGS";;
    gfortran) FCFLAGS="-fconvert=big-endian $FCFLAGS";;
    pgf95)    FCFLAGS="-byteswapio $FCFLAGS";;
    g95)      FCFLAGS="-fendian=big $FCFLAGS";;
    *)        AC_MSG_WARN([Endian swap flags for $FC unknown and not set]);;
  esac
fi

# ...Little-endian I/O on big-endian platforms
if test "${ENDIAN_IO}" = little -a "${ENDIAN_PLATFORM}" = big; then
  AC_MSG_WARN([Little-endian I/O on big-endian platforms not supported via compiler switches])
fi

# ...Final build type
AC_MSG_NOTICE([Building ${ENDIAN_IO}-endian version])


# Section for special cases for supported compilers

# ...Special case the ifort compiler to force byte-unit record lengths
if test "${FC}" = ifort; then
  FCFLAGS="-assume byterecl $FCFLAGS"
fi

# ...Special case the xlf2003 compiler to:
#      a) recognise the .fpp (Fortran PreProcessor) suffix
#      b) not insert "#line" directives when it encounters multiple empty lines
if test "${FC}" = xlf2003; then
  FPPFLAGS="-qsuffix=cpp=fpp -WF,-P $FPPFLAGS"
fi


# Generate variable substitutions
AC_SUBST(ENDIAN_PLATFORM)
AC_SUBST(ENDIAN_IO)
AC_SUBST(FPPFLAGS)


# Create the configure script
AC_OUTPUT
