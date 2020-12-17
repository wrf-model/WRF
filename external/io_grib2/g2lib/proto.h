#ifndef CRAY
# ifdef NOUNDERSCORE
#      define MOVA2I mova2i
#      define DEC_JPEG2000 dec_jpeg2000
#      define ENC_JPEG2000 enc_jpeg2000
#      define ENC_PNG enc_png
#      define DEC_PNG dec_png
# else
#   ifdef F2CSTYLE
#      define MOVA2I mova2i__
#      define DEC_JPEG2000 dec_jpeg2000__
#      define ENC_JPEG2000 enc_jpeg2000__
#      define ENC_PNG enc_png__
#      define DEC_PNG dec_png__
#   else
#      define MOVA2I mova2i_
#      define DEC_JPEG2000 dec_jpeg2000_
#      define ENC_JPEG2000 enc_jpeg2000_
#      define ENC_PNG enc_png_
#      define DEC_PNG dec_png_
#   endif
# endif
#endif

