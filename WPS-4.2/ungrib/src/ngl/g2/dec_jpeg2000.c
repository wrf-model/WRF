#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef USE_JPEG2000
#include "jasper/jasper.h"
#define JAS_1_700_2
#endif /* USE_JPEG2000 */


#ifdef __64BIT__
  typedef int g2int;
#else
  typedef long g2int;
#endif

#if defined _UNDERSCORE
   #define dec_jpeg2000 dec_jpeg2000_
#elif defined _DOUBLEUNDERSCORE
   #define dec_jpeg2000 dec_jpeg2000__
#endif

   int dec_jpeg2000(char *injpc,g2int *bufsize,g2int *outfld)
/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
*                .      .    .                                       .
* SUBPROGRAM:    dec_jpeg2000      Decodes JPEG2000 code stream
*   PRGMMR: Gilbert          ORG: W/NP11     DATE: 2002-12-02
*
* ABSTRACT: This Function decodes a JPEG2000 code stream specified in the
*   JPEG2000 Part-1 standard (i.e., ISO/IEC 15444-1) using JasPer 
*   Software version 1.500.4 (or 1.700.2) written by the University of British
*   Columbia and Image Power Inc, and others.
*   JasPer is available at http://www.ece.uvic.ca/~mdadams/jasper/.
*
* PROGRAM HISTORY LOG:
* 2002-12-02  Gilbert
*
* USAGE:     int dec_jpeg2000(char *injpc,g2int *bufsize,g2int *outfld)
*
*   INPUT ARGUMENTS:
*      injpc - Input JPEG2000 code stream.
*    bufsize - Length (in bytes) of the input JPEG2000 code stream.
*
*   INPUT ARGUMENTS:
*     outfld - Output matrix of grayscale image values.
*
*   RETURN VALUES :
*          0 = Successful decode
*         -3 = Error decode jpeg2000 code stream.
*         -5 = decoded image had multiple color components.
*              Only grayscale is expected.
*
* REMARKS:
*
*      Requires JasPer Software version 1.500.4 or 1.700.2
*
* ATTRIBUTES:
*   LANGUAGE: C
*   MACHINE:  IBM SP
*
*$$$*/

{
#ifdef USE_JPEG2000
    int ier;
    g2int i,j,k,n;
    jas_image_t *image=0;
    jas_stream_t *jpcstream,*istream;
    jas_image_cmpt_t cmpt,*pcmpt;
    char *opts=0;
    jas_matrix_t *data;

/*    jas_init(); */

/*   
 *     Create jas_stream_t containing input JPEG200 codestream in memory.
 */      

    jpcstream=jas_stream_memopen(injpc,*bufsize);

/*   
 *     Decode JPEG200 codestream into jas_image_t structure.
 */      
    image=jpc_decode(jpcstream,opts);
    if ( image == 0 ) {
       printf(" jpc_decode return = %d \n",ier);
       return -3;
    }
    
    pcmpt=image->cmpts_[0];
/*
    printf(" SAGOUT DECODE:\n");
    printf(" tlx %d \n",image->tlx_);
    printf(" tly %d \n",image->tly_);
    printf(" brx %d \n",image->brx_);
    printf(" bry %d \n",image->bry_);
    printf(" numcmpts %d \n",image->numcmpts_);
    printf(" maxcmpts %d \n",image->maxcmpts_);
#ifdef JAS_1_500_4
    printf(" colormodel %d \n",image->colormodel_);
#endif
#ifdef JAS_1_700_2
    printf(" colorspace %d \n",image->clrspc_);
#endif
    printf(" inmem %d \n",image->inmem_);
    printf(" COMPONENT:\n");
    printf(" tlx %d \n",pcmpt->tlx_);
    printf(" tly %d \n",pcmpt->tly_);
    printf(" hstep %d \n",pcmpt->hstep_);
    printf(" vstep %d \n",pcmpt->vstep_);
    printf(" width %d \n",pcmpt->width_);
    printf(" height %d \n",pcmpt->height_);
    printf(" prec %d \n",pcmpt->prec_);
    printf(" sgnd %d \n",pcmpt->sgnd_);
    printf(" cps %d \n",pcmpt->cps_);
#ifdef JAS_1_700_2
    printf(" type %d \n",pcmpt->type_);
#endif
*/

/*   Expecting jpeg2000 image to be grayscale only.
 *   No color components.
 */
    if (image->numcmpts_ != 1 ) {
       printf("dec_jpeg2000: Found color image.  Grayscale expected.\n");
       return (-5);
    }

/* 
 *    Create a data matrix of grayscale image values decoded from
 *    the jpeg2000 codestream.
 */
    data=jas_matrix_create(jas_image_height(image), jas_image_width(image));
    jas_image_readcmpt(image,0,0,0,jas_image_width(image),
                       jas_image_height(image),data);
/*
 *    Copy data matrix to output integer array.
 */
    k=0;
    for (i=0;i<pcmpt->height_;i++) 
      for (j=0;j<pcmpt->width_;j++) 
        outfld[k++]=data->rows_[i][j];
/*
 *     Clean up JasPer work structures.
 */
    jas_matrix_destroy(data);
    ier=jas_stream_close(jpcstream);
    jas_image_destroy(image);

#endif /* USE_JPEG2000 */
    return 0;

}
