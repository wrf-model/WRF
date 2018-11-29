/*
  Program Name:
  Author(s)/Contact(s):
  Abstract:
  History Log:
 
  Usage:
  Parameters: <Specify typical arguments passed>
  Input Files:
        <list file names and briefly describe the data they include>
  Output Files:
        <list file names and briefly describe the information they include>
 
  Condition codes:
        <list exit condition or error codes returned >
        If appropriate, descriptive troubleshooting instructions or
        likely causes for failures could be mentioned here with the
        appropriate error code
 
  User controllable options: <if applicable>
*/

#include <stdlib.h>
#include <stdio.h>
#include <jasper/jasper.h>

#if defined (CRAY)

#define info_jpeg2000 INFO_JPEG2000
#define decode_jpeg2000 DECODE_JPEG2000

#elif defined (__sgi) || defined (__sun) || defined (__alpha) || defined (__linux)

#define info_jpeg2000 info_jpeg2000_
#define decode_jpeg2000 decode_jpeg2000_

#elif defined (IBM) || defined (HP)

#define info_jpeg2000 info_jpeg2000
#define decode_jpeg2000 decode_jpeg2000

#endif


void info_jpeg2000(char *buffer, int *length, int *width, int *height, jas_image_t **image, jas_stream_t **instream) {
  int fmtid;
  char *opts=0;
  jas_image_cmpt_t *pcmpt;

  /* Initialize jasper stuff */
  jas_init();

  /* open the buffer */
  if ( ! (*instream = jas_stream_memopen(buffer, *length))) {
    fprintf(stderr, "cannot open input buffer\n");
    exit(1);
  }

  /* Get image format */
  if ((fmtid = jas_image_getfmt(*instream)) < 0) {
    fprintf(stderr, "decode_jpeg2000:  unknown image format:  fmtid = %i\n", fmtid);
    return;
    exit (1);
  }

  /* Decode the image. */

  if (!(*image = jpc_decode(*instream, opts))) {
    fprintf(stderr, "cannot load image\n");
    exit (1);
  }
  pcmpt=(*image)->cmpts_[0];
  *width = pcmpt->width_;
  *height = pcmpt->height_;
}


void decode_jpeg2000(jas_image_t **image, jas_stream_t **instream, int *width, int *height, int *buf) {
  /* Call info_jpeg2000_ before calling decode_jpeg2000_ */
  jas_matrix_t *data;
  jas_image_cmpt_t *pcmpt;
  int i, j, k, ierr;

  pcmpt=(*image)->cmpts_[0];
  data=jas_matrix_create(jas_image_height(*image), jas_image_width(*image));
  jas_image_readcmpt(*image,0,0,0,jas_image_width(*image), jas_image_height(*image), data);
  k=0;
  for (i=0;i<pcmpt->height_;i++) {
    for (j=0;j<pcmpt->width_;j++) {
      buf[k++]=data->rows_[i][j];
    }
  }
  jas_matrix_destroy(data);
  ierr=jas_stream_close(*instream);
  jas_image_destroy(*image);

}
