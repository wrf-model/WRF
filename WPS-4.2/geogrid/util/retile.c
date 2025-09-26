#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <math.h>
#include <string.h>

#define MSG_FLAG 0x80000001

#define N_BYTES 1
#define IN_TILE_DEGREES_LON 180.0
#define IN_TILE_DEGREES_LAT 180.0
#define IN_TILE_PTS_X 1250
#define IN_TILE_PTS_Y 1250
#define OUT_TILE_DEGREES_LON 180.0
#define OUT_TILE_DEGREES_LAT 180.0
#define OUT_TILE_PTS_X 1250
#define OUT_TILE_PTS_Y 1250
#define HALO_WIDTH 3
#define ZDIM 12

#define CACHESIZE 12

int **** data_cache = NULL;
char ** fname_cache = NULL;
int * lru = NULL;

int *** supertile = NULL;
int supertile_min_x = 999999;
int supertile_min_y = 999999;

void free_newtile(int *** x)
{
  int i, j, k;

  for(i=0; i<IN_TILE_PTS_X; i++)
  {
    for(j=0; j<IN_TILE_PTS_Y; j++)
    {
      free(x[i][j]);
    }
    free(x[i]);
  }
  free(x);
}

int *** read_tile(char * fname)
{
  int *** retval;
  int i, j, k, b;
  unsigned char buf[IN_TILE_PTS_X*IN_TILE_PTS_Y*ZDIM*N_BYTES];
  int fd;

  retval = (int ***)malloc(sizeof(int **)*IN_TILE_PTS_X); 
  for(i=0; i<IN_TILE_PTS_X; i++)
  {
    retval[i] = (int **)malloc(sizeof(int *)*IN_TILE_PTS_Y); 
    for(j=0; j<IN_TILE_PTS_Y; j++)
    {
      retval[i][j] = (int *)malloc(sizeof(int)*ZDIM); 
    }
  }

  if ((fd = open(fname,O_RDONLY)) == -1) 
  {
    fprintf(stderr,"Error opening source file %s\n", fname);
    return 0;
  }

  read(fd, (void *)&buf, IN_TILE_PTS_X*IN_TILE_PTS_Y*ZDIM*N_BYTES);

  close(fd);

  /* Put buf into retval */ 
  for(i=0; i<IN_TILE_PTS_X; i++)
  {
    for(j=0; j<IN_TILE_PTS_Y; j++)
    {
      for(k=0; k<ZDIM; k++)
      {
        retval[i][j][k] = 0;
        for(b=0; b<N_BYTES; b++)
        {
          retval[i][j][k] |= buf[k*N_BYTES*IN_TILE_PTS_X*IN_TILE_PTS_Y+j*N_BYTES*IN_TILE_PTS_X+i*N_BYTES+b] << 8*(N_BYTES-b-1);
        }
      }
    }
  }

  return retval;
}

int *** get_tile_from_cache(int i, int j)
{
  int ii, jj, kk, k, least, least_idx;
  int *** retval, *** localptr;
  char * fname;

  fname = (char *)malloc(256);

  i = (i/IN_TILE_PTS_X)*IN_TILE_PTS_X+1;
  j = (j/IN_TILE_PTS_Y)*IN_TILE_PTS_Y+1;

  snprintf(fname,256,"%5.5i-%5.5i.%5.5i-%5.5i",i,i+IN_TILE_PTS_X-1,j,j+IN_TILE_PTS_Y-1);

  /* Find out whether tile containing (i,j) is in cache */
  if (data_cache != NULL) 
  {
    for(k=0; k<CACHESIZE; k++)
    {
      if (fname_cache[k] != NULL)
      {
        if (strncmp(fname_cache[k],fname,256) == 0) 
        {
          free(fname);
          retval = data_cache[k];
          return retval;
        }
      }
    }
  }

  /* If not, read from file */
  localptr = read_tile(fname);

  /* Also store tile in the cache */
  if (data_cache == NULL)
  {
    data_cache = (int ****)malloc(sizeof(int ***)*CACHESIZE);
    fname_cache = (char **)malloc(sizeof(char *)*CACHESIZE);
    lru = (int *)malloc(sizeof(int)*CACHESIZE);
    for(k=0; k<CACHESIZE; k++)
    {
      data_cache[k] = NULL;
      fname_cache[k] = NULL;
      lru[k] = 0;
    }
  }

  least = 0;
  least_idx = 0;
  for(k=0; k<CACHESIZE; k++)
  {
    lru[k]++; 
    if (lru[k] > least)
    {
      least = lru[k];
      least_idx = k;
    }
  }

  if (data_cache[least_idx] == NULL)
  {
    data_cache[least_idx] = localptr;
    fname_cache[least_idx] = fname; 
    lru[least_idx] = 0;
  }
  else
  {
    free_newtile(data_cache[least_idx]);
    data_cache[least_idx] = localptr;
    free(fname_cache[least_idx]);
    fname_cache[least_idx] = fname; 
    lru[least_idx] = 0;
  }

  retval = localptr;
  return retval;
}

void build_supertile(int i, int j)
{
  int ii, jj, kk;
  int doflip;
  int *** newtile;

  if (i < 0)
    i = i + IN_TILE_PTS_X*(int)(360./IN_TILE_DEGREES_LON);
  if (i >= IN_TILE_PTS_X*(int)(360./IN_TILE_DEGREES_LON))
    i = i - IN_TILE_PTS_X*(int)(360./IN_TILE_DEGREES_LON);
  if (j < 0)
    j = j + IN_TILE_PTS_Y*(int)(180./IN_TILE_DEGREES_LAT);
  if (j >= IN_TILE_PTS_Y*(int)(180./IN_TILE_DEGREES_LAT))
    j = j - IN_TILE_PTS_Y*(int)(180./IN_TILE_DEGREES_LAT);

  if (supertile == NULL)
  {
    supertile = (int ***)malloc(sizeof(int **)*3*IN_TILE_PTS_X); 
    for(ii=0; ii<3*IN_TILE_PTS_X; ii++)
    {
      supertile[ii] = (int **)malloc(sizeof(int *)*3*IN_TILE_PTS_Y); 
      for(jj=0; jj<3*IN_TILE_PTS_Y; jj++)
      {
        supertile[ii][jj] = (int *)malloc(sizeof(int)*ZDIM); 
      }
    }
  }

  supertile_min_x = (i / IN_TILE_PTS_X)*IN_TILE_PTS_X;
  supertile_min_y = (j / IN_TILE_PTS_Y)*IN_TILE_PTS_Y;

  /* Get tile containing (i,j) from cache*/
  /* Get surrounding tiles from cache */ 
  
  /* Lower-left */
  ii = i - IN_TILE_PTS_X;
  if (ii < 0) ii += (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  doflip = 0;
  jj = j - IN_TILE_PTS_Y;
  if (jj < 0)
  {
    doflip = 1;
    jj = j;
    ii -= (int)(180./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
    if (ii < 0) ii += (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  }
  newtile = get_tile_from_cache(ii, jj); 
  if (doflip) 
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_Y; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii][jj][kk] = newtile[ii][IN_TILE_PTS_Y-1-jj][kk];
      }
    }
  }
  else
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_Y; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii][jj][kk] = newtile[ii][jj][kk];
      }
    }
  }

  /* Left */
  ii = i - IN_TILE_PTS_X;
  if (ii < 0) ii += (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  jj = j;
  newtile = get_tile_from_cache(ii, jj); 
  for(ii=0; ii<IN_TILE_PTS_X; ii++)
  {
    for(jj=0; jj<IN_TILE_PTS_Y; jj++)
    {
      for(kk=0; kk<ZDIM; kk++)
        supertile[ii][jj+IN_TILE_PTS_Y][kk] = newtile[ii][jj][kk];
    }
  }

  /* Upper-left */
  ii = i - IN_TILE_PTS_X;
  if (ii < 0) ii += (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  doflip = 0;
  jj = j + IN_TILE_PTS_Y;
  if (jj >= (int)(180./IN_TILE_DEGREES_LAT)*IN_TILE_PTS_Y)
  {
    doflip = 1;
    jj = j;
    ii -= (int)(180./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
    if (ii < 0) ii += (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  }
  newtile = get_tile_from_cache(ii, jj); 
  if (doflip)
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_X; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii][jj+2*IN_TILE_PTS_Y][kk] = newtile[ii][IN_TILE_PTS_Y-1-jj][kk];
      }
    }
  }
  else
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_Y; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii][jj+2*IN_TILE_PTS_Y][kk] = newtile[ii][jj][kk];
      }
    }
  }

  /* Below */
  ii = i;
  doflip = 0;
  jj = j - IN_TILE_PTS_Y;
  if (jj < 0)
  {
    doflip = 1;
    jj = j;
    ii -= (int)(180./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
    if (ii < 0) ii += (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  }
  newtile = get_tile_from_cache(ii, jj); 
  if (doflip)
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_Y; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii+IN_TILE_PTS_X][jj][kk] = newtile[ii][IN_TILE_PTS_Y-1-jj][kk];
      }
    }
  }
  else
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_Y; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii+IN_TILE_PTS_X][jj][kk] = newtile[ii][jj][kk];
      }
    }
  }

  /* Center */
  newtile = get_tile_from_cache(i, j); 
  for(ii=0; ii<IN_TILE_PTS_X; ii++)
  {
    for(jj=0; jj<IN_TILE_PTS_Y; jj++)
    {
      for(kk=0; kk<ZDIM; kk++)
        supertile[ii+IN_TILE_PTS_X][jj+IN_TILE_PTS_Y][kk] = newtile[ii][jj][kk];
    }
  }

  /* Above */
  ii = i;
  doflip = 0;
  jj = j + IN_TILE_PTS_Y;
  if (jj >= (int)(180./IN_TILE_DEGREES_LAT)*IN_TILE_PTS_Y)
  {
    doflip = 1;
    jj = j;
    ii -= (int)(180./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
    if (ii < 0) ii += (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  }
  newtile = get_tile_from_cache(ii, jj); 
  if (doflip) 
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_Y; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii+IN_TILE_PTS_X][jj+2*IN_TILE_PTS_Y][kk] = newtile[ii][IN_TILE_PTS_Y-1-jj][kk];
      }
    }
  }
  else
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_Y; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii+IN_TILE_PTS_X][jj+2*IN_TILE_PTS_Y][kk] = newtile[ii][jj][kk];
      }
    }
  }

  /* Lower-right */
  ii = i + IN_TILE_PTS_X;
  if (ii >= (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X) ii -= (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  doflip = 0;
  jj = j - IN_TILE_PTS_Y;
  if (jj < 0)
  {
    doflip = 1;
    jj = j;
    ii -= (int)(180./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
    if (ii < 0) ii += (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  }
  newtile = get_tile_from_cache(ii, jj); 
  if (doflip) 
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_Y; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii+2*IN_TILE_PTS_X][jj][kk] = newtile[ii][IN_TILE_PTS_Y-1-jj][kk];
      }
    }
  }
  else
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_Y; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii+2*IN_TILE_PTS_X][jj][kk] = newtile[ii][jj][kk];
      }
    }
  }

  /* Right */
  ii = i + IN_TILE_PTS_X;
  if (ii >= (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X) ii -= (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  jj = j;
  newtile = get_tile_from_cache(ii, jj); 
  for(ii=0; ii<IN_TILE_PTS_X; ii++)
  {
    for(jj=0; jj<IN_TILE_PTS_Y; jj++)
    {
      for(kk=0; kk<ZDIM; kk++)
        supertile[ii+2*IN_TILE_PTS_X][jj+IN_TILE_PTS_Y][kk] = newtile[ii][jj][kk];
    }
  }

  /* Upper-right */
  ii = i + IN_TILE_PTS_X;
  if (ii >= (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X) ii -= (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  doflip = 0;
  jj = j + IN_TILE_PTS_Y;
  if (jj >= (int)(180./IN_TILE_DEGREES_LAT)*IN_TILE_PTS_Y)
  {
    doflip = 1;
    jj = j;
    ii -= (int)(180./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
    if (ii < 0) ii += (int)(360./IN_TILE_DEGREES_LON)*IN_TILE_PTS_X;
  }
  newtile = get_tile_from_cache(ii, jj); 
  if (doflip)
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_Y; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii+2*IN_TILE_PTS_X][jj+2*IN_TILE_PTS_Y][kk] = newtile[ii][IN_TILE_PTS_Y-1-jj][kk];
      }
    }
  }
  else
  {
    for(ii=0; ii<IN_TILE_PTS_X; ii++)
    {
      for(jj=0; jj<IN_TILE_PTS_Y; jj++)
      {
        for(kk=0; kk<ZDIM; kk++)
          supertile[ii+2*IN_TILE_PTS_X][jj+2*IN_TILE_PTS_Y][kk] = newtile[ii][jj][kk];
      }
    }
  }
}

int get_value(int i, int j, int k, int irad)
{
  int i_src, j_src;
  int ii, jj;
  int n, sum;
  float r_rel;

  if (i < 0)
    i = i + IN_TILE_PTS_X*(int)(360./IN_TILE_DEGREES_LON);
  if (i >= IN_TILE_PTS_X*(int)(360./IN_TILE_DEGREES_LON))
    i = i - IN_TILE_PTS_X*(int)(360./IN_TILE_DEGREES_LON);
  if (j < 0)
    j = j + IN_TILE_PTS_Y*(int)(180./IN_TILE_DEGREES_LAT);
  if (j >= IN_TILE_PTS_Y*(int)(180./IN_TILE_DEGREES_LAT))
    j = j - IN_TILE_PTS_Y*(int)(180./IN_TILE_DEGREES_LAT);

  i_src = i % IN_TILE_PTS_X + IN_TILE_PTS_X;
  j_src = j % IN_TILE_PTS_Y + IN_TILE_PTS_Y;

  /* Interpolate values from supertile */
  sum = 0;
  n = 0;
  for(ii=i_src; ii<=i_src+irad; ii++)
  {
    for(jj=j_src; jj<=j_src+irad; jj++)
    {
      sum += supertile[ii][jj][k]; 
      n++;
    }
  }

  if (n > 0) return sum/n;
  else return 0;
}

int is_in_supertile(int i, int j)
{
  if (i < 0)
    i = i + IN_TILE_PTS_X*(int)(360./IN_TILE_DEGREES_LON);
  if (i >= IN_TILE_PTS_X*(int)(360./IN_TILE_DEGREES_LON))
    i = i - IN_TILE_PTS_X*(int)(360./IN_TILE_DEGREES_LON);
  if (j < 0)
    j = j + IN_TILE_PTS_Y*(int)(180./IN_TILE_DEGREES_LAT);
  if (j >= IN_TILE_PTS_Y*(int)(180./IN_TILE_DEGREES_LAT))
    j = j - IN_TILE_PTS_Y*(int)(180./IN_TILE_DEGREES_LAT);

  /* Check whether (i,j) is in the interior of supertile */
  if ((i >= supertile_min_x) && (i < supertile_min_x+IN_TILE_PTS_X) &&
      (j >= supertile_min_y) && (j < supertile_min_y+IN_TILE_PTS_Y))
    return 1;
  else
    return 0;
}

int main(int argc, char ** argv)
{
  int tile_x, tile_y, input_x, input_y, temp_x, temp_y;
  int i, j, k, z, ii, jj;
  int i_src, j_src;
  int *** intdata;
  int out_fd;
  int ir_rel;
  float r_rel;
  unsigned char * outdata;
  char out_filename[256];

  r_rel = (float)IN_TILE_PTS_X/(float)OUT_TILE_PTS_X*OUT_TILE_DEGREES_LON/IN_TILE_DEGREES_LON;
  ir_rel = (int)rint(r_rel);

  /* Allocate memory to hold a single output tile */
  intdata = (int ***)malloc(sizeof(int **)*(OUT_TILE_PTS_X+2*HALO_WIDTH)); 
  for(i=0; i<(OUT_TILE_PTS_X+2*HALO_WIDTH); i++)
  {
    intdata[i] = (int **)malloc(sizeof(int *)*(OUT_TILE_PTS_Y+2*HALO_WIDTH)); 
    for(j=0; j<(OUT_TILE_PTS_Y+2*HALO_WIDTH); j++)
    {
      intdata[i][j] = (int *)malloc(sizeof(int)*ZDIM); 
    }
  }

  /* Allocate output buffer */
  outdata = (unsigned char *)malloc((OUT_TILE_PTS_X+2*HALO_WIDTH)*(OUT_TILE_PTS_Y+2*HALO_WIDTH)*ZDIM*N_BYTES);

  for(tile_x=0; tile_x<OUT_TILE_PTS_X*(int)(360.0/OUT_TILE_DEGREES_LON); tile_x+=OUT_TILE_PTS_X)
  {
    for(tile_y=0; tile_y<OUT_TILE_PTS_Y*(int)(180.0/OUT_TILE_DEGREES_LAT); tile_y+=OUT_TILE_PTS_Y)
    {
      /* Build name of output file for current tile_x and tile_y */
      sprintf(out_filename,"retiled/%5.5i-%5.5i.%5.5i-%5.5i",tile_x+1,tile_x+OUT_TILE_PTS_X,tile_y+1,tile_y+OUT_TILE_PTS_Y);

      /* Initialize the output data for current tile */
      for(i=0; i<(OUT_TILE_PTS_X+2*HALO_WIDTH); i++) 
      {
        for(j=0; j<(OUT_TILE_PTS_Y+2*HALO_WIDTH); j++)
        {
            intdata[i][j][0] = MSG_FLAG;
        }
      }

      /* Attempt to open output file */
      if ((out_fd = open(out_filename, O_WRONLY|O_CREAT|O_TRUNC, S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH)) == -1)
      {
        fprintf(stderr, "Error: Could not create or open file %s\n", out_filename);
        return 1;
      }

      /* Fill tile with data */
      for(i=-1*HALO_WIDTH; i<OUT_TILE_PTS_X+HALO_WIDTH; i++) 
      {
        for(j=-1*HALO_WIDTH; j<OUT_TILE_PTS_Y+HALO_WIDTH; j++)
        {
          if (intdata[i+HALO_WIDTH][j+HALO_WIDTH][0] == MSG_FLAG)
          {
            i_src = ir_rel*(tile_x+i); 
            j_src = ir_rel*(tile_y+j); 

            build_supertile(i_src,j_src); 

            for(ii=-1*HALO_WIDTH; ii<OUT_TILE_PTS_X+HALO_WIDTH; ii++) 
            {
              for(jj=-1*HALO_WIDTH; jj<OUT_TILE_PTS_Y+HALO_WIDTH; jj++)
              {
                i_src = ir_rel*(tile_x+ii); 
                j_src = ir_rel*(tile_y+jj); 
                if (is_in_supertile(i_src,j_src))
                {
                  for(k=0; k<ZDIM; k++)
                    intdata[ii+HALO_WIDTH][jj+HALO_WIDTH][k] = get_value(i_src,j_src,k,ir_rel-1); 
                } 
              }
            }

          }
        }
      }

      /* Write out the data */
      for(i=0; i<(OUT_TILE_PTS_X+2*HALO_WIDTH); i++) 
      {
        for(j=0; j<(OUT_TILE_PTS_Y+2*HALO_WIDTH); j++)
        {
          for(z=0; z<ZDIM; z++)
          {
            for(k=0; k<N_BYTES; k++)
            {
              outdata[z*N_BYTES*(OUT_TILE_PTS_Y+2*HALO_WIDTH)*(OUT_TILE_PTS_X+2*HALO_WIDTH)+j*N_BYTES*(OUT_TILE_PTS_X+2*HALO_WIDTH)+i*N_BYTES+k] = 
                     (intdata[i][j][z] & (0xff << 8*(N_BYTES-k-1))) >> (8*(N_BYTES-k-1));
            }
          }
        }
      }
      write(out_fd,(void *)outdata,(OUT_TILE_PTS_X+2*HALO_WIDTH)*(OUT_TILE_PTS_Y+2*HALO_WIDTH)*ZDIM*N_BYTES);
      close(out_fd);
      printf("Wrote file %s\n",out_filename);
    }
  }

  /* Deallocate memory */
  for(i=0; i<(OUT_TILE_PTS_X+2*HALO_WIDTH); i++)
  {
    for(j=0; j<(OUT_TILE_PTS_Y+2*HALO_WIDTH); j++)
    {
      free(intdata[i][j]);
    }
    free(intdata[i]);
  }
  free(intdata);


  return 0;
}
