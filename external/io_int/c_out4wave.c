#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/file.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

/* NOTE: if you add to this list of error numbers, also update
   ext_out4wave_fail to call wrf_error_fatal with an appropriate
   message. */
#define OUT4WAVE_TOO_MANY_FILES 2000
#define OUT4WAVE_CANNOT_OPEN 2001
#define OUT4WAVE_CANNOT_SEEK 2002
#define OUT4WAVE_CANNOT_LOCK 2003
#define OUT4WAVE_DIMENSIONS_TOO_SMALL 2004
#define OUT4WAVE_CANNOT_MALLOC 2005
#define OUT4WAVE_CANNOT_WRITE 2006
#define OUT4WAVE_CANNOT_UNLOCK 2007
#define OUT4WAVE_CANNOT_CLOSE 2008

/* OUT4WAVE_LOCK - define to enable flock file locking.  This is
   needed for proper functioning of I/O servers.  Do not disable
   unless you know what you're doing. */
#define OUT4WAVE_LOCK 1

/* OUT4WAVE_DEBUG - define to enable extra diagnostic messages.
   Without this, all you will see are error messages and file locking
   messages. */
/* #define OUT4WAVE_DEBUG 1 */

typedef struct {
  int DataHandle;
  int fd, nosize, havedata, dryrun;
  FILE *fp;
  int32_t im,jm;
  uint32_t *fdata;
  float *u10,*v10,*lat,*lon,*acprec;
  char *fn;
} data;

/* Low-level implementation (file-local) */
static void out4wave_byteswap(uint32_t *d,size_t n);
static data *for_handle(int *dh);
static int new_handle(data**d);
static int out4wave_allocate(data *d);
static void out4wave_close(data *d,int *status);
static void out4wave_write(data *d,int *status);
static void out4wave_recordsize(data *d);
static int out4wave_field(data *d,const char *fieldname,float **ofield);
#ifdef OUT4WAVE_LOCK
static int out4wave_lock(data *d,int *status);
#endif

/* Fortran-called functions */
void c_out4wave_inquire(int *dh,char *filename,int *len,int *opened,int *dryrun);
void c_out4wave_init(int *status);
void c_out4wave_flush(int *dh, int *status);
void c_out4wave_commit(int *dh, int *status);
void c_out4wave_write(int *dh, int *status);
void c_out4wave_close(int *dh,int *status);
void c_out4wave_open(char *filename, int *len, int *dh, int*status);
void c_out4wave_ioexit(int *status);
void c_out4wave_field(int *dh,int *status,float *ifield,
                      int *mstart,int *mend,
                      int *dstart,int *dend,
                      int *pstart,int *pend,
                      char *fieldname,int *namelen);

/**********************************************************************/
/* Misnamed look-alikes for various Fortran compilers                 */
/**********************************************************************/

void c_out4wave_init_(int *i) { c_out4wave_init(i); }
void c_out4wave_init__(int *i) { c_out4wave_init(i); }
void C_OUT4WAVE_INIT(int *i) { c_out4wave_init(i); }
void C_OUT4WAVE_INIT_(int *i) { c_out4wave_init(i); }
void C_OUT4WAVE_INIT__(int *i) { c_out4wave_init(i); }

void c_out4wave_ioexit_(int *i) { c_out4wave_ioexit(i); }
void c_out4wave_ioexit__(int *i) { c_out4wave_ioexit(i); }
void C_OUT4WAVE_IOEXIT(int *i) { c_out4wave_ioexit(i); }
void C_OUT4WAVE_IOEXIT_(int *i) { c_out4wave_ioexit(i); }
void C_OUT4WAVE_IOEXIT__(int *i) { c_out4wave_ioexit(i); }

void c_out4wave_commit_(int *d,int *s) { c_out4wave_commit(d,s); }
void c_out4wave_commit__(int *d,int *s) { c_out4wave_commit(d,s); }
void C_OUT4WAVE_COMMIT(int *d,int *s) { c_out4wave_commit(d,s); }
void C_OUT4WAVE_COMMIT_(int *d,int *s) { c_out4wave_commit(d,s); }
void C_OUT4WAVE_COMMIT__(int *d,int *s) { c_out4wave_commit(d,s); }

void c_out4wave_write_(int *d,int *s) { c_out4wave_write(d,s); }
void c_out4wave_write__(int *d,int *s) { c_out4wave_write(d,s); }
void C_OUT4WAVE_WRITE(int *d,int *s) { c_out4wave_write(d,s); }
void C_OUT4WAVE_WRITE_(int *d,int *s) { c_out4wave_write(d,s); }
void C_OUT4WAVE_WRITE__(int *d,int *s) { c_out4wave_write(d,s); }

void c_out4wave_close_(int *d,int *s) { c_out4wave_close(d,s); }
void c_out4wave_close__(int *d,int *s) { c_out4wave_close(d,s); }
void C_OUT4WAVE_CLOSE(int *d,int *s) { c_out4wave_close(d,s); }
void C_OUT4WAVE_CLOSE_(int *d,int *s) { c_out4wave_close(d,s); }
void C_OUT4WAVE_CLOSE__(int *d,int *s) { c_out4wave_close(d,s); }

void c_out4wave_flush_(int *d,int *s) { c_out4wave_flush(d,s); }
void c_out4wave_flush__(int *d,int *s) { c_out4wave_flush(d,s); }
void C_OUT4WAVE_FLUSH(int *d,int *s) { c_out4wave_flush(d,s); }
void C_OUT4WAVE_FLUSH_(int *d,int *s) { c_out4wave_flush(d,s); }
void C_OUT4WAVE_FLUSH__(int *d,int *s) { c_out4wave_flush(d,s); }

void c_out4wave_open_(char*f,int*l,int*d,int*s) { c_out4wave_open(f,l,d,s); }
void c_out4wave_open__(char*f,int*l,int*d,int*s) { c_out4wave_open(f,l,d,s); }
void C_OUT4WAVE_OPEN(char*f,int*l,int*d,int*s) { c_out4wave_open(f,l,d,s); }
void C_OUT4WAVE_OPEN_(char*f,int*l,int*d,int*s) { c_out4wave_open(f,l,d,s); }
void C_OUT4WAVE_OPEN__(char*f,int*l,int*d,int*s) { c_out4wave_open(f,l,d,s); }

void c_out4wave_inquire_(int*d,char*f,int*l,int*o,int*h){c_out4wave_inquire(d,f,l,o,h);}
void c_out4wave_inquire__(int*d,char*f,int*l,int*o,int*h){c_out4wave_inquire(d,f,l,o,h);}
void C_OUT4WAVE_INQUIRE(int*d,char*f,int*l,int*o,int*h){c_out4wave_inquire(d,f,l,o,h);}
void C_OUT4WAVE_INQUIRE_(int*d,char*f,int*l,int*o,int*h){c_out4wave_inquire(d,f,l,o,h);}
void C_OUT4WAVE_INQUIRE__(int*d,char*f,int*l,int*o,int*h){c_out4wave_inquire(d,f,l,o,h);}

/* c_out4wave_field's wrappers result in too much typing so here's
   some macro magic instead */
#define FIELDIFY(X)                                            \
  void X(int *dh,int *status,float *ifield,int *mstart,int *mend,int *dstart, \
         int *dend,int *pstart,int *pend,char *fieldname,int *namelen) { \
    c_out4wave_field(dh,status,ifield,mstart,mend,dstart,dend,pstart,pend,fieldname,namelen); \
  }
FIELDIFY(c_out4wave_field_);
FIELDIFY(c_out4wave_field__);
FIELDIFY(C_OUT4WAVE_FIELD);
FIELDIFY(C_OUT4WAVE_FIELD_);
FIELDIFY(C_OUT4WAVE_FIELD__);

/**********************************************************************/
/* Static functions and low-level implementation                      */
/**********************************************************************/

#define NDATA 100
#define HANDLE_ADD 301

static data dat[NDATA];

static void out4wave_byteswap(uint32_t *d,size_t n) {
  /* Byte-swaps data on a little endian machine, leaves it untouched
     on a Big Endian machine. */
  static const int swap=1;
  size_t i;
  if(*(char*)&swap)
    for(i=0;i<n;i++)
      d[i] = ( ((d[i]&0xff000000)>>24) | 
               ((d[i]&0x00ff0000)>> 8) |
               ((d[i]&0x0000ff00)<< 8) |
               ((d[i]&0x000000ff)<<24) );
}

static data *for_handle(int *dh) {
  int loc=*dh-HANDLE_ADD;
  if(loc<0 || loc>=NDATA) {
    fprintf(stderr,"out4wave: invalid handle %d\n",*dh);
    return NULL;
  }
  return dat+loc;
}

static int new_handle(data**d) {
  int i;
  for(i=0;i<NDATA;i++)
    if(!dat[i].fp) {
      *d=dat+i;
      return i+HANDLE_ADD;
    }
  *d=NULL;
  return -1;
}

static int out4wave_allocate(data *d) {
  size_t allocsize;
  uint32_t imjm,there;

#ifdef OUT4WAVE_DEBUG
  fprintf(stderr,"out4wave: %s: requested dimensions %dx%d\n",
          d->fn,d->im,d->jm);
#endif

  /* Require the arrays to be at least 2x2 */
  if(d->im<1 || d->jm<1) {
    fprintf(stderr,"out4wave: %s: requested dimensions are too small (min=2x2)\n",d->fn);
    return OUT4WAVE_DIMENSIONS_TOO_SMALL;
  }
  
  /* Write the size information to the file if we haven't already. */
  if(d->nosize) {
    int32_t sizeinfo[4]={8,d->im,d->jm,8};
    out4wave_byteswap((uint32_t*)sizeinfo,4);
    if(1!=fwrite(sizeinfo,16,1,d->fp))
      return OUT4WAVE_CANNOT_WRITE;
    if(fseek(d->fp,0,SEEK_END))
      return OUT4WAVE_CANNOT_SEEK;
    d->nosize=0;
  }

  /* Allocate data for all five data records. */
  allocsize=(d->im*d->jm+2)*5;
  if(!(d->fdata=(uint32_t*)malloc(allocsize*4)))
    return OUT4WAVE_CANNOT_MALLOC;
  memset(d->fdata,0x65,allocsize*4);
  imjm=d->im*d->jm;

  /* Point the float arrays to the right spots. */
  d->u10=(float*)(d->fdata+1);
  d->v10=d->u10+imjm+2;
  d->lat=d->v10+imjm+2;
  d->lon=d->lat+imjm+2;
  d->acprec=d->lon+imjm+2;

  out4wave_recordsize(d);

#ifdef OUT4WAVE_DEBUG
  fprintf(stderr,"out4wave: %s: allocated %llu bytes of data.\n",
          d->fn,(unsigned long long)allocsize);
#endif
  return 0;
}

static void out4wave_recordsize(data *d) {
  uint32_t imjm=d->im*d->jm;
  size_t allocsize=5*(imjm+2),there;

  /* Add the record size information. */
  for(there=0;there<=allocsize;there+=imjm+2) {
    if(there>0) {
      d->fdata[there-1]=imjm*4;
      /*fprintf(stderr,"out4wave: %s: store size at %d/%d\n",d->fn,(int)(there-1),imjm);*/
    }
    if(there<allocsize) {
      /*fprintf(stderr,"out4wave: %s: store size at %d/%d\n",d->fn,(int)there,imjm);*/
      d->fdata[there]=imjm*4;
    }
  }
}

#ifdef OUT4WAVE_LOCK
static int out4wave_lock(data *d,int *status) {
  const int maxsleep=3000000, sleepper=500000, minsleep=50000;
  int bad=0;
  int slept=0,val,tries=0;
  while(slept<3000000) {
    tries++;
    if(!flock(d->fd,LOCK_EX|LOCK_NB)) {
      fprintf(stderr,"out4wave: %s: have lock.\n",d->fn);
      return 0;
    } else if(errno!=EWOULDBLOCK && !bad)
      bad=errno;
    val=(unsigned long long)rand() * sleepper / RAND_MAX;
    if(val+slept>maxsleep) val=maxsleep-slept;
    if(val<minsleep) val=minsleep;
#ifdef OUT4WAVE_DEBUG
    fprintf(stderr,"out4wave: %s: cannot lock after %d seconds and %d %s.  Will sleep %d microseconds and retry.\n",
            d->fn, slept, tries, (tries>1)?"tries":"try", val);
#endif
    usleep(val);
    slept+=val;
  }
  if(bad) {
    *status=OUT4WAVE_CANNOT_LOCK;
    fprintf(stderr,"out4wave: %s: cannot lock after %d microseconds and %d tries.  First error: %s\n",d->fn,slept,tries,strerror(errno));
    return 2;
  }
  fprintf(stderr,"out4wave: %s: warning: cannot lock file after %d microseconds and %d tries.  Will continue without locking.\n",d->fn,slept,tries);
  return 0;
}
#endif

static void out4wave_close(data *d,int *status) {
  *status=0;
#ifdef OUT4WAVE_DEBUG
  fprintf(stderr,"out4wave: %s: closing...\n",d->fn);

  fprintf(stderr,"out4wave: %s: final write...\n",d->fn);
#endif
  out4wave_write(d,status);

  fprintf(stderr,"out4wave: %s: unlock...\n",d->fn);
#ifdef OUT4WAVE_LOCK
  if(flock(d->fd,LOCK_UN)) {
    fprintf(stderr,"out4wave: %s: trouble unlocking: %s\n",d->fn,strerror(errno));
    *status=OUT4WAVE_CANNOT_UNLOCK;
  } else
    fprintf(stderr,"out4wave: %s: unlocked.\n",d->fn);
#endif

#ifdef OUT4WAVE_DEBUG
  fprintf(stderr,"out4wave: %s: free resources...\n",d->fn);
#endif
  if(fclose(d->fp)) {
    fprintf(stderr,"out4wave: %s: warning: error on close: %s\n",
            d->fn,strerror(errno));
    *status=OUT4WAVE_CANNOT_CLOSE;
  }
  if(d->fdata) free(d->fdata);
  memset(d,0,sizeof(data));
}

static void out4wave_write(data *d,int *status) {
  uint32_t imjm;
  imjm=d->im;

  *status=0;
  if(d->dryrun) {
    d->dryrun=0;
    d->havedata=0;
#ifdef OUT4WAVE_DEBUG
    fprintf(stderr,"out4wave: %s: dryrun complete.\n",d->fn);
#endif
    return;
  }

  if(d->havedata) {
    size_t count=5*(d->im*d->jm+2);
    d->havedata=0;
#ifdef OUT4WAVE_DEBUG
    fprintf(stderr,"out4wave: %s: byteswap %llu bytes of data\n",
            d->fn,(unsigned long long)(count*4));
#endif
    out4wave_byteswap(d->fdata,count);
    if(1!=fwrite(d->fdata,count*4,1,d->fp)) {
      fprintf(stderr,"out4wave: %s: cannot write: %s\n",
              d->fn,strerror(errno));
      *status=OUT4WAVE_CANNOT_WRITE;
      return;
    }
#ifdef OUT4WAVE_DEBUG
    fprintf(stderr,"out4wave: %s: wrote %llu bytes of data\n",
            d->fn,(unsigned long long)(count*4));
#endif

    memset(d->u10,0,imjm*4);
    memset(d->v10,0,imjm*4);
    memset(d->lat,0,imjm*4);
    memset(d->lon,0,imjm*4);
    memset(d->acprec,0,imjm*4);
    d->havedata=0;
    out4wave_recordsize(d);
  } else {
#ifdef OUT4WAVE_DEBUG
    fprintf(stderr,"out4wave: %s: nothing to write in c_out4wave_write\n",
            d->fn);
#endif
  }
}

static int out4wave_field(data *d,const char *fieldname,float **ofield) {
  *ofield=NULL;
  if(!strcasecmp(fieldname,"u10")) {
    *ofield=d->u10;
    return 1;
  } else if(!strcasecmp(fieldname,"v10")) {
    *ofield=d->v10;
    return 1;
  } else if(!strncasecmp(fieldname+1,"lat",3) ||
            !strncasecmp(fieldname,"lat",3)) {
    *ofield=d->lat;
    return 1;
  } else if(!strncasecmp(fieldname+1,"lon",3) || 
            !strncasecmp(fieldname,"lon",3)) {
    *ofield=d->lon;
    return 1;
  } else if(!strcasecmp(fieldname,"acprec") ||
            !strcasecmp(fieldname,"rainc") ||
            !strcasecmp(fieldname,"prec_acc_c")) {
    *ofield=d->acprec;
    return 1;
  } else {
#ifdef OUT4WAVE_DEBUG
    fprintf(stderr,"out4wave: %s: ignoring an invalid variable %s\n",d->fn,fieldname);
#endif
    return 0;
  }
}

/**********************************************************************/
/* High-level fortran-called implementation functions                 */
/**********************************************************************/

void c_out4wave_inquire(int *dh,char *filename,int *len,int *opened,int *dryrun) {
  data *d=for_handle(dh);

  if(!d) {
    fprintf(stderr,"out4wave: invalid handle %d\n",*dh);
    memset(filename,' ',*len);
    *len=0;
    *opened=0;
    *dryrun=0;
    return;
  } else {
    if(*len>0) {
      char *a=filename,*b=d->fn, *e=filename+*len;
      for(a=filename,b=d->fn; *b && a<=e; a++,b++)
        *a=*b;
      for(;a<=e;a++)
        *a=' ';
    }
    *opened=1;
    *dryrun=d->dryrun;
  }
}

void c_out4wave_init(int *status) {
  *status=0;
  memset(dat,0,sizeof(data)*NDATA);
}

void c_out4wave_ioexit(int *status) {
  int i,s2;
  *status=0;
  for(i=0;i<NDATA;i++) {
    data *d=dat+i;
    if(d->fp) {
      out4wave_close(d,&s2);
      if(s2 && !*status)
        *status=s2;
    }
  }
  memset(dat,0,sizeof(data)*NDATA);
}

void c_out4wave_field(int *dh,int *status,float *ifield,
                      int *mstart,int *mend,
                      int *dstart,int *dend,
                      int *pstart,int *pend,
                      char *fieldname,int *namelen)
{
  /* i0..iN, j0..jN = memory dimensions 
     i1..i2, j1..j2 = patch dimensions
      1..im,  1..jm = domain dimensions */
  int i0=mstart[0],iN=mend[0]-mstart[0]+1,i1=pstart[0],i2=pend[0],
      j0=mstart[1],jN=mend[1]-mstart[1]+1,j1=pstart[1],j2=pend[1];
  int i,j, im,jm;
  float *ofield;
  const size_t sm=30;
  char shortfield[sm];
  data *d=for_handle(dh);
  int len=*namelen,found;
  if(len>sm-1) len=sm-1;

  memset(shortfield,0,sm);
  strncpy(shortfield,fieldname,len);

  if(!out4wave_field(d,shortfield,&ofield))
    return;

  if(i1<dstart[0]) i1=dstart[0];
  if(i2>dend[0]) i2=dend[0];
  if(j1<dstart[1]) j1=dstart[1];
  if(j2>dend[1]) j2=dend[1];
  
  im=dend[0]-dstart[0]+1;
  jm=dend[1]-dstart[1]+1;

  if(d->nosize) {
    d->im=im;
    d->jm=jm;
    if(*status=out4wave_allocate(d))
      return;
    d->nosize=0;
  }

  if(d->dryrun) {
#ifdef OUT4WAVE_DEBUG
    fprintf(stderr,"out4wave: %s: dryrun; not copying data for %s\n",d->fn,shortfield);
#endif
    return;
  }

  found=out4wave_field(d,shortfield,&ofield);
  assert(found);
  assert(ofield);

#ifdef OUT4WAVE_DEBUG
  fprintf(stderr,"c_out4wave_field I dimension: m=%d..%d p=%d..%d d=%d..%d im=%d\n",
          mstart[0],mend[0],pstart[0],pend[0],dstart[0],dend[0],  im);
  fprintf(stderr,"c_out4wave_field J dimension: m=%d..%d p=%d..%d d=%d..%d jm=%d\n",
          mstart[1],mend[1],pstart[1],pend[1],dstart[1],dend[1],  jm);

  fprintf(stderr,"c_out4wave_field copying j=%d..%d i=%d..%d where i0=%d, j0=%d, iN=%d\n",j1,j2,i1,i2,i0,j0,iN);
  fprintf(stderr,"   data at 10,10 is %f\n",ifield[iN*(10-j0) + 10-i0]);
#endif
  for(j=j1;j<=j2;j++)
    for(i=i1;i<=i2;i++) {
      int oindex=im*(j-1)+(i-1);
      int iindex=iN*(j-j0) + i-i0;
      assert(oindex<im*jm);
      assert(oindex>=0);
      assert(iindex>=0);
      assert(iindex<(iN-i0+1)*(jN-j0+1));
      /*
      fprintf(stderr,"i=%d j=%d oindex=%d iindex=%d ofield=%llx ifield=%llx iN*jN=%d\n",
              i,j,oindex,iindex, (long long)ofield, (long long)ifield,iN*jN);
      */
      ofield[oindex] = ifield[iindex];
    }

#ifdef OUT4WAVE_DEBUG
  fprintf(stderr,"out4wave: %s: accepted %llu bytes of data for field %d/5\n",
          d->fn,(j2-j1+1)*(unsigned long long)(i2-i1+1)*4,
          (int)( ((uint32_t*)ofield-d->fdata-1)/((im*jm)+2) +1 ));
#endif
  d->havedata=1;
}

void c_out4wave_flush(int *dh, int *status) {
  data *d=for_handle(dh);

  *status=0;

  if(d) {
    out4wave_write(d,status);
    if(fflush(d->fp))
      fprintf(stderr,"out4wave: %s: error flushing stream: %s\n",
              d->fn,strerror(errno));
    else {
#ifdef OUT4WAVE_DEBUG
      fprintf(stderr,"out4wave: %s: flushed stream.\n",d->fn);
#endif
    }
  }
}

void c_out4wave_write(int *dh, int *status) {
  data *d=for_handle(dh);

  *status=0;
  if(d)
    out4wave_write(d,status);
}

void c_out4wave_commit(int *dh, int *status) {
  data *d=for_handle(dh);

  *status=0;
  if(d)
    d->dryrun=0;
}


void c_out4wave_close(int *dh,int *status) {
  data *d=for_handle(dh);
  *status=0;
  if(d)   out4wave_close(d,status);
}


void c_out4wave_open(char *filename, int *len, int *dh, int*status) {
  data *d=NULL;

  *dh=new_handle(&d);
  if(!d) {
    *status=OUT4WAVE_TOO_MANY_FILES;
    return;
  }

  *status=0;

  int32_t sizedat[4];
  d->dryrun=1;
  d->fn=(char*)malloc(*len+1);
  memcpy(d->fn,filename,*len);
  d->fn[*len]='\0';
  d->fd=-1;
  d->fp=fopen(d->fn,"ab+");
  if(!d->fp) {
    fprintf(stderr,"out4wave: %s: cannot open: %s\n",
            d->fn,strerror(errno));
    *status=OUT4WAVE_CANNOT_OPEN;
    goto bad;
  }
  d->fd=fileno(d->fp);
  if(d->fd==-1) {
    *status=OUT4WAVE_CANNOT_OPEN;
    goto bad;
  }
#ifdef OUT4WAVE_LOCK
  fprintf(stderr,"out4wave: %s: opened file; trying to lock...\n",d->fn);
  if(out4wave_lock(d,status)) {
    fprintf(stderr,"out4wave: %s: trouble locking: %s\n",
            d->fn,strerror(errno));
    *status=OUT4WAVE_CANNOT_LOCK;
    goto bad;
  }
#endif

  if(1==fread(sizedat,16,1,d->fp)) {
    out4wave_byteswap((uint32_t*)sizedat,4);
    d->im=sizedat[1];
    d->jm=sizedat[2];
    d->nosize=0;
#ifdef OUT4WAVE_DEBUG
    fprintf(stderr,"out4wave: %s: is %dx%d\n",d->fn,(int)d->im,(int)d->jm);
#endif
    if(fseek(d->fp,0,SEEK_END)) {
      *status=2002;
      goto bad;
    }
#ifdef OUT4WAVE_DEBUG
    fprintf(stderr,"out4wave: %s: seeked to end.\n",d->fn);
#endif
    if(*status=out4wave_allocate(d))
      goto bad;
  } else {
#ifdef OUT4WAVE_DEBUG
    fprintf(stderr,"out4wave: %s: cannot find size in file; will get size from first field written.\n",d->fn);
#endif
    d->nosize=1;
    d->im=-1;
    d->jm=-1;
  }

  return;

 bad: /* Error recovery section */
  if(d) {
    if(d->fp) {
      if(d->fd!=-1) {
#ifdef OUT4WAVE_LOCK
        if(flock(d->fd,LOCK_UN))
          fprintf(stderr,"out4wave: %s: warning: trouble unlocking: %s\n",
                  d->fn,strerror(errno));
        else
          fprintf(stderr,"out4wave: %s: released lock.\n",d->fn);
#endif
      }
      if(fclose(d->fp)) {
        fprintf(stderr,"out4wave: %s: warning: error closing file: %s\n",
                d->fn,strerror(errno));
      }
    }
    if(d->fdata) free(d->fdata);
    if(d->fn) free(d->fn);
    memset(d,0,sizeof(data));
  }
  return;
}

