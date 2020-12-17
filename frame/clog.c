#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

  static void clog_init();
  static void clog_finalize();

  void clog_set_buffer_len(int *wantlenp);
  void clog_write(int*len,char*c);
  void clog_flush(int*flag);

  static const size_t default_len=100;

  static FILE *stream=NULL;
  static char *buffer=NULL;
  static size_t len=0,used=0;
  static int inited=0;

  static void clog_init(void) {
    if(!inited) {
      atexit(clog_finalize);
      inited=1;
    }
  }

  static void clog_finalize(void) {
    int i=1;
    if(buffer)
      clog_flush(&i);
    free(buffer);
  }

  void clog_set_buffer_len(int *wantlenp) {
    /* Initialize or re-initialize the output buffer with size *wantlenp */
    int dummy;
    size_t wantlen=*wantlenp;
    if(buffer) {
      /* Already had a buffer */
      if(len!=wantlen) {
        /* User is requesting a change in the buffer size, so flush first */
        clog_flush(NULL);
        free(buffer);
        buffer=NULL;
        len=used=0;
      } else {
        /* Buffer size is not changing, so nothing to do. */
        return;
      }
    }

    /* We now need to allocate a new buffer and initialize the state */
    if(!(buffer=(char*)malloc(sizeof(char)*len)))
      len=0;
    len=wantlen;
    used=0;

    clog_init();

    if(!stream)
      stream=stdout;
    if(buffer)
      fprintf(stream,"Clog: buffering rsl.out with a %llu byte buffer...\n",
              (unsigned long long)len);
  }

  void clog_write(int *clen,char *c) {
    size_t slen=(size_t)*clen;
    size_t nlen=slen;

    if(c[slen-1]!='\n' && c[slen-1]!='\r')
      /* String is missing an end-of-line, so we need to add one. */
      nlen++;

    if(!stream)
      stream=stdout;
    if(!buffer) {
      /* Buffer is not allocated yet, so allocate it. */
      if(!(buffer=(char*)malloc(sizeof(char)*default_len))) {
        /* Failed to allocate the buffer, so write the string directly. */
        fwrite(c,slen,1,stream);
        if(nlen>slen) fputc('\n',stream);
        return;
      }
      len=default_len;
      used=0;
      clog_init();
      if(buffer)
        fprintf(stream,"Clog: buffering rsl.out with a %llu byte buffer...\n",
                (unsigned long long)len);
    }

    if(nlen>len) {
      /* Cannot fit the string in the buffer, so we'll have to write the
         string directly.  */
      if(used>0)
        clog_flush(NULL);
      fwrite(c,slen,1,stream);
      if(nlen>slen) fputc('\n',stream);
      return;
    } else if(nlen+used>len) {
      /* String is smaller than the buffer, but the buffer is filled enough
         so that the string cannot fit */
      clog_flush(NULL);
    }

    if(slen>0)
      memcpy(buffer+used,c,slen);

    if(nlen>slen)
      buffer[used+slen]='\n';
    used+=nlen;
  }

  void clog_flush(int *flag) {
    if(!stream)
      stream=stdout;
    if(buffer) {
      fwrite(buffer,used,1,stream);
      used=0;
    }
    if(flag && *flag) {
      fflush(stream);
    }
  }

  /* Lookalike functions for all common fortran name mangling schemes */

  void clog_init_(void) { clog_init(); }
  void clog_init__(void) { clog_init(); }
  void CLOG_INIT(void) { clog_init(); }
  void CLOG_INIT_(void) { clog_init(); }
  void CLOG_INIT__(void) { clog_init(); }

  void clog_set_buffer_len_(int *w) { clog_set_buffer_len(w); }
  void clog_set_buffer_len__(int *w) { clog_set_buffer_len(w); }
  void CLOG_SET_BUFFER_LEN(int *w) { clog_set_buffer_len(w); }
  void CLOG_SET_BUFFER_LEN_(int *w) { clog_set_buffer_len(w); }
  void CLOG_SET_BUFFER_LEN__(int *w) { clog_set_buffer_len(w); }

  void clog_write_(int *clen,char *c) { clog_write(clen,c); }
  void clog_write__(int *clen,char *c) { clog_write(clen,c); }
  void CLOG_WRITE(int *clen,char *c) { clog_write(clen,c); }
  void CLOG_WRITE_(int *clen,char *c) { clog_write(clen,c); }
  void CLOG_WRITE__(int *clen,char *c) { clog_write(clen,c); }

  void clog_flush_(int *flag) { clog_flush(flag); }
  void clog_flush__(int *flag) { clog_flush(flag); }
  void CLOG_FLUSH(int *flag) { clog_flush(flag); }
  void CLOG_FLUSH_(int *flag) { clog_flush(flag); }
  void CLOG_FLUSH__(int *flag) { clog_flush(flag); }

#ifdef __cplusplus
}
#endif

