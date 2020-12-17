#ifndef CRAY
#  ifdef NOUNDERSCORE
#    define DA_JOIN_IV_FOR_MULTI_INC da_join_iv_for_multi_inc
#  else
#    ifdef F2CSTYLE
#       define DA_JOIN_IV_FOR_MULTI_INC da_join_iv_for_multi_inc__
#    else
#       define DA_JOIN_IV_FOR_MULTI_INC da_join_iv_for_multi_inc_
#    endif
#  endif
#endif

#include <fnmatch.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef FALSE
#undef FALSE 
#endif

#ifdef TRUE
#undef TRUE
#endif

#define FALSE     0
#define TRUE      !FALSE

#ifndef FNM_EXTMATCH
#define FNM_EXTMATCH    (1 << 5)
#endif

#ifndef FNM_FILE_NAME
#define FNM_FILE_NAME   (1 << 0)
#endif


#define BUFF_SIZE 128

static const char *pattern="stub\\.[0-9][0-9][0-9]\\.[0-9][0-9][0-9][0-9]\\.[a-zA-Z_]*";

int file_select(const struct dirent *entry)
{
  if (fnmatch(pattern, entry->d_name, FNM_EXTMATCH | FNM_FILE_NAME) == 0 ) {
    return TRUE;
  } else {
    return FALSE;
  }
}
 
void DA_JOIN_IV_FOR_MULTI_INC()   
{ 
    int count,i,n,total  ;
    struct dirent **files;
    FILE *ifp, *ofp;
    char *obs_type, *ofile, *buff;
    char ts[4]={'\0'};
    int update_head;

    count = scandir(".", &files, file_select, alphasort);
 
    if(count <= 0) {
      return ;
    }

    buff = calloc(BUFF_SIZE, sizeof(char));
    if ( buff == '\0' ) return;

    for (i=0;i<count;i++) {
      
      n=strlen(files[i]->d_name);

      obs_type = calloc(n-13, sizeof(char));
      if ( obs_type == '\0') return;

      ofile = calloc(n-1, sizeof(char));
      if ( ofile == '\0' ) return;

      strncpy(ts, files[i]->d_name+5, 3);
      strncpy(obs_type, files[i]->d_name+14, n-14);
      sprintf(ofile, "gts_omb.%s.%s", ts, obs_type);

      ifp = fopen(files[i]->d_name, "r");
      if ( ifp == '\0' ) return;

      fscanf(ifp, "%s %i", buff, &n);

      if ((ofp = fopen(ofile, "r")) == '\0' ) {
        ofp = fopen(ofile, "w+");
        if (ofp == '\0') return;
        fprintf(ofp, "%20s%8i", buff, n);
        update_head = FALSE;
      } else {
        ofp = freopen(ofile, "r+", ofp);
        if (ofp == '\0') return;
        update_head = TRUE;
        fseek(ofp, -1, SEEK_END);
      }

      while ( fgets(buff, BUFF_SIZE, ifp) != NULL ) {
        fputs(buff,ofp);
      }

      if ( update_head ) {
        rewind(ofp);
        fscanf(ofp, "%s %i", buff, &total);
        total+=n;    
        rewind(ofp);
        fprintf(ofp, "%20s%8i", buff, total);
      }
 
      free(obs_type);
      free(ofile);

      fclose(ifp);
      fclose(ofp);

      remove(files[i]->d_name);

    }

  free(buff);

}
