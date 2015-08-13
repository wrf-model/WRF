/* This is part of the Model Coupling Environmental Library or MCEL
   If you have any questions mail matt at matt@mail.bettencourt.info 
   it is covered under the LGPL http://www.gnu.org/licenses/lgpl.html
   
   This code was developed with DoD HPCMO CHSSI and PET funding
   http://www.hpcmo.hpc.mil/  */
#include <stdio.h>
#include <time.h>
#include <string.h>


#define INIT2000 \
  t1.tm_sec=-1;   \
  t1.tm_min=-1;   \
  t1.tm_hour=-1;  \
  t1.tm_mday=-1;  \
  t1.tm_mon=-1;   \
  t1.tm_year=-1;  \
  t1.tm_wday=-1;  \
  t1.tm_yday=-1;  \
  t1.tm_isdst=0; \
  t1.tm_year=100;\
  t1.tm_mon=0;\
  t1.tm_mday=1;\
  t1.tm_hour = 0;\
  t1.tm_min = 0;\
  t1.tm_sec = 0;

#ifdef LOWERCASE
#  ifdef UNDERSCORE
#     	define YYMMDDHH2SECS yymmddhh2secs_
#     	define YYMMDDHHMMSS2SECS yymmddhhmmss2secs_
#     	define YYYYMMDDHHMMSS2SECS yyyymmddhhmmss2secs_
#       define SECS2YYYYMMDDHHMMSS secs2yyyymmddhhmmss_
#  else
#     	define YYMMDDHH2SECS yymmddhh2secs
#     	define YYMMDDHHMMSS2SECS yymmddhhmmss2secs
#     	define YYYYMMDDHHMMSS2SECS yyyymmddhhmmss2secs
#       define SECS2YYYYMMDDHHMMSS secs2yyyymmddhhmmss
#  endif
#else
/* Do nothing */
#endif

void YYMMDDHH2SECS(char* date, double* time);
void YYMMDDHHMMSS2SECS(char* date, double* time);
void YYYYMMDDHHMMSS2SECS(char* date, double* time);
 void SECS2YYYYMMDDHHMMSS(char* date, double* time);


void YYMMDDHH2SECS(char* date, double* time) {

  char date2[12];
  strncpy(date2,date,8);
  strncat(date2,"0000",4);
  
  YYMMDDHHMMSS2SECS(date2, time);

}


void YYMMDDHHMMSS2SECS(char* date, double* time) {
  

  char date2[14];
  int year;
  sscanf(date,"%2d",&year);

  if ( year < 50 ) 
    sprintf(date2,"20");
  else
    sprintf(date2,"19");
  strncat(date2,date,12);
  YYYYMMDDHHMMSS2SECS(date2,time);
}


void YYYYMMDDHHMMSS2SECS(char* date, double* time) {

  struct tm t1,t2;
  time_t tt1,tt2;

  INIT2000;
  t2=t1;
  sscanf(date,"%4d%2d%2d%2d%2d%2d",&t2.tm_year,&t2.tm_mon,&t2.tm_mday,
	 &t2.tm_hour,&t2.tm_min,&t2.tm_sec);
  /* Correct year, month */
  t2.tm_year -= 1900;
  t2.tm_mon -= 1;


  tt1 = mktime(&t1);
  tt2 = mktime(&t2);

  *time = difftime(tt2,tt1);

}
  

void SECS2YYYYMMDDHHMMSS(char* date, double* time) {

  struct tm t1,t2;
  time_t tt1,tt2;
  INIT2000;
  tt1 = mktime(&t1);
  tt2 = tt1+(unsigned long int )*time;
  t2=t1;
  localtime_r(&tt2,&t2);
  if (t2.tm_isdst) 
    tt2 -= 3600;
  localtime_r(&tt2,&t2);

  sprintf(date,"%04d%02d%02d%02d%02d%02d",
	  1900+t2.tm_year,
	  t2.tm_mon+1,
	  t2.tm_mday,
	  t2.tm_hour,
	  t2.tm_min,
	  t2.tm_sec);
}
