/* gribw_time.c */
#define _GRIBW_TIME_

void set_InitHH(unsigned char *pds, unsigned int hour);
unsigned int get_InitHH(unsigned char *pds);

void set_InitMn(unsigned char *pds, unsigned int minute);
unsigned int get_InitMn(unsigned char *pds);

void set_InitHHMn(unsigned char *pds, unsigned int hhmn);
unsigned int get_InitHHMn(unsigned char *pds);

void set_InitYYYYMMDDHH(unsigned char *pds, unsigned int yyyymmddhh);
unsigned int get_InitYYYYMMDDHH(unsigned char *pds);

void set_InitDate(unsigned char *pds, int year, int month, int day,
	int hour, int minute);

void get_InitDate(unsigned char *pds, int *year, int *month, int *day,
	int *hour, int *minute);

void Init_NextYear(unsigned char *pds);
void Init_NextMonth(unsigned char *pds);
void Init_NextDay(unsigned char *pds);

unsigned int get_NextDay(unsigned int time);
unsigned int get_NextMonth(unsigned int time);
unsigned int get_NextYear(unsigned int time);
