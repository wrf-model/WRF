/* gribw_time v1.2
 *
 * 1996 w. ebisuzaki
 *
 * time manipulation routines
 *
 * 7/97 Y2K bug fix change
 * 2/98 removed set_InitTime, get_InitTime
 *      added many new functions
 */

#include <stdio.h>
#include <stdlib.h>
#include "gribw_time.h"

/* v1.0 old: #define YYYY (YY + 100 * (CC - (YY != 0))) */
#define YYYY (YY + 100 * (CC - 1))

#define Mn   pds[16]
#define HH   pds[15]
#define DD   pds[14]
#define MM   pds[13]
#define YY   pds[12]
#define CC   pds[24]

static int days_in_month[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

void set_InitHH(unsigned char *pds, unsigned int hour) {
	HH = hour;
}
unsigned int get_InitHH(unsigned char *pds) {
	return HH;
}

void set_InitMn(unsigned char *pds, unsigned int minute) {
	Mn = minute;
}
unsigned int get_InitMn(unsigned char *pds) {
	return Mn;
}

void set_InitHHMn(unsigned char *pds, unsigned int hhmn) {
	Mn = hhmn % 100;	/* minute */
	hhmn /= 100;
	HH = hhmn % 100;	/* hours */
}
unsigned int get_InitHHMn(unsigned char *pds) {
	return (Mn + 100*HH);
}

void set_InitYYYYMMDDHH(unsigned char *pds, unsigned int yyyymmddhh) {
	Mn = 0;				/* minute */
	HH = yyyymmddhh % 100;		/* hour */
	yyyymmddhh /= 100;
	DD = yyyymmddhh % 100;		/* day */
	yyyymmddhh /= 100;
	MM = yyyymmddhh % 100;		/* month */
	yyyymmddhh /= 100;
        YY = yyyymmddhh % 100;
	if (YY == 0) YY = 100;  /* 2000, YY = 100, CC=20 */
	CC = (yyyymmddhh - YY) / 100 + 1; /* century */
}

unsigned int  get_InitYYYYMMDDHH(unsigned char *pds)
{
	return ((((100*(CC-1)+YY)*100+MM)*100+DD)*100+HH);
}

void set_InitDate(unsigned char *pds, int year, int month, int day,
	int hour, int minute) {

	Mn = minute;
	HH = hour;
	DD = day;
	MM = month;

        YY = year % 100;
	if (YY == 0) YY = 100;  /* 2000, YY = 100, CC=20 */
	CC = (year - YY) / 100 + 1; /* century */
}

void get_InitDate(unsigned char *pds, int *year, int *month, int *day,
	int *hour, int *minute) {

	*minute = Mn;
	*hour = HH;
	*day = DD;
	*month = MM;
	*year = YYYY;
}

/* Routines to modify the initial time in the PDS by 1 */

void Init_NextYear(unsigned char *pds) {
	if (YY++ == 100) {
		YY = 1;
		CC++;
	}
}

void Init_NextMonth(unsigned char *pds) {
        if (MM++ == 12) {
		MM = 1;
		Init_NextYear(pds);
	}
}

void Init_NextDay(unsigned char *pds) {

	if (MM == 2) {
		days_in_month[1] = 28;
		if (YY % 4 == 0) days_in_month[1] = 29;
		if (YYYY % 100 == 0) days_in_month[1] = 28;
		if (YYYY % 400 == 0) days_in_month[1] = 29;
	}

        if (DD > days_in_month[MM-1] || DD <= 0) {
		fprintf(stderr,"bad day .. day: %d month: %d year; %d\n",
			DD, MM, YYYY);
		exit(8);
	}
        if (DD < days_in_month[MM-1]) {
		DD++;
		return;
	}
	else {
		DD = 1;
		Init_NextMonth(pds);
	}
}

/* routines to get manipulate YYYYMMDD (time) codes */

unsigned int get_NextDay(unsigned int time) {

	int hour, day, month, year;

        day = (time/100) % 100;
	/* fast case .. 90% */
        if (day < 28) {
	    return (time + 100);
	}

        month = (time/10000) % 100;
	year = time/1000000;

        if (month == 2) {
	    days_in_month[1] = 28;
	    if (year % 4 == 0) days_in_month[1] = 29;
	    if (year % 100 == 0) days_in_month[1] = 28;
	    if (year % 400 == 0) days_in_month[1] = 29;
	}

        if (day < days_in_month[month-1]) {
	    return (time + 100);
	}

	/* have to go to the next month */
        hour = time % 100;
	day = 1;
	month++;
	if (month == 13) {
	    month = 1;
	    year++;
	}
	return (hour + 100*day + 10000*month + 1000000*year);
}

unsigned int get_NextMonth(unsigned int time) {

    int month;

    /* yyyymmddhh */
    month = (time / 10000) % 100;
    if (month < 12) return (time + 10000);
    return (time + 1000000 - 110000);
}

unsigned int get_NextYear(unsigned int time) {
    return (time + 1000000);
}
