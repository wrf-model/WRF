#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef struct{
        double tai93sec;
        short  year;
        short  month;
        short  day;
        short  hour;
        short  minute;
        short  second;
        short  ms;
        short  reserve;
} AM2_COMMON_SCANTIME;

void amsr2time_(int *num, double *tai93, AM2_COMMON_SCANTIME *st){
	// structure for leap second
	typedef struct{
		short year;
		short month;
		double tai93sec;
	} LEAP_SECOND;

	// fixed value
	char *fn = "leapsec.dat";  // leap second file name

	// interface variable
	int i, j;      // loop variable
	char *ret;     // return status
	char buf[512]; // text buffer
	short ibuf;    // integer buffer
	double rbuf;   // real buffer
	FILE *hnd;     // file handle
	
	// leap second data
	short lnum; // number of leap second from 1993
	LEAP_SECOND *ldat; // leap second data from 1993

	// leap second working variable
	int lcnt; // leap second count
	int flag; // strike flag
	time_t utime; // unix epoch time
	struct tm *stmp; // struct tm pointer

	// open
	hnd = fopen(fn, "r");
	if(hnd == NULL){
		printf("amsr2time: leap second file leapsec.dat open error\n");
		exit(1);
	}

	// count leap second entry
	lnum = 0;
	while(1){
		// read
		ret = fgets(buf, 512, hnd);

		// end check
		if(ret == NULL) break;

		// check & count
		if(buf[0] != '/'){
			sscanf(buf, "%hd", &ibuf);
			if(ibuf >= 1993){
				++lnum;
			}
		}
	}

	// read leap second data
	ldat = malloc(sizeof(LEAP_SECOND) * lnum);
	rewind(hnd);
	i = 0;
	while(1){
		// read
		ret = fgets(buf, 512, hnd);

		// end check
		if(ret == NULL) break;

		// check & count
		if(buf[0] != '/'){
			sscanf(buf, "%hd", &ibuf);
			if(ibuf >= 1993){
				sscanf(buf, "%hd %hd %lf %lf %lf %lf"
				, &ldat[i].year
				, &ldat[i].month
				, &rbuf
				, &rbuf
				, &rbuf
				, &ldat[i].tai93sec
				);
				// printf("amsr2time: year=%4d month=%2d tai93sec=%14.2lf\n"
				// , ldat[i].year
				// , ldat[i].month
				// , ldat[i].tai93sec
				// );
				++i;
			}
		}
	}
	// printf("amsr2time: number of leap second = %d\n", lnum);

	// close
	fclose(hnd);

	// convert
	for(i = 0; i < *num; ++i){
		// negative value is warning
		if(tai93[i] < 0){
			printf("amsr2time: negative value warning: "
			"%14.2lf (scan_by_1origin=%04d)\n", tai93[i],i+1);
			st[i].tai93sec = tai93[i];
			st[i].year     = 0;
			st[i].month    = 0;
			st[i].day      = 0;
			st[i].hour     = 0;
			st[i].minute   = 0;
			st[i].second   = 0;
			st[i].ms       = 0;
			st[i].reserve  = 0;
			continue;
		}

		// check leap second & strike
		lcnt = 0;
		flag = 0;
		for(j = lnum - 1; j >= 0; --j){
			if(tai93[i] >= ldat[j].tai93sec){
				lcnt = j + 1;
				break;
			}
			else if(tai93[i] >= ldat[j].tai93sec - 1){
				lcnt = j;
				flag = 1;
				break;
			}
		}

		// convert unix epoch time
		utime = tai93[i] + 725846400 - lcnt;

		// convert struct tm
		stmp = gmtime(&utime);

		// store result in AM2_COMMON_SCANTIME
		st[i].tai93sec = tai93[i];
		st[i].year     = stmp->tm_year + 1900;
		st[i].month    = stmp->tm_mon + 1;
		st[i].day      = stmp->tm_mday;
		st[i].hour     = stmp->tm_hour;
		st[i].minute   = stmp->tm_min;
		st[i].second   = stmp->tm_sec;
		st[i].ms       = (int)((tai93[i] - (long int)tai93[i]) * 1000 + 0.5);
		st[i].reserve  = 0;

		// deal for strike
		if(flag){
			// convert unix epoch time
			utime = tai93[i] + 725846400 - lcnt - 1;

			// convert struct tm
			stmp = gmtime(&utime);

			// store result in AM2_COMMON_SCANTIME
			st[i].tai93sec = tai93[i];
			st[i].year     = stmp->tm_year + 1900;
			st[i].month    = stmp->tm_mon + 1;
			st[i].day      = stmp->tm_mday;
			st[i].hour     = stmp->tm_hour;
			st[i].minute   = stmp->tm_min;
			st[i].second   = 60;
			st[i].ms       = (tai93[i] - (long int)tai93[i]) * 1000 + 0.5;
			st[i].reserve  = 0;
		}
	}

	// free
	free(ldat);
}
