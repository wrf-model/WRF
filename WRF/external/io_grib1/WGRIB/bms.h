/* version 1.2 of grib headers  w. ebisuzaki */

#define BMS_LEN(bms)		((bms) == NULL ? 0 : (bms[0]<<16)+(bms[1]<<8)+bms[2])
#define BMS_UnusedBits(bms)	((bms) == NULL ? 0 : bms[3])
#define BMS_StdMap(bms)		((bms) == NULL ? 0 : ((bms[4]<<8) + bms[5]))
#define BMS_bitmap(bms)		((bms) == NULL ? NULL : (bms)+6)
#define BMS_nxny(bms)		((((bms) == NULL) || BMS_StdMap(bms)) \
	? 0 : (BMS_LEN(bms)*8 - 48 - BMS_UnusedBits(bms)))
