#define _PDSTOOL_

/* 10/2/99 fixed P_dec_scale(n), P_def_dec_scale
   10/5/99 fixed P_unused(num_unused)
   11/23/99 fixed P_p1p2(time)
   10/04/00 fixed P_hgt_gnd(m) P_hgt_msl(m) P_sigma(sig) P_depth_sl(m)
 */


unsigned char *pdstool(unsigned char *pds, ...);

enum p_tool {p_end, p_init, p_byte, p_2bytes, p_s2bytes, 
   p_3bytes, p_s3bytes, p_4bytes, p_bit, p_and, p_or};


/* choose one */

/* new standard PDS */
#define New_PDS				NULL,p_init,28,0

/* longer than standard PDS */
#define New_ext_PDS(pdslen)		NULL,p_init,pdslen,0

/* new standard PDS, debug version */
#define New_PDS_debug			NULL,p_init,28,255

/* longer than standard PDS, debug version*/
#define New_ext_PDS_debug(pdslen)	NULL,p_init,pdslen,255


/* choose one */

#define P_param_table(table)		p_byte,3,table


/* choose one set */

#define P_center(center)		p_byte,4,center
#define P_subcenter(subcenter)		p_byte,25,subcenter

#define P_Center(center,subcenter)	P_center(center),P_subcenter(subcenter)

#define NCEP_opn			P_center(7),P_subcenter(0)

#define NCEP_reanl			P_center(7),P_subcenter(1)

#define NCEP_ensemble			P_center(7),P_subcenter(2)

#define ECMWF_opn			P_center(98),P_subcenter(0)

#define NESDIS_ORA			P_center(7),P_subcenter(12)

/* choose one */

#define P_process(process)		p_byte,5,process

#define P_NGM				P_process(39)


/* usually set by other routines */

#define P_grid(grid)			p_byte,6,grid
#define P_has_gds(bool)			p_bit,(7<<3)+7,bool
#define P_has_bms(bool)			p_bit,(7<<3)+6,bool


/* choose one */

#define P_param(type)			p_byte,8,type


/* choose one */

#define P_sfc				p_3bytes,9,0x010000

#define P_cloud_base			p_3bytes,9,0x020000

#define P_cloud_top			p_3bytes,9,0x030000

#define P_zeroC				p_3bytes,9,0x040000

#define P_ad_conden_lvl			p_3bytes,9,0x050000

#define P_max_wind_lvl			p_3bytes,9,0x060000

#define P_tropopause			p_3bytes,9,0x070000

#define P_top_atmos			p_3bytes,9,0x080000

#define P_sea_bottom			p_3bytes,9,0x090000

#define P_mb(mb)			p_byte,9,100,p_2bytes,10,(int) (mb+0.5)

#define P_hPa(hPa)			p_byte,9,100,p_2bytes,10,(int) (hPa+0.5)

#define P_prs_layer(top_kPa,bot_kPa)	p_byte,9,101,p_byte,10,(int)(top_kPa+0.5),p_byte,11,(int)(bot_kPa+0.5)

#define P_msl				p_3bytes,9,102<<16	/* mean sea level */

#define P_hgt_msl(m)			p_byte,9,103,p_2bytes,10,(int) (m+0.5)	/* height above msl */

#define P_hgt_layer_msl(top_hm,bot_hm)	p_byte,9,104,p_byte,10,(int)(top_hm+0.5),p_byte,11,(int)(bot_hm+0.5)

#define P_hgt_gnd(m)			p_byte,9,105,p_2bytes,10,(int) (m+0.5) /* height above ground */

#define P_hgt_layer_gnd(top_hm,bot_hm)	p_byte,9,106,p_byte,10,(int)(top_hm+0.5),p_byte,11,(int)(bot_hm+0.5)

#define P_sigma(sig)			p_byte,9,107,p_2bytes,10,(int) floor((sig)*10000+0.5)

#define P_hgt_layer_below_gnd(top_hm,bot_hm)	p_byte,9,112,p_byte,10,(int)(top_hm+0.5),p_byte,11,(int)(bot_hm+0.5)

#define P_eta(sig)			p_byte,9,119,p_2bytes,10,(int) ((sig)*10000+0.5)

#define P_depth_sl(m)			p_byte,9,160,p_2bytes,10,(int) (m+0.5)

#define P_atmos_clm			p_3bytes,9,200<<16	/* atmospheric column */

#define P_ocean_clm			p_3bytes,9,201<<16	/* oceanic column */

/* choose one set */

#define P_year(year)		p_byte,12,(((year)-1)%100)+1,p_byte,24,((year)-1)/100+1
#define P_month(month)		p_byte,13,(month)%100
#define P_day(day)		p_byte,14,(day)%100
#define P_hour(hour)		p_byte,15,(hour)%100
#define P_minute(min)		p_byte,16,(min)%100

#define P_date(date)		P_minute(0),P_day(date),P_month((date)/100),P_year((date)/10000)
/* #define P_hour(hour)		p_byte,15,(hour)%100 */


/* choose one set */

#define P_fcst_unit(unit)	p_byte,17,unit
#define P_p1(time1)		p_byte,18,(int) (time1)
#define P_p2(time2)		p_byte,19,(int) (time2)
#define P_time_range(time_rng)	p_byte,20,(int) (time_rng)
#define P_used(num_used)	p_2bytes,21, (int) (num_used)
/* P_unused will be dropped */
#define P_unused(num_unused)	p_byte,23, (int) (num_unused)
#define P_missing(num_unused)	p_byte,23, (int) (num_unused)

#define P_fcst_unit(unit)	p_byte,17,unit
#define P_p1p2(time)		p_2bytes,18,(int) (time)
#define P_time_range(time_rng)	p_byte,20,(int) (time_rng)

#define P_ext_fcst_unit_1(unit) p_byte,40,unit
#define P_ext_P1(P1)            p_4bytes,41,P1
#define P_ext_fcst_unit_2(unit) p_byte,45,unit
#define P_ext_P2(P2)            p_4bytes,46,P2
#define P_ext_time_range(range) p_byte,50,range

/* also need the following
#define P_used(num_used)	p_2bytes,21,(int) (num_used)
#define P_missing(num_unused)	p_byte,23,(int) (num_unused)
*/

#define P_fcst_hr(hours)	P_fcst_unit(1),P_p1p2(hours),P_time_range(10),p_3bytes,21,0

#define P_uninit_analysis	P_fcst_unit(1),P_p1p2(0),P_time_range(0),p_3bytes,21,0

#define P_init_analysis		P_fcst_unit(1),P_p1p2(0),P_time_range(1),p_3bytes,21,0

#define P_ave_hr(hr0,hr1)	P_fcst_unit(1),P_p1((int) hr0),P_p2((int) hr1),P_time_range(3),p_3bytes,21,0

#define P_ave_dy(dy0,dy1)	P_fcst_unit(2),P_p1((int) dy0),P_p2((int) dy1),P_time_range(3),p_3bytes,21,0

#define P_ave_mon(mon0,mon1)	P_fcst_unit(3),P_p1((int) mon0),P_p2((int) mon1),P_time_range(3),p_3bytes,21,0

#define P_acc_hr(hr0,hr1)	P_fcst_unit(1),P_p1((int) hr0),P_p2((int) hr1),P_time_range(4),p_3bytes,21,0

#define P_acc_dy(dy0,dy1)	P_fcst_unit(2),P_p1((int) dy0),P_p2((int) dy1),P_time_range(4),p_3bytes,21,0

#define P_acc_mon(mon0,mon1)	P_fcst_unit(3),P_p1((int) mon0),P_p2((int) mon1),P_time_range(4),p_3bytes,21,0

/* choose one */

#define P_dec_scale(n)		p_s2bytes,26,n
#define P_def_dec_scale		p_s2bytes,26,0

/* must include */

#define P_end			p_end
