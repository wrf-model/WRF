/* v0.1 w. ebisuzaki */
/* need to put in all values */

/* used by TimeRange */
#define MINUTE	0
#define HOUR	1
#define DAY	2
#define MONTH	3
#define YEAR	4
#define DECADE	5
#define NORMAL	6
#define CENTURY	7
#define SECOND	254

#define set_lev_sfc(pds)		set_PDSlevel(pds,1,0)
#define set_lev_toa(pds)		set_PDSlevel(pds,8,0)

#define set_lev_mb(pds,lev)		set_PDSlevel(pds,100,lev)
#define set_lev_hp(pds,lev)		set_PDSlevel(pds,100,lev)
#define set_lev_mb2mb(pds,top,bottom)	set_PDSlevel(pds,101,((int) top/10)*256+ (int) bottom/10)
#define set_lev_hp2hp(pds,top,bottom)	set_PDSlevel(pds,101,((int) top/10)*256+ (int) bottom/10)
#define set_lev_msl			set_PDSlevel(pds,102,0)
#define set_lev_m_MSL(pds,meters)	set_PDSlevel(pds,103,meters)
#define set_lev_m2m_MSL(pds,top,bot)	set_PDSlevel(pds,104,((int) top/10)*256+(int) bot/10)
#define set_lev_m_gnd(pds,meters)	set_PDSlevel(pds,105,meters)
#define set_lev_m2m_gnd(pds,top,bot)	set_PDSlevel(pds,106,((int) top/10)*256+(int) bot/10)
#define set_lev_sigma(pds,sigma)	set_PDSlevel(pds,107,(int) (sigma*10000))
#define set_lev_cm_ugnd(pds,cm)         set_PDSlevel(pds,111,(int) cm)
#define set_lev_cm2cm_ugnd(pds,top,bot) set_PDSlevel(pds,112,(int) top * 256 + (int) bot)
#define set_lev_K(pds,pot_tmp)		set_PDSlevel(pds,113, pot_tmp)
#define set_lev_clm(pds)		set_PDSlevel(pds,200,0)
#define set_lev_ocean(pds)		set_PDSlevel(pds,201,0)

/* tr p1 p2 unit nave nmissing */
#define set_Analysis(pds)		set_TimeRange(pds,0,0,0,1,0,0)
#define set_UninitAnalysis(pds)		set_TimeRange(pds,0,0,0,1,0,0)
#define set_InitAnalysis(pds)		set_TimeRange(pds,0,0,0,1,0,0)
#define set_Average(pds,p1,p2,unit,nave,nmissing)	set_TimeRange(pds,3,p1,p2,unit,nave,nmissing)
#define set_Accumulation(pds,p1,p2,unit,nave,nmissing)	set_TimeRange(pds,4,p1,p2,unit,nave,nmissing)
#define set_Forecast(pds,time,unit)	set_TimeRange(pds,10,time>>8,time&255,unit,0,0)

#define set_Climo(pds,dt,unit,nyear)	set_TimeRange(pds,51,0,dt,unit,nyear,0)
#define set_DiurnalClimo(pds,dt,unit,nyear)	set_TimeRange(pds,51,1,dt,unit,nyear,0)

#define set_AveForecast(pds,p1,p2,unit,nave,nmissing)	set_TimeRange(pds,113,p1,p2,unit,nave,nmissing)
#define set_AveAnalysis(pds,p2,unit,nave,nmissing)	set_TimeRange(pds,113,0,p2,unit,nave,nmissing)
#define set_AccumFcst(pds,p1,p2,unit,nave,nmissing)	set_TimeRange(pds,113,p1,p2,unit,nave,nmissing)
#define set_AccumAnalysis(pds,p2,unit,nave,nmissing)	set_TimeRange(pds,113,0,p2,unit,nave,nmissing)

#define set_Variance(pds,p1,dt,nave,nmissing)	set_TimeRange(pds,118,p1,dt,unit,nave,nmissing)
