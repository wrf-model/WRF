#ifdef __cplusplus
extern "C" {
    class SnowpackInterface;
    typedef SnowpackInterface COUPLER;
#else
    typedef struct COUPLER COUPLER;
#endif

COUPLER* create_coupler(int in_size);
COUPLER* allot_add_coupler(COUPLER* in_pointer, int loc);
int init_data_coupler(COUPLER* coupler, int,double Lat, double Lon, double Altitude, double sn_tsk, double in_calc_step_length, int f_counter, int in_grid,
                  int I, int J, int, int, int, int, int,int,double*,double*,double*,double*,double*,double*,
                  double*,double*,double*,double*,double*,double*,double*,double*,bool,double,double&,double&,double&,double&);

int run_coupler(COUPLER* coupler, int, double, double, double, double, double, double&, double&, double&, double&,double&, double&, double, double, double, 
            double&,double&,double&,double&,double&,double&,double&,double&, double&, double, double, double, double&, int, int, int, int, double,
            double&,double&,double&,double&,double&,double&,double&,double&,double&,double&,double&,double&,double&,double&,double&,double&,double*,
	    double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,bool,int&,double, 
            double&, double&, double&, double&, double&, double&, double&,double&,double, double, double, double, double);
#ifdef __cplusplus
}
#endif
