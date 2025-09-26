#include "coupler.h"
#include "Coupler.hpp"

#include <iostream>
using namespace std;
using namespace mio;

COUPLER* create_coupler(int in_size){
//     cout << "C API, creating a new object" << endl;
     COUPLER* kkk = new SnowpackInterface[in_size];
     double* ttt = new double;
//     cout << kkk << "\t" << sizeof(ttt) << endl;
     return kkk ; // new SnowpackInterface [in_size];
}

COUPLER* allot_add_coupler(COUPLER* in_type, int loc){
//     cout << in_type << "\t" << loc << "\t" << in_type+loc << endl;
     return in_type+loc;
}

int init_data_coupler(COUPLER* coupler,int snpack_layers_to_save,double Lat, double Lon, double Altitude, double sn_tsk, double in_calc_step_length, 
                  int f_counter, int in_grid, int I, int J,
                  int yr, int month, int day, int hour, int minute,int snpack_nlayers, double* arr_T,double* arr_thick,double* arr_volI,double* arr_volW,
                  double* arr_volV,double* arr_rg,double* arr_rb,double* arr_dd,double* arr_sp,double* arr_cdot,double* arr_meta,double* arr_depd,
                  double* arr_graintype, double* arr_mk, bool start_from_file,double wrf_rho, 
                  double& SNOWH, double& SNOW, double& snpack_dt,double& snpack_write_dt ){
    return coupler->init_sn(snpack_layers_to_save,Lat,Lon,Altitude,sn_tsk,in_calc_step_length,f_counter, in_grid, I, J,yr,month,day,hour,minute,
                        snpack_nlayers,arr_T,arr_thick,arr_volI,arr_volW,arr_volV,arr_rg,arr_rb,arr_dd,arr_sp,arr_cdot,
			arr_meta,arr_depd,arr_graintype,arr_mk,start_from_file,wrf_rho,SNOWH,SNOW,snpack_dt,snpack_write_dt);
}

int run_coupler(COUPLER* coupler,int xxx,double h_of_met_vals,double l_TA,double l_RH, double l_VW, 
            double l_VW_MAX,double& l_DW,double& l_drift,double& l_conc,double& l_csalt,
            double& l_q_lb, double& l_N_lb,
            double l_iswr, double l_ilwr,double l_psum,
            double& TSK, double& HFX, double& QFX, double& ALBEDO,
            double& SNOWC, double& SNOW, double& SNOWH, double& l_in_q_corr, double& l_in_N_corr,
            double log_z_z0, double in_QI, double in_QNI, double& ust_wrf, int f_counter, int grid_id, int I, int J, double PSFC,
            double& m_budg_precip,double& m_budg_erosion,double& m_budg_sublim,double& m_budg_deposit,double& m_budg_swe,
            double& m_budg_melt, double& m_budg_refreeze,
            double& e_budg_ilwr_in,double& e_budg_ilwr_out,double& e_budg_sw_in, double& e_budg_sw_out,double& e_budg_sensible,   
            double& e_budg_latent, double& e_budg_lower_bc, double& e_budg_raine, double& e_budg_totale, double* arr_T,
	    double* arr_thick,double* arr_volI,double* arr_volW,double* arr_volV,double* arr_rg,double* arr_rb,
	    double* arr_dd,double* arr_sp,double* arr_cdot,double* arr_meta,double* arr_depd,double* arr_graintype, double* arr_mk,
            bool bs_bool,int& sn_nlayer, double wrf_rho, double& bs_bdg_total, 
            double& qi_in, double& qni_in,double& bs_K, double& bs_mass_turb, 
            double& bs_number_turb, double& in_hsalt, double& psi_s, double loc_sza,double tau_qc,double tau_qi,
            double tau_qc_tot, double tau_qi_tot )
{

//     cout << "C API, run the model" << endl;
     return coupler->nextStep(xxx,h_of_met_vals,l_TA,l_RH,l_VW,l_VW_MAX,l_DW,l_drift,l_conc,l_csalt,l_q_lb,l_N_lb,l_iswr,
                          l_ilwr,l_psum,TSK,HFX,QFX,ALBEDO,SNOWC,SNOW,SNOWH,l_in_q_corr,l_in_N_corr,log_z_z0,
                          in_QI,in_QNI,ust_wrf,f_counter,grid_id, I, J, PSFC,
            m_budg_precip,m_budg_erosion,m_budg_sublim,m_budg_deposit,m_budg_swe,
            m_budg_melt,m_budg_refreeze,
            e_budg_ilwr_in,e_budg_ilwr_out,e_budg_sw_in, e_budg_sw_out,e_budg_sensible,   
            e_budg_latent, e_budg_lower_bc,e_budg_raine, e_budg_totale,arr_T,arr_thick,arr_volI,arr_volW,
	    arr_volV,arr_rg,arr_rb,arr_dd,arr_sp,arr_cdot,arr_meta,arr_depd,arr_graintype,arr_mk,bs_bool,sn_nlayer,wrf_rho,
            bs_bdg_total,qi_in,qni_in,bs_K,bs_mass_turb,bs_number_turb,in_hsalt,psi_s,loc_sza,tau_qc,tau_qi,tau_qc_tot,
            tau_qi_tot );
}
