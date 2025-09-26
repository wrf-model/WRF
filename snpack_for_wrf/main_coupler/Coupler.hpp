  #include <snowpack/libsnowpack.h>
  #include <meteoio/MeteoIO.h>
  #include <assert.h>
  #include <iostream>
  #include <string>
  #include <sstream>
  #include <ctime>
  #ifdef _MSC_VER
          /*
          This software contains code under BSD license (namely, getopt for Visu
          Therefore, this product includes software developed by the University 
          California, Berkeley and its contributors when compiling with Visual C
          */
          #include "getopt.h"
  #else
          //#include <unistd.h> //for getopt
          #include <getopt.h> //for getopt_long
  #endif
  
  using namespace std;
  using namespace mio;
  
  #ifdef DEBUG_ARITHM
          #ifndef _GNU_SOURCE
                  #define _GNU_SOURCE
          #endif
          #ifndef __USE_GNU
                  #define __USE_GNU
          #endif
          #include <fenv.h>
  #endif


  /************************************************************
   * static section                                           *
   ************************************************************/
  
  //Global variables in this file:
  static string cfgfile = "io.ini";
  static string mode = "RESEARCH";
  static mio::Date dateBegin, dateEnd;
 // static vector<string> vecStationIDs;

  /// @brief Main control parameters
  struct MainControl
  {
          size_t nStep;        ///< Time step number
          size_t nAvg;         ///< Number of calculation time steps to average 
          size_t HzStep;       ///< Hazard step number (should be half of nStep 
          bool   TsDump;       ///< Flag for time series dump
          bool   HzDump;       ///< Calculation of hazard information will be pe
          bool   PrDump;       ///< Flag for profile dump
          bool   XdataDump;    ///< Backup of Xdata will be performed
          bool   sdbDump;      ///< Dump to data base if required in operational
          bool   resFirstDump; ///< Flag to dump initial state of snowpack
  };

class Slope {

        public:
                Slope(const mio::Config& cfg);

                double prevailing_wind_dir;
                unsigned int nSlopes;
                unsigned int mainStation;  ///< main station, flat field or slope
                unsigned int sector;       ///< main station (0) or current slope sector (1:nSlopes)
                unsigned int first;        ///< first virtual slope station in computing sequence
                unsigned int luv;
                unsigned int lee;
                bool north, south;
                bool snow_erosion, mainStationDriftIndex;
                bool snow_redistribution, luvDriftIndex;

                unsigned int getSectorDir(const double& dir_or_expo) const;
                void setSlope(const unsigned int slope_sequence, SnowStation &vecXdata, double& wind_dir);

        private:
                double sector_width;       ///< width of slope sector: 360./std::max((unsigned)1, nSlopes-1) deg
};

class Cumsum {

        public:
                Cumsum(const unsigned int nSlopes);

                double precip;
                double drift, snow, runoff, rain;
                vector<double> erosion; // Cumulated eroded mass; dumped to file as rate
};


  class SnowpackInterface {

            public:
                   SnowpackInterface();
                   int init_sn(int,double Lat,double Lon,double Altitude,double sn_tsk, double in_calc_step_length, int, 
			       int, int, int, int,int,int,int,int,int,double*,double*,double*,double*,double*,double*,
			       double*,double*,double*,double*,double*,double*,double*,double*,bool,double,double&, 
                               double&, double&, double&);
                   int nextStep(int,double,double,double,double,double,double&,double&,double&,double&,double&,double&,
                                double,double,double,double&,double&,double&,double&,double&,double&,double&,
                                double&, double&, double,double,double,double&, int, int, int, int, double,
                                double&, double&, double&, double&, double&,
                                double&, double&,
                                double&, double&, double&, double&, double&,
                                double&, double&, double&, double&, double*,
			        double*, double*, double*, double*, double*,
                                double*, double*, double*, double*, double*,double*,double*,double*,bool,int&,double, 
                                double&, double&, double&, double&, double&, double&,double&,double&, 
                                double, double, double, double, double);
            private:
                   Snowpack* snowpack;
                   SnowpackConfig cfg;
                   SnowpackIO snowpackio;
                   mio::IOManager io;
                   SN_SNOWSOIL_DATA vecSSdata;
                   SnowStation vecXdata;
                   CurrentMeteo Mdata;
                   SurfaceFluxes surfFluxes;
                   BoundCond sn_Bdata;
                   MainControl mn_ctrl;
                   mio::Timer meteoRead_timer;
                   mio::Timer run_timer;
                   mio::Date current_date; // TO EDIT and/or REMOVE
                   double i_time_zone ;
                   std::string variant ;
                   std::string experiment;
                   std::string outpath    ;
                   bool useSoilLayers      ;
                   bool useCanopyModel      ;
                   double calculation_step_length ;
                   double sn_dt ;
                   int nSolutes;
                   double backup_days_between;
                   double first_backup;
                   bool   snowPrep          ;
                   bool   classify_profile  ;
                   bool   profwrite         ;
                   double profstart         ;
                   double profdaysbetween   ;
                   bool   tswrite           ;
                   double tsstart           ;
                   double tsdaysbetween     ;
                   bool   precip_rates      ;
                   bool   avgsum_time_series;
                   bool   cumsum_mass       ;
                   double thresh_rain       ;
                   bool   advective_heat    ;
                   bool   soil_flux         ;
                   bool   computed_one_timestep;
                   double meteo_step_length ;
                   bool enforce_snow_height ;
                   Slope slope             ; // TO EDIT and/or REMOVE
                   Cumsum cumsum           ;
                   double lw_in ;
                   double wind_scaling_factor ;
                   double time_count_deltaHS ;
                   ZwischenData sn_Zdata;
                   double duration ;
                   int counter;
                   vector<string> vecStationIDs ;
                   int compute_counter;
                   int write_counter  ;
                   double tmp_HFX;
                   double tmp_QFX;
                   double TA_avg      ;
                   double RH_avg      ;
                   double ISWR_avg    ;
                   double ILWR_avg    ;
                   double PSFC_avg    ;
                   double PSUM_avg    ;
                   double VW_avg      ;
                   double TA_final    ;   
                   double RH_final    ;  
                   double ISWR_final  ;  
                   double ILWR_final  ;                  
                   double PSFC_final  ;  
                   double PSUM_final  ;  
                   double VW_final    ;  
                   int loc_snpack_lay_to_sav ;
  };

void editMeteoData(mio::MeteoData& md, const string& variant, const double& thresh_rain);

bool validMeteoData(const mio::MeteoData& md, const string& StationName, const string& variant, const bool& enforce_snow_height, const bool& advective_heat, const bool& soil_flux, const unsigned int& nslopes);

void copyMeteoData(const mio::MeteoData& md, CurrentMeteo& Mdata,
                     const double prevailing_wind_dir, const double wind_scaling_factor);

double getHS_last3hours(mio::IOManager &io, const mio::Date& current_date);

void setShortWave(CurrentMeteo& Mdata, const SnowStation& Xdata, const bool& iswr_is_net);

void dataForCurrentTimeStep(CurrentMeteo& Mdata, SurfaceFluxes& surfFluxes, SnowStation &vecXdata,
                              const Slope& slope, SnowpackConfig& cfg,
//                              SunObject &sun,
                              double& precip, const double& lw_in, const double hs_a3hl6,
                              double& tot_mass_in,
                              const std::string& variant);

void getOutputControl(MainControl& mn_ctrl, const mio::Date& step, const mio::Date& sno_step,
                        const double& calculation_step_length,
                        const double& tsstart, const double& tsdaysbetween,
                        const double& profstart, const double& profdaysbetween,
                        const double& first_backup, const double& backup_days_between);

bool readSlopeMeta(mio::IOManager& io, SnowpackIO& snowpackio, SnowpackConfig& cfg, const size_t& i_stn,
                     Slope& slope, mio::Date &current_date, SN_SNOWSOIL_DATA &vecSSdata,
                     SnowStation &vecXdata, ZwischenData &sn_Zdata, CurrentMeteo& Mdata,
                     double &wind_scaling_factor, double &time_count_deltaHS, double &Lat, double &Lon, double &Altitude);

void addSpecialKeys(SnowpackConfig &cfg);

void writeForcing(Date d1, const Date& d2, const double& Tstep, IOManager &io);

void printStartInfo(const SnowpackConfig& cfg, const std::string& name);


