/*
*  PROFEVAL stand-alone
*
*  Copyright WSL Institute for Snow and Avalanche Research SLF, DAVOS, SWITZERLAND
*/
/*  This file is part of Snowpack.
Snowpack is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Snowpack is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Snowpack.  If not, see <http://www.gnu.org/licenses/>.
*/
#include <snowpack/libsnowpack.h>
#include <meteoio/MeteoIO.h>

#include <iostream>
#include <string>
#include <sstream>

using namespace std;
using namespace mio;


/**
 *                        MICHAEL LEHNING and CHARLES FIERZ
 *                              SLF-DAVOS/WSL-BIRMENSDORF
 *
 *                           ProfEval_MIP.c - Version 9.0                                   */
/*                To be used for SnowMIP Project and ProfEval tests                            */
/*  Based on ProfEval_IGS03.c - V3.0, ProfEval_MIP.c - V6.0, ProfEval.c - V_r(9.0)             */
/*                  ProfEval algorithm: from ProfEval.c - V_r(9.0)                             */
/*                                                                                             */
/***********************************************************************************************/
/*---------------------------------------------------------------------------------------------+
| This module contains the driving routine for the Profile Comparison Program                 |
+---------------------------------------------------------------------------------------------*/
/***********************************************************************************************/
/***********************************************************************************************/
/*  Swiss Federal Institute for Snow and Avalanche Research    SLF-DAVOS                       */
/***********************************************************************************************/
/*---------------------------------------------------------------------------------------------+
| This program will help the SnowMIP party to compare measured pit (MAster)                   |
| profiles with simulated (SLave) profiles (Pit-Model-mode). Note that at least               |
| two slave values are required for profile measurement comparison.                           |
| Thus, to compare one-layer models, master and slave may be inverted                         |
| (Model-Pit-mode).                                                                           |
| Includes the possibility to include  surface and ground temperatures in the Tprf comparison |
| well as the possibility to compare models with models (MM-mode and MM121-mode; note that    |
| MM121-mode is for test purposes only with MAster == SLave).                                 |
| NEW Version 7.0                                                                             |
| a) SNOWPACK [SNP] output (tabular form, full or aggr) can be used in Pit-Model-mode,        |
|      as well as SnowMIP output from SNOWPACK [SNO] for CDP.                                 |
| b) Possibility to print out layer property distances layer by layer (search for prn).       |
| c) Possibility to do Pit-Pit comparisons (PP-mode; for PP121-mode see above).               |
+---------------------------------------------------------------------------------------------*/

/// @name Switches set by the code
//@{
	int NO_TYP;         ///< Checks whether TypDistance has to be calculated
	int NO_SIZ;         ///< Checks whether SizDistance has to be calculated
	int NO_WET;         ///< Checks whether WatDistance has to be calculated
	int NO_HRD;         ///< Checks whether HrdDistance has to be calculated
//@}

/**
 */
int peOnly_ReadModelProfile(PROFILE *ModP, char *ModLab, char *fname,
			   long *ModPDate, long *DateToMatch,
			   long *MMreadMA)
/*---------------------------------------------------------------------------------------------+ 
 |                                                           |
 +---------------------------------------------------------------------------------------------*/
{
  FILE *fp;

  char   stn[MAX_STRING_LENGTH], line_dum[MAX_LINE_LENGTH];
  int    ReadAllNow = 0;
  int    i = 0, ncol = 0;
  int    YYYY, MM, DD, HH;
  double add_dhl, min_dhl = 1.e-4;
  double type;

  // Open SnowMIP 3h-profile or SNOWPACK full/aggr.pro file
  if( (fp=fopen(fname, "r")) == 0 )
    {
      prn_msg(__FILE__, __LINE__, "err", -1., 
	      "Cannot open model file %s", fname);
      return(ERROR);
    }

  ModP->nL = ModP->nHW = ModP->nRHO = ModP->nTH_W = ModP->nT = 0;
  ModP->hs = 0.;

  // 1. SEARCH MATCHING PROFILE
  // Check DateToMatch
  if( *MMreadMA )
    {
      if( *DateToMatch == 0 )
	*DateToMatch = FIRST_MM_PROFILE;
      else if( (*MMreadMA < 0) || (*DateToMatch > (LAST_MM_PROFILE - 3)) )
	{
	  *DateToMatch = 0;
	  fclose(fp);
	  return(NO_ERROR);
	}
    }
  else if( *DateToMatch == 0 )
    { 
      *ModPDate = 0;
      fclose(fp);
      return(NO_ERROR);
    }
  // Search file for profile date
  if( strcmp(ModLab, "SNP") != 0 )
    {
      // 1.a Search SnowMIP 3h-profile file
      for(i=0; i<2; i++)
	fgets(line_dum, MAX_LINE_LENGTH, fp); // Skip Header
      if( strcmp(ModLab, "VIS") != 0 )
	{
	  do{
	    if( strcmp(ModLab, "SNO") == 0 )
	      {
		ncol = fscanf(fp,"%*f, %*d %ld %lf %d\n", ModPDate, &ModP->hs, &ModP->nL);
		while ( !ReadAllNow && ModP->nL == 0 && ncol > 1 )
		  ncol = fscanf(fp,"%*f, %*d %ld %lf %d\n", ModPDate, &ModP->hs, &ModP->nL);
	      }
	    else if( strcmp(ModLab, "ACA") != 0 && strcmp(ModLab, "SWA") != 0 )
	      {
		ncol = fscanf(fp,"%*d %ld %lf %d\n", ModPDate, &ModP->hs, &ModP->nL);
		while ( !ReadAllNow && ModP->nL == 0 && ncol > 1 )
		  ncol = fscanf(fp,"%*d %ld %lf %d\n", ModPDate, &ModP->hs, &ModP->nL);
	      }
	    else
	      {
		ncol = fscanf(fp,"%*d %ld %lf %d\n", ModPDate, &ModP->hs, &ModP->nL);
	      }
	    if( (abs(*ModPDate - *DateToMatch) > 1) && (*ModPDate < (*DateToMatch - 1)) && ncol > 1 )
	      for(i=0; i<ModP->nL; i++)
		fgets(line_dum, MAX_LINE_LENGTH, fp);
	    else if( *ModPDate > *DateToMatch )
	      *DateToMatch = *ModPDate;
	    ReadAllNow = 1;
	  } while ( (abs(*ModPDate - *DateToMatch) > 1) && ncol > 1 );

	  if( *MMreadMA && !feof(fp) )
	    {
	      for(i=0; i<ModP->nL; i++)
		fgets(line_dum, MAX_LINE_LENGTH, fp);
	      if( strcmp(ModLab, "SNO") == 0 )
		fscanf(fp,"%*f, %*d %ld %lf %d\n", ModPDate, &ModP->hs, &ModP->nL);
	      else
		fscanf(fp,"%*d %ld %lf %d\n", ModPDate, &ModP->hs, &ModP->nL);
	      *DateToMatch = *ModPDate;
	    }
	}
      else // Read awkward VIS-Format
	{
	  do{
	    ncol = fscanf(fp,"%*d %ld %d %lf ", ModPDate, &ModP->nL, &ModP->hs);
	    while ( !ReadAllNow && (ModP->nL == 0 && ncol > 1) )
	      {
		fscanf(fp, " %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f\n");
		ncol = fscanf(fp,"%*d %ld %d %lf ", ModPDate, &ModP->nL, &ModP->hs);
	      }
	    if( (abs(*ModPDate - *DateToMatch) > 1) && (*ModPDate < (*DateToMatch -1)) && ncol > 1 )
	      fscanf(fp, " %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f\n");
	    else if( *ModPDate > *DateToMatch )
	      *DateToMatch = *ModPDate;
	    ReadAllNow = 1;
	  } while ( (abs(*ModPDate - *DateToMatch) > 1) && ncol > 1 );

	  if( *MMreadMA && !feof(fp) )
	    {
	      fscanf(fp, " %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f %*f\n");
	      fscanf(fp,"%*d %ld %d %lf ", ModPDate, &ModP->nL, &ModP->hs);
	      *DateToMatch = *ModPDate;
	    }
	}
    } // end matching date in SnowMIP file
  else
    {
      // 1.b Search SNOWPACK full/aggr.pro for profile date
      //   stn,Termin,julian,step,nlay,aspect,slope,hs,swe,ts,tg
      if( *MMreadMA > FIRST_MM_PROFILE )
	*DateToMatch = *MMreadMA;
      *ModPDate = 0;
      while ( (abs(*DateToMatch - *ModPDate) > 1) && !feof(fp) )
	{
	  ModP->nL = 0;
	  while ( ModP->nL == 0 && !feof(fp) )
	    {
	      fgets(line_dum, MAX_LINE_LENGTH, fp);
	      fgets(line_dum, MAX_LINE_LENGTH, fp);
	      ncol = sscanf(line_dum, "%[^,]", stn);
	      if( strcmp(stn, "CDP") != 0 )
		{
		  ncol = sscanf(line_dum, "%*[^,],%d.%d.%d %d:%*d,%*f,%*f,%d,%*f,%*f,%lf",
				&DD, &MM, &YYYY, &HH, &ModP->nL, &ModP->hs);
		  *ModPDate = (long) (YYYY*1.e6 + MM*1.e4 + DD*1.e2 + HH);
		  ModP->hs = CM_TO_M(ModP->hs);
		}
	      else
		// Read SnowMIP output for CDP integrated in SNP-file
		ncol = sscanf(line_dum, "%*[^,],%*f, %*d %ld %lf %d", ModPDate, &ModP->hs, &ModP->nL);
	      fgets(line_dum, MAX_LINE_LENGTH, fp);
	    }
	  if( feof(fp) )
	    break;
	  if( *MMreadMA && (*ModPDate >= *DateToMatch) )
	    *DateToMatch = *ModPDate;
	  for(i=0; i<6; i++)
	    fgets(line_dum, MAX_LINE_LENGTH, fp);
	  if( abs(*DateToMatch - *ModPDate) > 1 )
	    for(i=0; i<ModP->nL+1; i++)
	      fgets(line_dum, MAX_LINE_LENGTH, fp);
	}

      if( feof(fp) )
	{
	  prn_msg(__FILE__, __LINE__, "msg", -1., 
		  "EOF reached in %s; Last DateToMatch is %ld\n",
		  fname, *DateToMatch);
	  ModP->nL = ModP->nHW = ModP->nRHO = ModP->nTH_W = ModP->nT = 0;
	  *DateToMatch = 0;
	  fclose(fp);
	  return(NO_ERROR);
	}
      else 
	ncol = 2;
    } // end matching date in full/aggr.pro file

  if( (*MMreadMA && ModP->hs <= 0.) || (ModP->nL == 0) || (ncol <= 1) )
    {
      if( ncol > 1 )
	{
	  *ModPDate = *DateToMatch;
	  if( ModP->nL == 0 )
	    {
	      ModP->hs = 0.;
	      NO_TYP = NO_SIZ = NO_WET = NO_HRD = 1;
	    }
	}
      else
	*ModPDate = 0;
      fclose(fp);
      return(NO_ERROR);
    }


  // 2. ALLOCATE SPACE
  ModP->nHW = ModP->nRHO = ModP->nTH_W = ModP->nT = ModP->nL;

  if( !(ModP->lay=(LAYER *) malloc((ModP->nL+2)*sizeof( LAYER ))) )  
    {
      prn_msg(__FILE__, __LINE__, "err", -1., 
	      "Cannot allocate ModP_LAYER data");
      exit(ERROR);
    }
  memset(ModP->lay, 0, sizeof(LAYER)*ModP->nL+2);
  if( !(ModP->hw=(BLK *) malloc((ModP->nHW+2)*sizeof( BLK ))) )  
    {
      prn_msg(__FILE__, __LINE__, "err", -1., 
	      "Cannot allocate ModP_HW data");
      exit(ERROR);
    }
  memset(ModP->hw, 0, sizeof(BLK)*ModP->nHW+2);
  if( !(ModP->rho=(PNT *) malloc((ModP->nRHO+2)*sizeof( PNT ))) ) 
    {
      prn_msg(__FILE__, __LINE__, "err", -1., 
	      "Cannot allocate ModP_RHO data");
      exit(ERROR);
    }
  memset(ModP->rho, 0, sizeof(PNT)*ModP->nRHO+2);
  if( !(ModP->th_w=(PNT *) malloc((ModP->nTH_W+2)*sizeof( PNT ))) ) 
    {
      prn_msg(__FILE__, __LINE__, "err", -1., 
	      "Cannot allocate ModP_THW data");
      exit(ERROR);
    }
  memset(ModP->th_w, 0, sizeof(PNT)*ModP->nTH_W+2);
  if( !(ModP->T=(PNT *) malloc((ModP->nT+3)*sizeof( PNT ))) )
    {
      prn_msg(__FILE__, __LINE__, "err", -1., 
	      "Cannot allocate ModP_T data");
      exit(ERROR);
    }
  memset(ModP->T, 0, sizeof(PNT)*ModP->nT+3);


  // 3. READ PROFILE

  NO_TYP = NO_SIZ = NO_WET = NO_HRD = 0;
  ModP->lay[ModP->nL].hl = 0.;

  if( strcmp(ModLab, "SNP") != 0 )
    {
      // 3.a Full standard SnowMIP format:
      //  First layer <=> Bottom layer
      //  JD Date HL dHL HW LWC T GSZ F1 F2 N3 BSZ DD SP HIST LAYDATE
      if( strcmp(ModLab, "VIS") != 0 )
	{
	  if( strcmp(ModLab, "ACA") == 0 )
	    {
	      NO_TYP = NO_SIZ = NO_WET = NO_HRD = 1;
	      for(i=ModP->nL-1; i>=0; i--)
		{
		  fscanf(fp, "%*d %*d %lf %lf %*f %*f %lf %*f %*d %*d %*f %*f %*f %*f %*d %*d\n",
			 &ModP->lay[i].hl, &ModP->lay[i].dhl, &ModP->T[i].p_val);
		}
	    }
	  else if( strcmp(ModLab, "CRO") == 0 )
	    {
	      NO_HRD = 1;
	      for(i=ModP->nL-1; i>=0; i--)
		{
		  fscanf(fp, "%*d %*d %lf %lf %lf %lf %lf %lf %d %d %*f %*f %*f %*f %*d %*d\n",
			 &ModP->lay[i].hl, &ModP->lay[i].dhl, &ModP->hw[i].b_val, &ModP->lay[i].theta_w,
			 &ModP->T[i].p_val, &ModP->lay[i].size, &ModP->lay[i].F1, &ModP->lay[i].F2);
		  ModP->lay[i].size = MAX(0.3, ModP->lay[i].size);
		}
	    }
	  else if( strcmp(ModLab, "CSI") == 0 )
	    {
	      NO_TYP = NO_SIZ = NO_WET = NO_HRD = 1;
	      for(i=ModP->nL-1; i>=0; i--)
		{
		  fscanf(fp, "%*d %*d %*f %lf %lf %*f %lf %*f %*d %*d %*f %*f %*f %*f %*d\n",
			 &ModP->lay[i].dhl, &ModP->hw[i].b_val,
			 &ModP->T[i].p_val);
		  ModP->lay[i].hl = ModP->lay[i+1].hl + ModP->lay[i].dhl;
		}
	    }
	  else if( strcmp(ModLab, "ISB") == 0 )
	    {
	      NO_TYP = NO_HRD = 1;
	      for(i=ModP->nL-1; i>=0; i--)
		fscanf(fp, "%*d %*d %lf %lf %lf %lf %lf %lf %*d %*d %*f %*f %*f %*f %*d %*f\n",
		       &ModP->lay[i].hl, &ModP->lay[i].dhl, &ModP->hw[i].b_val, &ModP->lay[i].theta_w,
		       &ModP->T[i].p_val, &ModP->lay[i].size);
	    }
	  else if( strcmp(ModLab, "SNO") == 0 )
	    {
	      NO_HRD = 1;
	      for(i=ModP->nL-1; i>=0; i--)
		fscanf(fp, "%*d %*d %lf %lf %lf %lf %lf %lf %d %d %*f %*f %*f %*f %*d %*d\n",
		       &ModP->lay[i].hl, &ModP->lay[i].dhl, &ModP->hw[i].b_val, &ModP->lay[i].theta_w,
		       &ModP->T[i].p_val, &ModP->lay[i].size, &ModP->lay[i].F1, &ModP->lay[i].F2);
	    }
	  else if( strcmp(ModLab, "SNT") == 0 )
	    {
	      NO_TYP = NO_HRD = 1;
	      for(i=ModP->nL-1; i>=0; i--)
		{
		  fscanf(fp, "%*d %*d %*f %lf %lf %lf %lf %lf %d %d %*f %*f %*f %*f %*d %*d\n",
			 &ModP->lay[i].dhl, &ModP->hw[i].b_val, &ModP->lay[i].theta_w,
			 &ModP->T[i].p_val, &ModP->lay[i].size, &ModP->lay[i].F1, &ModP->lay[i].F2);
		  ModP->lay[i].hl  = ModP->lay[i+1].hl + ModP->lay[i].dhl;
		}
	    }
	  else if( strcmp(ModLab, "SWA") == 0 )
	    {
	      NO_TYP = NO_SIZ = NO_HRD = 1;
	      for(i=ModP->nL-1; i>=0; i--)
		{
		  fscanf(fp, "%*d %*d %lf %lf %lf %lf %lf %*f %*d %*d %*f %*f %*f %*f %*d %*f\n",
			 &ModP->lay[i].hl, &ModP->lay[i].dhl, &ModP->hw[i].b_val, &ModP->lay[i].theta_w,
			 &ModP->T[i].p_val);
		}
	    }
	  else if( strcmp(ModLab, "TSM") == 0 )
	    {
	      NO_SIZ = NO_HRD = 1;
	      for(i=ModP->nL-1; i>=0; i--)
		{
		  fscanf(fp, "%*d %*d %lf %lf %lf %lf %lf %lf %d %d\n",
			 &ModP->lay[i].hl, &ModP->lay[i].dhl, &ModP->hw[i].b_val, &ModP->lay[i].theta_w,
			 &ModP->T[i].p_val, &ModP->lay[i].size, &ModP->lay[i].F1, &ModP->lay[i].F2);
		  if( ModP->lay[i].F2 == 0 )
		    ModP->lay[i].F2 = ModP->lay[i].F1;
		}
	    }
	  else
	    {
	      prn_msg(__FILE__, __LINE__, "err", -1., 
		      "No model of this name (%3s) implemented yet", ModLab);
	      return(ERROR);
	    }
	  // For checks only
	  if( *ModPDate == 1902110209 )
	    printf("\n [pe_check][%04d]: i=%d, hl=%.4lf, dhl=%.4lf",
		   __LINE__, i, ModP->lay[i].hl, ModP->lay[i].dhl);
	}
      else if( strcmp(ModLab, "VIS") == 0 )
	{ // read awkward VIS format !!!
	  NO_TYP = NO_SIZ = NO_HRD = 1;
	  fscanf(fp, " %*f "); // HSW will be re-evaluated below
	  for(i=0; i<ModP->nL; i++) fscanf(fp, " %lf", &ModP->lay[i].dhl);
	  for(i=ModP->nL; i<3; i++) fscanf(fp, " %*f");
	  for(i=0; i<ModP->nL; i++) fscanf(fp, " %lf", &ModP->T[i].p_val);
	  for(i=ModP->nL; i<3; i++) fscanf(fp, " %*f");
	  for(i=0; i<ModP->nL; i++) fscanf(fp, " %lf", &ModP->hw[i].b_val);
	  for(i=ModP->nL; i<3; i++) fscanf(fp, " %*f");
	  for(i=0; i<ModP->nL; i++) fscanf(fp, " %lf", &ModP->lay[i].theta_w);
	  for(i=ModP->nL; i<3; i++) fscanf(fp, " %*f");
	  fscanf(fp, "\n");
	  for(i=ModP->nL-1; i>=0; i--)
	    {
	      ModP->lay[i].hl = ModP->lay[i+1].hl + ModP->lay[i].dhl;
	      ModP->hw[i].b_val = ModP->hw[i].b_val + ModP->lay[i].theta_w;
	    }
	}
      else
	{
	  prn_msg(__FILE__, __LINE__, "err", -1., 
		  "No model of this name (%3s) implemented yet", ModLab);
	  return(ERROR);
	}
    } // end reading SnowMIP file
  else
    {
      // 3.b full/aggr.pro SNOWPACK format:
      //  First layer <=> Bottom layer
      //  Termin,Hn,He,Te,k,rho,th_w,gsz,bsz,dd,sp,class,mk,hard,strength,S_d,S_n,S_s,ps2rb,ITI
      ModP->lay[ModP->nL].hl = 0.;
      for(i=ModP->nL-1; i>=0; i--)
	{
	  fgets(line_dum, MAX_LINE_LENGTH, fp);
	  if( strcmp(stn, "CDP") != 0 )
	    {
	      ncol = sscanf(line_dum, "%*[^,],%lf,%*f,%lf,%*f,%lf,%lf,%lf,%*f,%*f,%*f,%d,%*d,%lf",
			    &ModP->lay[i].hl, &ModP->T[i].p_val, &ModP->hw[i].b_val, &ModP->lay[i].theta_w,
			    &ModP->lay[i].size, &ModP->lay[i].F3, &ModP->lay[i].hard);
	      ModP->T[i].p_val   = C_TO_K(ModP->T[i].p_val);
	      ModP->lay[i].hl    = CM_TO_M(ModP->lay[i].hl);
	      ModP->lay[i].dhl   = ModP->lay[i].hl - ModP->lay[i+1].hl;
	      ModP->hw[i].b_val *= ModP->lay[i].dhl;
	    }
	  else
	    // Read SnowMIP output for CDP integrated in SNP-file
	    {
	      NO_HRD = 1;
	      ncol = sscanf(line_dum, "%*d %*d %lf %lf %lf %lf %lf %lf %d %d %*f %*f %*f %*f %*d %*d",
			    &ModP->lay[i].hl, &ModP->lay[i].dhl, &ModP->hw[i].b_val, &ModP->lay[i].theta_w,
			    &ModP->T[i].p_val, &ModP->lay[i].size, &ModP->lay[i].F1, &ModP->lay[i].F2);
	      ModP->lay[i].F3 = 0;
	    }
	} // end reading full/aggr.pro file
      
      if( *MMreadMA )
	for(i=0; i<3; i++)
	  fgets(line_dum, MAX_LINE_LENGTH, fp);
      if( !feof(fp) )
	{
	  ncol = sscanf(line_dum, "%[^,]", stn);
	  if( strcmp(stn, "CDP") != 0 )
	    {
	      ncol = sscanf(line_dum, "%*[^,],%d.%d.%d %d:%*d,%*f,%*f,%*d,%*f,%*f,%*f",
			    &DD, &MM, &YYYY, &HH);
	      *MMreadMA = (long) (YYYY*1.e6 + MM*1.e4 + DD*1.e2 + HH);
	    }
	  else
	    // Read SnowMIP output for CDP integrated in SNP-file
	    ncol = sscanf(line_dum, "%*[^,],%*f, %*d %ld %*f %*d", MMreadMA);
	  fgets(line_dum, MAX_LINE_LENGTH, fp);
	}
      else
	*MMreadMA = -1;
    }


  // 4. ADAPT AND ADD SOME VALUES FOR USE WITH PROFEVAL_MIP

  ModP->hsw = 0.0;
  add_dhl = 0.;
  for(i=ModP->nL-1; i>=0; i--)
    {
      // Total snow water equivalent
      ModP->hsw += ModP->hw[i].b_val;

      // Make sure all layers are thicker than 0.0000!
      ModP->lay[i].hl += add_dhl;
      if( (ModP->lay[i].dhl < min_dhl) )
	{
	  ModP->lay[i].dhl = min_dhl;
	  add_dhl += min_dhl;
	}
      if( !(ModP->lay[i].hl > ModP->lay[i+1].hl) )
	ModP->lay[i].hl += min_dhl;

      // Height of layer top for bulk values
      ModP->hw[i].hl = ModP->lay[i].hl;

      // Height of point values
      ModP->rho[i].h = ModP->th_w[i].h = ModP->T[i].h = ModP->lay[i].hl - ModP->lay[i].dhl/2.;

      // Density
      ModP->rho[i].p_val = ModP->hw[i].b_val/ModP->lay[i].dhl;

      // Express water content in % by volume (before eventual stretching)
      if( (ModP->lay[i].theta_w < 0.0) || NO_WET )
	{
	  NO_WET = 1;
	  ModP->nTH_W = 0;
	  free(ModP->th_w);
	}
      else if( !NO_WET )
	{
	  if( strcmp(ModLab, "SNP") != 0 || strcmp(stn, "CDP") == 0 )
	    ModP->lay[i].theta_w /= DENSITY_WATER*ModP->lay[i].dhl;
	  ModP->th_w[i].p_val = 100*ModP->lay[i].theta_w;
	}

      // Initialize wetness comparison values
      if( !NO_WET )
	ModP->lay[i].wet_cv = -9;
      else
	ModP->lay[i].wet_cv = -1;

      // Shape 
      if( strcmp(ModLab, "SNP") != 0 || strcmp(stn, "CDP") == 0 )
	// Convert intl classification to SLF classification
	{
	  ModP->lay[i].F1 = pe_TranslateTypeCode(ModP->lay[i].F1, &ModP->lay[i].F3);
	  ModP->lay[i].F2 = pe_TranslateTypeCode(ModP->lay[i].F2, &ModP->lay[i].F3);
	}
      else // convert EMS[e].type to F1,F2,F3
	{
	  type =  ModP->lay[i].F3;
	  ModP->lay[i].F1 = (int)(floor(type/100.));
	  type -= (int)(ModP->lay[i].F1*100);
	  ModP->lay[i].F2 = (int)(floor(type/10.));
	  ModP->lay[i].F3 = (int)(type - ModP->lay[i].F2*10);
	}

    } // End loop over layers

  // Reset snow depth
  ModP->hs = ModP->lay[0].hl;

  // Set the ground coordinate for layer and bulk values
  ModP->lay[ModP->nL].hl = ModP->hw[ModP->nHW].hl = 0.;


  // 5. IF EITHER T_SRF OR T_GND OR BOTH ARE SET:

  // Missing surface and ground temperatures will be extrapolated.
  if( T_SRF && ((ModP->hs - ModP->T[0].h) > 1.e-3) )
    {
      if( ModP->nT > 1 )
	{
	  for(i=ModP->nT; i>0; i--)
	    {
	      ModP->T[i].h = ModP->T[i-1].h;
	      ModP->T[i].p_val = ModP->T[i-1].p_val;
	    }
	  ModP->T[0].h = ModP->hs;
	  ModP->T[0].p_val = ModP->T[1].p_val +
	    (ModP->hs - ModP->T[1].h)*((ModP->T[1].p_val - ModP->T[2].p_val)/(ModP->T[1].h - ModP->T[2].h));
	  ModP->T[0].p_val = MIN(PHASE_CHANGE_TK, ModP->T[0].p_val);
	}
      else
	{
	  ModP->T[1].p_val = ModP->T[0].p_val;
	  ModP->T[1].h = ModP->T[0].h;
	  ModP->T[0].h = ModP->hs;
	}
      ModP->nT++;
    }
  if( T_GND && (ModP->T[ModP->nT-1].h > 1.e-3) )
    {
      if( ModP->nT > 1 )
	ModP->T[ModP->nT].p_val = ModP->T[ModP->nT-1].p_val - ModP->T[ModP->nT-1].h
	  *(ModP->T[ModP->nT-2].p_val - ModP->T[ModP->nT-1].p_val)/(ModP->T[ModP->nT-2].h - ModP->T[ModP->nT-1].h);
      else
	ModP->T[ModP->nT].p_val = ModP->T[ModP->nT-1].p_val;
      ModP->T[ModP->nT].p_val = MIN(PHASE_CHANGE_TK, ModP->T[ModP->nT].p_val);
      ModP->T[ModP->nT].h = 0.;
      ModP->nT++;
    }
  else if( !T_GND && (ModP->T[ModP->nT-1].h < 1.e-3) )
    ModP->nT--;

  // Close the File
  fclose(fp);

  return(NO_ERROR);

} // End peOnly_ReadModelProfile

/***********************************************************************************************/
int peOnly_ReadProfiles(int CompType, long *MMreadMA,
		       char *MAlabel, char *MAfname, char *SLlabel, char *SLfname, 
		       PROFILE *MA, PROFILE *SL, int *PrfNr, long *SLdate, long *MAdate)
/*---------------------------------------------------------------------------------------------+ 
 | Read master (MA) & (SL) slave profiles according to test mode                               |
 +---------------------------------------------------------------------------------------------*/
{
  int  MAerr=0, SLerr=0;
  long MMread=0;

  switch(CompType){
  case 11: // Pit-Model-mode
    MAerr = !pe_CopyObservedF(MA, PrfNr, MAfname);
    if( MAerr )
      prn_msg(__FILE__, __LINE__, "msg", -1., 
	      "ERROR while reading master profile!");
    NO_TYP_0 = NO_TYP; NO_SIZ_0 = NO_SIZ; NO_WET_0 = NO_WET; NO_HRD_0 = NO_HRD;
	SLerr = !peOnly_ReadModelProfile(SL, SLlabel, SLfname, SLdate, MAdate, &MMread);
    if( SLerr )
      prn_msg(__FILE__, __LINE__, "msg", -1., 
	      "ERROR while reading slave profile!");
    break;
  case 12: // Model-Pit-mode
    SLerr = !pe_CopyObservedF(SL, PrfNr, SLfname);
    if( SLerr )
      prn_msg(__FILE__, __LINE__, "msg", -1., 
	      "ERROR while reading slave profile!");
    NO_TYP_0 = NO_TYP; NO_SIZ_0 = NO_SIZ; NO_WET_0 = NO_WET; NO_HRD_0 = NO_HRD;
	MAerr = !peOnly_ReadModelProfile(MA, MAlabel, MAfname, MAdate, SLdate, &MMread);
    if( MAerr )
      prn_msg(__FILE__, __LINE__, "msg", -1., 
	      "ERROR while reading master profile!");
    break;
  case 15: case 16: // Pit-Pit- and Pit-Pit-121-mode, respectively
    MAerr = !pe_CopyObservedF(MA, PrfNr, MAfname);
    if( MAerr )
      prn_msg(__FILE__, __LINE__, "msg", -1., 
	      "ERROR while reading master profile!");
    NO_TYP_0 = NO_TYP; NO_SIZ_0 = NO_SIZ; NO_WET_0 = NO_WET; NO_HRD_0 = NO_HRD;
    SLerr = !pe_CopyObservedF(SL, PrfNr, SLfname);
    if( SLerr )
      prn_msg(__FILE__, __LINE__, "msg", -1., 
	      "ERROR while reading slave profile!");
    NO_TYP_0 = NO_TYP; NO_SIZ_0 = NO_SIZ; NO_WET_0 = NO_WET; NO_HRD_0 = NO_HRD;
    break;
  case 21: case 22: // Model-Model- and Model-Model-121-mode, respectively
    MAerr = !peOnly_ReadModelProfile(MA, MAlabel, MAfname, SLdate, MAdate, MMreadMA);
    if( MAerr )
      prn_msg(__FILE__, __LINE__, "msg", -1., 
	      "ERROR while reading master profile!");
    NO_TYP_0 = NO_TYP; NO_SIZ_0 = NO_SIZ; NO_WET_0 = NO_WET; NO_HRD_0 = NO_HRD;
    SLerr = !peOnly_ReadModelProfile(SL, SLlabel, SLfname, SLdate, MAdate, &MMread);
    if( SLerr )
      prn_msg(__FILE__, __LINE__, "msg", -1., 
	      "ERROR while reading slave profile!");
    break;
  default:	    
      prn_msg(__FILE__, __LINE__, "msg", -1., 
	      "Comp Type (%d) not implemented!", CompType);
    break;
  }
  NO_TYP = (NO_TYP || NO_TYP_0);
  NO_SIZ = (NO_SIZ || NO_SIZ_0); 
  NO_WET = (NO_WET || NO_WET_0);
  NO_HRD = (NO_HRD || NO_HRD_0);

  return( !(MAerr || SLerr) );

} // End peOnly_ReadProfiles

/***********************************************************************************************/
int peOnly_CheckProfiles(PROFILE *Prf)
/*---------------------------------------------------------------------------------------------+ 
 | Dump profile to screen (for checks)                                   	               |
 +---------------------------------------------------------------------------------------------*/
{
  int i;

  if( Prf->nL > 0 )
    {
      printf("\n%3d STRATI:", Prf->nL);
      printf("\n  h");
      for(i=Prf->nL-1; i>=0; i--) printf("%7.3lf", Prf->lay[i].hl);
      printf("\n  W");
      if( !NO_WET )
	for(i=Prf->nL-1; i>=0; i--) printf("%7d", Prf->lay[i].wet_cv);
      printf("\n F1");
      if( !NO_TYP )
	{
	  for(i=Prf->nL-1; i>=0; i--) printf("%7d", Prf->lay[i].F1);
	  printf("\n F2");
	  for(i=Prf->nL-1; i>=0; i--) printf("%7d", Prf->lay[i].F2);
	  printf("\n F3");
	  for(i=Prf->nL-1; i>=0; i--) printf("%7d", Prf->lay[i].F3);
	}
      printf("\n  E");
      if( !NO_SIZ )
	for(i=Prf->nL-1; i>=0; i--) printf("%7.2lf", Prf->lay[i].size);
      printf("\n  K");
      if( !NO_TYP )
	for(i=Prf->nL-1; i>=0; i--) printf("%7.1lf", Prf->lay[i].hard);
    }
  if( Prf->nHW > 0 )
    {
      printf("\n%3d HW:\n ", Prf->nHW);
      for(i=Prf->nHW-1; i>=0; i--) printf("%9.3lf", Prf->hw[i].hl); printf("\n ");
      for(i=Prf->nHW-1; i>=0; i--) printf("%9.1lf", Prf->hw[i].b_val);
    }
  if( Prf->nRHO > 0 )
    {
      printf("\n%3d RHO:\n ", Prf->nRHO);
      for(i=Prf->nRHO-1; i>=0; i--) printf("%9.3lf", Prf->rho[i].h); printf("\n ");
      for(i=Prf->nRHO-1; i>=0; i--) printf("%9.1lf", Prf->rho[i].p_val);
    }
  if( Prf->nTH_W > 0 )
    {
      printf("\n%3d TH_W:\n ", Prf->nTH_W);
      for(i=Prf->nTH_W-1; i>=0; i--) printf("%9.3lf", Prf->th_w[i].h); printf("\n ");
      for(i=Prf->nTH_W-1; i>=0; i--) printf("%9.1lf", Prf->th_w[i].p_val);
    }
  if( Prf->nT > 0 )
    {
      printf("\n%3d nT:\n ", Prf->nT);
      for(i=Prf->nT-1; i>=0; i--) printf("%9.3lf", Prf->T[i].h); printf("\n ");
      for(i=Prf->nT-1; i>=0; i--) printf("%9.2lf", Prf->T[i].p_val);
    }
  printf("\n");
 
  return(NO_ERROR);

} // End peOnly_CheckProfiles

/***********************************************************************************************/
int peOnly_Date2Termin(long date, char *termin)
/*---------------------------------------------------------------------------------------------+ 
 | Returns a string containing the calendar date corresponding to date (YYYYMMDDHH)            |
 +---------------------------------------------------------------------------------------------*/
{
  int  YYYY=1900, MM=1, DD=1, HH=0;

  YYYY  = date/1000000;
  date -= YYYY*1000000;
  MM    = date/10000;
  date -= MM*10000;
  DD    = date/100;
  HH    = date - DD*100;

  sprintf(termin, "%02d.%02d.%04d %02d:00", DD, MM, YYYY, HH);

  return(NO_ERROR);

} // End peOnly_Date2Termin


/***********************************************************************************************/
/***********************************************************************************************/
int main (int argc, char *argv[]) 
/*---------------------------------------------------------------------------------------------+
 | Driving routine for profile evaluation                                                      |
 +---------------------------------------------------------------------------------------------*/
{
  FILE    *fout;

  char    MasterFname[MAX_STRING_LENGTH], SlaveFname[MAX_STRING_LENGTH], PevFname[MAX_STRING_LENGTH];
  char    MasterLabel[MAX_STRING_LENGTH], SlaveLabel[MAX_STRING_LENGTH];
  char    dummy[MAX_STRING_LENGTH], line_dum[MAX_LINE_LENGTH];
  char    ACA_on, MainLoopOn=0, NoComp=0;
  size_t  i, j, l; //unsigned int i;
  int     c, int_dum1=0, int_dum2=0, err, nErr=0;
  int     ind_up, ind_low, layer_up, layer_low;
  int     PrfNr=0, Fprf=0, Lprf=0, Dprf=1;
  int     C_Type=11;
  int     nTyp=0, nSiz=0, nWet=0, nHrd=0, nHW=0, nRHO=0, nTH_W=0, nT=0, nTmD=0;
  long    SlaveDate=0, MasterDate=0, MMreadMA=0;
  double  stretch=1., t_up, t_low, weight=0.2;
  double  MA_Smax=0., SL_Smax=0.;
  double  dist_typ, dist_siz, dist_wet, dist_hard;
  double  dist_hw;
  double  dist_T, dist_TmD, dist_rho, dist_th_w;
  double  Amean_typ=0., Amean_siz=0., Amean_wet=0., Amean_hard=0.;
  double  Amean_hw=0.;
  double  Amean_rho=0., Amean_th_w=0., Amean_T=0., Amean_TmD=0.;
  double  Avar_typ=0., Avar_siz=0., Avar_wet=0., Avar_hard=0.;
  double  Avar_hw=0.;
  double  Avar_rho=0., Avar_th_w=0., Avar_T=0., Avar_TmD=0.;
  double  Amin_typ=9.99, Amin_siz=9.99, Amin_wet=9.99, Amin_hard=9.99;
  double  Amin_hw=9.99;
  double  Amin_rho=9.99, Amin_th_w=9.99, Amin_T=9.99, Amin_TmD=9.99;

  // These structures contain the two profiles to be compared
  PROFILE Master;	        //  Usually Observed Profile 
  PROFILE Slave;	        //  Usually Model Profile    

/*---------------------------------------------------------------------------------------------+ 
 | 1. SET PARAMETERS AND READ COMMAND LINE ARGUMENTS                                           |
 +---------------------------------------------------------------------------------------------*/
  strcpy(MODEL, "SNO");
  strcpy(RESEARCH_STATION, "WFJ_9293");
  strcpy(SLAVE_STATION, "");
  strcpy(EXPERIMENT, "TST");
  strcpy(MA_EXPERIMENT, "TST");
  strcpy(COMP_TYPE, "PM"); // or MP or MM or MM121 or PP or PP121
  T_SRF = 0;
  T_GND = 0;

  // Check for command line arguments
  //   complete set of command line arguments (example):
  //   "1" "5" ["CRO" "WFJ_9293" "PF.REF" "PM"  "none" "PF.NSD"]
  switch (argc)
    {
    case 9:
      strcpy(MA_EXPERIMENT, argv[8]);
    case 8:
      if( strcmp(argv[7], "ts") == 0 )
	T_SRF = 1;
      else if( strcmp(argv[7], "tg") == 0 )
	T_GND = 1;
      else if( strcmp(argv[7], "tstg") == 0 )
	{
	  T_SRF = 1;
	  T_GND = 1;
	}
      else if( strcmp(argv[7], "none") != 0 )
	{
	  prn_msg(__FILE__, __LINE__, "err", -1., 
		  "Provide either no 7th parameter or one of none, ts, tg or tstg !\n");
	  exit(ERROR);
	}
    case 7:
      strcpy(COMP_TYPE, argv[6]);    
    case 6:
      if( argv[5][0] == 'P' && argv[5][1] == 'P' )
	strcpy(COMP_TYPE, argv[5]);
      else
	strcpy(EXPERIMENT, argv[5]);
    case 5:
      strcpy(RESEARCH_STATION, argv[4]);    
    case 4:
      strcpy(MODEL, argv[3]);
      if( strcmp(MODEL, "CRO") == 0 )
	{EXPERIMENT[0] = 'P'; EXPERIMENT[1] = 'F';}
    case 3:
      i=0;
      while ( c == *argv[2]++ )
	{
	  c -= 48;
	  Lprf *= 10;
	  Lprf += c;
	  i++;
	}
    case 2:
      i=0;
      while ( c == *argv[1]++ )
	{
	  c -= 48;
	  Fprf *= 10;
	  Fprf += c;
	  i++;
	}
      if( Lprf > 0 )
	{
	  Fprf = MAX(1, Fprf);
	  if( Lprf < Fprf )
	    {
	      Dprf = Lprf; Lprf = Fprf; Fprf = Dprf;
	    }
	}
    case 1:
      break;
    default:
      {
	prn_msg(__FILE__, __LINE__, "err", -1., 
		"EXIT: too many arguments provided!\n");
	exit(ERROR);
      }
      break;
    }

  if( (strcmp(MODEL, "CSI") == 0) || (strcmp(MODEL, "ISB") == 0) ||
      (strcmp(MODEL, "SWA") == 0) || (strcmp(MODEL, "VIS") == 0) )
    PHASE_CHANGE_TK = 273.16;
  else
    PHASE_CHANGE_TK = 273.15;

  // Set time limits
  if( strcmp(RESEARCH_STATION, "CDP_9697") == 0 )
    {
      FIRST_MM_PROFILE = 1996111912; // 1996111912 Test: 1996111403 1996111506 1996122606
      LAST_MM_PROFILE  = 1997040112;
      // nMaxComp: 1064
    }
  else if( strcmp(RESEARCH_STATION, "CDP_9798") == 0 )
    {
      FIRST_MM_PROFILE = 1997120112;
      LAST_MM_PROFILE  = 1998050112;
      // nMaxComp: 1208
    }
  else if( strcmp(RESEARCH_STATION, "SLR_9697") == 0 )
    {
      FIRST_MM_PROFILE = 1996110112; // or: 1996120212
      LAST_MM_PROFILE  = 1997042312; // or: 1997043012
      // nMaxComp: 1384
    }
  else if( strcmp(RESEARCH_STATION, "WFJ_9293") == 0 )
    {
      FIRST_MM_PROFILE = 1992101712;
      LAST_MM_PROFILE  = 1993061612;
      // nMaxComp: 1936
    }
  else
    {
      FIRST_MM_PROFILE = 1936080100;
      LAST_MM_PROFILE  = 2050080100;
    }

  // Set IO-filetruncs
  if( strcmp(MODEL, "SNP") != 0 )
    { 
      strcpy(INPATH,  "/home/fierz/snowpack/profeval/input/snowmip/");
      strcpy(OUTPATH, "/home/fierz/snowpack/profeval/output/snowmip/");
    }
  else
    {  
     strcpy(INPATH,  "/home/fierz/snowpack/profeval/input/sens_tst/");
     strcpy(OUTPATH, "/home/fierz/snowpack/profeval/output/sens_tst/");
    }

/*---------------------------------------------------------------------------------------------+ 
 | 2. SET MASTER & SLAVE ACCORDING TO COMP_TYPE                                                |
 +---------------------------------------------------------------------------------------------*/
  if( (strcmp(COMP_TYPE, "PM") == 0) )
    {
      // Master: Pit profile; Slave: Model profile (standard)
      C_Type = 11;
      strcpy(MasterLabel, "PIT");
      strcpy(SlaveLabel, MODEL);
      sprintf(MasterFname, "%spit/%s.pit", INPATH, RESEARCH_STATION);
      if( strcmp(MODEL, "SNP") != 0 )
	sprintf(SlaveFname, "%s%s_3h_%s_%s", INPATH, SlaveLabel, RESEARCH_STATION, EXPERIMENT);
      else
	sprintf(SlaveFname, "%s%s_%s_%s", INPATH, SlaveLabel, RESEARCH_STATION, EXPERIMENT);
    }
  else if( strcmp(COMP_TYPE, "MP") == 0 )
    {
      // Master: Model profile; Slave: Pit profile
      C_Type = 12;
      strcpy(MasterLabel, MODEL);
      strcpy(SlaveLabel, "PIT");
      if( strcmp(MODEL, "SNP") != 0 )
	sprintf(MasterFname, "%s%s_3h_%s_%s", INPATH, MasterLabel, RESEARCH_STATION, EXPERIMENT);
      else
	sprintf(MasterFname, "%s%s_%s_%s", INPATH, MasterLabel, RESEARCH_STATION, EXPERIMENT);
      sprintf(SlaveFname, "%spit/%s.pit", INPATH, RESEARCH_STATION);
    }
  else if( strcmp(COMP_TYPE, "PP") == 0 || strcmp(COMP_TYPE, "PP121") == 0 )
    {
      // Master: Pit profile; Slave: Pit profile
      strcpy(MasterLabel, "PIT");
      sprintf(MasterFname, "%spit/%s.pit", INPATH, RESEARCH_STATION);
      if( strcmp(COMP_TYPE, "PP") == 0 )
	{	
	  C_Type = 15;
	  printf("\n Master pit station : %s", RESEARCH_STATION);
	  printf("\n Now enter slave pit station\n\t");
	  fgets(line_dum, MAX_LINE_LENGTH, stdin);
	  sscanf(line_dum,"%s",SLAVE_STATION);
	  if( strcmp(SLAVE_STATION, "") == 0 )
	    strcpy(SLAVE_STATION, RESEARCH_STATION);
	}
      else // Check mode: compares pits 1 to 1 w/o height range tolerance
	{
	  C_Type = 16;
	  strcpy(SLAVE_STATION, RESEARCH_STATION);
	}
      if( strcmp(EXPERIMENT, "TST") != 0 )
	sprintf(SlaveFname, "%spit/%s-%s.pit", INPATH, SLAVE_STATION, EXPERIMENT);
      else
	sprintf(SlaveFname, "%spit/%s.pit", INPATH, SLAVE_STATION);
    }
  else if( (strcmp(COMP_TYPE, "MM") == 0 || strcmp(COMP_TYPE, "MM121") == 0) )
    {
      // Master: Model_1 profile; Slave: Model_2 profile
      MMreadMA = 1;
      C_Type = 21;
      strcpy(SlaveLabel, MODEL);
      if( strcmp(SlaveLabel, "SNP") != 0 )
	sprintf(SlaveFname, "%s%s_3h_%s_%s", INPATH, SlaveLabel, RESEARCH_STATION, EXPERIMENT);
      else
	sprintf(SlaveFname, "%s%s_%s_%s", INPATH, SlaveLabel, RESEARCH_STATION, EXPERIMENT);
      printf("\n Slave model is : %s:%s", MODEL, EXPERIMENT);
      if( strcmp(COMP_TYPE, "MM121") != 0 )
	{
	  printf("\n Now enter master model choosing from ACA, CRO, CSI, ISB, SNO, SNP, SNT, SWA, TSM, VIS\n\t");
	  fgets(line_dum, MAX_LINE_LENGTH, stdin);
	  sscanf(line_dum,"%s",MasterLabel);
	  if( strcmp(MasterLabel, "CRO") == 0 )
	    {
	      sprintf(MA_EXPERIMENT, "%s", "PF");
	    }
	  else if( strcmp(MasterLabel, "SNT") == 0 || strcmp(MasterLabel, "TSM") == 0 )
	    {
	      printf("\n PF or ES version?\n\t");
	      scanf("%s", dummy);
	      fgets(dummy, MAX_STRING_LENGTH, stdin);
	      sscanf(dummy,"%2s",MA_EXPERIMENT);
	    }
	  else if( strcmp(MasterLabel, "SNP") == 0 )
	    {
	      printf("\n Now enter master experiment\n\t");
	      fgets(line_dum, MAX_LINE_LENGTH, stdin);
	      sscanf(line_dum,"%s",MA_EXPERIMENT);
	    }
	}
      else // Check mode MM121: compares models 1 to 1 w/o height range tolerance
	{
	  C_Type = 22;
	  strcpy(MasterLabel, MODEL);
	  strcpy(MA_EXPERIMENT, EXPERIMENT);
	}
      if( strcmp(MasterLabel, "SNP") != 0 )
	sprintf(MasterFname, "%s%s_3h_%s_%s", INPATH, MasterLabel, RESEARCH_STATION, MA_EXPERIMENT);
      else
	sprintf(MasterFname, "%s%s_%s_%s", INPATH, MasterLabel, RESEARCH_STATION, MA_EXPERIMENT);
    }
  else
    {
      prn_msg(__FILE__, __LINE__, "err", -1., 
	      "Comparison type %s not implemented yet!\n Enter either [PM], MP, MM or MM121 as argument\n",
	      COMP_TYPE);
      exit(ERROR);
    }
  ACA_on = (strcmp("ACA", MasterLabel) == 0) || (strcmp("ACA", SlaveLabel) == 0);

  // Build output filename
  if( (strcmp(MODEL, "SNP") != 0) )
    {
      strcpy(dummy, RESEARCH_STATION);
      for(i=0; i<strlen(dummy); i++)
	dummy[i] = tolower(dummy[i]);
      sprintf(PevFname, "%s%s/pev", OUTPATH, dummy);
    }
  else
    sprintf(PevFname, "%spev", OUTPATH);

  strcat(PevFname, "-"); strcat(PevFname, COMP_TYPE);
  if( T_SRF ) strcat(PevFname, "ts");
  if( T_GND ) strcat(PevFname, "tg");
  if( C_Type != 15 && C_Type != 16 )
    {
      strcat(PevFname, "_"); strcat(PevFname, MasterLabel);
      if( C_Type > 20 )
	{
	  strcat(PevFname, "-"); 
	  strcat(PevFname, MA_EXPERIMENT);
	}
      strcat(PevFname, "-");
      sprintf(dummy, "%s_%s_%s", SlaveLabel, RESEARCH_STATION, EXPERIMENT);
      strcat(PevFname, dummy);
    }
  else
    {
      strcat(PevFname, "_");
      strcat(PevFname, RESEARCH_STATION);
      strcat(PevFname, "-");
      strcat(PevFname, SLAVE_STATION);
      if( strcmp(EXPERIMENT, "TST") != 0 )
	{
	  strcat(PevFname, "-");
	  strcat(PevFname, EXPERIMENT);
	}
    }
  if( Fprf > 0 )
    {
      strcat(PevFname, "_");
      sprintf(dummy, "%d", Fprf); strcat(PevFname, dummy);
      if( Lprf > Fprf )
	{
	  strcat(PevFname, "-"); sprintf(dummy, "%d", Lprf); strcat(PevFname, dummy);
	}
    }

  prn_msg(__FILE__, __LINE__, "msg", -1., 
	  "MasterFname : %s", MasterFname);
  prn_msg(__FILE__, __LINE__, "msg+", -1., 
	  " SlaveFname : %s", SlaveFname);
  prn_msg(__FILE__, __LINE__, "msg+", -1., 
	  "OutputFname : %s\n", PevFname);

  // Write Header to output file
  if( (fout=fopen(PevFname, "w")) == 0 )
    {
      prn_msg(__FILE__, __LINE__, "err", -1., 
	      "Cannot open file %s\n", PevFname);
      exit(ERROR);
    }
  fprintf(fout, "Comparison run %s; ", PevFname);
  if( T_SRF && T_GND )
    fprintf(fout, "(Tsrf and Tgnd BOTH included!)\n");
  else if( T_SRF )
    fprintf(fout, "(Tsrf included, Tgnd NOT included!)\n");
  else if( T_GND )
    fprintf(fout, "(Tsrf NOT included, Tgnd included!)\n");
  else
    fprintf(fout, "(NEITHER Tsrf NOR Tgnd included!)\n");
  fprintf(fout, "Agreement scores for slave %s:%s with master %s",
	  SlaveLabel, EXPERIMENT,
	  MasterLabel);
  if( strcmp(MasterLabel, "PIT") != 0 )
    fprintf(fout, ":%s\n", MA_EXPERIMENT);
  else
    fprintf(fout, "\n");
  fprintf(fout,"Prf#\t     Termin     ");
  fprintf(fout,"\tshape\t size\twetness\thardness\t   hw\t  rho\t th_w\t    T\tT_50cm\toverall");
  fprintf(fout,"\tSLhsw/MAhsw\tSLhs/MAhs\tSL.hsw\tMA.hsw\t SL.hs\t MA.hs");
  fprintf(fout,"\tMA.nL\tMA.nHW\tMA.nRHO\tMA.nTH_W\tMA.nT\tMA.nTmD");
  fprintf(fout,"\tSL.nL\tSL.nHW\tSL.nRHO\tSL.nTH_W\tSL.nT\tSL.nTmD\n");


/*---------------------------------------------------------------------------------------------+
 | 3. MAIN LOOP STARTS                                                                         |
 +---------------------------------------------------------------------------------------------*/
  if( Fprf > 0 || Lprf > 0 )
    printf("\n\n [%04d] Fprf=%d Lprf=%d", __LINE__, Fprf, Lprf);
  if( C_Type < 20 && Fprf > 0 )
    PrfNr = Fprf - 1;
  do{
    NoComp = 0;
    nErr = 0;
    NO_TYP = NO_SIZ = NO_WET = 0;

    // 3.1. READ PROFILES:
    SlaveDate = 0;
    PrfNr++;
    err = !peOnly_ReadProfiles(C_Type, &MMreadMA,
			      MasterLabel, MasterFname, SlaveLabel, SlaveFname, 
			      &Master, &Slave, &PrfNr, &SlaveDate, &MasterDate);
    if( C_Type >= 20 )
      while ( PrfNr < Fprf )
	{
	  PrfNr++;
	  err = !peOnly_ReadProfiles(C_Type, &MMreadMA,
				    MasterLabel, MasterFname, SlaveLabel, SlaveFname, 
				    &Master, &Slave, &PrfNr, &SlaveDate, &MasterDate);
	  if( err )
	    break;
	}
    if( err )
      {
	prn_msg(__FILE__, __LINE__, "err", -1., 
		"While attempting to read profiles on %ld\n", MasterDate);
	return(ERROR);
      }
   
    // 3.2. ON-LINE CHECK (OPTIONAL, PREVIOUS TO EVENTUAL STRETCHING)
    // Master
    if( TEST_MASTER )
      {
	printf("\n\n Master (pit) profile # %d, of %10ld: HS=%6.3lf m",
	       PrfNr, MasterDate, Master.hs);
	peOnly_CheckProfiles(&Master);
	if( TEST_MASTER > 1 )
	  {
	    printf("\n");
	    exit(NO_ERROR);
	  }
      }
    // Slave
    if( TEST_SLAVE )
      {
	if( !TEST_MASTER ) printf("\n");
	printf("\n Slave (modelled) profile on %10ld:  HS=%9.3lf, Htoplay=%9.3lf (NOT stretched yet!)",
	       SlaveDate, Slave.hs, Slave.lay[0].hl);
	peOnly_CheckProfiles(&Slave);
	if( TEST_SLAVE > 1 )
	  {
	    printf("\n");
	    exit(NO_ERROR);
	  }
      } // End On-Line check

    // 3.3. Preliminary checks
    if( MasterDate == 0 )
      {
	printf("\n [%04d] No more master profiles to compare with!\n", __LINE__);
	NoComp = 1;
      }
    else
      {
	peOnly_Date2Termin(MasterDate, dummy);
	fprintf(fout, "%4d\t%s", PrfNr, dummy);
	printf("\n\n MAster profile # %d on MasterDate %10ld", PrfNr, MasterDate);
	if( Master.hs <= 0. )
	  {
	    printf("\n  -> Master snow depth is zero. No comparison is done!\n");
	    fprintf(fout, "\t-9.99\t-9.99\t  -9.99\t   -9.99\t-9.99\t-9.99\t-9.99\t-9.99\t -9.99\t  -9.99");
	    fprintf(fout, "\t           0\t%9.2lf\t%6.1lf\t     0\t%6.3lf\t%6.3lf", 
		    Slave.hs/Master.hs,Slave.hsw, Slave.hs, Master.hs);
	    fprintf(fout, "\t    0\t     0\t      0\t       0\t    0\t      0");
	    fprintf(fout, "\t%5d\t     0\t      0\t       0\t    0\t      0\n", Slave.nL);
	    NoComp = 1;
	  }
	else if( Slave.hs <= 0. )
	  {
	    printf("\n  -> Slave snow depth is zero. No comparison is done!\n");
	    fprintf(fout, "\t-9.99\t-9.99\t  -9.99\t   -9.99\t-9.99\t-9.99\t-9.99\t-9.99\t -9.99\t  -9.99");
	    fprintf(fout, "\t           0\t        0\t     0\t%6.1lf\t     0\t%6.3lf",
		    Master.hsw, Master.hs);
	    fprintf(fout, "\t%5d\t     0\t      0\t       0\t    0\t      0", Master.nL);
	    fprintf(fout, "\t    0\t     0\t      0\t       0\t    0\t      0\n");
	    NoComp = 1;
	  }
	else if( SlaveDate == 0 )
	  {
	    if( PrfNr > 0 )
	      {
		printf("\n  -> No slave profile matching that date. No comparison is done!\n");
		fprintf(fout, "\t-9.99\t-9.99\t  -9.99\t   -9.99\t-9.99\t-9.99\t-9.99\t-9.99\t -9.99\t  -9.99");
		fprintf(fout, "\t           0\t        0\t     0\t%6.1lf\t     0\t%6.3lf",
			Master.hsw, Master.hs);
		fprintf(fout, "\t%5d\t     0\t      0\t       0\t    0\t      0", Master.nL);
		fprintf(fout, "\t    0\t     0\t      0\t       0\t    0\t      0\n");
	      }
	    NoComp = 1;
	  }
	else if( !NoComp && !ACA_on )
	  {
	    switch (C_Type)
	      {
	      case 16: case 22:
		NoComp = 0;
		break;
	      case 11: case 15: case 21:
		NoComp = !((Slave.hsw/Master.hsw > 0.6) && (Slave.hsw/Master.hsw < 1.4));
		break;
	      case 12:
		NoComp = !((Master.hsw/Slave.hsw > 0.6) && (Master.hsw/Slave.hsw < 1.4));
		break;
	      default:
		prn_msg(__FILE__, __LINE__, "err", -1., 
			"This COMP_TYPE %s is not implemented yet!\n", COMP_TYPE);
		exit(ERROR);
		break;
	      }
	    if( NoComp )
	      {
		printf("\n  -> Slave HSW out of range: SLhsw/MAhsw=%.2lf. No comparison is done!\n\n",
		       Slave.hsw/Master.hsw);
		fprintf(fout, "\t-9.99\t-9.99\t  -9.99\t   -9.99\t-9.99\t-9.99\t-9.99\t-9.99\t -9.99\t  -9.99");
		fprintf(fout, "\t%11.2lf\t%9.2lf\t%6.1lf\t%6.1lf\t%6.3lf\t%6.3lf",
			Slave.hsw/Master.hsw, Slave.hs/Master.hs, Slave.hsw, Master.hsw, Slave.hs, Master.hs);
		fprintf(fout, "\t%5d\t     0\t      0\t       0\t    0\t      0", Master.nL);
		fprintf(fout, "\t%5d\t     0\t      0\t       0\t    0\t      0\n", Slave.nL);
	      }
	  }
	else
	  {
	    printf(" (SlaveDate %10ld):\n", SlaveDate); 
	    printf("\n\tMaster-file: %s", MasterFname);
	    printf("\n\tSlave-file : %s", SlaveFname);
	    if( ACA_on )
	      printf("\n\tWARNING : No check on HSW because ACA is involved!!");
	  }
      }


/*---------------------------------------------------------------------------------------------+
 | 4. COMPARISON                                                                               |
 +---------------------------------------------------------------------------------------------*/
    // 4.1. INITIALIZATION  AND STRE-E-E-E-TCHING
    if( !NoComp )
      {
	// Initialize distances to zero
	dist_typ = dist_siz = dist_wet = dist_hard = 0.;

	// Slave layer boundaries:
	stretch = Master.hs/Slave.hs;
	for(l=0; l<Slave.nL;    l++)
	  Slave.lay[l].hl *= stretch;
	// To avoid troubles looking for the search range, set snow depths equal
	if( Slave.nL > 0 )
	  Slave.lay[0].hl = Master.hs;
	for(l=0; l<Slave.nHW;   l++)
	  Slave.hw[l].hl *= stretch;
	for(l=0; l<Slave.nRHO;  l++)
	  Slave.rho[l].h *= stretch;
	for(l=0; l<Slave.nTH_W; l++)
	  Slave.th_w[l].h *= stretch;
	for(l=0; l<Slave.nT;    l++)
	  Slave.T[l].h *= stretch;

	// Series of slave point measurements:
	for(l=0; l<Slave.nRHO; l++)
	  Slave.rho[l].p_val /= stretch;
	for(l=0; l<Slave.nTH_W; l++)
	  Slave.th_w[l].p_val /= stretch;
	for(l=0; l<Slave.nL; l++)
	  Slave.lay[l].theta_w /= stretch;
	if( !NO_WET )
	  {
	    // Assign wetness comparison values
	    //   a) master profile (MP- and MM-modes):
	    if( C_Type == 12 || C_Type > 20 )
	      for(i=0; i<Master.nL; i++)
		{
		  if( Master.lay[i].theta_w <= 0.010 )
		    Master.lay[i].wet_cv = 0;
		  else if( Master.lay[i].theta_w >= 0.050 )
		    Master.lay[i].wet_cv = 2;
		  else
		    Master.lay[i].wet_cv = 1;
		}
	    //   b) slave profile (all except PP121-mode):
	    if( C_Type != 16 )
	      for(l=0; l<Slave.nL; l++)
		{
		  if( Slave.lay[l].theta_w <= 0.010 )
		    Slave.lay[l].wet_cv = 0;
		  else if( Slave.lay[l].theta_w >= 0.050 )
		    Slave.lay[l].wet_cv = 2;
		  else
		    Slave.lay[l].wet_cv = 1;
		}
	  }

	// Series of slave bulk measurements:
	//   No stretching as HW, e.g., is just the mass of a layer

	// Normalization factor for grain size
	if( !NO_SIZ )
	  {
	    MA_Smax = SL_Smax = 0.0;
	    for(i=0; i<Master.nL; i++)
	      MA_Smax = MAX(MA_Smax, Master.lay[i].size);
	    for(i=0; i<Slave.nL; i++)
	      SL_Smax = MAX(SL_Smax, Slave.lay[i].size);
	  }

	// 4.2. LAYER CHARACTERISTICS
	if( !NO_TYP || !NO_SIZ || !NO_WET || !NO_HRD )
	  {
	    MATRIX *d_typmat;

	    if( !NO_TYP )
	      // Initialize the type distance matrix
	      pe_TypeDistanceMat(d_typmat);

	    for(i=0; i<Master.nL; i++) // For all stratigraphic layers
	      {
		
		// Mapping of Layers
		//   Tolerance for height range in slave profile
		if( C_Type != 22 && C_Type != 16 ) // MP-, PM-, MM- or PP-mode
		  {
		    t_up =  (Master.hs - Master.lay[i].hl)*Master.lay[i].hl/Master.hs;
		    t_low = (Master.hs - Master.lay[i+1].hl)*Master.lay[i+1].hl/Master.hs;
		  }
		else
		  t_up = t_low = 0.;

		//   Determine upper and lower layer indices of slave height range
		j = 0;
		while ( (Master.lay[i].hl + weight*t_up) < Slave.lay[j].hl )
		  j++;
		if( ((j > 0) && (((Master.lay[i].hl + weight*t_up) - Slave.lay[j].hl)
				 > (0.75*(Slave.lay[j-1].hl - Slave.lay[j].hl))))
		    || ((Master.lay[i+1].hl - weight*t_low) >= (Slave.lay[j].hl)) )
		  j--;
		j = ind_up = MIN(j, Slave.nL-1);
		while ( (Master.lay[i+1].hl - weight*t_low) <= Slave.lay[j+1].hl )
		  {
		    j++;
		    if( j > Slave.nL-1 )
		      break;
		  }
		if( (j <= Slave.nL-1)
		    &&
		    ((Slave.lay[j].hl - (Master.lay[i+1].hl - weight*t_low))
		     > (0.75*(Slave.lay[j].hl - Slave.lay[j+1].hl))) )
		  ind_low = MAX(j, ind_up);
		else
		  ind_low = MAX(j-1, ind_up);
		if( !( ind_up >= 0 && ind_up <= ind_low) )
		  {
		    prn_msg(__FILE__, __LINE__, "err", -1., 
			    "RANGE UPPER: i=%d ind_up=%d ind_low=%d nSL=%d\n",
			    i, ind_up, ind_low, Slave.nL);
		    exit(ERROR);
		  }
		if( !( ind_low >= ind_up && ind_low < Slave.nL) )
		  {
		    prn_msg(__FILE__, __LINE__, "err", -1., 
			    "RANGE LOWER: i=%d ind_low=%d ind_up=%d nSL=%d\n",
			    i, ind_low, ind_up, Slave.nL);
		    exit(ERROR);
		  }
		layer_up = layer_low = -1;

		// Grain Type distance
		if( !NO_TYP )
		  dist_typ += pe_LayPropDistance("type", i, ind_up, ind_low, &layer_up, &layer_low,
						    -1., -1., &Master, &Slave, d_typmat);

		// Grain Size distance
		if( !NO_SIZ )
		  dist_siz += pe_LayPropDistance("size", i, ind_up, ind_low, &layer_up, &layer_low,
						    MA_Smax, SL_Smax, &Master, &Slave);
	    
		// Hardness distance
		if( !NO_HRD )
		  dist_hard += pe_LayPropDistance("hard", i, ind_up, ind_low, &layer_up, &layer_low,
						    6., 6., &Master, &Slave);

		// Wetness Comparison Values distance
		if( !NO_WET )
		  dist_wet += pe_LayPropDistance("wet", i, ind_up, ind_low, &layer_up, &layer_low,
						    2., 2., &Master, &Slave);

	      } // end for all stratigraphic layers

	    // Average distances over the Number of Observed Layers; No Depth Weighting.
	    if( !NO_TYP )
	      {
		dist_typ /= Master.nL;
		if( dist_typ > 1. )
		  {
		    prn_msg(__FILE__, __LINE__, "wrn", -1., 
			    "Distance for grain type larger than 1!\n");
		    nErr++;
		  }
		else
		  {
		    nTyp++;
		    Amean_typ *= (nTyp-1.)/nTyp;
		    Amean_typ += (1. - dist_typ)/nTyp;
		    Avar_typ  *= (nTyp-1.)/nTyp;
		    if( nTyp > 1 )
		      Avar_typ += (1. - dist_typ - Amean_typ)*(1. - dist_typ - Amean_typ)/(nTyp-1.);
		    Amin_typ = MIN(1.- dist_typ, Amin_typ);
		  }
	      }
	    else
	      dist_typ = 10.99;

	    if( !NO_SIZ )
	      {
		dist_siz /= Master.nL;
		if( dist_siz > 1. )
		  {
		    prn_msg(__FILE__, __LINE__, "wrn", -1., 
			    "Distance for grain size larger than 1!\n");
		    nErr++;
		  }
		else
		  {
		    nSiz++;
		    Amean_siz *= (nSiz-1.)/nSiz;
		    Amean_siz += (1. - dist_siz)/nSiz;
		    Avar_siz  *= (nSiz-1.)/nSiz;
		    if( nSiz > 1 )
		      Avar_siz += (1. - dist_siz - Amean_siz)*(1. - dist_siz - Amean_siz)/(nSiz-1.);
		    Amin_siz = MIN(1.- dist_siz, Amin_siz);
		  }
	      }
	    else
	      dist_siz = 10.99;

	    if( !NO_WET )
	      {
		dist_wet /= Master.nL;
		if( dist_wet > 1. )
		  {
		    prn_msg(__FILE__, __LINE__, "wrn", -1., 
			    "Distance for wetness larger than 1!\n");
		    nErr++;
		  }
		else
		  {
		    nWet++;
		    Amean_wet *= (nWet-1.)/nWet;
		    Amean_wet += (1. - dist_wet)/nWet;
		    Avar_wet  *= (nWet-1.)/nWet;
		    if( nWet > 1 )
		      Avar_wet += (1. - dist_wet - Amean_wet)*(1. - dist_wet - Amean_wet)/(nWet-1.);
		    Amin_wet = MIN(1.- dist_wet, Amin_wet);
		  }
	      }
	    else
	      dist_wet = 10.99;

	    if( !NO_HRD )
	      {
		dist_hard /= Master.nL;
		if( dist_hard > 1. )
		  {
		    prn_msg(__FILE__, __LINE__, "wrn", -1., 
			    "Distance for hardness larger than 1!\n");
		    nErr++;
		  }
		else
		  {
		    nHrd++;
		    Amean_hard *= (nHrd-1.)/nHrd;
		    Amean_hard += (1. - dist_hard)/nHrd;
		    Avar_hard  *= (nHrd-1.)/nHrd;
		    if( nHrd > 1 )
		      Avar_hard += (1. - dist_hard - Amean_hard)*(1. - dist_hard - Amean_hard)/(nHrd-1.);
		    Amin_hard = MIN(1.- dist_hard, Amin_hard);
		  }
	      }
	    else
	      dist_hard = 10.99;
	  }
	else
	  dist_typ = dist_siz = dist_wet = dist_hard = 10.99;

	// 4.3. SERIES OF BULK MEASUREMENTS
	//  Snow Water Equivalent of Layers HW
	if( Master.nHW > 0 && Slave.nHW > 0 && !ACA_on )
	  {
	    dist_hw = pe_BlkSerDistance(11, Master.nHW, Master.hw, Slave.nHW, Slave.hw);
	    if( dist_hw <= -9.9 )
	      {
		dist_hw = 10.99;
	      }
	    else if( dist_hw < 0. || dist_hw > 1. )
	      {
		prn_msg(__FILE__, __LINE__, "wrn", -1., 
			"Erroneous distance for layer water content HW!\n");
		nErr++;
	      }
	    else
	      {
		nHW++;
		Amean_hw *= (nHW-1.)/nHW;
		Amean_hw += (1. - dist_hw)/nHW;
		Avar_hw  *= (nHW-1.)/nHW;
		if( nHW > 1 )
		  Avar_hw += (1. - dist_hw - Amean_hw)*(1. - dist_hw - Amean_hw)/(nHW-1.);
		Amin_hw = MIN(1.- dist_hw, Amin_hw);
	      }
	  }
	else
	  dist_hw = 10.99;

	// 4.4. SERIES OF POINT MEASUREMENTS
	//  Series of Density Measurements
	if( Master.nRHO > 0 && Slave.nRHO > 0 && !ACA_on )
	  {
	    dist_rho = pe_PntSerDistance("RHO", Master.hs,
					    C_Type,
					    Master.nRHO, Master.rho, Slave.nRHO, Slave.rho,
					    -1., &int_dum1, &int_dum2,
					    0, 0);
	    if( dist_rho <= -9.9 )
	      dist_rho = 10.99;
	    else if( dist_rho < 0. || dist_rho > 1. )
	      {
		prn_msg(__FILE__, __LINE__, "wrn", -1., 
			"Erroneous distance for density!\n");
		nErr++;
	      }
	    else
	      {
		nRHO++;
		Amean_rho *= (nRHO-1.)/nRHO;
		Amean_rho += (1. - dist_rho)/nRHO;
		Avar_rho  *= (nRHO-1.)/nRHO;
		if( nRHO > 1 )
		  Avar_rho += (1. - dist_rho - Amean_rho)*(1. - dist_rho - Amean_rho)/(nRHO-1.);
		Amin_rho = MIN(1.- dist_rho, Amin_rho);
	      }
	  }
	else
	  dist_rho = 10.99;

	//  Series of Liquid Water Content Measurements
	if( Master.nTH_W > 0 && Slave.nTH_W > 0 && !ACA_on )
	  {
	    dist_th_w = pe_PntSerDistance("TH_W", Master.hs,
					     C_Type,
					     Master.nTH_W, Master.th_w, Slave.nTH_W, Slave.th_w,
					     -1., &int_dum1, &int_dum2,
					     0, 0);
	    if( dist_th_w <= -9.9 )
	      dist_th_w = 10.99;
	    else if( dist_th_w < 0. || dist_th_w > 1. )
	      {
		prn_msg(__FILE__, __LINE__, "wrn", -1., 
			"Erroneous distance for liquid water content (LWC)!\n");
		nErr++;
	      }
	    else
	      {
		nTH_W++;
		Amean_th_w *= (nTH_W-1.)/nTH_W;
		Amean_th_w += (1. - dist_th_w)/nTH_W;
		Avar_th_w  *= (nTH_W-1.)/nTH_W;
		if( nTH_W > 1 )
		  Avar_th_w += (1. - dist_th_w - Amean_th_w)*(1. - dist_th_w - Amean_th_w)/(nTH_W-1.);
		Amin_th_w = MIN(1.- dist_th_w, Amin_th_w);
	      }
	  }
	else
	  dist_th_w = 10.99;

	//  Series of Temperature Measurements (Full Depth)
	if( Master.nT > 0 && Slave.nT > 0 )
	  {
	    dist_T = pe_PntSerDistance("T", Master.hs,
					  C_Type,
					  Master.nT, Master.T, Slave.nT, Slave.T,
					  -1., &int_dum1, &int_dum2,
					  T_GND, T_SRF);
	    if( dist_T <= -9.9 )
	      dist_T = 10.99;
	    else if( dist_T < 0. || dist_T > 1. )
	      {
		prn_msg(__FILE__, __LINE__, "wrn", -1., 
			"Erroneous distance for temperature!\n");
		nErr++;
	      }
	    else
	      {
		nT++;
		Amean_T *= (nT-1.)/nT;
		Amean_T += (1. - dist_T)/nT;
		Avar_T  *= (nT-1.)/nT;
		if( nT > 1 )
		  Avar_T += (1. - dist_T - Amean_T)*(1. - dist_T - Amean_T)/(nT-1.);
		Amin_T = MIN(1.- dist_T, Amin_T);
	      }
	
	    //  Series of Temperature Measurements at Top of Snowpack (MaxDepth)
	    dist_TmD = pe_PntSerDistance("TmD", Master.hs,
					    C_Type,
					    Master.nT, Master.T, Slave.nT, Slave.T,
					    0.5, &Master.nTmD, &Slave.nTmD,
					    T_GND, T_SRF);
	    if( dist_TmD <= -9.9 )
	      dist_TmD = 10.99;
	    else if( dist_TmD < 0. || dist_TmD > 1. )
	      {
		prn_msg(__FILE__, __LINE__, "wrn", -1., 
			"Erroneous distance for top temperature!\n");
		nErr++;
	      }
	    else
	      {
		nTmD++;
		Amean_TmD *= (nTmD-1.)/nTmD;
		Amean_TmD += (1. - dist_TmD)/nTmD;
		Avar_TmD  *= (nTmD-1.)/nTmD;
		if( nTmD > 1 )
		  Avar_TmD += (1. - dist_TmD - Amean_TmD)*(1. - dist_TmD - Amean_TmD)/(nTmD-1.);
		Amin_TmD = MIN(1.- dist_TmD, Amin_TmD);
	      }
	  }
	else
	  {
	    Master.nTmD = Slave.nTmD = 0;
	    dist_T = dist_TmD = 10.99;
	  }


/*---------------------------------------------------------------------------------------------+ 
 | 5. OUTPUT                                                                                   |
 +---------------------------------------------------------------------------------------------*/
	// Write Agreement Scores (1.-dist) to Output File
	fprintf(fout, "\t%5.2lf\t%5.2lf\t%7.2lf\t%8.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%6.2lf\t%7.2lf",
		1.-dist_typ, 1.-dist_siz, 1.-dist_wet, 1.-dist_hard, 1.-dist_hw, 1.-dist_rho, 1.-dist_th_w,
		1.-dist_T, 1.-dist_TmD, -9.99);
	fprintf(fout, "\t%11.2lf\t%9.2lf\t%6.1lf\t%6.1lf\t%6.3lf\t%6.3lf",
		Slave.hsw/Master.hsw, Slave.hs/Master.hs, Slave.hsw, Master.hsw, Slave.hs, Master.hs);
	fprintf(fout, "\t%5d\t%6d\t%7d\t%8d\t%5d\t%7d",
		Master.nL, Master.nHW, Master.nRHO, Master.nTH_W, Master.nT, Master.nTmD);
	fprintf(fout, "\t%5d\t%6d\t%7d\t%8d\t%5d\t%7d\n",
		Slave.nL, Slave.nHW, Slave.nRHO, Slave.nTH_W, Slave.nT, Slave.nTmD);

	// Create a screen output
	printf("  MOD (slave):  SLhsw=%.1lf, SLhs=%.3lf\n  OBS (master): MAhsw=%.1lf, MAhs=%.3lf",
	       Slave.hsw, Slave.hs, Master.hsw, Master.hs);
	if( Master.hsw > 0. )
	  printf("; SLhsw/MAhsw=%.2lf, SLhs/MAhs=%.2lf", Slave.hsw/Master.hsw, Slave.hs/Master.hs);
	printf("\n                   score  nMA  nSL");
	printf("\n  Grain Type    :%7.2lf  %3d  %3d", 1.-dist_typ, Master.nL,   Slave.nL);
	printf("\n  Grain Size    :%7.2lf  %3d  %3d", 1.-dist_siz, Master.nL,   Slave.nL);
	printf("\n  Wetness       :%7.2lf  %3d  %3d", 1.-dist_wet, Master.nL,   Slave.nL);
	printf("\n  Hardness      :%7.2lf  %3d  %3d", 1.-dist_hard, Master.nL,    Slave.nL);
	printf("\n  HW            :%7.2lf  %3d  %3d", 1.-dist_hw,  Master.nHW,  Slave.nHW);
	printf("\n  Density       :%7.2lf  %3d  %3d", 1.-dist_rho, Master.nRHO, Slave.nRHO);
	printf("\n  Water content :%7.2lf  %3d  %3d", 1.-dist_th_w,Master.nTH_W,Slave.nTH_W);
	printf("\n  Temperature   :%7.2lf  %3d  %3d", 1.-dist_T,   Master.nT,   Slave.nT);
	if( T_SRF && T_GND )
	  printf("\tTsrf and Tgnd BOTH included!");
	else if( T_SRF )
	  printf("\tTsrf included, Tgnd NOT included!");
	else if( T_GND )
	  printf("\tTsrf NOT included, Tgnd included!");
	else
	  printf("\tNEITHER Tsrf NOR Tgnd included!");
	printf("\n  TopTemperature:%7.2lf  %3d  %3d", 1.-dist_TmD, Master.nTmD, Slave.nTmD);
	printf("\n\n");
      } // End !NoComp


/*---------------------------------------------------------------------------------------------+ 
 | 6. FREE THE DATA STRUCTURES                                                                 |
 +---------------------------------------------------------------------------------------------*/
    if( Master.nL > 0 )     free(Master.lay);
    if( Master.nHW > 0 )    free(Master.hw);
    if( Master.nRHO > 0 )   free(Master.rho);
    if( Master.nTH_W > 0 )  free(Master.th_w);
    if( Master.nT > 0 )     free(Master.T);

    if( !NoComp )
      {
	if( Slave.nL > 0 )     free(Slave.lay);
	if( Slave.nHW > 0 )    free(Slave.hw);
	if( Slave.nRHO > 0 )   free(Slave.rho);
	if( Slave.nTH_W > 0 )  free(Slave.th_w);
	if( Slave.nT > 0 )     free(Slave.T);
      }

    if( Lprf > 0 )
      MainLoopOn = (MasterDate != 0 && (PrfNr > 0 && PrfNr < Lprf));
    else
      MainLoopOn = (MasterDate != 0 && PrfNr > 0 );

  } while ( MainLoopOn ); 
  // MAIN LOOP ENDS

/*---------------------------------------------------------------------------------------------+ 
 | 7. ADD RUN STATISTICS TO OUTPUT & EXIT                                                      |
 +---------------------------------------------------------------------------------------------*/
  // Very, very special, for IGS_03 only
  if( !strcmp(RESEARCH_STATION, "WFJ_9293") && (nWet == 17) )
    nWet = 8;

  // Evaluate averaged distances, STD, min
  if( nTyp > 0 ) Avar_typ   = sqrt(Avar_typ);  else {Amean_typ = -9.99; Avar_typ = -9.99; Amin_typ = -9.99;}
  if( nSiz > 0 ) Avar_siz   = sqrt(Avar_siz);  else {Amean_siz = -9.99; Avar_siz = -9.99; Amin_siz = -9.99;}
  if( nWet > 0 ) Avar_wet   = sqrt(Avar_wet);  else {Amean_wet = -9.99; Avar_wet = -9.99; Amin_wet = -9.99;}
  if( nHrd > 0 ) Avar_hard  = sqrt(Avar_hard); else {Amean_hard = -9.99; Avar_hard = -9.99; Amin_hard = -9.99;}
  if( nHW  > 0 ) Avar_hw    = sqrt(Avar_hw);   else {Amean_hw = -9.99; Avar_hw = -9.99; Amin_hw = -9.99;}
  if( nRHO > 0 ) Avar_rho   = sqrt(Avar_rho);  else {Amean_rho = -9.99; Avar_rho = -9.99; Amin_rho = -9.99;}
  if( nTH_W > 0 ) Avar_th_w = sqrt(Avar_th_w); else {Amean_th_w = -9.99; Avar_th_w = -9.99; Amin_th_w = -9.99;}
  if( nT > 0 )   Avar_T     = sqrt(Avar_T);    else {Amean_T   = -9.99; Avar_T = -9.99; Amin_T = -9.99;}
  if( nTmD > 0 ) Avar_TmD   = sqrt(Avar_TmD);  else {Amean_TmD = -9.99; Avar_TmD = -9.99; Amin_TmD = -9.99;}

  // Show on screen
  printf("\n SUMMARY for %s:", PevFname);
  printf("\n             typ   siz   wet  hard    hw   rho   thw     T  TopT  nTyp nSiz nWet nHrd  nHW nRHO nTH_W   nT nTmD");
  printf("\n AveScore: %5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5d,%4d,%4d,%4d,%4d,%4d,%5d,%4d,%4d",
	 Amean_typ, Amean_siz, Amean_wet, Amean_hard, Amean_hw, Amean_rho, Amean_th_w, Amean_T, Amean_TmD,
	 nTyp, nSiz, nWet, nHrd, nHW, nRHO, nTH_W, nT, nTmD);
  printf("\n      STD: %5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf",
	 Avar_typ, Avar_siz, Avar_wet, Avar_hard, Avar_hw, Avar_rho, Avar_th_w, Avar_T, Avar_TmD);
  printf("\n      Min: %5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf,%5.2lf",
	 Amin_typ, Amin_siz, Amin_wet, Amin_hard, Amin_hw, Amin_rho, Amin_th_w, Amin_T, Amin_TmD);

  printf("\n\n ProfEval_MIP V7 : End of run!\n\n");

  if( SUMMARY_TO_FILE )
    {
      // Append to output file and close output file
      fprintf(fout, "\n\t SUMMARY");
      fprintf(fout, "\t  typ\t  siz\t  wet\t hard\t   hw\t  rho\t  thw\t    T\t TopT\tnTyp\tnSiz\tnWet\t nHW\tnRHO\tnTH_W\t  nT\tnTmD");
      fprintf(fout, "\n\tAveScore:\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%4d\t%4d\t%4d\t%4d\t%4d\t%4d\t%5d\t%4d\t%4d",
	      Amean_typ, Amean_siz, Amean_wet, Amean_hard, Amean_hw, Amean_rho, Amean_th_w, Amean_T, Amean_TmD,
	      nTyp, nSiz, nWet, nHrd, nHW, nRHO, nTH_W, nT, nTmD);
      fprintf(fout, "\n\t     STD:\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf",
	      Avar_typ, Avar_siz, Avar_wet, Avar_hard, Avar_hw, Avar_rho, Avar_th_w, Avar_T, Avar_TmD);
      fprintf(fout, "\n\t     Min:\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf\t%5.2lf",
	      Amin_typ, Amin_siz, Amin_wet, Amin_hard, Amin_hw, Amin_rho, Amin_th_w, Amin_T, Amin_TmD);
    }

  fclose(fout);

  exit(NO_ERROR);

} // End main


/***********************************************************************************************/
/* End of ProfEval_MIP.c                                                                       */
/***********************************************************************************************/
