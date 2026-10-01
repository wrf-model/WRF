/* This code is part of RRTMGP

Contacts: Robert Pincus and Eli Mlawer
email:  rrtmgp@aer.com

Copyright 2024-
   Trustees of Columbia University in the City of New York
   All right reserved.

Use and duplication is permitted under the terms of the
    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause

This header files defines the C bindings for the kernels used in RRTMGP
  Adapted from code written by Chiel van Heerwaarden at Wageningen University and Research

*/
#pragma once
#include "rte_types.h"

extern "C"
{
    void rrtmgp_interpolation(
        const int& ncol, const int& nlay,
        const int& ngas, const int& nflav, const int& neta,
        const int& npres, const int& ntemp,
        const int* flavor,  // (2,nflav)
        const Float* press_ref_log, // (npres)
        const Float* temp_ref, // (ntemp)
        const Float& press_ref_log_delta,
        const Float& temp_ref_min,
        const Float& temp_ref_delta,
        const Float& press_ref_trop_log,
        const Float* vmr_ref, //(2,ngas+1,ntemp)
        const Float* play, // (ncol,nlay)
        const Float* tlay,    // (ncol,nlay)
        const Float* col_gas, // (ncol,nlay,ngas+1)
        int* jtemp, // [out] (ncol*nlay)
        Float* fmajor, // [out] (2,2,2,ncol,nlay,nflav)
        Float* fminor, // [out[ (2,2,  ncol,nlay,nflav))
        Float* col_mix, // [out] (2,    ncol,nlay,nflav)
        Bool* tropo, // [out] size (ncol*nlay)
        int* jeta,// [out] size (2*ncol*nlay*nflav)
        int* jpress // [out] size (ncol*nlay)
    );

    void rrtmgp_compute_tau_absorption(
        const int& ncol, const int& nlay, const int& nband, const int& ngpt,
        const int& ngas, const int& nflav, const int& neta,
        const int& npres, const int& ntemp,
        const int& nminorlower, const int& nminorklower,
        const int& nminorupper, const int& nminorkupper,
        const int& idx_h2o,
        const int* gpoint_flavor, // (2,ngpt)
        const int* band_lims_gpt, // (2,nbnd)
        const Float* kmajor, // (ntemp,neta,npres+1,ngpt)
        const Float* kminor_lower, // (ntemp,neta,nminorklower)
        const Float* kminor_upper, // (ntemp,neta,nminorkupper)
        const int* minor_limits_gpt_lower, // (2,nminorlower)
        const int* minor_limits_gpt_upper, // (2,nminorupper)
        const Bool* minor_scales_with_density_lower, // (  nminorlower)
        const Bool* minor_scales_with_density_upper,// (  nminorupper)
        const Bool* scale_by_complement_lower,// (  nminorlower)
        const Bool* scale_by_complement_upper,// (  nminorupper)
        const int* idx_minor_lower, // (  nminorlower)
        const int* idx_minor_upper, // (  nminorupper)
        const int* idx_minor_scaling_lower,// (  nminorlower)
        const int* idx_minor_scaling_upper,// (  nminorupper)
        const int* kminor_start_lower, // (  nminorlower)
        const int* kminor_start_upper,// (  nminorupper)
        const Bool* tropo, // (ncol,nlay)
        const Float* col_mix, // (2,    ncol,nlay,nflav       )
        const Float* fmajor, // (2,2,2,ncol,nlay,nflav       )
        const Float* fminor, // (2,2,  ncol,nlay,nflav       )
        const Float* play, // (ncol,nlay)
        const Float* tlay, // (ncol,nlay)
        const Float* col_gas, // (ncol,nlay,ngas+1)
        const int* jeta, // (2,    ncol,nlay,nflav       )
        const int* jtemp, // (ncol,nlay)
        const int* jpress, // (ncol,nlay)
        Float* tau // [inout] (ncol,nlay.ngpt)
    );

    void rrtmgp_compute_tau_rayleigh(
        const int& ncol, const int& nlay, const int& nband, const int& ngpt,
        const int& ngas, const int& nflav, const int& neta, const int& npres, const int& ntemp,
        const int* gpoint_flavor, // (2,ngpt)
        const int* band_lims_gpt,  // (2,nbnd)
        const Float* krayl,  // (ntemp,neta,ngpt,2)
        const int& idx_h2o,
        const Float* col_dry, // (ncol,nlay)
        const Float* col_gas, // (ncol,nlay,ngas+1)
        const Float* fminor, // (2,2,ncol,nlay,nflav)
        const int* jeta, // (2,  ncol,nlay,nflav)
        const Bool* tropo, // (ncol,nlay)
        const int* jtemp, // (ncol,nlay)
        Float* tau_rayleigh  // [inout] (ncol,nlay.ngpt)
    );

    void rrtmgp_compute_Planck_source(
        const int& ncol, const int& nlay, const int& nbnd, const int& ngpt,
        const int& nflav, const int& neta, const int& npres, const int& ntemp,
        const int& nPlanckTemp,
        const Float* tlay, // (ncol,nlay  )
        const Float* tlev, // (ncol,nlay+1)
        const Float* tsfc, //(ncol       )
        const int& sfc_lay,
        const Float* fmajor, // (2,2,2,ncol,nlay,nflav)
        const int* jeta, // (2,    ncol,nlay,nflav)
        const Bool* tropo, // (            ncol,nlay)
        const int* jtemp,  // (            ncol,nlay)
        const int* jpress, // (            ncol,nlay)
        const int* gpoint_bands, // (ngpt)
        const int* band_lims_gpt, // (2, nbnd)
        const Float* pfracin, // (ntemp,neta,npres+1,ngpt)
        const Float& temp_ref_min, const Float& totplnk_delta,
        const Float* totplnk, // (nPlanckTemp,nbnd)
        const int* gpoint_flavor, // (2,ngpt)
        Float* sfc_src, // [out] (ncol,       ngpt)
        Float* lay_src, // [out] (ncol,nlay,  ngpt)
        Float* lev_src, // [out] (ncol,nlay+1,ngpt)
        Float* sfc_src_jac // [out] (ncol,       ngpt)
    );
}
