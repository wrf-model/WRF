/* This code is part of Radiative Transfer for Energetics (RTE)

Contacts: Robert Pincus and Eli Mlawer
email:  rrtmgp@aer.com

Copyright 2024-
   Trustees of Columbia University in the City of New York
   All right reserved.

Use and duplication is permitted under the terms of the
    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause

This header files defines the C bindings for the kernels used in RTE
  Adapted from code written by Chiel van Heerwaarden at Wageningen University and Research

*/

#include "rte_types.h"

extern "C"
{
    //
    // Shortwave solvers
    //
    void rte_sw_solver_noscat(
            const int& ncol, const int& nlay, const int& ngpt, const Bool& top_at_1,
            const Float* tau,          // (ncol,nlay,  ngpt)
            const Float* mu0,          // (ncol,nlay)
            const Float* inc_flux_dir, // (ncol,       ngpt)
            Float* flux_dir);  // [out]   (ncol,nlay+1,ngpt)

    void rte_sw_solver_2stream(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt, 
            const Bool& top_at_1,
            const Float* tau,          // (ncol,nlay,  ngpt)
            const Float* ssa,          // (ncol,nlay,  ngpt)
            const Float* g,            // (ncol,nlay,  ngpt)
            const Float* mu0,          // (ncol,nlay)
            const Float* sfc_alb_dir,  // (ncol,       ngpt)
            const Float* sfc_alb_dif,  // (ncol,       ngpt)
            const Float* inc_flux_dir, // (ncol,       ngpt)
            Float* flux_up,    // [out]   (ncol,nlay+1,ngpt)
            Float* flux_dn,    // [out]   (ncol,nlay+1,ngpt)
            Float* flux_dir,   // [out]   (ncol,nlay+1,ngpt)
            const Bool& has_dif_bc,
            const Float* inc_flux_dif, // (ncol,       ngpt)
            const Bool& do_broadband,
            Float* broadband_up,    // [out]   (ncol,nlay+1)
            Float* broadband_dn,    // [out]   (ncol,nlay+1)
            Float* broadband_dir);  // [out]   (ncol,nlay+1)

    void rte_lw_solver_noscat(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            const Bool& top_at_1, 
            const int& nmus,
            const Float* secants, // (nmus)
            const Float* weights, // (nmus)
            const Float* tau,        // (ncol,nlay,  ngpt)
            const Float* lay_source, // (ncol,nlay,  ngpt)
            const Float* lev_source, // (ncol,nlay+1,ngpt)
            const Float* sfc_emis,   // (ncol,       ngpt)
            const Float* sfc_src,    // (ncol,       ngpt)
            const Float* inc_flux,   // (ncol,       ngpt)
            Float* flux_up,  // [out]   (ncol,nlay+1,ngpt)
            Float* flux_dn,  // [out]   (ncol,nlay+1,ngpt)
            const Bool& do_broadband,
            Float* broadband_up,
                             // [out]   (ncol,nlay+1)
            Float* broadband_dn,
                             // [out]   (ncol,nlay+1)
            const Bool& do_jacobians,
            const Float* sfc_src_jac,
                                   // (ncol,       ngpt)
            Float* flux_up_jac,
                           // [out]   (ncol,nlay+1,ngpt)
            const Bool& do_rescaling,
            const Float* ssa,      // (ncol,nlay,  ngpt)
            const Float* g);       // (ncol,nlay,  ngpt)


    void rte_lw_solver_2stream(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt, 
            const Bool& top_at_1,
            const Float* tau,        // (ncol,nlay,  ngpt)
            const Float* ssa,        // (ncol,nlay,  ngpt)
            const Float* g,          // (ncol,nlay,  ngpt)
            const Float* lay_source, // (ncol,nlay,  ngpt)
            const Float* lev_source, // (ncol,nlay+1,ngpt)
            const Float* sfc_emis,   // (ncol,       ngpt)
            const Float* sfc_src,    // (ncol,       ngpt)
            const Float* inc_flux,   // (ncol,       ngpt)
            Float* flux_up,  // [out]   (ncol,nlay+1,ngpt)
            Float* flux_dn   // [out]   (ncol,nlay+1,ngpt)
    );

    //
    // OPTICAL PROPS - INCREMENT
    //
    void rte_increment_1scalar_by_1scalar(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            const Float* tau_in); //     (ncol,nlay,ngpt)


    void rte_increment_1scalar_by_2stream(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,ngpt)
            const Float* ssa_in); //     (ncol,nlay,ngpt)

    void rte_increment_1scalar_by_nstream(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,ngpt)
            const Float* ssa_in); //     (ncol,nlay,ngpt)

    void rte_increment_2stream_by_1scalar(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlay,ngpt)
            const Float* tau_in); //     (ncol,nlay,ngpt)

    void rte_increment_2stream_by_2stream(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlay,ngpt)
            Float*   g_inout, // [inout] (ncol,nlay,ngpt)
            const Float* tau_in, //      (ncol,nlay,ngpt)
            const Float* ssa_in, //      (ncol,nlay,ngpt)
            const Float*   g_in); //     (ncol,nlay,ngpt)


    void rte_increment_2stream_by_nstream(
            const int& ncol, const int& nlay, const int& ngpt, const int& nmom,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlay,ngpt)
            Float*   g_inout, // [inout] (ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,ngpt)
            const Float* ssa_in,  //     (ncol,nlay,ngpt)
            const Float*   p_in); //(nmom,ncol,nlay,ngpt)

    void rte_increment_nstream_by_1scalar(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,ngpt)
            const Float* ssa_in); //     (ncol,nlay,ngpt)

    void rte_increment_nstream_by_2stream(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            const int& nmom1,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlay,ngpt)
            Float*   p_inout, // [inout] (nmom,ncol,nlay,ngpt)
            const Float* tau_in, //      (ncol,nlay,ngpt)
            const Float* ssa_in, //      (ncol,nlay,ngpt)
            const Float*   g_in); //     (ncol,nlay,ngpt)

    void rte_increment_nstream_by_nstream(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            const int& nmom1, 
            const int& nmom2,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlay,ngpt)
            Float*   p_inout, // [inout](nmom1,ncol,nlay,ngpt)
            const Float* tau_in,  //   (ncol,nlay,ngpt)
            const Float* ssa_in,  //   (ncol,nlay,ngpt)
            const Float*   p_in); //  (nmom2,ncol,nlay,ngpt)

    //
    // OPTICAL PROPS - INCREMENT BYBND
    //
    void rte_inc_1scalar_by_1scalar_bybnd(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            Float* tau_inout,// [inout] (ncol,nlay,ngpt)
            const Float* tau_in, //     (ncol,nlay,nbnd)
            const int& nbnd,
            const int* band_lims_gpoint); // (2,nbnd)

    void rte_inc_1scalar_by_2stream_bybnd(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,nbnd)
            const Float* ssa_in,  //     (ncol,nlay,nbnd)
            const int& nbnd,
            const int* band_lims_gpoint); // (2,nbnd)

    void rte_inc_1scalar_by_nstream_bybnd(
            const int& ncol,
            const int& nlay,
            const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,nbnd)
            const Float* ssa_in,  //     (ncol,nlay,nbnd)
            const int& nbnd,
            const int* band_lims_gpoint); // (2,nbnd)

    void rte_inc_2stream_by_1scalar_bybnd(
            const int& ncol,
            const int& nlay,
            const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,nbnd)
            const int& nbnd,
            const int* band_lims_gpoint); // (2,nbnd)

    void rte_inc_2stream_by_2stream_bybnd(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlay,ngpt)
            Float* g_inout,   // [inout] (ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,nbnd)
            const Float* ssa_in,  //     (ncol,nlay,nbnd)
            const Float* g_in,    //     (ncol,nlay,nbnd)
            const int& nbnd,
            const int* band_lims_gpoint); // (2,nbnd)

    void rte_inc_2stream_by_nstream_bybnd(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt, 
            const int& nmom,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlay,ngpt)
            Float* g_inout,   // [inout] (ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,nbnd)
            const Float* ssa_in,  //     (ncol,nlay,nbnd)
            const Float* p_in,   // (nmom,ncol,nlay,nbnd)
            const int& nbnd,
            const int* band_lims_gpoint); // (2,nbnd)

    void rte_inc_nstream_by_1scalar_bybnd(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,nbnd)
            const int& nbnd,
            const int* band_lims_gpoint); // (2,nbnd)

    void rte_inc_nstream_by_2stream_bybnd(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt, 
            const int& nmom1,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlay,ngpt)
            Float* p_inout,
                        // [inout]  (nomo,ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,nbnd)
            const Float* ssa_in,  //     (ncol,nlay,nbnd)
            const Float* g_in,    //     (ncol,nlay,nbnd)
            const int& nbnd,
            const int* band_lims_gpoint); // (2,nbnd)

    void rte_inc_nstream_by_nstream_bybnd(
            const int& ncol, 
            const int& nlay, 
            const int& ngpt, 
            const int& nmom1, 
            const int& nmom2,
            Float* tau_inout, // [inout] (ncol,nlay,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlay,ngpt)
            Float* p_inout,
                        // [inout]  (nomo,ncol,nlay,ngpt)
            const Float* tau_in,  //     (ncol,nlay,nbnd)
            const Float* ssa_in,  //     (ncol,nlay,nbnd)
            const Float* p_in,   // (nmom,ncol,nlay,nbnd)
            const int& nbnd,
            const int* band_lims_gpoint); // (2,nbnd)

    //
    // OPTICAL PROPS - DELTA SCALING
    //
    void rte_delta_scale_2str_k(
            const int& ncol, const int& nlay, const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlev,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlev,ngpt)
            Float* g_inout);  // [inout] (ncol,nlev,ngpt)

    void rte_delta_scale_2str_f_k(
            const int& ncol, const int& nlay, const int& ngpt,
            Float* tau_inout, // [inout] (ncol,nlev,ngpt)
            Float* ssa_inout, // [inout] (ncol,nlev,ngpt)
            Float* g_inout,   // [inout] (ncol,nlev,ngpt)
            const Float* f);  //         (ncol,nlev,ngpt)

    //
    // OPTICAL PROPS - SUBSET
    //
    void rte_extract_subset_dim1_3d(
            const int& ncol, const int& nlay, const int& ngpt,
            Float* array_in, // (ncol,nlay,ngpt)
            const int& ncol_start, const int& ncol_end,
            Float* array_out); // [out] (ncol_end-ncol_start+1,nlay,ngpt)

    void rte_extract_subset_dim2_4d(
            const int& nmom, const int& ncol, const int& nlay, const int& ngpt,
            const Float* array_in, // (nmom,ncol,nlay,ngpt)
            const int& ncol_start, const int& ncol_end,
            Float* array_out); // [out] (nmom,ncol_end-ncol_start+1,nlay,ngpt)

    void rte_extract_subset_absorption_tau(
            const int& ncol, const int& nlay, const int& ngpt,
            const Float* tau_in, // (ncol,nlay,ngpt)
            const Float* ssa_in, // (ncol,nlay,ngpt)
            const int& ncol_start, const int& ncol_end,
            Float* tau_out);  // [out] (ncol_end-ncol_start+1,nlay,ngpt)

    //
    // Fluxes - reduction
    //
    void rte_sum_broadband(
            const int& ncol, const int& nlev, const int& ngpt,
            const Float* gpt_flux, // (ncol,nlev,ngpt)
            Float* flux); //  [out]   (ncol,nlev)

    void rte_net_broadband_full(
            const int& ncol, const int& nlev, const int& ngpt,
            const Float* gpt_flux_dn, // (ncol,nlev,ngpt)
            const Float* gpt_flux_up, // (ncol,nlev,ngpt)
            Float* flux_net); // [out]   (ncol,nlev)

    void rte_net_broadband_precalc(
            const int& ncol, const int& nlev,
            const Float* broadband_flux_dn, // (ncol, nlev)
            const Float* broadband_flux_up, // (ncol, nlev)
            Float* broadband_flux_net);//[out] (ncol, nlev)

    void rte_sum_byband(
            const int& ncol, const int& nlev, const int& ngpt, const int& nbnd,
            const int* band_lims, // dimension(2, nbnd)
            const Float* gpt_flux, // (ncol, nlev, ngpt)
            Float* bnd_flux); // [out] (ncol, nlev, ngpt)

    void rte_net_byband_full(
      const int& ncol, const int& nlev, const int& ngpt, const int& nbnd, int* band_lims,
      const Float* bnd_flux_dn, // (ncol,nlev,nbnd)
      const Float* bnd_flux_up, // (ncol,nlev,nbnd)
            Float* bnd_flux_net); // [out] (ncol,nlev)
    //
    // Array utilities
    //
    void zero_array_1D(
      const int& ni,
      Float* array); // [out] (ni)

    void zero_array_2D(
      const int& ni, const int& nj,
      Float* array); // [out] (ni, nj)

    void zero_array_3D(
      const int& ni, const int& nj, const int& nk,
      Float* array); // [out] (ni, nj, nk)

    void zero_array_4D(
      const int& ni, const int& nj, const int& nk, const int& nl,
      Float* array); // [out] (ni, nj, nk, nl)

}
