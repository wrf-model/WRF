/* This code is part of Radiative Transfer for Energetics (RTE)

Contacts: Robert Pincus and Eli Mlawer
email:  rrtmgp@aer.com

Copyright 2024-
   Trustees of Columbia University in the City of New York
   All right reserved.

Use and duplication is permitted under the terms of the
    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause

This header files C-compatible Boolean and floating point types (see mo_rte_type.F90 for the Fortran equivalent)
  Adapted from code written by Chiel van Heerwaarden at Wageningen University and Research

*/
#pragma once

#ifdef RTE_USE_CBOOL
using Bool = signed char;
#else
using Bool = int;
#endif

#ifdef RTE_USE_SP
using Float = float;
const Float Float_epsilon = FLT_EPSILON;
#else
using Float = double;
const Float Float_epsilon = DBL_EPSILON;
#endif



