/*
 *  SNOWPACK stand-alone
 *
 *  Copyright WSL Institute for Snow and Avalanche Research SLF, DAVOS, SWITZERLAND
*/
/*  This file is part of libsnowpack.
    libsnowpack is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    libsnowpack is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with libsnowpack.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * @file libsnowpack.h
 * @version 10.02
 * This is the header file to include for projects making use of libsnowpack
 */

#ifndef LIBSNOWPACK_H
#define LIBSNOWPACK_H

#include <snowpack/Constants.h>
#include <snowpack/DataClasses.h>
#include <snowpack/Hazard.h>
#include <snowpack/Laws_sn.h>
#include <snowpack/Meteo.h>
#include <snowpack/Saltation.h>
#include <snowpack/SnowDrift.h>
#include <snowpack/SnowpackConfig.h>
#include <snowpack/Stability.h>
#include <snowpack/TechnicalSnow.h>
#include <snowpack/Utils.h>

#include <snowpack/plugins/SnowpackIO.h>
#include <snowpack/plugins/SnowpackIOInterface.h>
#include <snowpack/plugins/AsciiIO.h> //for direct calls to AsciiIO
#include <snowpack/plugins/SmetIO.h> //for direct calls to SmetIO

#include <snowpack/snowpackCore/Aggregate.h>
#include <snowpack/snowpackCore/Canopy.h>
#include <snowpack/snowpackCore/Metamorphism.h>
#include <snowpack/snowpackCore/PhaseChange.h>
#include <snowpack/snowpackCore/ReSolver1d.h>
#include <snowpack/snowpackCore/Snowpack.h>
#include <snowpack/snowpackCore/Solver.h>
#include <snowpack/snowpackCore/WaterTransport.h>

#endif

