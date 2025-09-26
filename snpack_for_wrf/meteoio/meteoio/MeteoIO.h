/***********************************************************************************/
/*  Copyright 2009-2011 WSL Institute for Snow and Avalanche Research    SLF-DAVOS */
/***********************************************************************************/
/* This file is part of MeteoIO.
    MeteoIO is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MeteoIO is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with MeteoIO.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef METEOIO_H
#define METEOIO_H

#ifdef _MSC_VER
//VC++ complains that it can not generate an assignment operator
//for some classes (those having CONST members)
	#pragma warning (disable:4512)
#endif

//list in alphabetical order
//find meteoio -name "*.h" | sort | xargs -i echo "#include <{}>"
#include <meteoio/Config.h>

#include <meteoio/dataClasses/Array1D.h>
#include <meteoio/dataClasses/Array2D.h>
#include <meteoio/dataClasses/Array3D.h>
#include <meteoio/dataClasses/Array4D.h>
#include <meteoio/dataClasses/CoordsAlgorithms.h>
#include <meteoio/dataClasses/Coords.h>
#include <meteoio/dataClasses/Date.h>
#include <meteoio/dataClasses/DEMObject.h>
#include <meteoio/dataClasses/DEMAlgorithms.h>
#include <meteoio/dataClasses/Grid2DObject.h>
#include <meteoio/dataClasses/Grid3DObject.h>
#include <meteoio/dataClasses/Matrix.h>
#include <meteoio/dataClasses/MeteoData.h>
#include <meteoio/dataClasses/StationData.h>
#include <meteoio/dataClasses/Buffer.h>

#include <meteoio/DataGenerator.h>
#include <meteoio/FileUtils.h>
#include <meteoio/dataGenerators/GeneratorAlgorithms.h>
#include <meteoio/Graphics.h>
#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>
#include <meteoio/IOExceptions.h>
#include <meteoio/IOHandler.h>
#include <meteoio/IOInterface.h>
#include <meteoio/TimeSeriesManager.h>
#include <meteoio/GridsManager.h>
#include <meteoio/IOManager.h>
#include <meteoio/IOUtils.h>
//#include <meteoio/MainPage.h> //only for doxygen
#include <meteoio/MathOptim.h>
//#include <meteoio/MessageBoxX11.h>
#include <meteoio/Meteo1DInterpolator.h>
#include <meteoio/Meteo2DInterpolator.h>

//skip all the filters' implementations header files
#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <meteoio/meteoFilters/ProcessingStack.h>
//#include <meteoio/meteoFilters/template.h>
#include <meteoio/meteoFilters/WindowedFilter.h>

//#include <meteoio/MeteoIO.h>

#include <meteoio/meteoLaws/Atmosphere.h>
#include <meteoio/meteoLaws/Meteoconst.h>
#include <meteoio/meteoLaws/Sun.h>
#include <meteoio/meteoLaws/Suntrajectory.h>

#include <meteoio/MeteoProcessor.h>
//#include <meteoio/meteoStats/libfit1DCore.h>
#include <meteoio/meteoStats/libfit1D.h>
#include <meteoio/meteoStats/libinterpol1D.h>
#include <meteoio/meteoStats/libinterpol2D.h>
#include <meteoio/meteoStats/libresampling2D.h>
#include <meteoio/meteoStats/RandomNumberGenerator.h>

//skip all plugins' implementations header files
#include <meteoio/plugins/libsmet.h>

#include <meteoio/meteoResampling/ResamplingAlgorithms.h>
#include <meteoio/Timer.h>

#endif
