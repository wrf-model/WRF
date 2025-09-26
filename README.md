# CRYOWRF v1.0

## Description

CRYOWRF is a coupled atmosphere-snow cover model with WRF (v4.2.1) acting as the atmospheric core and SNOWPACK acting as snow cover model. SNOWPACK is an option to be used as land-surface scheme within WRF and interacts with WRF via the specially developed coupler. 

In the process of coupling, significant changes had to be made to both WRF. See the [here](changelog_WRF.md) for the changelog for WRF modifications. 
Additionally, some edits were made to the WPS programs as well. A list of those edits can be found [here](changelog_WPS.md)

For installation instructions read [this](INSTALL.md)

## Developed by

CRYOWRF is developed by members of the Laboratory of Cryospheric Sciences (CRYOS), ENAC, EPFL, Lausanne

and 

the Snow processes group, Snow and Atmosphere Research Unit, SLF Davos.

v1.0: Varun Sharma (varun.sharma@epfl.ch / varun.sharma@slf.ch / varun.sharma@sunwell.tech), Franziska Gerber (franziska.gerber@slf.ch)

## References
* For scientific background: **Introducing CRYOWRF v1.0: Multiscale atmospheric flow simulations with advanced snow cover modelling**, Submitted to Geoscientific Model Development (DOI to be added later)
* Fpr goodies for running simulations and postprocessing: **Sharma, Varun (2021). Reproducibility Dataset for CRYOWRF v1.0. EnviDat. doi:10.16904/envidat.232**

## Funding

This development was funded by:
* Swiss National Science Foundation's grant for "From cloud to ground: Snow deposition in extreme environments". Grant id: 200020-179130
* Ecole Polytechnique Federale de Lausanne (EPFL) ENAC-School of Architecture, Civil and Environmental Engineering's Big Call grant: Project LOSUMEA
* Swiss National Supercomputer Center (CSCS) projects s873 and s938 computing resources grant

## LICENSE
CRYOWRF v1.0 is released with GNU General Public License Version 3. See attached license file, LICENSE.txt for details.
Each component within CRYOWRF comes with its own license policy. Please find individual licenses for WRF, WPS, meteoio and SNOWPACK in their respective folders.

