<!-- omit in toc -->
# WRF-TEB

WRF-TEB ([Meyer et al., 2020a]()) couples the single layer Town Energy Balance (TEB; [Masson, 2000](), and subsequent papers) to the Weather Research and Forecasting (WRF; [Skamarock et al., 2019](https://doi.org/10.5065/1dfh-6p97)). The WRF-TEB software is available in WRF from version 4.1.5 and can be downloaded at https://github.com/teb-model/wrf-teb.


- [Installation](#installation)
  - [WRF-CMake](#wrf-cmake)
  - [WRF](#wrf)
- [Usage](#usage)
- [How to cite](#how-to-cite)
- [Copyright and license](#copyright-and-license)
- [References](#references)


## Installation

The WRF-TEB software depends on the [TEB model software library](https://github.com/teb-model/teb) ([Meyer et al., 2020b]()). Depending on the flavour of WRF software you want to use (i.e. [WRF](https://github.com/wrf-model) or [WRF-CMake](https://github.com/WRF-CMake/wrf)), you may need to manually build the TEB software library on your system before attempting to build WRF.


### WRF-CMake

If you want to build using [WRF-CMake](https://github.com/WRF-CMake/wrf) ([Riechert and Meyer, 2010](https://doi.org/10.21105/joss.01468)) please follow the standard installation instructions used to [install WRF-CMake from source](https://github.com/WRF-CMake/wrf-teb/blob/wrf-teb/doc/cmake/INSTALL.md#install-wrf-wps-cmake-from-source). No additional steps are required as the [TEB model software library](https://github.com/teb-model/teb) is automatically downloaded and installed on your system at build time.


### WRF

If you want to build using the [official WRF model](https://github.com/wrf-model) (with Makefiles), you will need to first build TEB manually on your system before building WRF. Please ensure that you build TEB on you system following the [TEB installation instructions](https://github.com/teb-model/teb#installing) before attempting to install WRF. 

Then, before building WRF, you will have to set `WRF_TEB=1` and `TEB_PATH=/path/to/teb/build` as environment vars before running the `./configure` script as per [standard WRF build instructions](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php). Please ensure that both TEB and WRF are compiled using the same compiler vendor and version (e.g. `GNU 7.4.0`) as well as with the same compiler options (e.g. `Release`, `REAL8` ) to avoid issues at compile/run-time and, if you are using the standard WRF configure options, use `-DUSE_REAL8=OFF` when building the TEB model software library (i.e. `cmake -DUSE_REAL8=OFF ..`).


## Usage

Standard WRF user documentation applies then, to run a case in WRF with TEB enabled, choose TEB from the `namelist.input` file (i.e. `sf_urban_physics = 4`) and rename the file `URBPARM_TEB.TBL` file to `URBPARM.TBL`. Please make sure sure you adapt all values based on specific parameters for the city you are going to model.


## How to cite

When using WRF-TEB, please cite the paper and software (with version) as follow:

| Model              | Software and Version       |
| ------------------ | -------------------------- |
| Meyer et al., 2020 | [see Zenodo](https://XXXX) |

The corresponding reference list should be as follows:

> Meyer, D., and Schoetter, R., and Riechert, M., and Verrelle, A., and Tewari, M., and Dudhia, J., and Masson, V., and van Reeuwijk, M., and Grimmond, S., 2020a: WRF-TEB: Implementation and evaluation of the coupled Weather Researchand Forecasting (WRF) and Town Energy Balance (TEB) model. *Submitted to Journal of Advances in Modeling Earth Systems*.


## Copyright and license

Additional files provided by WRF-TEB are licensed under the MIT licence and indicated in the header of each source file as follows:

```
WRF-TEB (https://github.com/teb-model/wrf-teb).
Copyright <year> D. Meyer. Licensed under the MIT License.
```

## References

> Masson, V., 2000: A Physically-Based Scheme For The Urban Energy Budget In Atmospheric Models. Boundary-Layer Meteorology, 94, 357–397, https://doi.org/10.1023/A:1002463829265.

> Meyer, D., and Schoetter, R., and Riechert, M., and Verrelle, A., and Tewari, M., and Dudhia, J., and Masson, V., and van Reeuwijk, M., and Grimmond, S., 2020a: WRF-TEB: Implementation and evaluation of the coupled Weather Researchand Forecasting (WRF) and Town Energy Balance (TEB) model. *Submitted to Journal of Advances in Modeling Earth Systems*.

> Meyer, D., and Schoetter, R., and Masson, V., and Grimmond, S., 2020b: Enhanced software and platform for the Town Energy Balance (TEB) model.Journal of Open Source Software. https://doi.org/10.21105/joss.02008

> Riechert, M., and D. Meyer, 2019: WRF-CMake: integrating CMake support into the Advanced Research WRF (ARW) modelling system. Journal of Open Source Software, 4, 1468, https://doi.org/10.21105/joss.01468

> Skamarock, W. C., and Coauthors, 2019: A Description of the Advanced Research WRF Model Version 4. NCAR Technical Note NCAR/TN-556+STR, 145, https://doi.org/10.5065/1dfh-6p97.
