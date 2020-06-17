<!-- omit in toc -->
# WRF-TEB

WRF-TEB ([Meyer et al., 2020a](https://doi.org/10.1029/2019MS001961)) couples the single layer Town Energy Balance (TEB) model ([Masson, 2000](https://doi.org/10.1023/A:1002463829265), and subsequent papers) model, software ([Meyer et al., 2020b](https://doi.org/10.21105/joss.02008)), to the Weather Research and Forecasting (WRF; [Skamarock et al., 2019](https://doi.org/10.5065/1dfh-6p97)). The WRF-TEB software is available for download from version 4.1.5 at https://github.com/teb-model/wrf-teb.


- [Installation](#installation)
  - [WRF-CMake](#wrf-cmake)
  - [WRF](#wrf)
- [Usage](#usage)
- [How to cite](#how-to-cite)
- [Copyright and license](#copyright-and-license)
- [References](#references)


## Installation

The WRF-TEB software depends on the [TEB model software library](https://github.com/teb-model/teb) ([Meyer et al., 2020b](https://doi.org/10.21105/joss.02008)). Depending on the 'flavour' of WRF (software) you want to use (i.e. [WRF](https://github.com/wrf-model) or [WRF-CMake](https://github.com/WRF-CMake/wrf)), you may need to manually build the TEB model software library before attempting to build WRF on your system.


### WRF-CMake

No additional steps are required to build WRF-TEB using [WRF-CMake](https://github.com/WRF-CMake/wrf) ([Riechert and Meyer, 2010](https://doi.org/10.21105/joss.01468)) as the [TEB model software library](https://github.com/teb-model/teb) is automatically downloaded and installed on your system at build time. To install, download (and extract) WRF-TEB from the [WRF-TEB release page](https://github.com/TEB-model/wrf-teb/releases), then follow the [standard installation instructions to build WRF-CMake from source](https://github.com/TEB-model/wrf-teb/blob/wrf-cmake-teb/doc/cmake/INSTALL.md).

### WRF

To build WRF-TEB using the [official WRF model](https://github.com/wrf-model) (i.e. with Makefiles), first follow the standard [TEB installation instructions](https://github.com/teb-model/teb) to build the TEB model software library on your system.

Then, download (and extract) WRF-TEB from the [WRF-TEB release page](https://github.com/TEB-model/wrf-teb/releases), set `WRF_TEB=1` and `TEB_PATH=/path/to/teb/build` as environment variables before running `./configure`. From here, configure and build WRF as per [standard WRF build instructions](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php). Please ensure that both TEB and WRF are compiled using the same compiler vendor and version (e.g. `GNU 7.4.0`), and compiler options (e.g. `Release`, `REAL8` ) to avoid issues at compile/run-time. If you are using the standard WRF configure options, use `-DUSE_REAL8=OFF` when building the TEB model software library (i.e. `cmake -DUSE_REAL8=OFF ..`) as WRF defaults to 4 byte wide real.


## Usage

Standard/general [WRF documentation](https://www2.mmm.ucar.edu/wrf/users/docs/user_guide_v4/contents.html) applies. To run a case using WRF-TEB, first enable TEB from the `namelist.input` file (i.e. `sf_urban_physics = 4`), then rename the `URBPARM_TEB.TBL` file to `URBPARM.TBL`. Make sure to change options and paramter values to match your case study. More information about the options avalable in TEB/WRF-TEB are documented in [TEB model software library repository](https://github.com/teb-model/teb).


## How to cite

When using WRF-TEB, please cite the paper and software (with version) as follow:

| Model                                                       | Software and Version*             |
| ----------------------------------------------------------- | --------------------------------- |
| [Meyer et al., 2020a](https://doi.org/10.1029/2019MS001961) | [see Zenodo](https://zenodo.org/) |

The corresponding reference list should be as follows:

> Meyer, D., Schoetter, R., Riechert, M., Verrelle, A., Tewari, M., Dudhia, J., Masson, V., Reeuwijk, M., & Grimmond, S. (2020). WRF‐TEB: implementation and evaluation of the coupled Weather Research and Forecasting (WRF) and Town Energy Balance (TEB) model. Journal of Advances in Modeling Earth Systems. https://doi.org/10.1029/2019ms001961

*please make sure to cite the same version you are using with the correct DOI. For a list of all available versions see the list of versions on Zenodo.

## Copyright and license

Additional files provided in WRF-TEB are licensed under the MIT licence and indicated in the header of each source file with:

```
WRF-TEB (https://github.com/teb-model/wrf-teb).
Copyright <year> D. Meyer. Licensed under the MIT License.
```

## References

> Masson, V. (2000). A Physically-Based Scheme For The Urban Energy Budget In Atmospheric Models. Boundary-Layer Meteorology, 94(3), 357–397. https://doi.org/10.1023/a:1002463829265

> Meyer, D., Schoetter, R., Riechert, M., Verrelle, A., Tewari, M., Dudhia, J., Masson, V., Reeuwijk, M., & Grimmond, S. (2020a). WRF‐TEB: implementation and evaluation of the coupled Weather Research and Forecasting (WRF) and Town Energy Balance (TEB) model. Journal of Advances in Modeling Earth Systems. https://doi.org/10.1029/2019ms001961

> Meyer, D., Schoetter, R., Masson, V., & Grimmond, S. (2020b). Enhanced software and platform for the Town Energy Balance (TEB) model. Journal of Open Source Software, 5(50), 2008. https://doi.org/10.21105/joss.02008

> Riechert, M., & Meyer, D. (2019). WRF-CMake: integrating CMake support into the Advanced Research WRF (ARW) modelling system. Journal of Open Source Software, 4(41), 1468. https://doi.org/10.21105/joss.01468

> Skamarock, W. C., Klemp, J. B., Dudhia, J., Gill, D. O., Liu, Z., Berner, J., … Huang, X.-Y. (2019). A Description of the Advanced Research WRF Model Version 4. https://doi.org/10.5065/1DFH-6P97
