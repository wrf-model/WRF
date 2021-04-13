# RFC Forecasts Module

### General Purpose and Functionality

The purpose of this module is to integrate RFC (River Forecast Center) produced forecasts for individual reservoirs into
the Reservoir module in real-time. This utilizes the expertise that each RFC has for forecasting reservoirs in their domain.
This module replaces the normal Level Pool object for a particular reservoir that is selected to run as RFC_Forecast.


### Module Architecture

This module builds off of the same class structure and architecture of the Level Pool module. As in Level Pool, each reservoir in this
module is instantiated into an object at model initialization. The **RFC_Forecasts** directory contains the following files:

* **module_rfc_forecasts.F** defines and instantiates objects for an rfc forecasts type
reservoir. **module_RT.F** will call and pass parameters to the constructor in this module to instantiate the rfc forecasts reservoir
object and its sub-objects. The rfc forecasts reservoir type inherits input and output types from the reservoir base module and calls
instantiation of these into sub-objects. The rfc forecasts reservoir type also points to types for parameters and state and calls
instantiation of these into sub-objects. This reservoir type will also point to and instantiate a corresponding level pool reservoir, that
also runs at every timestep. The run function will output the corresponding discharges from the RFC time series discharge array. If the
corresponding RFC time series file is not found or the array values are unusable, then the level pool output will be used instead.

* **module_rfc_forecasts_properties.F** defines and instantiates objects for an rfc forecasts type reservoir's
properties. Properties holds static/unchanging variables that are set when the given reservoir object is
initialized/instantiated.

* **module_rfc_forecasts_state.F** defines and instantiates objects for an rfc forecasts type reservoir's state.
State holds and tracks dynamic/changing variables that are only relevant to the given reservoir object and not other
modules or areas of the system.

* **module_rfc_forecasts_tests.F** holds unit tests that test for all components of an rfc forecasts reservoir
are properly initialized.

* **module_reservoir_read_rfc_time_series_data.F**, within the **Reservoirs** directory, searches for and reads RFC time series
forecast files for an array of time series forecast values.

### Input Parameters

This module requires four input parameters that are set in hydro.namelist.

* ```reservoir_rfc_forecasts``` is a boolean parameter that will need to be set to ```.TRUE.``` for this module to be activated.
This will set the model variable ```reservoir_type_specified``` to ```TRUE``` and cause the model to read the reservoir_type variable from the
reservoir parameter file. The reservoir_type for a RFC (River Forecast Center) Forecast reservoir is currently set to '4' in the reservoir
parameter file.

* ```reservoir_rfc_forecasts_time_series_path``` is the path to all the
RFC time series files used by this module.

* ```reservoir_rfc_forecasts_lookback_hours``` is an integer parameter that specifies how many hours before the model start time the module will
search for a corresponding time series file, and '28' would be a typical value and is the default.

* ```reservoir_parameter_file``` is the NetCDF parameter file that holds the corresponding gage ID for each lake ID.

### Testing

To compile and run the unit tests, first go to the NDHMS directory and type
```
./compile_offline_NoahMP.sh template/setEnvar.sh
```

and hit enter. Then go to the Reservoir directory in the terminal and type

```
make
```

and hit enter. Then type

```
make test
```

and hit enter. Then type

```
./reservoir_tests
```

and hit enter.
The user should see "All Reservoir Tests Passed".
