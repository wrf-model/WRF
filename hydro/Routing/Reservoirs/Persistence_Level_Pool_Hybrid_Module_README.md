# Peristence-Level Pool Hybrid Module

### General Purpose and Functionality

The purpose of this module is to persist observed reservoir discharges in order to improve the accuracy of discharge predictions. This
module replaces the normal Level Pool object for a particular reservoir that is selected to run as Persistence type though it does
instantiate a Level Pool object as a member. The Hybrid object simultaneously runs this Level Pool member to calculate a release
and combines that release with an observed discharge to produce a final release at each timestep.

The observed discharge and the calculated
level pool release are each multiplied by a fractional weight and then summed to give a final calculated release. The fractional weights
are determined by the length of model time between the observed discharge and the current release calculation. For example, if a release
is calculated 12 hours in model time after an observation is read, then a weight of 1.0 might be applied to this observed discharge, and
a corresponding weight of 0.0 would be applied to the level pool calculated release. The resulting summed release would be the same as
the observed discharge. If a release is calculated 5 days in model time after an observation is read, then a weight of 0.2 might be
applied to the observed discharge, and a corresponding weight of 0.8 would be applied to the level pool calculated release. These weighted
values would then be summed to give a final release. If observations are missing or are not good quality for a given time window, then
full weight of 1.0 is given to the level pool release.

Partial autocorrelation is performed in advance from historical reservoir data to determine the appropriate weights for each reservoir.
These weights are read in from a reservoir persistence parameter file whenever a reservoir of this type is initialized.

Before the final calculated release/outflow is returned from this module back to the model at each timestep, mass balance checks are
performed to ensure the calculated release does not cause the reservoir storage to fall below the minimum or exceed the max, and the
release is modified accordingly.


### Module Architecture

This module builds off of the same class structure and architecture of the Level Pool module. As in Level Pool, each reservoir in this
module is instantiated into an object at model initialization. The **Persistence_Level_Pool_Hybrid** directory contains the following files:

* **module_persistence_levelpool_hybrid.F** defines and instantiates objects for a hybrid persistence levelpool type
reservoir. **module_RT.F** will call and pass properties to the constructor in this module to instantiate the hybrid reservoir
object and its sub-objects. The hybrid reservoir type inherits input and output types from the reservoir base module and calls
instantiation of these into sub-objects. The hybrid reservoir type also points to types for hybrid properties and state and calls
instantiation of these into sub-objects. A pointer to a levelpool reservoir object is also held in state, and this module
instantiates that levelpool object. There is also a subroutine to run hybrid reservoir that is derived from the reservoir base
type interface to run reservoir. The run reservoir function will periodically call a function in **module_reservoir_read_timeslice_data.F**
that will read a timeslice file and return a corresponding observed discharge. The
timeslice files will be read at a particular update time. For a particular timestep, the first hybrid reservoir on each processor to
reach an update time at that timestep will call the function to read the timeslice files, which will read the observations for every
reservoir and store those values in an array. Each subsequent reservoir held by the same processor at that timestep that reaches its
update time will read its corresponding observation from the array. The run reservoir function also performs the functionality described
above including calling level pool run reservoir along with weighting and combining that release with the weighted observed discharge,
and finally calling mass balance checks before returning the release/outflow back to the model.

* **module_persistence_levelpool_hybrid_properties.F** defines and instantiates objects for a hybrid type reservoir's
parameters/properties. Properties holds static/unchanging variables that are set when the given reservoir object is
initialized/instantiated.

* **module_persistence_levelpool_hybrid_state.F** defines and instantiates objects for a hybrid type reservoir's state.
State holds and tracks dynamic/changing variables that are only relevant to the given hybrid reservoir object and not other
modules or areas of the system.

* **module_persistence_levelpool_hybrid_tests.F** holds unit tests that test for all components of a hybrid reservoir
are properly initialized.

* **module_reservoir_read_timeslice_data.F** within the **Reservoirs** directory, reads USGS (U.S. Geological Survey) or
USACE (U.S. Army Corps of Engineers) timeslice files to get gage discharge values that will be used by reservoirs. An
observation lookback period is passed in to determine how far back in time from the current model time the module will
look for timeslice files. The observation resolution determines the time increments the module will look back. For instance,
a standard lookback period would be 24 hours with an observation resolution of 15 minutes, where a model current time of
8:00 PM would search for timeslice files at every 15 minute increment between 8:00 PM that day and 8:00 PM the day before. The module will
first search for the most recent timeslice files and grab the discharge for a particular lake/reservoir if the gage quality
standard is met at that time. If a gage discharge is missing or if the gage quality standard is not met for any particular
lake/reservoir in the given timeslice file, the module will continue to look back at every observation resolution increment
until either all lakes/reservoirs have a good quality discharge or the end of the lookback period is reached. The total
lookback seconds from current model time that the discharge is read will also be returned.

* **module_reservoir_utilities.F**, also within the **Reservoirs** directory,
contains multiple functions used by the Hybrid module. The modify_for_projected_storage function is called from the
hybrid run reservoir function and modifies the outflow if a projected storage falls below the minimum or exceeds the
maximum for the reservoir. There are also multiple functions used for reading variables from the reservoir
persistence parameter and timeslice NetCDF files.


### Input Parameters

This module requires six input parameters that are set in hydro.namelist.

* ```reservoir_persistence_usgs``` is a boolean parameter that will need to be set to ```.TRUE.``` for this module to be activated for USGS persistence. This will set
the model variable ```reservoir_type_specified``` to ```TRUE``` and cause the model to read the reservoir_type variable from the reservoir parameter file.
The reservoir_type for a USGS hybrid persistence reservoir is currently set to '2' in the reservoir parameter file.

* ```reservoir_persistence_usace``` is a boolean parameter that will need to be set to ```.TRUE.``` for this module to be activated for USACE. This will set
the model variable ```reservoir_type_specified``` to ```TRUE``` and cause the model to read the reservoir_type variable from the reservoir parameter file.
The reservoir_type for a USCAE hybrid persistence reservoir is currently set to '3' in the reservoir parameter file.

* ```reservoir_parameter_file``` is the NetCDF parameter file that holds the weights and corresponding gage ID for each lake ID.

* ```reservoir_usgs_timeslice_path``` is the path to all USGS
timeslice files used by this module.

* ```reservoir_usace_timeslice_path``` is the path to all USACE
timeslice files used by this module.

* ```reservoir_observation_lookback_hours``` is an integer parameter that specifies how many hours before the model start time the module will
search for a corresponding timeslice file, and '24' would be a typical value and is the default.

* ```reservoir_observation_update_time_interval_seconds``` is an integer parameter that determines how often the reservoirs will look for a new timeslice
observation, and '86400', the number of seconds in a day, is the default. This should be set to 1,000,000,000 seconds for Short-Range
and Medium-Range Forecasts in order to prevent the module from reading any new timeslice observations after the first timestep. It should be set to 3600
seconds for Standard and Extended AnA simulations.


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
