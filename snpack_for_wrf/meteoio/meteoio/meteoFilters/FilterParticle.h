/***********************************************************************************/
/*  Copyright 2019 Avalanche Warning Service Tyrol                  LWD-TIROL      */
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

#ifndef FILTERPARTICLE_H
#define FILTERPARTICLE_H

#include <meteoio/dataClasses/Matrix.h>
#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <meteoio/meteoStats/RandomNumberGenerator.h>

#include <meteoio/thirdParty/tinyexpr.h>

#include <inttypes.h> //for RNG int types
#include <string>
#include <vector>

namespace mio {

/**
 * @class FilterParticle
 * @ingroup processing
 * @author Michael Reisecker
 * @date   2019-07
 * @brief A Monte Carlo sampling method: the <a href="https://en.wikipedia.org/wiki/Particle_filter">particle filter</a>.
 * @details
 *
 * \ref particleintroduction <br>
 * \ref particlefilter : \ref particleoverview, \ref particlealgorithm, \ref particleexample, \ref particleexampleliterature, \ref particleexamplegridded <br>
 * \ref particlefeatures : \ref particleresampling, \ref particleestimation, \ref particleonline, \ref particleseeding <br>
 * \ref particlekeys, \ref statfilterroadmap <br>
 * \ref statfilterbibliography
 *
 * @section particleintroduction Introduction
 *
 * @note Some concepts are explained beforehand in FilterKalman, you should read that first. The documentation is meant to
 * be read in conjunction.
 * @note It is advised that you have a TeX installation in place to compile this part of the documentation yourself.
 *
 * __Disclaimer__: These particle and Kalman filters are <b>not</b> meant for operational use, rather they aim to be an accessible framework to
 * scientific research backed by the convenience of MeteoIO, to quickly try a few things.
 *
 * @section particlefilter The particle filter
 *
 * This program implements a particle filter, also called a _sequential Monte Carlo filter_. Briefly, a particle filter propagates a state
 * in state space by _drawing from a proposed importance density_ and weights these particles according to the _likelihood that the system
 * is in a certain state_ taking into account noisy (!) measurements via Bayesian statistics.
 *
 * Implemented is a SIR (Sampling Importance Resampling) algorithm, which is derived from the SIS (Sequential Importance Sampling) algorithm by
 * - Choosing the prior as proposed importance density.
 * - Resampling at every time step (although this can be tuned with a simple heuristic here).
 *
 * (For a SIS algorithm, we would not use the prior pdf as importance density q, but rather choose one that is specific to the model.
 * Then, the update formula for the weights becomes a little more complex, but the work is in actually choosing q. Such filters may be more
 * robust against outliers.)
 *
 * It operates in two stages: _prediction and update_ based on new measurements coming in by applying a recursive Bayesian filter.
 * This can be done online, which means that any new measurement can be augmented without having to recalculate the past.
 * Although these are Monte Carlo simulations, they form a _rigorous framework for state dynamics_. With a large enough number of particles,
 * the filter approximates the optimal solution to the problem of estimating the most likely state of a system taking into account all
 * available information.
 *
 * Physically speaking, we generate _mass-energy-points_ (which, in our favor, can't change too rapidly due to their inertia)
 * and place them in the state space. Mathematically, this is represented by delta-functions that are scaled by
 * the prior probability density and summing over them yields the probability p, or rather, an approximation thereof.
 * These particles are assigned a weight that is essentially (derived from) the _Bayesian statistics formula_
 * \f$P(A|B) = P(B|A) * P(A) / P(B)\f$ expressing a degree of belief in the estimated state.
 *
 * Contrary to a Kalman filter (FilterKalman), a particle filter is able to emulate truly _nonlinear model functions_ as well as _arbitrary
 * probability density functions_.
 *
 * The syntax follows Ref. [AM+02] and all remarks here can be investigated further therein.
 *
 * INPUT:
 *
 * <b>Two models are required</b>:
 * - A model describing the _evolution of the state_ with time (the system model).
 * - A model _relating the noisy measurements_ to the state (the measurement or observation model).
 *
 * OUTPUT:
 *
 * - The _likely posterior probability density function_ of the state based on all available information.
 *
 * @subsection particleoverview Overview
 *
 * The particle filter needs a <b>model function</b> that describes the evolution of the states, and its <b>starting values</b>.
 * Alternatively, you can pick a meteo parameter from your dataset that a curve is fitted against. You also need an
 * analytical <b>observation model</b> to relate the measured observables to the internal states of the model function. If the
 * model describes the measured parameter directly, then \f$obx(x) = x\f$. Finally, the <b>distributions</b> for the model noise,
 * observation noise, and prior distribution (for \f$x_0\f$) must be configured.
 *
 * This implementation propagates a single state only, but you can obviously run the filter on each meteo parameter separately
 * and you can also use every meteo parameter in the model. However, there is no way to update more than one state in the model
 * equation itself (i. e. no vector maths).
 *
 * @subsection particlealgorithm Algorithm
 *
 * The following algorithm is implemented (algorithm 4 of Ref. [AM+02]):
 * 1. Set an <b>initial state</b> \f$x_{0, 0}\f$. Add noise to this with the prior distribution to generate \f$x_{0, 1...N}\f$
 * starting particles.
 * 2. For each of the `N` particles at \f$T=0\f$ compute the <b>model function</b>.
 * 3. Add <b>system noise</b> to each particle with the model noise distribution.
 * 4. Calculate the <b>observation function</b> of each particle and add noise with the observation noise distribution.
 * 5. Calculate the <b>likelihood</b> of this noisy modeled observation given the real measurement via the observation
 * distribution's probability density function.
 * 6. Set <b>weights</b> for the particles connected to this likelihood ([AM+02] Eq. 63).
 * 7. <b>Resample</b> the particle trajectories (algorithm 2 of Ref. [AM+02]).
 * 8. Repeat 2-7 for each time step `T`.
 * 9. Take the <b>mean particle path</b> (or the one with the highest weight) and calculate the observation function.
 *
 * @note Critique about these workings welcome <a href="https://models.slf.ch/p/meteoio/issues/">here</a>. The core parts
 * in the code are prefaced with `* PARTICLE FILTER *` and `* KALMAN FILTER *` and shouldn't be too hard to read.
 *
 * @subsection particleexample Example: Mockup snow model
 *
 * Let's work through another example. Once more we fake an observation so we know the true process.
 * If some snow melts in the first half of the month before it starts to snow again we could describe this with:
 * \f[
 * x_k = x_{k-1} * a + b * u
 * \f]
 * With `a` and `b` being simple scalars and `u` being a factor that depends on the time step `k`.
 * The program in `/doc/examples/statistical_filters.cc` will calculate this function for some preset \f$x_0\f$, add system and
 * observation noise to it, and write the simulated measurements to a file, as well as `u`.
 * Within MeteoIO, we act as if this was a real measurement.
 *
 * @note Ref. [MG+14] is a great text to see these Bayesian methods be put to use for meteorological data. You will find a lot
 * of useful information including a small snow model. [LM11] is written to similar ends. Both papers have a look at whether
 * these statistical filters are viable for snow cover estimation and they compare different methods in basic research form.
 *
 * @code
 * HS::FILTER1 = PARTICLE
 * @endcode
 *
 * We enter the <b>system model</b> with the `SYSTEM_MODEL` key. The control signal `u` from above is saved into the
 * meteo data as `CTRL`, so when MeteoIO runs it has it available as this parameter. The <b>observation model</b> is simply
 * \f$obs(x) = x\f$ again (cf. identity matrix in sensor fusion example) and we assume 2.5 m snow <b>initially</b>:
 * @code
 * MODEL_FUNCTION     = x_km1 * 0.999 + 0.001 * meteo(CTRL)
 * OBS_MODEL_FUNCTION = xx
 * INITIAL_STATE      = 2.5
 * @endcode
 *
 * \image html particlefilter_hs_overview.png "Fig. 1: Overview of the mockup snow model: The dark blue line is the true model function, light blue is with superimposed noise (twice actually - with system and observation noise), and the violet one is Kalman filtered."
 *
 * @note You can use the standard arithmetic operations, angular functions, exponentials, and some combinatorics which are parsed by <a href="https://github.com/codeplea/tinyexpr">tinyexpr</a>.
 * See here for a <a href="https://github.com/codeplea/tinyexpr#functions-supported">list of supported functions</a>.
 * @note Additionally, the following substitutions are available for the system and observation models:
 * 1. Current <b>index</b> (number of the time step): "kk"
 * 2. Current <b>time step</b>: "tt"
 * 3. Current <b>state</b> variable: "xx"
 * 4. <b>State at the previous time</b> step: "x_km1" (read: \f$x_{k-1}\f$)
 * 5. Any available <b>meteo parameter</b>: "meteo(PARAM)" <br>
 * Number 3 is only available for the observations model and number 4 picks the initial state for `T=0` for the observation model.
 * @note The time vector `tt` is a normalized and shifted version of the date such that the time of the first measurement is at 0 and the
 * second one is at 1. Suppose you had measurements at 00:00, 01:00, and 01:30 then `kk` would be `[0, 1, 2]` and `tt` would be `[0, 1, 1.5]`.
 *
 * At last, we set up our <b>random number generators</b>:
 * @code
 * MODEL_RNG_DISTRIBUTION = GAUSS
 * MODEL_RNG_PARAMETERS   = 0 0.1 ;mean sigma
 * OBS_RNG_DISTRIBUTION   = GAUSS
 * OBS_RNG_PARAMETERS     = 0 0.3
 * @endcode
 *
 * @note _If the process noise is zero, then the particle filter is not appropriate_ as it is a method of estimating
 * _dynamic states_.
 * @note Since we are not setting a different prior distribution via `PRIOR_RNG_DISTRIBUTION` and `PRIOR_RNG_PARAMETERS` we
 * can omit these keys and the prior distribution will be chosen to be the model distribution. Same would be true for the observation
 * distribution, but that one is slightly different here.
 * @note Read more about the random number generator settings here: RandomNumberGenerator.
 *
 * Together, our ini file looks like this:
 * @code
 * HS::FILTER1                          =   PARTICLE
 * HS::ARG1::MODEL_FUNCTION             =   x_km1 * 0.999 + 0.001 * meteo(CTRL)
 * HS::ARG1::OBS_MODEL_FUNCTION         =   xx
 * HS::ARG1::INITIAL_STATE              =   2.5
 * HS::ARG1::MODEL_RNG_DISTRIBUTION     =   GAUSS
 * HS::ARG1::MODEL_RNG_PARAMETERS       =   0 0.1
 * HS::ARG1::OBS_RNG_DISTRIBUTION       =   GAUSS
 * HS::ARG1::OBS_RNG_PARAMETERS         =   0 0.3
 * @endcode
 *
 * \image html particlefilter_hs_detail.png "Fig. 2: Particle filtering on a mockup snow model. The distribution choices will greatly influence the result and can completely destroy the simulation."
 *
 * @note Neither of the two features separating the particle from the Kalman filter was actually used in this example. First, the noise was Gaussian
 * again (cf. <a href="https://en.wikipedia.org/wiki/Central_limit_theorem">central limit theorem</a>). Cumulative distribution functions are relatively
 * hard to write, and only Gauss is implemented so far. If you need a specific one, you can <a href="https://models.slf.ch/p/meteoio/issues/">request
 * one here</a>. Maybe at this time C99's gamma functions will be available. Secondly, the model function was linear again. The next example uses a
 * nonlinear one.
 *
 * @subsection particleexampleliterature Example: Literature model function
 *
 * The following example function has nothing to do with meteo data, but it has been analyzed a lot in literature
 * (e. g. in Ref. [AM+02] and Ref. [CPS92]) and it's nonlinear.
 *
 * We change the functions and RNG parameters in the control program `statistical_filters.cc`:
 * @code
 * RNGU.setDistributionParameter("mean", 0);
 * RNGU.setDistributionParameter("sigma", 3.3);
 * RNGU.setDistributionParameter("mean", 0);
 * RNGU.setDistributionParameter("sigma", 1);
 * ...
 * xx = xx / 2. + 25. * xx / (1. + xx*xx) + 8. * cos(1.2 * kk);
 * const double obs = (xx + rr) * (xx + rr) / 20. + RNGV.doub();
 * @endcode
 *
 * Of course, we have to adapt our ini settings accordingly:
 * @code
 * HS::ARG1::MODEL_FUNCTION             =   x_km1 / 2 + 25 * x_km1 / (1 + x_km1*x_km1) + 8 * cos(1.2 * kk)
 * HS::ARG1::OBS_MODEL_FUNCTION         =   xx*xx / 20
 * HS::ARG1::INITIAL_STATE              =   0
 * HS::ARG1::MODEL_RNG_DISTRIBUTION     =   GAUSS
 * HS::ARG1::MODEL_RNG_PARAMETERS       =   0 3.3
 * HS::ARG1::OBS_RNG_DISTRIBUTION       =   GAUSS
 * HS::ARG1::OBS_RNG_PARAMETERS         =   0 1
 * @endcode
 *
 * \image html particlefilter_literature.png "Fig. 3: Result of particle filtering a nonlinear function from literature. Also, the observation model is not trivial and we do not see a direct presentation of it which makes it harder to tell which spikes are 'real'."
 *
 *
 * @subsubsection particleexamplegridded Example: Gridded solar model input
 *
 * For a particle filter, as well as for a Kalman filter, you _absolutely need a model function_. However, often you will only have
 * _data points_ that your or somebody else's model spits out. For this reason the particle filter allows you to <b>fit a curve</b> against the
 * data points and use that as model function. The following fits are available: Fit1D, and the polynomial fit takes a 2nd
 * argument with the degree of interpolation (default: 3).
 *
 * Let's use a MeteoIO internal model for the global radiation:
 *
 * @code
 * [INPUT]
 * ISWR_MODEL::CREATE = CLEARSKY_SW ;depends on TA, RH
 * @endcode
 *
 * Comparing the values created this way with the measurements we can see that there is up to a couple of hundret W/m^2
 * difference between the two. This gap needs to be covered by the standard deviations of the random number generators.
 * If you put small numbers here then the huge difference will be so unlikely that everything is filtered (as it should,
 * because obviously it is not justified to put complete trust in this model). For example, if we say that the standard deviation for
 * the measurement sensor is 30, and some of the discrepancy remaining is covered by a standard deviation of 100 in the model
 * then the highest peaks are still deemed unlikely while the rest stays pretty much the same.
 *
 * Using a little more particles our ini section reads:
 * @code
 * ISWR::FILTER1                        =   PARTICLE
 * ISWR::ARG1::NO_OF_PARTICLES          =   1000
 * ISWR::ARG1::MODEL_FUNCTION           =   FIT POLYNOMIAL 5
 * ISWR::ARG1::MODEL_FIT_PARAM          =   ISWR_MODEL
 * ISWR::ARG1::OBS_MODEL_FUNCTION       =   xx
 * ISWR::ARG1::MODEL_RNG_DISTRIBUTION   =   GAUSS
 * ISWR::ARG1::MODEL_RNG_PARAMETERS     =   0 100
 * ISWR::ARG1::OBS_RNG_DISTRIBUTION     =   GAUSS
 * ISWR::ARG1::OBS_RNG_PARAMETERS       =   0 30
 * @endcode
 *
 * \image html particlefilter_gridded.png "Fig. 4: A 5th degree polynomial fit is now the model function of the particle filter. Assigning different standard deviations to the model and measurements will greatly influence the results."
 *
 * @subsection particlefeatures Other features
 *
 * @subsubsection particleresampling Resampling
 *
 * Resampling of the particle paths is turned on and off with the `PATH_RESAMPLING` key. If resampling is turned off then a lot
 * of computational power may be devoted to particles with low weights that do not contribute significantly to the end result,
 * and the paths may lose track of the main line. Usually, it is needed.
 *
 * The `RESAMPLE_PERCENTILE` \f$r_p\f$ lets you pick a factor to decide whether to resample:
 * `if` \f$N_{eff} < r_p * N\f$ `then resample`, where `N` is the number of particles and \f$N_{eff}\f$ the effective sample size,
 * [AM+02] Eq. 50. Strictly, a SIR algorithm requires resampling at each time step, but in practice a heuristic like this is
 * appropriate (e. g. Ref. [DJ09]).
 *
 * For now, only one resampling strategy is implemented:
 *
 * SYSTEMATIC: Straight-forward <b>systematic resampling</b>. This helps avoid degeneracy by skipping particles with small
 * weights in favor of more meaningful ones (algorithm 2 of Ref. [AM+02]). However, it does decrease diversity (by selecting
 * particles with high weight multiple times) and for very small process noise the particles collapse to a single point
 * within a couple of iterations ("sample impoverishment").
 *
 * @note Bruteforcing the particle number helps with the _degeneracy problem_.
 *
 * \image html particlefilter_kerneldensity.png "Fig. 5: Kernel densities of the particles before and after resampling for the literature function example."
 *
 * @note The minimalistic MATLAB code used for this plot can be found as a comment in the `/doc/examples/statistical_filters.cc` program.
 *
 * @subsubsection particleestimation Final state estimation
 *
 * In the end, the particle filter will estimate the most likely observation by taking the mean of all particles
 * (sum of particles multiplied with their weight) and use that as input for the observation model. This is only one
 * possible measure however and a second one is offered: at each time step, the particle with the highest weight is
 * the observation model input:
 * @code
 * ESTIMATION_MEASURE = MAX_WEIGHT
 * @endcode
 *
 * @subsubsection particleonline Online state tracking
 *
 * The basics are there to aggregate new information online, i. e. to keep the state of the filter and simply apply
 * any number of new measurements to it without recalculating the past. Although it may make sense to formulate models
 * that take into account earlier time steps, often the added complexity outweighs the benefits. Hence, if each time step
 * is calculated from only the previous state then it is enough to save the last state (particles and their weights) and
 * resume from there.
 *
 * @note So far, a similar mechanism is lacking for the Kalman filter, but if needed / wanted it will be done if there's time.
 *
 * @code
 * DUMP_INTERNAL_STATES_FILE  = ./output/particle_states.dat
 * INPUT_INTERNAL_STATES_FILE = ./output/particle_states.dat
 * @endcode
 *
 * If the input file is not found the filter will silently start with the initial states that are set through the ini file.
 * In the example above the filter will not find an input file for the first run and thus use the initial states. In the end it
 * writes the states to a file. The next time the filter is started the input file will exist and the calculation is resumed
 * at this point. However, as of yet the model parameters (like `kk`, `tt`, ...) are not saved and that has to be taken
 * into account for the analytical model output. If this would be needed / wanted, it will be built in. Be aware though that
 * Ref. [DJ09] mentions that the number of time steps should not be too high per run.
 *
 * You can also output the particles' paths as a big matrix:
 * @code
 * DUMP_PARTICLES_FILE = ./output/particles.dat
 * @endcode
 *
 * @note This is the input for Fig. 5.
 *
 * @subsubsection particleseeding Seeding the random number generators
 *
 * You can seed all random number generators for reproducibility. To do this, set the appropriate keys as follows:
 *
 * @code
 * MODEL_RNG_SEED    = 89023457862 347891329870 6712368354 234890234 ;4 for XOR generator
 * PRIOR_RNG_SEED    = ...
 * OBS_RNG_SEED      = ...
 * RESAMPLE_RNG_SEED = ...
 * @endcode
 *
 * @note The last one is an internal uniform generator needed by the particle filter itself (with fixed distribution parameters).
 * @note The number of seeds must match the one the RNG algorithm expects: cf. RandomNumberGenerator.
 *
 * @subsection particlekeys List of ini keys
 *  <table>
 *  <tr><th>Keyword</th><th>Meaning</th><th>Optional</th><th>Default Value</th></tr>
 *  <tr><td>MODEL_FUNCTION</td><td>State transition function.</td><td>no, but can also be a fit</td><td>empty</td></tr>
 *  <tr><td>MODEL_FIT_PARAM</td><td>Parameter in meteo set to fit model curve against.</td><td>yes, if MODEL_FUNCTION is set</td><td>empty</td></tr>
 *  <tr><td>INITIAL_STATE</td><td>State variable at T=0.</td><td>yes</td><td>1st observation</td></tr>
 *  <tr><td>OBS_MODEL_FUNCTION</td><td>Model relating the observations to the states.</td><td>yes, but you need one</td><td>"xx"</td></tr>
 *  <tr><td>ESTIMATION_MEASURE</td><td>Which measure to use for the final estimation.</td><td>yes</td><td>MEAN (choices: MAX_WEIGHT)</td></tr>
 *  <tr><td>NO_OF_PARTICLES</td><td>Number of particles per time step.</td><td>yes</td><td>500</td></tr>
 *  <tr><td>PATH_RESAMPLING</td><td>Perform particle path resampling?</td><td>yes</td><td>TRUE, this is normally needed</td></tr>
 *  <tr><td>RESAMPLE_PERCENTILE</td><td>Scalar for simple resampling heuristic.</td><td>yes</td><td>0.5</td></tr>
 *  <tr><td>DUMP_PARTICLES_FILE</td><td>Output file path for the particles.</td><td>yes</td><td>empty</td></tr>
 *  <tr><td>DUMP_INTERNAL_STATES_FILE</td><td>Output file path for the internal states.</td><td>yes</td><td>empty</td></tr>
 *  <tr><td>INPUT_INTERNAL_STATES_FILE</td><td>Input file path for the internal states.</td><td>yes</td><td>empty</td></tr>
 *  <tr><td>VERBOSE</td><td>Output warnings to the console.</td><td>yes</td><td>TRUE, warnings should be mitigated</td></tr>
 *  <tr><td>MODEL_RNG_ALGORITHM</td><td>Random numbers generator function for the model.</td><td>yes</td><td>XOR</td></tr>
 *  <tr><td>MODEL_RNG_DISTRIBUTION</td><td>Random numbers distribution for the model.</td><td>yes</td><td>GAUSS</td></tr>
 *  <tr><td>MODEL_RNG_PARAMETERS</td><td>Random numbers distribution parameters for the model.</td><td>yes, but the filter is sensitive to them</td><td>RNG defaults</td></tr>
 *  <tr><td>MODEL_RNG_SEED</td><td>Random numbers generator seed for the model.</td><td>yes</td><td>hardware seed</td></tr>
 *  <tr><td>PRIOR_RNG_ALGORITHM</td><td>Random numbers generator function for the prior distribution.</td><td>yes</td><td>XOR</td></tr>
 *  <tr><td>PRIOR_RNG_DISTRIBUTION</td><td>Random numbers distribution for the prior.</td><td>yes</td><td>GAUSS</td></tr>
 *  <tr><td>PRIOR_RNG_PARAMETERS</td><td>Random numbers distribution parameters for the prior.</td><td>yes</td><td>RNG defaults</td></tr>
 *  <tr><td>PRIOR_RNG_SEED</td><td>Random numbers generator seed for the prior.</td><td>yes</td><td>hardware seed</td></tr>
 *  <tr><td>OBS_RNG_ALGORITHM</td><td>Random numbers generator function for the observations.</td><td>yes</td><td>XOR</td></tr>
 *  <tr><td>OBS_RNG_DISTRIBUTION</td><td>Random numbers distribution for the observations.</td><td>yes</td><td>GAUSS</td></tr>
 *  <tr><td>OBS_RNG_PARAMETERS</td><td>Random numbers distribution parameters for the observations.</td><td>yes</td><td>RNG defaults</td></tr>
 *  <tr><td>OBS_RNG_SEED</td><td>Random numbers generator seed for the observations.</td><td>yes</td><td>hardware seed</td></tr>
 *  <tr><td>RESAMPLE_RNG_SEED</td><td>Random numbers generator seed for the resampling.</td><td>yes</td><td>hardware seed</td></tr>
 * </table>
 *
 * @subsection statfilterroadmap Roadmap
 *
 * An interesting development would be to slowly follow Ref. [MG+14] to reach similar results, but generalized to arbitrary
 * data and terrain. For this, spatially correlated random numbers are necessary and we would move towards gridded filters
 * and spatial interpolations.
 *
 * See Ref. [DJ09] for smoothing algorithms of this class that would run through the data forwards and backwards and have
 * knowledge of all measurements, i. e. no live filtering.
 *
 * @section statfilterbibliography Bibliography
 * - [AM+02] M. Sanjeev Arulampalam, Simon Maskell, Neil Gordon, and Tim Clapp.
 *        <i>A Tutorial on Particle Filters for Online Nonlinear/Non-Gaussian Bayesian Tracking.</i>
 *        IEEE Transactions on Signal Processing, Vol. 50, No. 2, February 2002.
 * - [CPS92] Bradley P. Carlin, Nicholas G. Polson, and David S. Stoffer.
 *        <i>A Monte Carlo Approach to Nonnormal and Nonlinear State-Space Modeling.</i>
 *        Journal of the American Statistical Association, Vol. 87, issue 418, 493-500, 1992.
 * - [DJ09] Arnaoud Doucet and Adam M. Johansen.
 *        <i>A Tutorial on Particle Filtering and Smoothing: Fifteen years later.</i>
 *        Handbook of Nonlinear Filtering, Vol. 12, 2009.
 * - [LM11] Marc Leisenring and Hamid Moradkhani.
 *        <i>Snow water equivalent prediction using Bayesian data assimilation methods.</i>
 *        Stoch Environ Res Risk Assess 25, 253-270, 2011.
 * - [MG+14] Jan Magnusson, David Gustafsson, Fabia Hüsler, and Tobias Jonas.
 *        <i> Assimilation of point SWE data into a distributed snow cover model comparing two contrasting methods.</i>
 *        Water Resources Research, 50, 7816-7835, 2014.
 * - [SC06] Andrew G. Slater and Martyn P. Clark.
 *        <i>Snow Data Assimilation via an Ensemble Kalman Filter</i>.
 *        Journal of Hydrometeorology, Vol. 7, 478ff., 2006.
 */

class FilterParticle : public ProcessingBlock {
	public:
		FilterParticle(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);
		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		        std::vector<MeteoData>& ovec);

	private:
		void resamplePaths(Matrix& xx, Matrix& ww, const size_t& kk, RandomNumberGenerator& RNU) const;
		bool checkInitialState(const std::vector<MeteoData>& ivec, const size_t& param);
		void initFunctionVars(te_variable* vars, const std::vector<std::string>& names, const std::vector<double>& meteo) const;
		te_expr* compileExpression(const std::string& expression, const te_variable* te_vars, const size_t& sz) const;
		void parseSubstitutionStrings(std::string& line_m, std::string& line_o, std::vector<std::string>& sub_expr,
		        std::vector<std::string>& sub_params) const;
		void parseBracketExpression(std::string& line, std::vector<std::string>& sub_expr,
		        std::vector<std::string>& sub_params) const;
		void dumpInternalStates(Matrix& particles, Matrix& weights) const;
		bool readInternalStates(Matrix& particles, Matrix& weights) const;
		void dumpParticlePaths(Matrix& particles) const;
		std::vector<double> buildTimeVector(const std::vector<MeteoData>& ivec) const;
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		void seedGeneratorsFromIni(RandomNumberGenerator& RNGU, RandomNumberGenerator& RNGV, RandomNumberGenerator& RNG0,
		        RandomNumberGenerator& RNU) const;
		void vecMeteoToMatrix(const std::vector<MeteoData>& vec, Matrix& mat, const unsigned int& param) const;
		void readLineToVec(const std::string& line_in, std::vector<uint64_t>& vec_out) const;
		bool isNan(const double& xx) const;

		typedef enum PF_FILTER_ALGORITHM {
			PF_SIR, //only SIR is implemented so far
			PF_SIS
		} pf_filter_algorithm;
		typedef enum PF_RESAMPLE_ALGORITHM {
			PF_SYSTEMATIC, //only SYSTEMATIC implemented so far
			PF_EPANECHNIKOV
		} pf_resample_algorithm;
		typedef enum PF_ESTIMATION_MEASURE  {
			PF_MEAN,
			PF_MAX_WEIGHT
		} pf_estimation_measure;

		pf_filter_algorithm filter_alg;
		pf_resample_algorithm resample_alg; //resampling of particle paths

		unsigned int NN; //number of particles
		bool path_resampling; //has nothing to do with temporal or spatial meteo resampling

		std::string model_expression; //model formula
		std::string obs_model_expression;
		std::string fit_expression; //fit data points to get a model function
		std::string fit_param; //meteo parameter that will be fitted for model
		unsigned int fit_degree; //degree of interpolation
		double model_x0; //initial state at T=0

		double resample_percentile; //simple resampling heuristic
		pf_estimation_measure estim_measure; //how to choose most likely particle path

		struct rng_settings {
			RandomNumberGenerator::RNG_TYPE algorithm;
			RandomNumberGenerator::RNG_DISTR distribution;
			std::vector<double> parameters;
			std::vector<uint64_t> seed;
			rng_settings() : algorithm(RandomNumberGenerator::RNG_XOR),
			                 distribution(RandomNumberGenerator::RNG_GAUSS),
			                 parameters(),
			                 seed() {}
		};
		struct rng_settings rng_model; //noise generator for model function
		struct rng_settings rng_obs; //observation noise pdf
		struct rng_settings rng_prior; //generator to sample from prior pdf
		std::vector<uint64_t> resample_seed;

		bool be_verbose; //output warnings/info?
		std::string unrecognized_keys; //to warn about unknown ini keys
		std::string dump_particles_file;
		std::string dump_states_file;
		std::string input_states_file;
};

} //namespace

#endif //FILTERPARTICLE_H
