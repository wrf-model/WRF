/***********************************************************************************/
/*  Copyright 2018 Michael Reisecker and work cited in documentation and source    */
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

#ifndef RANDOMNUMBERGENERATOR_H
#define RANDOMNUMBERGENERATOR_H

#include <ctime> //for time seed
#include <inttypes.h> //for uint64_t etc. (ULL guarantees at least 64 bits and should be ok)
#include <string> //for toString()
#include <vector> //for internal states

namespace mio {

/**
 * @class RandomNumberGenerator
 * @ingroup stats
 * @author Michael Reisecker
 * @date 2018-10
 *
 * @section rng_rng Random Number Generator
 * Random integer:
 * @code
 * mio::RandomNumberGenerator RNG;
 * int nn = RNG.int64();
 * @endcode
 * Random double with Gaussian distribution:
 * @code
 * RNG.setDistribution(mio::RandomNumberGenerator::RNG_GAUSS)
 * double rr = RNG.doub();
 * @endcode
 *
 * @section rng_purpose Purpose of this class
 * We offer two inherently 32 bit generators, and an inherently 64 bit generator
 * (although all three need 64 bits space), as well as some convenience methods.
 *
 * The goal is to have a generator suite that satisfies all needs for statistical filters / Monte Carlo methods (and not
 * more), especially when working within MeteoIO. In a way, statistical filters are what ultimately justify this class,
 * and therefore it is meant to be tailored to their needs (and be C++98).
 *
 * So, if you are currently using this (cf. \ref rng_appendix_A):
 * @code
 *     srand( time(NULL) );
 *     return rand() % range;
 *     return rand() / double(RAND_MAX + 1);
 * @endcode
 * then switch to MeteoIO's RNG. If however you rely heavily on the best quality random numbers, maybe even crypto-secure,
 * there are some links to dedicated libraries in the \ref rng_bibliography.
 * Apart from the generators and distributions, this class aims to take away all the small steps that are often quickly
 * deemed good enough, i.e. \ref rng_algorithms "generator choice", \ref rng_seeding "seeding", \ref rng_seeding "saving states",
 * \ref rng_range "range calculations", ...
 * @note There is an example program exercising most of the RNG's features in the `/doc/examples` folder.
 *
 * @section rng_overview Overview
 * What it can already do:
 *  - produce quality 64 bit, 32 bit and double random numbers with one simple call
 *  - doubles with different probability distributions
 *  - some probability density functions and cumulative distribution functions
 *  - make use of quality hardware and time seeds
 *  - fast downscaling of random numbers to a range
 *  - true floating point random numbers without rounding
 *  - can be resumed from a saved state
 *  - sidesteps some widespread misuse of quick & dirty solutions
 *  - sidesteps some issues with the insidious standard library
 *  - offers a ready-to-use interface for implementing new distributions (or even generators)
 *  - passes statistical tests
 *  - good benchmarks for the generator cores
 *
 * What's left to do:
 *  - some distributions
 *  - Monte Carlo sampling template for arbitrary distribution functions
 *
 * @section rng_integers Random integers
 * To draw random integers, you can use either the int32() or int64() function call to receive
 * 32 or 64 bit pseudo-random values, respectively:
 * @code
 *  uint32_t rn = RNG.int32();
 *  uint64_t rm = RNG.int64();
 * @endcode
 * @note The generators guarantee exactly 32 or 64 random bits, so the appropriate
 * types defined by `inttypes.h` are suited best. However, if you can live with
 * `conversion` compiler warnings then any integer type will do. Of course, you
 * can also simply use the type uintNN_t is mapped to on your machine. In short,
 * `int rn = RNG.int32()` will work for a quick try.
 *
 * @section rng_doubles Double values
 *
 * Usually, you will simply get a double value like this:
 * @code
 * double rr = RNG.doub();
 * @endcode
 *
 * You can call the doub() function with an `RNG_BOUND` argument including or excluding 0 and 1 (cf. the enum below).
 * This can only be done for the uniform distribution, where it's clear what the borders are.
 * @code
 *     double rr = RNG.doub(RNG_AEXCBINC); //make sure it's not 0
 *     rr = log(rr);
 * @endcode
 *
 * Uniform random double values are quite hard to generate. The code example at Ref. [TC14] provides a method to do it,
 * which is to interpret a random stream of bits as fractional part of the binary expansion of a number in [0, 1].
 * The file also goes into details about why other methods are troublesome if we rely on quality, e. g. sensitive random searches
 * on a plane due to the gap size of `1/2^(bits)`.
 *
 * In short, the doub() function returns a double within `[0, 1]` that is rounded to
 * the nearest `1/2^64`th.
 * To get around this, you can set `true_double` to use an algorithm that calculates doubles in `[0, 1]` without the usual limitation of
 * floating point randoms being on a grid (but then you must use `RNG_AINCBINC` and guard that in your own code
 * if it must not happen even once).
 * @code
 *     double rr;
 *     do {
 *         rr = RNG.doub(RNG_AINCBINC, true); //get a random float on continuous axis
 *     } while (rr == 0.); //make sure it's not 0
 *     rr = log(rr);
 * @endcode
 *
 *
 * @section rng_distribution Distributions / Random deviates
 * For doubles, you can select from a number of distribution functions.
 *
 * For example, you can draw a Student-t variate like this:
 * @code
 * RNG.setDistribution(mio::RandomNumberGenerator::RNG_STUDENTT);
 * double rr = RNG.doub();
 * @endcode
 * If you don't set any distribution parameters, they will be defaulted (cf. \ref
 * rng_distributionparams).
 *
 * So far, the following deviates are available, defined by their probability density:
 *
 * - <a href="https://en.wikipedia.org/wiki/Uniform_distribution">Uniform</a>, `RNG_UNIFORM`:
 * \f[
 * f(x) = \frac 1{b-a} \quad \mathrm{for} \quad a \le x \le b,
 * \f]
 *
 * - <a href="https://en.wikipedia.org/wiki/Normal_distribution">Gauss</a> (= Normal), `RNG_GAUSS`:
 * \f[
 * f(x \mid\mu,\sigma^2)=\frac{1}{\sqrt{2\pi\sigma^2}}\exp\left(-\frac{(x-\mu)^2}{2\sigma^2}\right)
 * \quad \mathrm{for} \quad -\infty < x < +\infty
 * \f]
 * - <a href="https://en.wikipedia.org/wiki/Gamma_distribution">Gamma</a>, `RNG_GAMMA`:
 * \f[
 * f(x)=\frac{\beta^\alpha}{\Gamma(\alpha)}x^{\alpha-1}e^{-\beta x} \quad \mathrm{for} \quad x > 0
 * \f]
 * \f[
 * \alpha > 0, \quad \beta > 0
 * \f]
 * - <a href="https://en.wikipedia.org/wiki/Student%27s_t-distribution">Student-t</a>, `RNG_STUDENTT`:
 * \f[
 * f_\nu(x)=\frac{\Gamma\left(\frac{\nu+1}{2}\right)}
 * {\sqrt{\nu\pi}~\Gamma\left(\frac{\nu}{2}\right)}
 * \left(1+\frac{x^{2}}{\nu}\right)^{-\frac{\nu+1}{2}}
 * \quad \mathrm{for} \quad -\infty < x < +\infty
 * \f]
 * \f[
 * \nu > 0
 * \f]
 * - <a href="https://en.wikipedia.org/wiki/Chi-squared_distribution">Chi-squared</a>, `RNG_CHISQUARED`:
 * \f[
 * Z\sim\mathcal{N}(0,1) \rightarrow Z^2\sim\chi^2(1) \rightarrow
 * Y=\chi^2(r_1)+\ldots+\chi^2(r_\nu)\sim\chi^2(r_1+\ldots+r_\nu)
 * \f]
 * \f[
 * \nu > 0
 * \f]
 * - <a href="https://en.wikipedia.org/wiki/Beta_distribution">Beta</a>, `RNG_BETA`:
 * \f[
 * f(x)=\frac{1}{\mathrm{B}(\alpha, \beta)} x^{\alpha-1}(1-x)^{\beta-1}
 * \quad \mathrm{for} \quad 0 < x < 1
 * \f]
 * \f[
 * \alpha > 0, \quad \beta > 0
 * \f]
 * - <a href="https://en.wikipedia.org/wiki/F-distribution">F</a>, `RNG_F`:
 * \f[
 * f(x|\nu _1, \nu _2) = m^{\frac{\nu _1}{2}} \nu _2^{\frac{\nu _2}{2}} \cdot
 * \frac{\Gamma (\frac{\nu _1}{2}+\frac{\nu _2}{2})}{\Gamma (\frac{\nu _1}{2})
 * \Gamma(\frac{\nu _2}{2})} \cdot \frac{x^{\frac{\nu _1}{2}-1}}
 * {(\nu _1 x+\nu _2)^\frac{\nu _1+\nu _2}{2}}
 * \quad \mathrm{for} \quad x \geq 0
 * \f]
 *
 * @note Integer values are always drawn with Uniform distribution, so if you really need a
 * different one, for now you'll have to draw a double value and multiply accordingly.
 *
 * @section rng_distributionparams Distribution parameters
 *
 * When you change the distribution, you switch to a completely new one.
 * The distribution parameters <i>have to be</i> provided each time, or they will be defaulted.
 *
 * You can set distribution parameters in two ways:
 *  -# By setting (getting) them one by one after a distribution has been set:
 *  @code
 *        RNG.setDistribution(mio::RandomNumberGenerator::RNG_GAUSS);
 *        RNG.setDistributionParameter("mean", 5.);
 *        RNG.setDistributionParameter("sigma", 2.);
 *        double mean_out = RNG.getDistributionParameter("mean");
 *  @endcode
 *  -# Via accessing the `DistributionParameters` vector directly.
 *    A `std::vector<double>` is provided with input parameters, or it has the output stored to it.
 *    This vector is given to the setDistribution() or getDistribution() call (the latter also returns the distribution type).
 *
 *    Set distribution and parameters:
 *        @code
 *        std::vector<double> distribution_params;
 *        const double alpha = 1.2, beta = 1.;
 *        distribution_params.push_back(alpha);
 *        distribution_params.push_back(beta);
 *        RNG.setDistribution(mio::RandomNumberGenerator::RNG_GAMMA, distribution_params);
 *        @endcode
 *
 *    Get distribution and parameters:
 *        @code
 *        distribution_params.clear();
 *        const mio::RandomNumberGenerator::RNG_DISTR dist_t = RNG.getDistribution(distribution_params);
 *        const double alpha_out = distribution_params.at(0); //check doc for indices
 *        @endcode
 *
 * Here are the names within MeteoIO, arguments and default values of the distributions described above:
 *
 * <table>
 *  <tr><th>Distribution</th><th>Index</th><th>Parameter</th><th>Description</th><th>Default value</th></tr>
 *  <tr><td>RNG_UNIFORM</td><td>-</td><td>-</td><td>no parameteres</td><td>-</td></tr>
 *  <tr><td rowspan="2">RNG_GAUSS = RNG_NORMAL</td><td>1</td><td>mean</td><td>center of curve</td><td>0</td></tr>
 *  <tr><td>2</td><td>sigma</td><td>standard deviation</td><td>1</td></tr>
 *  <tr><td rowspan="2">RNG_GAMMA</td><td>1</td><td>alpha</td><td>shape parameter 1</td><td>1</td></tr>
 *  <tr><td>2</td><td>beta</td><td>shape parameter 2</td><td>1</td></tr>
 *  <tr><td>RNG_CHISQUARED</td><td>1</td><td>nu</td><td>number of degrees of freedom</td><td>1</td></tr>
 *  <tr><td rowspan="3">RNG_STUDENTT</td><td>1</td><td>nu</td><td>number of degrees of freedom</td><td>1</td></tr>
 *  <tr><td>2</td><td>mean</td><td>center of curve</td><td>0</td></tr>
 *  <tr><td>3</td><td>sigma</td><td>standard deviation</td><td>1</td></tr>
 *  <tr><td rowspan="2">RNG_BETA</td><td>1</td><td>alpha</td><td>shape parameter 1</td><td>1</td></tr>
 *  <tr><td>2</td><td>beta</td><td>shape parameter 2</td><td>1</td></tr>
 *  <tr><td rowspan="2">RNG_F</td><td>1</td><td>nu1</td><td>degrees of freedom in numerator</td><td>1</td></tr>
 *  <tr><td>2</td><td>nu2</td><td>degrees of freedom in denominator</td><td>1</td></tr>
 * </table>
 *
 * @section rng_pdf PDFs and CDFs
 * So far, the probability density function and cumulative distribution function are
 * available for the Gauss distribution like this:
 * @code
 * mio::RandomNumberGenerator RNG;
 * RNG.setDistribution(mio::RandomNumberGenerator::RNG_GAUSS);
 * const double rg = RNG.doub();
 * std::cout << "Drew: " << rg << std::endl;
 * std::cout << std::setprecision(4) << "Probability to hit a number close to this one: "
 *           << RNG.pdf(rg)*100 << " %" << std::endl;
 * std::cout << "Probability to hit below this number: " << RNG.cdf(rg)*100 << " %" << std::endl;
 * @endcode
 * @section rng_range Range calculations
 * You can draw 32 and 64 bit integers in a given range like this:
 * @code
 * uint32_t rn = RNG.range32(10, 20);
 * uint64_t rn = RNG.range64(100, 2000);
 * @endcode
 *
 * Note that whatever you do, for an arbitrary count of random numbers you cannot downscale them and keep the distribution
 * completely intact (although "non-trivial" methods are under investigation) due to the
 * <a href="https://en.wikipedia.org/wiki/Pigeonhole_principle">Pigeonhole principle</a>.
 * The only way not to distort the (uniform) distribution is to generate lots of numbers and reject
 * out of boundary values. This is done by the trueRange32() function with a default `1e6` tries before
 * resorting to downscaling (indicated by the return boolean). You can crank this up, but to state the obvious
 * if the range gets small this gets costly quickly.
 * @code
 * uint32_t rt;
 * const bool true_range_success = RNG.trueRange32(100, 3000, rt);
 * @endcode
 *
 * @section rng_algorithms Random number generator algorithms
 * Three algorithms are available, namely the Mersenne Twister, a "classical" combined generator,
 * and a rather new algorithm with promising statistical qualities.
 * You can set them when initializing the RNG like this:
 * @code
 * mio::RandomNumberGenerator RNG(mio::RandomNumberGenerator::RNG_XOR); //default
 * mio::RandomNumberGenerator MTW(mio::RandomNumberGenerator::RNG_MTW);
 * mio::RandomNumberGenerator PCG(mio::RandomNumberGenerator::RNG_PCG);
 * @endcode
 *
 * @subsection rng_mtw Mersenne Twister
 * - Implementation of the wide-spread Mersenne Twister algorithm by M. Matsumoto and T. Nishimura (Ref. [MN98]).
 * - By using 624 internal states, the period is extremely long.
 * - This does not make it crypto-secure (the state can be derived from 624 random numbers), but it
 *   passes many statistical tests and is the standard RNG in numerous well-known software packages.
 * - It needs a few kB buffer size, which is relatively large compared to the other generators.
 * - Facts:
 *   size: 32 bit, period: 2^19937-1 (Mersenne prime) ~ 4.3e6001
 *
 * @subsection rng_xor Combined generator
 * - Generator with xor, shift and multiplication
 * - This is a fast combined generator that should be suitable for all but very special Monte Carlo applications.
 *   Since more than one internal states are being propagated and combined to the output, this makes it
 *   somewhat less predictable than similar generators.
 * - Seed with any value except vv.
 * - Facts:
 *   size: 64 bit, period: ~3.138e57
 *
 * @subsection rng_pcg PCG
 * - Permuted linear congruential generator by Prof. Melissa O'Neill
 * - Range is overestimated, and this generator performs very well in statistical tests, i. e. it is less
 *   predictable than related generators. Even smaller versions with only 32 bit entropy pass SmallCrunch,
 *   which is only barely theoretically possible.
 *   The key element is the hashing function from the internal states to the random number.
 *   The algorithm author describes this RNG family in her paper (Ref. [MO14]) and offers a huge
 *   sophisticated <a href="https://github.com/imneme/pcg-c">C-library</a> for free download with from tiny to 128 bit generators.
 * - You should seed true 64 bit values or discard the first numbers.
 * - If drawing 64 bit naturally is slow on your machine, try this one.
 * - Facts:
 *   size: 32 bit, period: ~2^64 ~ 1.8e19
 *
 * @note The numbers were subjected to the dieharder random number test suite (Ref. [RB03]), passing
 * most tests (while `rand()` fails horribly). Here is a quick benchmark (cf. \ref rng_appendix_B
 * for dieharder results):
 *
 * Generator | Bit | # per second
 * ----------|-----|-----------------
 * XOR       |  64 |    5.73e7
 * XOR       |  32 |    7.04e7
 * PCG       |  32 |   13.57e7
 * MTW       |  32 |    5.19e7
 * MTW GAUSS |     |    0.84e7
 * crand     |  32 |   17.21e7
 * STL MTW   |  32 |    7.69e7
 * STL GAUSS |     |    0.48e7
 * Hardware  |  64 |    2.61e4
 *
 * Tested on Intel Core i7-7700HQ CPU @ 2.80GHz with 32GB RAM.
 *
 * @section rng_seeding Seeding
 * Each time a RNG is constructed, it auto-seeds from hardware noise, or if that fails by hashing the
 * system time. If the system time is the same when you initialize more generators at once, it will still
 * seed differently. Successful hardware noise can be checked with getHardwareSeedSuccess(), and it's
 * also noted in the toString() info.
 * Manually seeding the generator is done after the fact with setState(), for example, to resume
 * experiments after the state was saved via getState(). Note that this will not reset if you seed the generator
 * yourself; i. e. if you seed from hardware and then later resume the chain by re-seeding, it will still show
 * as hardware seeded.
 * Finally, we offer the getUniqueSeed() function, so if you have set up your
 * calculations with a grandfathered in, better, faster, ... RNG we can at least help with the seeding.
 *
 * Example: By default, the Mersenne Twister initializes its 624 states with a linear congruential
 * generator and then mixes that together with 64 hardware noise (resp. time hash) values. If you wanted
 * to seed all 624 internal states with hardware noise (or time hashes) you could do it like this:
 * @code
 * mio::RandomNumberGenerator MTW(mio::RandomNumberGenerator::RNG_MTW);
 * std::vector<uint64_t> seed_array;
 * uint64_t seed;
 * bool success(false);
 * for (size_t i = 0; i < 624; ++i) {
 *     success = MTW.getUniqueSeed(seed);
 *     seed_array.push_back(seed);
 * }
 * MTW.setState(seed_array);
 * @endcode
 *
 * You can retrieve the generator's state to later resume experiments at exactly this point:
 * @code
 * std::vector<uint64_t> out_seed;
 * RNG.getState(out_seed);
 * mio::RandomNumberGenerator RN2;
 * RN2.setState(out_seed);
 * @endcode
 *
 * @section rng_developer Developer's guide
 * For developers of statistical filters it may be important to be able to implement custom probability distributions,
 * for example for an empirical nonlinear sensor response. This class tries to be easy to expand in that regard.
 * There are comment markers in the header and source files leading with "`CUSTOM_DIST step #`: ..." in the 7 places
 * you need to register your custom distribution functions at. These 7 steps are:
 *  -# Give your distribution a name within MeteoIO
 *  -# Put your functions' prototypes in the header
 *  -# Point to your distribution function in the generic setDistribution() function,
 *     and use the interface to the caller to set your distribution parameters
 *  -# Give a small output info string
 *  -# Write your distribution function, its pdf and cdf (if only to throw a not-implemented error)
 *  -# If you want, you can map your parameters to names in the get- and setDistributionParameter() functions.
 *  -# Map a string shorthand to the name of your distribution.
 *
 * @section rng_bibliography Bibliography
 * - [AS73] Abramowitz, Stegun.
 *        <i>Handbook of Mathematical Functions.</i>
 *        Applied Mathematics Series 55, 10th edition, 1973.
 * - [DK81] Donald E. Knuth.
 *        <i>The art of computer programming 2.</i>
 *        Addison-Wesley series in computer science and information processing, 2nd edition, 1981.
 * - [GM03] George Marsaglia.
 *        <i>Xorshift RNGs.</i>
 *        Journal of Statistical Software, Articles, 8/14, 2003.
 * - [MN98] Makoto Matsumoto and Takuji Nishimura.
 *        <i>Mersenne Twister: A 623-dimensionally equidistributed uniform pseudo-random number generator.</i>
 *        ACM Transactions on Modeling and Computer Simulation, 8/1, 1998.
 *        http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
 * - [MO14] Melissa O'Neill.
 *        <i>PCG: A family of simple fast space-efficient statistically good algorithms
 *        for random number generation.</i>
 *        Harvey Mudd College, 2014.
 *        http://www.pcg-random.org
 * - [MT00] G. Marsaglia and W. Tsang.
 *        <i>A simple method for generating gamma variables.</i>
 *        ACM Transactions on Mathematical Software, 26/3, 2000.
 * - [NR3] Press, Teukolsky, Vetterling, Flannery.
 *        <i>Numerical Recipes. The Art of Scientific Computing.</i>
 *        Cambridge University Press, 3rd edition, 2007.
 * - [PE97] Pierre L'Ecuyer.
 *        <i>Distribution properties of multiply-with-carry random number generators.</i>
 *        Mathematics of Computation, 66/218i, 1997.
 * - [PE99] Pierr L'Ecuyer.
 *        <i>Tables of linear congruential generators of different sizes and good
 *        lattice structure.</i>
 *        Mathematics of Computation, 68/225, 1999.
 *        <a href="https://www.iro.umontreal.ca/~lecuyer/myftp/papers/latrules99Errata.pdf">Errata</a>
 *        for the paper, read on 18-10-23)
 * - [RB03] Robert G. Brown.
 *        <i>Dieharder: A random number test suite.</i>
 *        http://webhome.phy.duke.edu/~rgb/General/dieharder.php
 * - [TC14] Taylor R. Campbell.
 *        http://mumble.net/~campbell/tmp/random_real.c (read on 18-10-23)
 *
 * @section rng_appendix_A Appendix A
 * Why is
 * @code
 * srand( time(NULL) );
 * return rand() % range;
 * return rand() / double(RAND_MAX + 1);
 * @endcode
 * bad?
 * - A purely linear congruential RNG has purely bad statistical qualities
 * - Quiet type collision between time() and srand()
 * - `(% range)` distorts the distribution at the borders and `(range + 1)` should be used
 * - Careful not to hit RAND_MAX = INT_MAX, maybe `((double)RAND_MAX) + 1`.
 *
 * \image html rng_planes.png "Triplets of random numbers generated by a poor linear congruential generator."
 *
 * Why is
 * @code
 * std::random_device RNG;
 * std::seed_seq seed{RNG()};
 * std::mt19937 RNG_MT(seed);
 * @endcode
 * not good?
 * - A 624 state Mersenne Twister is seeded with a single 32 bit value, not a sequence
 * - Leads to statistical flaws; some numbers are never drawn
 * - `std::seed_seq` isn't a bijection like it's supposed to be (as of C++17)
 * - Can produce zero-state
 *
 * @section rng_appendix_B Appendix B
 * Random number quality summary:
 * The RNG performs as expected and passes statistical tests within reason.
 *
 * The generators were subjected to the test suite dieharder, alongside with the
 * hardware device, rand(), and a state of the art crypto-generator. Please refer
 * to the <a href="https://linux.die.net/man/1/dieharder">man page for this project</a>
 * for an interpretation of the results. In short:
 * - The p-number denotes how likely it is that a perfect generator would produce
 *   this sequence.
 * - Values below 0.05 and above 0.95 are usually considered bad. However, if a
 *   generator does not produce p-values below 0.05 in 5% of the tests, this is
 *   equally bad, since the p-value itself is a uniform test statistic.
 * - <b>This means that in a full test run, a handful of "weak" results are expected!</b>
 *
 * Even with default values, dieharder uses up massive amounts of random numbers,
 * and it is designed to be able to push all generators to failure and make a
 * stronger assessment than the ambiguous "weak". However, this also pushes the
 * runtime. A billion (1e9) random numbers were used per test (ca. 10.7 GB) and still
 * the file was rewound 1266 times per test; some "weak" results may be due to this.
 * The hardware seed was piped to dieharder for a continuous flow of random words.
 *
 * @code
 * #=============================================================================#
 * #            dieharder version 3.31.1 Copyright 2003 Robert G. Brown          #
 * #=============================================================================#
 * # ---------- C's rand() function ----------
 * #=============================================================================#
 *         test_name   |ntup| tsamples |psamples|  p-value |Assessment
 * #=============================================================================#
 *    diehard_birthdays|   0|       100|     100|0.92354359|  PASSED
 *       diehard_operm5|   0|   1000000|     100|0.05552403|  PASSED
 *   diehard_rank_32x32|   0|     40000|     100|0.00000000|  FAILED
 *     diehard_rank_6x8|   0|    100000|     100|0.98180550|  PASSED
 *    diehard_bitstream|   0|   2097152|     100|0.00000000|  FAILED
 *         diehard_opso|   0|   2097152|     100|0.10873316|  PASSED
 *         diehard_oqso|   0|   2097152|     100|0.98545797|  PASSED
 *          diehard_dna|   0|   2097152|     100|0.00000000|  FAILED
 * diehard_count_1s_str|   0|    256000|     100|0.00000000|  FAILED
 * diehard_count_1s_byt|   0|    256000|     100|0.00000000|  FAILED
 *  diehard_parking_lot|   0|     12000|     100|0.00000000|  FAILED
 *     diehard_2dsphere|   2|      8000|     100|0.00000000|  FAILED
 *     diehard_3dsphere|   3|      4000|     100|0.00000000|  FAILED
 *      diehard_squeeze|   0|    100000|     100|0.00000000|  FAILED
 *         diehard_sums|   0|       100|     100|0.00000000|  FAILED
 *         diehard_runs|   0|    100000|     100|0.57088403|  PASSED
 *         diehard_runs|   0|    100000|     100|0.72115015|  PASSED
 *        diehard_craps|   0|    200000|     100|0.00000000|  FAILED
 *        diehard_craps|   0|    200000|     100|0.00000000|  FAILED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.00000000|  FAILED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.48796452|  PASSED
 *          sts_monobit|   1|    100000|     100|0.00000000|  FAILED
 *             sts_runs|   2|    100000|     100|0.00000000|  FAILED
 *           sts_serial|   1|    100000|     100|0.00000000|  FAILED
 *           sts_serial|   2|    100000|     100|0.00000000|  FAILED
 *           sts_serial|   3|    100000|     100|0.00000000|  FAILED
 *           sts_serial|   3|    100000|     100|0.89813018|  PASSED
 *           sts_serial|   4|    100000|     100|0.00000000|  FAILED
 *           sts_serial|   4|    100000|     100|0.75692532|  PASSED
 *           sts_serial|   5|    100000|     100|0.00000000|  FAILED
 *           sts_serial|   5|    100000|     100|0.69863769|  PASSED
 *           sts_serial|   6|    100000|     100|0.00000000|  FAILED
 *           sts_serial|   6|    100000|     100|0.97959672|  PASSED
 *           sts_serial|   7|    100000|     100|0.00000000|  FAILED
 *           sts_serial|   7|    100000|     100|0.35286943|  PASSED
 *           sts_serial|   8|    100000|     100|0.00000000|  FAILED
 *           sts_serial|   8|    100000|     100|0.02669475|  PASSED
 *           sts_serial|   9|    100000|     100|0.00000000|  FAILED
 *           sts_serial|   9|    100000|     100|0.10088919|  PASSED
 *           sts_serial|  10|    100000|     100|0.00000000|  FAILED
 *           sts_serial|  10|    100000|     100|0.67626624|  PASSED
 *           sts_serial|  11|    100000|     100|0.00000000|  FAILED
 *           sts_serial|  11|    100000|     100|0.24830326|  PASSED
 *           sts_serial|  12|    100000|     100|0.00000000|  FAILED
 *           sts_serial|  12|    100000|     100|0.51859521|  PASSED
 *           sts_serial|  13|    100000|     100|0.00000000|  FAILED
 *           sts_serial|  13|    100000|     100|0.54684078|  PASSED
 *           sts_serial|  14|    100000|     100|0.00000000|  FAILED
 *           sts_serial|  14|    100000|     100|0.24749310|  PASSED
 *           sts_serial|  15|    100000|     100|0.00000000|  FAILED
 *           sts_serial|  15|    100000|     100|0.28317200|  PASSED
 *           sts_serial|  16|    100000|     100|0.00000000|  FAILED
 *           sts_serial|  16|    100000|     100|0.96702381|  PASSED
 *          rgb_bitdist|   1|    100000|     100|0.00000000|  FAILED
 *          rgb_bitdist|   2|    100000|     100|0.00000000|  FAILED
 *          rgb_bitdist|   3|    100000|     100|0.00000000|  FAILED
 *          rgb_bitdist|   4|    100000|     100|0.00000000|  FAILED
 *          rgb_bitdist|   5|    100000|     100|0.00000000|  FAILED
 *          rgb_bitdist|   6|    100000|     100|0.00000000|  FAILED
 *          rgb_bitdist|   7|    100000|     100|0.00000000|  FAILED
 *          rgb_bitdist|   8|    100000|     100|0.00000000|  FAILED
 *          rgb_bitdist|   9|    100000|     100|0.00000000|  FAILED
 *          rgb_bitdist|  10|    100000|     100|0.00000000|  FAILED
 *          rgb_bitdist|  11|    100000|     100|0.00000000|  FAILED
 *          rgb_bitdist|  12|    100000|     100|0.00000000|  FAILED
 * rgb_minimum_distance|   2|     10000|    1000|0.00000000|  FAILED
 * rgb_minimum_distance|   3|     10000|    1000|0.00000000|  FAILED
 * rgb_minimum_distance|   4|     10000|    1000|0.00000000|  FAILED
 * rgb_minimum_distance|   5|     10000|    1000|0.00000000|  FAILED
 *     rgb_permutations|   2|    100000|     100|0.70561378|  PASSED
 *     rgb_permutations|   3|    100000|     100|0.78449684|  PASSED
 *     rgb_permutations|   4|    100000|     100|0.74532512|  PASSED
 *     rgb_permutations|   5|    100000|     100|0.64539281|  PASSED
 *       rgb_lagged_sum|   0|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|   1|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|   2|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|   3|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|   4|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|   5|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|   6|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|   7|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|   8|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|   9|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  10|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  11|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  12|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  13|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  14|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  15|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  16|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  17|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  18|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  19|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  20|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  21|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  22|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  23|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  24|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  25|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  26|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  27|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  28|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  29|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  30|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  31|   1000000|     100|0.00000000|  FAILED
 *       rgb_lagged_sum|  32|   1000000|     100|0.00000000|  FAILED
 *      rgb_kstest_test|   0|     10000|    1000|0.00000000|  FAILED
 *      dab_bytedistrib|   0|  51200000|       1|0.00000000|  FAILED
 *              dab_dct| 256|     50000|       1|0.00000000|  FAILED
 *         dab_filltree|  32|  15000000|       1|0.27021810|  PASSED
 *         dab_filltree|  32|  15000000|       1|0.19091321|  PASSED
 *        dab_filltree2|   0|   5000000|       1|0.00000000|  FAILED
 *        dab_filltree2|   1|   5000000|       1|0.00000000|  FAILED
 *         dab_monobit2|  12|  65000000|       1|1.00000000|  FAILED
 * #=============================================================================#
 * # ---------- MeteoIO's XOR generator ----------
 * #=============================================================================#
 *         test_name   |ntup| tsamples |psamples|  p-value |Assessment
 * #=============================================================================#
 * # The file file_input was rewound 88 times
 *    diehard_birthdays|   0|       100|     100|0.12045448|  PASSED
 *       diehard_operm5|   0|   1000000|     100|0.68176209|  PASSED
 *   diehard_rank_32x32|   0|     40000|     100|0.38302444|  PASSED
 *     diehard_rank_6x8|   0|    100000|     100|0.70959824|  PASSED
 *    diehard_bitstream|   0|   2097152|     100|0.13085472|  PASSED
 *         diehard_opso|   0|   2097152|     100|0.54027846|  PASSED
 *         diehard_oqso|   0|   2097152|     100|0.96236177|  PASSED
 *          diehard_dna|   0|   2097152|     100|0.10289128|  PASSED
 * diehard_count_1s_str|   0|    256000|     100|0.72671076|  PASSED
 * diehard_count_1s_byt|   0|    256000|     100|0.74290626|  PASSED
 *  diehard_parking_lot|   0|     12000|     100|0.45602493|  PASSED
 *     diehard_2dsphere|   2|      8000|     100|0.60117959|  PASSED
 *     diehard_3dsphere|   3|      4000|     100|0.86642811|  PASSED
 *      diehard_squeeze|   0|    100000|     100|0.43870967|  PASSED
 *         diehard_sums|   0|       100|     100|0.64147029|  PASSED
 *         diehard_runs|   0|    100000|     100|0.77132323|  PASSED
 *         diehard_runs|   0|    100000|     100|0.20403616|  PASSED
 *        diehard_craps|   0|    200000|     100|0.93550853|  PASSED
 *        diehard_craps|   0|    200000|     100|0.18756198|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.81899786|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.36808636|  PASSED
 *          sts_monobit|   1|    100000|     100|0.21979851|  PASSED
 *             sts_runs|   2|    100000|     100|0.02284836|  PASSED
 *           sts_serial|   1|    100000|     100|0.82689663|  PASSED
 *           sts_serial|   2|    100000|     100|0.99713803|   WEAK
 *           sts_serial|   3|    100000|     100|0.18497580|  PASSED
 *           sts_serial|   3|    100000|     100|0.05111402|  PASSED
 *           sts_serial|   4|    100000|     100|0.07758415|  PASSED
 *           sts_serial|   4|    100000|     100|0.45390764|  PASSED
 *           sts_serial|   5|    100000|     100|0.12301209|  PASSED
 *           sts_serial|   5|    100000|     100|0.92184175|  PASSED
 *           sts_serial|   6|    100000|     100|0.86072819|  PASSED
 *           sts_serial|   6|    100000|     100|0.86762303|  PASSED
 *           sts_serial|   7|    100000|     100|0.51949446|  PASSED
 *           sts_serial|   7|    100000|     100|0.94327933|  PASSED
 *           sts_serial|   8|    100000|     100|0.03901476|  PASSED
 *           sts_serial|   8|    100000|     100|0.14766057|  PASSED
 *           sts_serial|   9|    100000|     100|0.32167782|  PASSED
 *           sts_serial|   9|    100000|     100|0.85589005|  PASSED
 *           sts_serial|  10|    100000|     100|0.27561237|  PASSED
 *           sts_serial|  10|    100000|     100|0.84549731|  PASSED
 *           sts_serial|  11|    100000|     100|0.81071716|  PASSED
 *           sts_serial|  11|    100000|     100|0.19921550|  PASSED
 *           sts_serial|  12|    100000|     100|0.77836792|  PASSED
 *           sts_serial|  12|    100000|     100|0.94431528|  PASSED
 *           sts_serial|  13|    100000|     100|0.57153659|  PASSED
 *           sts_serial|  13|    100000|     100|0.15777123|  PASSED
 *           sts_serial|  14|    100000|     100|0.70324315|  PASSED
 *           sts_serial|  14|    100000|     100|0.69293364|  PASSED
 *           sts_serial|  15|    100000|     100|0.13673480|  PASSED
 *           sts_serial|  15|    100000|     100|0.22964883|  PASSED
 *           sts_serial|  16|    100000|     100|0.61626290|  PASSED
 *           sts_serial|  16|    100000|     100|0.82155600|  PASSED
 *          rgb_bitdist|   1|    100000|     100|0.58867260|  PASSED
 *          rgb_bitdist|   2|    100000|     100|0.24041805|  PASSED
 *          rgb_bitdist|   3|    100000|     100|0.90631007|  PASSED
 *          rgb_bitdist|   4|    100000|     100|0.27943794|  PASSED
 *          rgb_bitdist|   5|    100000|     100|0.30483125|  PASSED
 *          rgb_bitdist|   6|    100000|     100|0.91824913|  PASSED
 *          rgb_bitdist|   7|    100000|     100|0.06336972|  PASSED
 *          rgb_bitdist|   8|    100000|     100|0.43249369|  PASSED
 *          rgb_bitdist|   9|    100000|     100|0.36140764|  PASSED
 *          rgb_bitdist|  10|    100000|     100|0.67846826|  PASSED
 *          rgb_bitdist|  11|    100000|     100|0.88433105|  PASSED
 *          rgb_bitdist|  12|    100000|     100|0.93743658|  PASSED
 * rgb_minimum_distance|   2|     10000|    1000|0.57731305|  PASSED
 * rgb_minimum_distance|   3|     10000|    1000|0.37804172|  PASSED
 * rgb_minimum_distance|   4|     10000|    1000|0.07249321|  PASSED
 * rgb_minimum_distance|   5|     10000|    1000|0.21384664|  PASSED
 *     rgb_permutations|   2|    100000|     100|0.71271095|  PASSED
 *     rgb_permutations|   3|    100000|     100|0.38717961|  PASSED
 *     rgb_permutations|   4|    100000|     100|0.42087970|  PASSED
 *     rgb_permutations|   5|    100000|     100|0.63534024|  PASSED
 *       rgb_lagged_sum|   0|   1000000|     100|0.29051338|  PASSED
 *       rgb_lagged_sum|   1|   1000000|     100|0.70594074|  PASSED
 *       rgb_lagged_sum|   2|   1000000|     100|0.07553691|  PASSED
 *       rgb_lagged_sum|   3|   1000000|     100|0.96291303|  PASSED
 *       rgb_lagged_sum|   4|   1000000|     100|0.99029236|  PASSED
 *       rgb_lagged_sum|   5|   1000000|     100|0.58679539|  PASSED
 *       rgb_lagged_sum|   6|   1000000|     100|0.50294005|  PASSED
 *       rgb_lagged_sum|   7|   1000000|     100|0.51399132|  PASSED
 *       rgb_lagged_sum|   8|   1000000|     100|0.75970188|  PASSED
 *       rgb_lagged_sum|   9|   1000000|     100|0.64410621|  PASSED
 *       rgb_lagged_sum|  10|   1000000|     100|0.31410172|  PASSED
 *       rgb_lagged_sum|  11|   1000000|     100|0.49745204|  PASSED
 *       rgb_lagged_sum|  12|   1000000|     100|0.63601533|  PASSED
 *       rgb_lagged_sum|  13|   1000000|     100|0.67325992|  PASSED
 *       rgb_lagged_sum|  14|   1000000|     100|0.89565303|  PASSED
 *       rgb_lagged_sum|  15|   1000000|     100|0.22455715|  PASSED
 *       rgb_lagged_sum|  16|   1000000|     100|0.67474052|  PASSED
 *       rgb_lagged_sum|  17|   1000000|     100|0.86231270|  PASSED
 *       rgb_lagged_sum|  18|   1000000|     100|0.83858353|  PASSED
 *       rgb_lagged_sum|  19|   1000000|     100|0.76827640|  PASSED
 *       rgb_lagged_sum|  20|   1000000|     100|0.86217123|  PASSED
 *       rgb_lagged_sum|  21|   1000000|     100|0.82616486|  PASSED
 *       rgb_lagged_sum|  22|   1000000|     100|0.71637003|  PASSED
 *       rgb_lagged_sum|  23|   1000000|     100|0.10813332|  PASSED
 *       rgb_lagged_sum|  24|   1000000|     100|0.24239024|  PASSED
 *       rgb_lagged_sum|  25|   1000000|     100|0.49073378|  PASSED
 *       rgb_lagged_sum|  26|   1000000|     100|0.48516249|  PASSED
 *       rgb_lagged_sum|  27|   1000000|     100|0.50884127|  PASSED
 *       rgb_lagged_sum|  28|   1000000|     100|0.81183837|  PASSED
 *       rgb_lagged_sum|  29|   1000000|     100|0.51041034|  PASSED
 *       rgb_lagged_sum|  30|   1000000|     100|0.48703712|  PASSED
 *       rgb_lagged_sum|  31|   1000000|     100|0.37454423|  PASSED
 *       rgb_lagged_sum|  32|   1000000|     100|0.24603530|  PASSED
 *      rgb_kstest_test|   0|     10000|    1000|0.42451693|  PASSED
 *      dab_bytedistrib|   0|  51200000|       1|0.53431819|  PASSED
 *              dab_dct| 256|     50000|       1|0.49869025|  PASSED
 *         dab_filltree|  32|  15000000|       1|0.97577160|  PASSED
 *         dab_filltree|  32|  15000000|       1|0.78867864|  PASSED
 *        dab_filltree2|   0|   5000000|       1|0.92150183|  PASSED
 *        dab_filltree2|   1|   5000000|       1|0.86433669|  PASSED
 *         dab_monobit2|  12|  65000000|       1|0.68684982|  PASSED
 * #=============================================================================#
 * # ---------- MeteoIO's PCG generator ----------
 * #=============================================================================#
 *         test_name   |ntup| tsamples |psamples|  p-value |Assessment
 * #=============================================================================#
 *    diehard_birthdays|   0|       100|     100|0.30611733|  PASSED
 *       diehard_operm5|   0|   1000000|     100|0.85337458|  PASSED
 *   diehard_rank_32x32|   0|     40000|     100|0.13861533|  PASSED
 *     diehard_rank_6x8|   0|    100000|     100|0.52227522|  PASSED
 *    diehard_bitstream|   0|   2097152|     100|0.97554697|  PASSED
 *         diehard_opso|   0|   2097152|     100|0.51552460|  PASSED
 *         diehard_oqso|   0|   2097152|     100|0.53264545|  PASSED
 *          diehard_dna|   0|   2097152|     100|0.61817549|  PASSED
 * diehard_count_1s_str|   0|    256000|     100|0.99764411|   WEAK
 * diehard_count_1s_byt|   0|    256000|     100|0.70976036|  PASSED
 *  diehard_parking_lot|   0|     12000|     100|0.87630056|  PASSED
 *     diehard_2dsphere|   2|      8000|     100|0.65075574|  PASSED
 *     diehard_3dsphere|   3|      4000|     100|0.05268042|  PASSED
 *      diehard_squeeze|   0|    100000|     100|0.62515552|  PASSED
 *         diehard_sums|   0|       100|     100|0.41680388|  PASSED
 *         diehard_runs|   0|    100000|     100|0.95126389|  PASSED
 *         diehard_runs|   0|    100000|     100|0.19295069|  PASSED
 *        diehard_craps|   0|    200000|     100|0.56451449|  PASSED
 *        diehard_craps|   0|    200000|     100|0.70173861|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.30987088|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.01578185|  PASSED
 *          sts_monobit|   1|    100000|     100|0.37553060|  PASSED
 *             sts_runs|   2|    100000|     100|0.58229832|  PASSED
 *           sts_serial|   1|    100000|     100|0.14004258|  PASSED
 *           sts_serial|   2|    100000|     100|0.11515591|  PASSED
 *           sts_serial|   3|    100000|     100|0.10588840|  PASSED
 *           sts_serial|   3|    100000|     100|0.31060254|  PASSED
 *           sts_serial|   4|    100000|     100|0.17520685|  PASSED
 *           sts_serial|   4|    100000|     100|0.88866431|  PASSED
 *           sts_serial|   5|    100000|     100|0.66233000|  PASSED
 *           sts_serial|   5|    100000|     100|0.84998032|  PASSED
 *           sts_serial|   6|    100000|     100|0.97556496|  PASSED
 *           sts_serial|   6|    100000|     100|0.32807318|  PASSED
 *           sts_serial|   7|    100000|     100|0.33865125|  PASSED
 *           sts_serial|   7|    100000|     100|0.26793941|  PASSED
 *           sts_serial|   8|    100000|     100|0.04701350|  PASSED
 *           sts_serial|   8|    100000|     100|0.34346444|  PASSED
 *           sts_serial|   9|    100000|     100|0.47954800|  PASSED
 *           sts_serial|   9|    100000|     100|0.58661247|  PASSED
 *           sts_serial|  10|    100000|     100|0.92972114|  PASSED
 *           sts_serial|  10|    100000|     100|0.39666160|  PASSED
 *           sts_serial|  11|    100000|     100|0.99744594|   WEAK
 *           sts_serial|  11|    100000|     100|0.55783437|  PASSED
 *           sts_serial|  12|    100000|     100|0.52260461|  PASSED
 *           sts_serial|  12|    100000|     100|0.72804698|  PASSED
 *           sts_serial|  13|    100000|     100|0.22331086|  PASSED
 *           sts_serial|  13|    100000|     100|0.99708143|   WEAK
 *           sts_serial|  14|    100000|     100|0.24692884|  PASSED
 *           sts_serial|  14|    100000|     100|0.11816038|  PASSED
 *           sts_serial|  15|    100000|     100|0.48867820|  PASSED
 *           sts_serial|  15|    100000|     100|0.64283400|  PASSED
 *           sts_serial|  16|    100000|     100|0.57175474|  PASSED
 *           sts_serial|  16|    100000|     100|0.80413247|  PASSED
 *          rgb_bitdist|   1|    100000|     100|0.78286393|  PASSED
 *          rgb_bitdist|   2|    100000|     100|0.41751882|  PASSED
 *          rgb_bitdist|   3|    100000|     100|0.99225317|  PASSED
 *          rgb_bitdist|   4|    100000|     100|0.26843160|  PASSED
 *          rgb_bitdist|   5|    100000|     100|0.41019859|  PASSED
 *          rgb_bitdist|   6|    100000|     100|0.00857013|  PASSED
 *          rgb_bitdist|   7|    100000|     100|0.35404539|  PASSED
 *          rgb_bitdist|   8|    100000|     100|0.78633455|  PASSED
 *          rgb_bitdist|   9|    100000|     100|0.94074517|  PASSED
 *          rgb_bitdist|  10|    100000|     100|0.98280710|  PASSED
 *          rgb_bitdist|  11|    100000|     100|0.36084540|  PASSED
 *          rgb_bitdist|  12|    100000|     100|0.10641731|  PASSED
 * rgb_minimum_distance|   2|     10000|    1000|0.35560587|  PASSED
 * rgb_minimum_distance|   3|     10000|    1000|0.78872602|  PASSED
 * rgb_minimum_distance|   4|     10000|    1000|0.32824016|  PASSED
 * rgb_minimum_distance|   5|     10000|    1000|0.62849158|  PASSED
 *     rgb_permutations|   2|    100000|     100|0.81115433|  PASSED
 *     rgb_permutations|   3|    100000|     100|0.95847665|  PASSED
 *     rgb_permutations|   4|    100000|     100|0.21432525|  PASSED
 *     rgb_permutations|   5|    100000|     100|0.32399489|  PASSED
 *       rgb_lagged_sum|   0|   1000000|     100|0.68964293|  PASSED
 *       rgb_lagged_sum|   1|   1000000|     100|0.99736916|   WEAK
 *       rgb_lagged_sum|   2|   1000000|     100|0.88914743|  PASSED
 *       rgb_lagged_sum|   3|   1000000|     100|0.69040691|  PASSED
 *       rgb_lagged_sum|   4|   1000000|     100|0.56425845|  PASSED
 *       rgb_lagged_sum|   5|   1000000|     100|0.72356996|  PASSED
 *       rgb_lagged_sum|   6|   1000000|     100|0.72350239|  PASSED
 *       rgb_lagged_sum|   7|   1000000|     100|0.96595205|  PASSED
 *       rgb_lagged_sum|   8|   1000000|     100|0.46167022|  PASSED
 *       rgb_lagged_sum|   9|   1000000|     100|0.66944952|  PASSED
 *       rgb_lagged_sum|  10|   1000000|     100|0.11552089|  PASSED
 *       rgb_lagged_sum|  11|   1000000|     100|0.47340674|  PASSED
 *       rgb_lagged_sum|  12|   1000000|     100|0.88912904|  PASSED
 *       rgb_lagged_sum|  13|   1000000|     100|0.85095112|  PASSED
 *       rgb_lagged_sum|  14|   1000000|     100|0.88590882|  PASSED
 *       rgb_lagged_sum|  15|   1000000|     100|0.96677181|  PASSED
 *       rgb_lagged_sum|  16|   1000000|     100|0.71908437|  PASSED
 *       rgb_lagged_sum|  17|   1000000|     100|0.37838020|  PASSED
 *       rgb_lagged_sum|  18|   1000000|     100|0.33524328|  PASSED
 *       rgb_lagged_sum|  19|   1000000|     100|0.05116258|  PASSED
 *       rgb_lagged_sum|  20|   1000000|     100|0.68284302|  PASSED
 *       rgb_lagged_sum|  21|   1000000|     100|0.18686823|  PASSED
 *       rgb_lagged_sum|  22|   1000000|     100|0.82848681|  PASSED
 *       rgb_lagged_sum|  23|   1000000|     100|0.68156360|  PASSED
 *       rgb_lagged_sum|  24|   1000000|     100|0.14155120|  PASSED
 *       rgb_lagged_sum|  25|   1000000|     100|0.85876186|  PASSED
 *       rgb_lagged_sum|  26|   1000000|     100|0.95982860|  PASSED
 *       rgb_lagged_sum|  27|   1000000|     100|0.93969005|  PASSED
 *       rgb_lagged_sum|  28|   1000000|     100|0.65020839|  PASSED
 *       rgb_lagged_sum|  29|   1000000|     100|0.88746765|  PASSED
 *       rgb_lagged_sum|  30|   1000000|     100|0.25119089|  PASSED
 *       rgb_lagged_sum|  31|   1000000|     100|0.29378236|  PASSED
 *       rgb_lagged_sum|  32|   1000000|     100|0.28869932|  PASSED
 *      rgb_kstest_test|   0|     10000|    1000|0.70588166|  PASSED
 *      dab_bytedistrib|   0|  51200000|       1|0.98774439|  PASSED
 *              dab_dct| 256|     50000|       1|0.19956807|  PASSED
 *         dab_filltree|  32|  15000000|       1|0.86399972|  PASSED
 *         dab_filltree|  32|  15000000|       1|0.10289422|  PASSED
 *        dab_filltree2|   0|   5000000|       1|0.05301030|  PASSED
 *        dab_filltree2|   1|   5000000|       1|0.97755469|  PASSED
 *         dab_monobit2|  12|  65000000|       1|0.49159043|  PASSED
 * #=============================================================================#
 * # ---------- MeteoIO's Mersenne Twister ----------
 * #=============================================================================#
 *         test_name   |ntup| tsamples |psamples|  p-value |Assessment
 * #=============================================================================#
 *    diehard_birthdays|   0|       100|     100|0.82253247|  PASSED
 *       diehard_operm5|   0|   1000000|     100|0.21178957|  PASSED
 *   diehard_rank_32x32|   0|     40000|     100|0.31973686|  PASSED
 *     diehard_rank_6x8|   0|    100000|     100|0.89363426|  PASSED
 *    diehard_bitstream|   0|   2097152|     100|0.66751295|  PASSED
 *         diehard_opso|   0|   2097152|     100|0.68394748|  PASSED
 *         diehard_oqso|   0|   2097152|     100|0.97944473|  PASSED
 *          diehard_dna|   0|   2097152|     100|0.03763098|  PASSED
 * diehard_count_1s_str|   0|    256000|     100|0.88917038|  PASSED
 * diehard_count_1s_byt|   0|    256000|     100|0.13483835|  PASSED
 *  diehard_parking_lot|   0|     12000|     100|0.59549008|  PASSED
 *     diehard_2dsphere|   2|      8000|     100|0.79186313|  PASSED
 *     diehard_3dsphere|   3|      4000|     100|0.99908164|   WEAK
 *      diehard_squeeze|   0|    100000|     100|0.69206636|  PASSED
 *         diehard_sums|   0|       100|     100|0.00166453|   WEAK
 *         diehard_runs|   0|    100000|     100|0.10483337|  PASSED
 *         diehard_runs|   0|    100000|     100|0.14514648|  PASSED
 *        diehard_craps|   0|    200000|     100|0.81768910|  PASSED
 *        diehard_craps|   0|    200000|     100|0.20136565|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.10953859|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.13366752|  PASSED
 *          sts_monobit|   1|    100000|     100|0.91230913|  PASSED
 *             sts_runs|   2|    100000|     100|0.14335596|  PASSED
 *           sts_serial|   1|    100000|     100|0.49939858|  PASSED
 *           sts_serial|   2|    100000|     100|0.32024996|  PASSED
 *           sts_serial|   3|    100000|     100|0.90074110|  PASSED
 *           sts_serial|   3|    100000|     100|0.53246121|  PASSED
 *           sts_serial|   4|    100000|     100|0.08954348|  PASSED
 *           sts_serial|   4|    100000|     100|0.06305190|  PASSED
 *           sts_serial|   5|    100000|     100|0.01483603|  PASSED
 *           sts_serial|   5|    100000|     100|0.20630707|  PASSED
 *           sts_serial|   6|    100000|     100|0.42662161|  PASSED
 *           sts_serial|   6|    100000|     100|0.80660235|  PASSED
 *           sts_serial|   7|    100000|     100|0.00018564|   WEAK
 *           sts_serial|   7|    100000|     100|0.04110236|  PASSED
 *           sts_serial|   8|    100000|     100|0.20346520|  PASSED
 *           sts_serial|   8|    100000|     100|0.87953884|  PASSED
 *           sts_serial|   9|    100000|     100|0.43224325|  PASSED
 *           sts_serial|   9|    100000|     100|0.62598394|  PASSED
 *           sts_serial|  10|    100000|     100|0.80793422|  PASSED
 *           sts_serial|  10|    100000|     100|0.99492509|  PASSED
 *           sts_serial|  11|    100000|     100|0.91235510|  PASSED
 *           sts_serial|  11|    100000|     100|0.35551241|  PASSED
 *           sts_serial|  12|    100000|     100|0.27855050|  PASSED
 *           sts_serial|  12|    100000|     100|0.14287708|  PASSED
 *           sts_serial|  13|    100000|     100|0.47268781|  PASSED
 *           sts_serial|  13|    100000|     100|0.86584342|  PASSED
 *           sts_serial|  14|    100000|     100|0.79177664|  PASSED
 *           sts_serial|  14|    100000|     100|0.59808724|  PASSED
 *           sts_serial|  15|    100000|     100|0.07924232|  PASSED
 *           sts_serial|  15|    100000|     100|0.02515889|  PASSED
 *           sts_serial|  16|    100000|     100|0.09788033|  PASSED
 *           sts_serial|  16|    100000|     100|0.47327069|  PASSED
 *          rgb_bitdist|   1|    100000|     100|0.90859656|  PASSED
 *          rgb_bitdist|   2|    100000|     100|0.65219299|  PASSED
 *          rgb_bitdist|   3|    100000|     100|0.86767388|  PASSED
 *          rgb_bitdist|   4|    100000|     100|0.17870586|  PASSED
 *          rgb_bitdist|   5|    100000|     100|0.98686633|  PASSED
 *          rgb_bitdist|   6|    100000|     100|0.98019975|  PASSED
 *          rgb_bitdist|   7|    100000|     100|0.43739173|  PASSED
 *          rgb_bitdist|   8|    100000|     100|0.67351096|  PASSED
 *          rgb_bitdist|   9|    100000|     100|0.98913128|  PASSED
 *          rgb_bitdist|  10|    100000|     100|0.19281365|  PASSED
 *          rgb_bitdist|  11|    100000|     100|0.66186242|  PASSED
 *          rgb_bitdist|  12|    100000|     100|0.93080409|  PASSED
 * rgb_minimum_distance|   2|     10000|    1000|0.40935556|  PASSED
 * rgb_minimum_distance|   3|     10000|    1000|0.30879540|  PASSED
 * rgb_minimum_distance|   4|     10000|    1000|0.59912826|  PASSED
 * rgb_minimum_distance|   5|     10000|    1000|0.64492200|  PASSED
 *     rgb_permutations|   2|    100000|     100|0.90616743|  PASSED
 *     rgb_permutations|   3|    100000|     100|0.89734728|  PASSED
 *     rgb_permutations|   4|    100000|     100|0.57694248|  PASSED
 *     rgb_permutations|   5|    100000|     100|0.91845418|  PASSED
 *       rgb_lagged_sum|   0|   1000000|     100|0.51985981|  PASSED
 *       rgb_lagged_sum|   1|   1000000|     100|0.74730732|  PASSED
 *       rgb_lagged_sum|   2|   1000000|     100|0.96077254|  PASSED
 *       rgb_lagged_sum|   3|   1000000|     100|0.64992886|  PASSED
 *       rgb_lagged_sum|   4|   1000000|     100|0.08889463|  PASSED
 *       rgb_lagged_sum|   5|   1000000|     100|0.19288526|  PASSED
 *       rgb_lagged_sum|   6|   1000000|     100|0.30230622|  PASSED
 *       rgb_lagged_sum|   7|   1000000|     100|0.57923993|  PASSED
 *       rgb_lagged_sum|   8|   1000000|     100|0.96730234|  PASSED
 *       rgb_lagged_sum|   9|   1000000|     100|0.83008767|  PASSED
 *       rgb_lagged_sum|  10|   1000000|     100|0.62428422|  PASSED
 *       rgb_lagged_sum|  11|   1000000|     100|0.49734948|  PASSED
 *       rgb_lagged_sum|  12|   1000000|     100|0.81433084|  PASSED
 *       rgb_lagged_sum|  13|   1000000|     100|0.33109879|  PASSED
 *       rgb_lagged_sum|  14|   1000000|     100|0.84036199|  PASSED
 *       rgb_lagged_sum|  15|   1000000|     100|0.38518051|  PASSED
 *       rgb_lagged_sum|  16|   1000000|     100|0.67239247|  PASSED
 *       rgb_lagged_sum|  17|   1000000|     100|0.57189104|  PASSED
 *       rgb_lagged_sum|  18|   1000000|     100|0.11060300|  PASSED
 *       rgb_lagged_sum|  19|   1000000|     100|0.08207939|  PASSED
 *       rgb_lagged_sum|  20|   1000000|     100|0.96797696|  PASSED
 *       rgb_lagged_sum|  21|   1000000|     100|0.85276133|  PASSED
 *       rgb_lagged_sum|  22|   1000000|     100|0.62396333|  PASSED
 *       rgb_lagged_sum|  23|   1000000|     100|0.10741997|  PASSED
 *       rgb_lagged_sum|  24|   1000000|     100|0.00133815|   WEAK
 *       rgb_lagged_sum|  25|   1000000|     100|0.93440744|  PASSED
 *       rgb_lagged_sum|  26|   1000000|     100|0.62208635|  PASSED
 *       rgb_lagged_sum|  27|   1000000|     100|0.67716950|  PASSED
 *       rgb_lagged_sum|  28|   1000000|     100|0.84200928|  PASSED
 *       rgb_lagged_sum|  29|   1000000|     100|0.12315098|  PASSED
 *       rgb_lagged_sum|  30|   1000000|     100|0.85076946|  PASSED
 *       rgb_lagged_sum|  31|   1000000|     100|0.98674289|  PASSED
 *       rgb_lagged_sum|  32|   1000000|     100|0.98132089|  PASSED
 *      rgb_kstest_test|   0|     10000|    1000|0.85518280|  PASSED
 *      dab_bytedistrib|   0|  51200000|       1|0.03865242|  PASSED
 *              dab_dct| 256|     50000|       1|0.45151852|  PASSED
 *         dab_filltree|  32|  15000000|       1|0.43711867|  PASSED
 *         dab_filltree|  32|  15000000|       1|0.17002662|  PASSED
 *        dab_filltree2|   0|   5000000|       1|0.75275151|  PASSED
 *        dab_filltree2|   1|   5000000|       1|0.43152216|  PASSED
 *         dab_monobit2|  12|  65000000|       1|0.56354274|  PASSED
 * #=============================================================================#
 * # ---------- MeteoIO's hardware seed through /dev/urandom ----------
 * #=============================================================================#
 *        test_name   |ntup| tsamples |psamples|  p-value |Assessment
 * #=============================================================================#
 *    diehard_birthdays|   0|       100|     100|0.34995998|  PASSED
 *       diehard_operm5|   0|   1000000|     100|0.32746493|  PASSED
 *   diehard_rank_32x32|   0|     40000|     100|0.45529134|  PASSED
 *     diehard_rank_6x8|   0|    100000|     100|0.45464472|  PASSED
 *    diehard_bitstream|   0|   2097152|     100|0.74174566|  PASSED
 *         diehard_opso|   0|   2097152|     100|0.85391211|  PASSED
 *         diehard_oqso|   0|   2097152|     100|0.03289769|  PASSED
 *          diehard_dna|   0|   2097152|     100|0.18584203|  PASSED
 * diehard_count_1s_str|   0|    256000|     100|0.26587361|  PASSED
 * diehard_count_1s_byt|   0|    256000|     100|0.76487483|  PASSED
 *  diehard_parking_lot|   0|     12000|     100|0.78148856|  PASSED
 *     diehard_2dsphere|   2|      8000|     100|0.00404096|   WEAK
 *     diehard_3dsphere|   3|      4000|     100|0.07650764|  PASSED
 *      diehard_squeeze|   0|    100000|     100|0.76720468|  PASSED
 *         diehard_sums|   0|       100|     100|0.15021486|  PASSED
 *         diehard_runs|   0|    100000|     100|0.73190429|  PASSED
 *         diehard_runs|   0|    100000|     100|0.11641745|  PASSED
 *        diehard_craps|   0|    200000|     100|0.51327580|  PASSED
 *        diehard_craps|   0|    200000|     100|0.91790555|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.95940428|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.71308014|  PASSED
 *          sts_monobit|   1|    100000|     100|0.95078372|  PASSED
 *             sts_runs|   2|    100000|     100|0.68447953|  PASSED
 *           sts_serial|   1|    100000|     100|0.94111724|  PASSED
 *           sts_serial|   2|    100000|     100|0.91997054|  PASSED
 *           sts_serial|   3|    100000|     100|0.33160131|  PASSED
 *           sts_serial|   3|    100000|     100|0.36015219|  PASSED
 *           sts_serial|   4|    100000|     100|0.35524447|  PASSED
 *           sts_serial|   4|    100000|     100|0.80131461|  PASSED
 *           sts_serial|   5|    100000|     100|0.40432154|  PASSED
 *           sts_serial|   5|    100000|     100|0.29670898|  PASSED
 *           sts_serial|   6|    100000|     100|0.19624684|  PASSED
 *           sts_serial|   6|    100000|     100|0.78008913|  PASSED
 *           sts_serial|   7|    100000|     100|0.48943065|  PASSED
 *           sts_serial|   7|    100000|     100|0.79865985|  PASSED
 *           sts_serial|   8|    100000|     100|0.10168430|  PASSED
 *           sts_serial|   8|    100000|     100|0.02619230|  PASSED
 *           sts_serial|   9|    100000|     100|0.47818037|  PASSED
 *           sts_serial|   9|    100000|     100|0.65931186|  PASSED
 *           sts_serial|  10|    100000|     100|0.57902903|  PASSED
 *           sts_serial|  10|    100000|     100|0.20841366|  PASSED
 *           sts_serial|  11|    100000|     100|0.20262420|  PASSED
 *           sts_serial|  11|    100000|     100|0.04916154|  PASSED
 *           sts_serial|  12|    100000|     100|0.36846324|  PASSED
 *           sts_serial|  12|    100000|     100|0.67908792|  PASSED
 *           sts_serial|  13|    100000|     100|0.28850671|  PASSED
 *           sts_serial|  13|    100000|     100|0.27326200|  PASSED
 *           sts_serial|  14|    100000|     100|0.18634033|  PASSED
 *           sts_serial|  14|    100000|     100|0.77790083|  PASSED
 *           sts_serial|  15|    100000|     100|0.07263144|  PASSED
 *           sts_serial|  15|    100000|     100|0.03880330|  PASSED
 *           sts_serial|  16|    100000|     100|0.15115141|  PASSED
 *           sts_serial|  16|    100000|     100|0.40485798|  PASSED
 *          rgb_bitdist|   1|    100000|     100|0.26387368|  PASSED
 *          rgb_bitdist|   2|    100000|     100|0.41977272|  PASSED
 *          rgb_bitdist|   3|    100000|     100|0.81246062|  PASSED
 *          rgb_bitdist|   4|    100000|     100|0.53180325|  PASSED
 *          rgb_bitdist|   5|    100000|     100|0.57916879|  PASSED
 *          rgb_bitdist|   6|    100000|     100|0.70438684|  PASSED
 *          rgb_bitdist|   7|    100000|     100|0.28560217|  PASSED
 *          rgb_bitdist|   8|    100000|     100|0.58521528|  PASSED
 *          rgb_bitdist|   9|    100000|     100|0.32013386|  PASSED
 *          rgb_bitdist|  10|    100000|     100|0.61939902|  PASSED
 *          rgb_bitdist|  11|    100000|     100|0.08846552|  PASSED
 *          rgb_bitdist|  12|    100000|     100|0.70170402|  PASSED
 * rgb_minimum_distance|   2|     10000|    1000|0.02671540|  PASSED
 * rgb_minimum_distance|   3|     10000|    1000|0.94613230|  PASSED
 * rgb_minimum_distance|   4|     10000|    1000|0.54465261|  PASSED
 * rgb_minimum_distance|   5|     10000|    1000|0.05533111|  PASSED
 *     rgb_permutations|   2|    100000|     100|0.24551831|  PASSED
 *     rgb_permutations|   3|    100000|     100|0.48396629|  PASSED
 *     rgb_permutations|   4|    100000|     100|0.39585636|  PASSED
 *     rgb_permutations|   5|    100000|     100|0.83858400|  PASSED
 *       rgb_lagged_sum|   0|   1000000|     100|0.52754001|  PASSED
 *       rgb_lagged_sum|   1|   1000000|     100|0.61577700|  PASSED
 *       rgb_lagged_sum|   2|   1000000|     100|0.97312394|  PASSED
 *       rgb_lagged_sum|   3|   1000000|     100|0.95265125|  PASSED
 *       rgb_lagged_sum|   4|   1000000|     100|0.61691352|  PASSED
 *       rgb_lagged_sum|   5|   1000000|     100|0.22625956|  PASSED
 *       rgb_lagged_sum|   6|   1000000|     100|0.44895384|  PASSED
 *       rgb_lagged_sum|   7|   1000000|     100|0.67063631|  PASSED
 *       rgb_lagged_sum|   8|   1000000|     100|0.80618162|  PASSED
 *       rgb_lagged_sum|   9|   1000000|     100|0.95412586|  PASSED
 *       rgb_lagged_sum|  10|   1000000|     100|0.43904741|  PASSED
 *       rgb_lagged_sum|  11|   1000000|     100|0.00027503|   WEAK
 *       rgb_lagged_sum|  12|   1000000|     100|0.59191040|  PASSED
 *       rgb_lagged_sum|  13|   1000000|     100|0.96138321|  PASSED
 *       rgb_lagged_sum|  14|   1000000|     100|0.23925605|  PASSED
 *       rgb_lagged_sum|  15|   1000000|     100|0.95895455|  PASSED
 *       rgb_lagged_sum|  16|   1000000|     100|0.86479019|  PASSED
 *       rgb_lagged_sum|  17|   1000000|     100|0.99271063|  PASSED
 *       rgb_lagged_sum|  18|   1000000|     100|0.07340033|  PASSED
 *       rgb_lagged_sum|  19|   1000000|     100|0.30090484|  PASSED
 *       rgb_lagged_sum|  20|   1000000|     100|0.07296293|  PASSED
 *       rgb_lagged_sum|  21|   1000000|     100|0.07541724|  PASSED
 *       rgb_lagged_sum|  22|   1000000|     100|0.99744800|   WEAK
 *       rgb_lagged_sum|  23|   1000000|     100|0.11186717|  PASSED
 *       rgb_lagged_sum|  24|   1000000|     100|0.99086821|  PASSED
 *       rgb_lagged_sum|  25|   1000000|     100|0.02194406|  PASSED
 *       rgb_lagged_sum|  26|   1000000|     100|0.92171159|  PASSED
 *       rgb_lagged_sum|  27|   1000000|     100|0.88602559|  PASSED
 *       rgb_lagged_sum|  28|   1000000|     100|0.98115042|  PASSED
 *       rgb_lagged_sum|  29|   1000000|     100|0.02165800|  PASSED
 *       rgb_lagged_sum|  30|   1000000|     100|0.61661470|  PASSED
 *       rgb_lagged_sum|  31|   1000000|     100|0.20261751|  PASSED
 *       rgb_lagged_sum|  32|   1000000|     100|0.59218537|  PASSED
 *      rgb_kstest_test|   0|     10000|    1000|0.27361658|  PASSED
 *      dab_bytedistrib|   0|  51200000|       1|0.94592564|  PASSED
 *              dab_dct| 256|     50000|       1|0.41859755|  PASSED
 *         dab_filltree|  32|  15000000|       1|0.76739133|  PASSED
 *         dab_filltree|  32|  15000000|       1|0.26581069|  PASSED
 *        dab_filltree2|   0|   5000000|       1|0.02605838|  PASSED
 *        dab_filltree2|   1|   5000000|       1|0.74389817|  PASSED
 *         dab_monobit2|  12|  65000000|       1|0.28647627|  PASSED
 * #=============================================================================#
 * # ---------- Modern AES crypto generator ----------
 *    rng_name    |rands/second|   Seed   |
 *         AES_OFB|  6.37e+06  |3232005462|
 * #=============================================================================#
 *         test_name   |ntup| tsamples |psamples|  p-value |Assessment
 * #=============================================================================#
 *    diehard_birthdays|   0|       100|     100|0.74435744|  PASSED
 *       diehard_operm5|   0|   1000000|     100|0.32904570|  PASSED
 *   diehard_rank_32x32|   0|     40000|     100|0.85406683|  PASSED
 *     diehard_rank_6x8|   0|    100000|     100|0.44255811|  PASSED
 *    diehard_bitstream|   0|   2097152|     100|0.51899516|  PASSED
 *         diehard_opso|   0|   2097152|     100|0.41191163|  PASSED
 *         diehard_oqso|   0|   2097152|     100|0.32705297|  PASSED
 *          diehard_dna|   0|   2097152|     100|0.00002584|   WEAK
 * diehard_count_1s_str|   0|    256000|     100|0.26033835|  PASSED
 * diehard_count_1s_byt|   0|    256000|     100|0.65992315|  PASSED
 *  diehard_parking_lot|   0|     12000|     100|0.49340705|  PASSED
 *     diehard_2dsphere|   2|      8000|     100|0.49486843|  PASSED
 *     diehard_3dsphere|   3|      4000|     100|0.69480866|  PASSED
 *      diehard_squeeze|   0|    100000|     100|0.24509533|  PASSED
 *         diehard_sums|   0|       100|     100|0.56234006|  PASSED
 *         diehard_runs|   0|    100000|     100|0.54652419|  PASSED
 *         diehard_runs|   0|    100000|     100|0.61171723|  PASSED
 *        diehard_craps|   0|    200000|     100|0.63350710|  PASSED
 *        diehard_craps|   0|    200000|     100|0.34587483|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.56466735|  PASSED
 *  marsaglia_tsang_gcd|   0|  10000000|     100|0.54209067|  PASSED
 *          sts_monobit|   1|    100000|     100|0.58300255|  PASSED
 *             sts_runs|   2|    100000|     100|0.07772198|  PASSED
 *           sts_serial|   1|    100000|     100|0.46471954|  PASSED
 *           sts_serial|   2|    100000|     100|0.15862944|  PASSED
 *           sts_serial|   3|    100000|     100|0.42568332|  PASSED
 *           sts_serial|   3|    100000|     100|0.68293665|  PASSED
 *           sts_serial|   4|    100000|     100|0.73346962|  PASSED
 *           sts_serial|   4|    100000|     100|0.61288736|  PASSED
 *           sts_serial|   5|    100000|     100|0.83897171|  PASSED
 *           sts_serial|   5|    100000|     100|0.84743739|  PASSED
 *           sts_serial|   6|    100000|     100|0.83328092|  PASSED
 *           sts_serial|   6|    100000|     100|0.72778824|  PASSED
 *           sts_serial|   7|    100000|     100|0.46201158|  PASSED
 *           sts_serial|   7|    100000|     100|0.61236377|  PASSED
 *           sts_serial|   8|    100000|     100|0.82259071|  PASSED
 *           sts_serial|   8|    100000|     100|0.14753897|  PASSED
 *           sts_serial|   9|    100000|     100|0.90776543|  PASSED
 *           sts_serial|   9|    100000|     100|0.70647508|  PASSED
 *           sts_serial|  10|    100000|     100|0.89681973|  PASSED
 *           sts_serial|  10|    100000|     100|0.46076365|  PASSED
 *           sts_serial|  11|    100000|     100|0.67367369|  PASSED
 *           sts_serial|  11|    100000|     100|0.37293255|  PASSED
 *           sts_serial|  12|    100000|     100|0.84656977|  PASSED
 *           sts_serial|  12|    100000|     100|0.76373116|  PASSED
 *           sts_serial|  13|    100000|     100|0.39318660|  PASSED
 *           sts_serial|  13|    100000|     100|0.80848926|  PASSED
 *           sts_serial|  14|    100000|     100|0.73276243|  PASSED
 *           sts_serial|  14|    100000|     100|0.89562062|  PASSED
 *           sts_serial|  15|    100000|     100|0.21683312|  PASSED
 *           sts_serial|  15|    100000|     100|0.50060895|  PASSED
 *           sts_serial|  16|    100000|     100|0.25979182|  PASSED
 *           sts_serial|  16|    100000|     100|0.81651079|  PASSED
 *          rgb_bitdist|   1|    100000|     100|0.35818457|  PASSED
 *          rgb_bitdist|   2|    100000|     100|0.18499831|  PASSED
 *          rgb_bitdist|   3|    100000|     100|0.83785965|  PASSED
 *          rgb_bitdist|   4|    100000|     100|0.13635981|  PASSED
 *          rgb_bitdist|   5|    100000|     100|0.80288280|  PASSED
 *          rgb_bitdist|   6|    100000|     100|0.26835958|  PASSED
 *          rgb_bitdist|   7|    100000|     100|0.65062732|  PASSED
 *          rgb_bitdist|   8|    100000|     100|0.93339630|  PASSED
 *          rgb_bitdist|   9|    100000|     100|0.65592490|  PASSED
 *          rgb_bitdist|  10|    100000|     100|0.92999418|  PASSED
 *          rgb_bitdist|  11|    100000|     100|0.08819807|  PASSED
 *          rgb_bitdist|  12|    100000|     100|0.83994867|  PASSED
 * rgb_minimum_distance|   2|     10000|    1000|0.20870831|  PASSED
 * rgb_minimum_distance|   3|     10000|    1000|0.14801298|  PASSED
 * rgb_minimum_distance|   4|     10000|    1000|0.67682632|  PASSED
 * rgb_minimum_distance|   5|     10000|    1000|0.52034430|  PASSED
 *     rgb_permutations|   2|    100000|     100|0.98884473|  PASSED
 *     rgb_permutations|   3|    100000|     100|0.14452545|  PASSED
 *     rgb_permutations|   4|    100000|     100|0.55317692|  PASSED
 *     rgb_permutations|   5|    100000|     100|0.36027454|  PASSED
 *       rgb_lagged_sum|   0|   1000000|     100|0.33982549|  PASSED
 *       rgb_lagged_sum|   1|   1000000|     100|0.31595934|  PASSED
 *       rgb_lagged_sum|   2|   1000000|     100|0.65722479|  PASSED
 *       rgb_lagged_sum|   3|   1000000|     100|0.99972394|   WEAK
 *       rgb_lagged_sum|   4|   1000000|     100|0.24595444|  PASSED
 *       rgb_lagged_sum|   5|   1000000|     100|0.51646049|  PASSED
 *       rgb_lagged_sum|   6|   1000000|     100|0.28270631|  PASSED
 *       rgb_lagged_sum|   7|   1000000|     100|0.58433720|  PASSED
 *       rgb_lagged_sum|   8|   1000000|     100|0.82820708|  PASSED
 *       rgb_lagged_sum|   9|   1000000|     100|0.74532620|  PASSED
 *       rgb_lagged_sum|  10|   1000000|     100|0.16422313|  PASSED
 *       rgb_lagged_sum|  11|   1000000|     100|0.43606954|  PASSED
 *       rgb_lagged_sum|  12|   1000000|     100|0.79274604|  PASSED
 *       rgb_lagged_sum|  13|   1000000|     100|0.57575293|  PASSED
 *       rgb_lagged_sum|  14|   1000000|     100|0.68237030|  PASSED
 *       rgb_lagged_sum|  15|   1000000|     100|0.89142299|  PASSED
 *       rgb_lagged_sum|  16|   1000000|     100|0.17680959|  PASSED
 *       rgb_lagged_sum|  17|   1000000|     100|0.46577456|  PASSED
 *       rgb_lagged_sum|  18|   1000000|     100|0.91604868|  PASSED
 *       rgb_lagged_sum|  19|   1000000|     100|0.89800891|  PASSED
 *       rgb_lagged_sum|  20|   1000000|     100|0.79179949|  PASSED
 *       rgb_lagged_sum|  21|   1000000|     100|0.52070680|  PASSED
 *       rgb_lagged_sum|  22|   1000000|     100|0.98784262|  PASSED
 *       rgb_lagged_sum|  23|   1000000|     100|0.59760181|  PASSED
 *       rgb_lagged_sum|  24|   1000000|     100|0.95193486|  PASSED
 *       rgb_lagged_sum|  25|   1000000|     100|0.55947446|  PASSED
 *       rgb_lagged_sum|  26|   1000000|     100|0.70940243|  PASSED
 *       rgb_lagged_sum|  27|   1000000|     100|0.13067759|  PASSED
 *       rgb_lagged_sum|  28|   1000000|     100|0.67353164|  PASSED
 *       rgb_lagged_sum|  29|   1000000|     100|0.39176071|  PASSED
 *       rgb_lagged_sum|  30|   1000000|     100|0.26042688|  PASSED
 *       rgb_lagged_sum|  31|   1000000|     100|0.94158603|  PASSED
 *       rgb_lagged_sum|  32|   1000000|     100|0.09088013|  PASSED
 *      rgb_kstest_test|   0|     10000|    1000|0.31359471|  PASSED
 *      dab_bytedistrib|   0|  51200000|       1|0.34836976|  PASSED
 *              dab_dct| 256|     50000|       1|0.58392168|  PASSED
 *         dab_filltree|  32|  15000000|       1|0.45835465|  PASSED
 *         dab_filltree|  32|  15000000|       1|0.54421849|  PASSED
 *        dab_filltree2|   0|   5000000|       1|0.16036442|  PASSED
 *        dab_filltree2|   1|   5000000|       1|0.77754723|  PASSED
 *         dab_monobit2|  12|  65000000|       1|0.66929755|  PASSED
 * @endcode
*/

class RngCore {
	public:
		bool hardware_seed_success; //store if hardware seed went as planned (Windows?) 
	
		RngCore();	
		virtual ~RngCore();
	
		virtual uint64_t int64() = 0;
		virtual uint32_t int32() = 0;
		virtual void getState(std::vector<uint64_t>& ovec_seed) const = 0;
		virtual void setState(const std::vector<uint64_t>& ivec_seed) = 0;
		//hardware or time seed; everyone may retrieve those from our RNG from outside:
		bool getUniqueSeed(uint64_t& store) const;

	protected: //some lower level functions
		uint64_t combine32to64(const uint32_t& low, const uint32_t& high) const;
		double doubFromInt(const uint64_t& rn) const;
		double trueDoub(); //[0, 1]

	private:
		bool getEntropy(uint64_t& store) const; //hardware seed
		uint64_t timeMixer(const time_t& tt, const clock_t& cc) const;
		uint32_t hash(const uint32_t& nn) const;
		unsigned int countLeadingZeros(const uint64_t& nn) const;
};

class RandomNumberGenerator : private RngCore {
	public:
		enum RNG_TYPE //computation method
		{
			RNG_XOR, //!< Combined generator
			RNG_PCG, //!< Permuted linear congruential generator
			RNG_MTW //!< Mersenne Twister generator
		};
		
//CUSTOM_DIST step 1/7: Give your distribution a name in this enum
		enum RNG_DISTR //desired distribution, only used for doubles!
		{
			RNG_UNIFORM, //!< Uniform deviates
			RNG_GAUSS, //!< Gaussian deviates
			RNG_NORMAL, //!< = RNG_GAUSS
			RNG_GAMMA, //!< Gamma deviates
			RNG_CHISQUARED, //!< Chi-Squared deviates
			RNG_STUDENTT, //!< Student-t deviates
			RNG_BETA, //!< Beta deviates
			RNG_F //!< Fisher deviates
		};
		enum RNG_BOUND //return uniform double respecting these boundaries
		{
			RNG_AINCBINC, //!< [0, 1]
			RNG_AINCBEXC, //!< [0, 1)
			RNG_AEXCBINC, //!< (0, 1]
			RNG_AEXCBEXC  //!< (0, 1)
		};

		//distribution_params's default is an empty vector, which means choose default params
		RandomNumberGenerator(const RNG_TYPE& type = RNG_XOR, const RNG_DISTR& distribution = RNG_UNIFORM,
    		    const std::vector<double>& distribution_params = std::vector<double>());
		RandomNumberGenerator(const RandomNumberGenerator& rng);
		virtual ~RandomNumberGenerator();

		RandomNumberGenerator& operator=(const RandomNumberGenerator& rng);

		uint64_t int64();
		uint32_t int32();
		double doub(); //we keep this separate for speed
		double doub(const RNG_BOUND& bounds, const bool& true_double = false);
		double draw(); //alias for uniform double

		double pdf(const double& xx); //probability density function
		double cdf(const double& xx); //cumulative distribution function

		uint64_t range64(const uint64_t& aa, const uint64_t& bb); //[a, b]
		uint32_t range32(const uint32_t& aa, const uint32_t& bb); //[a, b]
		bool trueRange32(const uint32_t& aa, const uint32_t& bb, uint32_t& result,
		    const unsigned int& nmax = 1e6); //[a, b]
		
		void getState(std::vector<uint64_t>& ovec_seed) const;
		void setState(const std::vector<uint64_t>& ivec_seed);

		RNG_DISTR getDistribution(std::vector<double>& vec_params) const;
		void setDistribution(const RNG_DISTR& distribution, const std::vector<double>& vec_params =
		    std::vector<double>()); //construct empty vector as default
		double getDistributionParameter(const std::string& param_name) const;
		void setDistributionParameter(const std::string& param_name, const double& param_val);
		
		bool getHardwareSeedSuccess() const;
		bool getUniqueSeed(uint64_t& store) const; //allow for outside calls to the seeding function
		std::string toString();

		static RNG_TYPE strToRngtype(const std::string& str); //get an RNG_TYPE from a string shorthand
		static RNG_DISTR strToRngdistr(const std::string& str); //get an RNG_DISTR from a string shorthand

	private:
		RngCore* rng_core; //generator algorithm
		RNG_TYPE rng_type; //for output only so far
		RNG_DISTR rng_distribution;
		std::vector<double> DistributionParameters; //anything needed by the distributions can be stored here
		
		bool rng_muller_generate; //bookkeeping Box-Muller transform
		double rng_muller_z1; //cache
		//(tradeoff between readability with the vector and speed with globals)
		
		double (RandomNumberGenerator::*doubFunc)(); //double random numbers algorithm for distribution
		double (RandomNumberGenerator::*pdfFunc)(const double& xx) const; //probability density function
		double (RandomNumberGenerator::*cdfFunc)(const double& xx) const; //cumulative distribution function
		
//CUSTOM_DIST step 2/7: Add your distribution function, its pdf and cdf here, matching exactly this type:
		double doubUniform();
		double pdfUniform(const double& xx) const;
		double cdfUniform(const double& xx) const;
		double doubGauss(); //=normal
		double pdfGauss(const double& xx) const;
		double cdfGauss(const double& xx) const;
		double doubGamma();
		double doubChiSquare();
		double doubStudentT();
		double doubBeta();
		double doubF();

		double pdfNotImplemented(const double& xx) const;
		double cdfNotImplemented(const double& xx) const;

		double doubGaussKernel(const double& mean, const double& sigma); //internal calls with specific params
		double doubGammaKernel(const double& alpha, const double& beta);
		double doubBetaKernel(const double& alpha, const double& beta);
};

class RngXor : public RngCore { //combined generator with xor, shift and multiply
	public: //new generators must provide these
		RngXor();
		uint64_t int64();
		uint32_t int32();
		void getState(std::vector<uint64_t>& ovec_seed) const;
		void setState(const std::vector<uint64_t>& ivec_seed);

	private:
		uint64_t state;
		uint64_t uu, vv, ww;

		bool initAllStates();
};

class RngPcg : public RngCore { //Permuted linear congruential generator
	public:
		RngPcg();
		uint64_t int64();
		uint32_t int32( );
		void getState(std::vector<uint64_t>& ovec_seed) const;
		void setState(const std::vector<uint64_t>& ivec_seed);

	private:
		uint64_t state;
		uint64_t inc;

		bool initAllStates();
};

class RngMtw : public RngCore { //Mersenne Twister
	public:
		RngMtw();
		uint64_t int64();
		uint32_t int32( );
		void getState(std::vector<uint64_t>& ovec_seed) const;
		void setState(const std::vector<uint64_t>& ivec_seed);

	private:
		const unsigned int MT_NN; //number of states
		const unsigned int MT_MM; //middle word / offset
		unsigned int current_mt_index;
		
		std::vector<uint32_t> vec_states;

		bool initAllStates();
};

class RngFactory { //factory for the generator algorithm
	public: //(so that memory dedicated to the states lives only as long as the RNG)
		static RngCore* getCore(const RandomNumberGenerator::RNG_TYPE& algorithm);
};

} //namespace

#endif

