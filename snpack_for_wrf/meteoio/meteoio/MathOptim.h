/***********************************************************************************/
/*  Copyright 2012 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef MATHOPTIM_H
#define MATHOPTIM_H

#include <stdint.h>
#include <cmath>
#include <string.h>

//Quake3 fast 1/x² approximation
// For Magic Derivation see: Chris Lomont http://www.lomont.org/Math/Papers/2003/InvSqrt.pdf
// Credited to Greg Walsh.
// 32  Bit float magic number - for 64 bits doubles: 0x5fe6ec85e7de30da
#define SQRT_MAGIC_D 0x5f3759df
#define SQRT_MAGIC_F 0x5f375a86

namespace mio {

namespace Optim {

	/**
	* @brief Optimized version of c++ round()
	* This version works with positive and negative numbers but does not
	* comply with IEEE handling of border cases (like infty, Nan, etc).
	* Please benchmark your code before deciding to use this!!
	* @param x number to round
	* @return rounded number cast as int
	*/
	inline long int round(const double& x) {
		if (x>=0.) return static_cast<long int>( x+.5 );
		else return static_cast<long int>( x-.5 );
	}

	/**
	* @brief Optimized version of c++ floor()
	* This version works with positive and negative numbers but does not
	* comply with IEEE handling of border cases (like infty, Nan, etc).
	* Please benchmark your code before deciding to use this!!
	* @param x number to floor
	* @return floored number cast as int
	*/
	inline long int floor(const double& x) {
		const long int xi = static_cast<long int>(x);
		if (x >= 0 || static_cast<double>(xi) == x) return xi ;
		else return xi - 1 ;
	}

	/**
	* @brief Optimized version of c++ ceil()
	* This version works with positive and negative numbers but does not
	* comply with IEEE handling of border cases (like infty, Nan, etc).
	* Please benchmark your code before deciding to use this!!
	* @param x number to ceil
	* @return ceiled number cast as int
	*/
	inline long int ceil(const double& x) {
		const long int xi = static_cast<long int>(x);
		if (x <= 0 || static_cast<double>(xi) == x) return xi ;
		else return xi + 1 ;
	}

	inline double intPart(const double &x) {
		double intpart;
		modf(x, &intpart);
		return intpart;
	}

	inline double fracPart(const double &x) {
		double intpart;
		return modf(x, &intpart);
	}

	#ifdef _MSC_VER
	#pragma warning( push ) //for Visual C++
	#pragma warning(disable:4244) //Visual C++ rightfully complains... but this behavior is what we want!
	#endif
	//maximum relative error is <1.7% while computation time for sqrt is <1/4. At 0, returns a large number
	//on a large scale interpolation test on TA, max relative error is 1e-6
	inline float invSqrt(const float x) {
		const float xhalf = 0.5f*x;

		union { // get bits for floating value
			float x;
			int i;
		} u;
		u.x = x;
		u.i = SQRT_MAGIC_F - (u.i >> 1);  // gives initial guess y0
		return u.x*(1.5f - xhalf*u.x*u.x);// Newton step, repeating increases accuracy
	}

	#ifdef __clang__
	#pragma clang diagnostic push
	#pragma clang diagnostic ignored "-Wdouble-promotion"
	#endif
	inline double invSqrt(const double x) {
		const double xhalf = 0.5f*x;

		union { // get bits for floating value
			float x;
			int i;
		} u;
		u.x = static_cast<float>(x);
		u.i = SQRT_MAGIC_D - (u.i >> 1);  // gives initial guess y0
		return u.x*(1.5f - xhalf*u.x*u.x);// Newton step, repeating increases accuracy
	}
	#ifdef __clang__
	#pragma clang diagnostic pop
	#endif
	
	#ifdef _MSC_VER
	#pragma warning( pop ) //for Visual C++, restore previous warnings behavior
	#endif

	inline float fastSqrt_Q3(const float x) {
		return x * invSqrt(x);
	}

	inline double fastSqrt_Q3(const double x) {
		return x * invSqrt(x);
	}

	inline double pow2(const double& val) {return (val*val);}
	inline double pow3(const double& val) {return (val*val*val);}
	inline double pow4(const double& val) {return (val*val*val*val);}

	//please do not use this method directly, call fastPow() instead!
	inline double fastPowInternal(double a, double b) {
		//see http://martin.ankerl.com/2012/01/25/optimized-approximative-pow-in-c-and-cpp/
		// calculate approximation with fraction of the exponent
		int e = (int) b;
		union {
			double d;
			int x[2];
		} u = { a };
		u.x[1] = (int)((b - e) * (u.x[1] - 1072632447) + 1072632447);
		u.x[0] = 0;

		// exponentiation by squaring with the exponent's integer part
		// double r = u.d makes everything much slower, not sure why
		double r = 1.0;
		while (e) {
			if (e & 1) {
				r *= a;
			}
			a *= a;
			e >>= 1;
		}

		return r * u.d;
	}

	/**
	* @brief Optimized version of c++ pow()
	* This version works with positive and negative exponents and handles exponents bigger than 1.
	* The relative error remains less than 6% for the benchmarks that we ran (argument between 0 and 500
	* and exponent between -10 and +10). It is ~3.3 times faster than cmath's pow().
	* Source: http://martin.ankerl.com/2012/01/25/optimized-approximative-pow-in-c-and-cpp/
	*
	* Please benchmark your code before deciding to use this!!
	* @param a argument
	* @param b exponent
	* @return a^b
	*/
	inline double fastPow(double a, double b) {
		if (b>0.) {
			return fastPowInternal(a,b);
		} else {
			const double tmp = fastPowInternal(a,-b);
			return 1./tmp;
		}
	}

	#ifdef __clang__
	#pragma clang diagnostic push
	#pragma clang diagnostic ignored "-Wundefined-reinterpret-cast"
	#endif
	//see http://metamerist.com/cbrt/cbrt.htm
	template <int n> inline float nth_rootf(float x) {
		const bool sgn = (x<0.f)? true : false;
		if (sgn) x = -x;
		static const int ebits = 8;
		static const int fbits = 23;

		const int bias = (1 << (ebits-1))-1;
		int& i = reinterpret_cast<int&>(x);
		i = (i - (bias << fbits)) / n + (bias << fbits);

		if (sgn) return -x;
		else return x;
	}

	template <int n> inline double nth_rootd(double x) {
		const bool sgn = (x<0.)? true : false;
		if (sgn) x = -x;
		static const int ebits = 11;
		static const int fbits = 52;

		const int64_t bias = (1 << (ebits-1))-1;
		int64_t& i = reinterpret_cast<int64_t&>(x);
		i = (i - (bias << fbits)) / n + (bias << fbits);

		if (sgn) return -x;
		else return x;
	}
	#ifdef __clang__
	#pragma clang diagnostic pop
	#endif

	/**
	* @brief Optimized version of cubic root
	* This version is based on a single iteration Halley's method (see https://en.wikipedia.org/wiki/Halley%27s_method)
	* with a seed provided by a bit hack approximation. It should offer 15-16 bits precision and be three times
	* faster than pow(x, 1/3). In some test, between -500 and +500, the largest relative error was 1.2e-4.
	* Source:  Otis E. Lancaster, Machine Method for the Extraction of Cube Root Journal of the American Statistical Association, Vol. 37, No. 217. (Mar., 1942), pp. 112-115.
	* and http://metamerist.com/cbrt/cbrt.htm
	*
	* Please benchmark your code before deciding to use this!!
	* @param x argument
	* @return x^(1/3)
	*/
	inline double cbrt(double x) {
		const double a = nth_rootd<3>(x);
		const double a3 = a*a*a;
		const double b = a * ( (a3 + x) + x) / ( a3 + (a3 + x) );
		return b; //single iteration, otherwise set a=b and do it again
	}

	/**
	* @brief Optimized version of 10^x
	* This works for 0 <= x <= 1 and offers a theoritical precision of 5e-5
	* Source: Approximations for digital computers, Cecil Hastings, JR, Princeton University Press, 1955.
	*
	* Tests have shown a maximum error of 7e-5 and an almost 4x speed up.
	* Please benchmark your code before deciding to use this!!
	* @param x argument
	* @return 10^x with x in [0;1]
	*/
	inline double pow10(double x) {
		static const double a1 = 1.1499196;
		static const double a2 = 0.6774323;
		static const double a3 = 0.2080030;
		static const double a4 = 0.1268089;

		const double x2 = x*x;
		const double tmp = 1. + a1*x + a2*x*x + a3*x*x2 + a4*x2*x2;
		return tmp*tmp;
	}

	template <typename T> T fastPow(T p, unsigned q) {
		T r(1);

		while (q != 0) {
			if (q % 2 == 1) {    // q is odd
				r *= p;
				q--;
			}
			p *= p;
			q /= 2;
		}

		return r;
	}

	/**
	* @brief Optimized version of ln(1+x)
	* This works for 0 <= x <= 1 and offers a theoritical precision of 5e-5
	* Source: Approximations for digital computers, Cecil Hastings, JR, Princeton University Press, 1955.
	*
	* Tests have shown a maximum error of 2.5e-3 and a 1.7x speed up.
	* Please benchmark your code before deciding to use this!!
	* @param x argument
	* @return ln(1+x) with x in [0;1]
	*/
	inline double ln_1plusX(double x) {
		static const double a1 = 0.9974442;
		static const double a2 = -.4712839;
		static const double a3 = 0.2256685;
		static const double a4 = -.0587527;

		const double x2 = x*x;
		return a1*x + a2*x2 + a3*x*x2 + a4*x2*x2;
	}
	
	inline unsigned long int powerOfTwo(const unsigned int& n) {
		return (1UL << n);
	}

}

} //end namespace

#endif
