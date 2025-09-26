/***********************************************************************************/
/*  Copyright 2011 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef GRAPHICS_H
#define GRAPHICS_H

#include <meteoio/dataClasses/Array2D.h>

#include <string>
#include <vector>

namespace mio {

/**
 * @class Legend
 * @brief This creates a legend as pixels in a Grid2DObject.
 * This should be used with/by a plugin that would then convert this Grid2DObject into a true
 * graphic file (png, etc).
 *
 * @ingroup graphics
 * @author Mathias Bavay
 * @date   2011-12-23
 */
class Legend {
	public:
		/**
		* @brief Constructor.
		* @param height available height of the plot (for centering the legend)
		* @param minimum start value of the legend
		* @param maximum end value of the legend
		*/
		Legend(const unsigned int &height, const double &minimum, const double &maximum);

		/**
		* @brief Get the actual width of the legend
		* This is constant but depends on various parameters of the legend: font size, number of characters, spacing etc.
		* @return width of the legend
		*/
		static unsigned int getLegendWidth();

		/**
		* @brief Get the legend in an array
		* The legend is coded as values between min and max (+background and text colors) in an array. This array can then be used
		* alongside the data array to build the full plot
		* @return legend array
		*/
		const Array2D<double> getLegend() const;

		static const double bg_color; ///<marker for solid background
		static const double text_color; ///<marker for solid text

	private:
		Array2D<double> grid;
		void simpleLegend(const unsigned int &height, const double &minimum, const double &maximum);
		void smartLegend(const unsigned int &height, const double &minimum, const double &maximum);
		void drawLegend(const unsigned int &height, const double &minimum, const double &maximum);
		void writeLine(const double& val, const unsigned int& px_row);
		void writeChar(const unsigned int i_char[10][6], const unsigned int& px_col, const unsigned int& px_row);

		static const unsigned int char_width;
		static const unsigned int char_height;

		static const unsigned int text_chars_nb;
		static const unsigned int char_space;
		static const unsigned int text_width;
		static const unsigned int sample_width;
		static const unsigned int sample_text_space;
		static const unsigned int legend_plot_space;
		static const unsigned int total_width;

		static const unsigned int interline;
		static const unsigned int label_height;
		static const unsigned int nb_labels;
		static const unsigned int total_height;

		static const unsigned int font_0[10][6], font_1[10][6], font_2[10][6], font_3[10][6], font_4[10][6];
		static const unsigned int font_5[10][6], font_6[10][6], font_7[10][6], font_8[10][6], font_9[10][6];
		static const unsigned int font_plus[10][6], font_minus[10][6], font_dot[10][6], font_E[10][6];
};

namespace Color {
	/**
	* @brief convert RGB to HSV.
	* This converts Red-Green-Blue values to Hue-Saturation-Value.
	* See https://secure.wikimedia.org/wikipedia/en/wiki/HSL_and_HSV
	* or http://www.cs.rit.edu/~ncs/color/t_convert.html
	* @param r red (between 0 and 1)
	* @param g green (between 0 and 1)
	* @param b blue (between 0 and 1)
	* @param h hue (between 0 and 360)
	* @param s saturation (between 0 and 1)
	* @param v value (between 0 and 1)
	* @ingroup graphics
	*/
	void RGBtoHSV(const double& r, const double& g, const double& b, double &h, double &s, double &v);

	/**
	* @brief convert HSV to RGB.
	* This converts Hue-Saturation-Value to Red-Green-Blue values.
	* See https://secure.wikimedia.org/wikipedia/en/wiki/HSL_and_HSV
	* or http://www.cs.rit.edu/~ncs/color/t_convert.html
	* @param h hue (between 0 and 360)
	* @param s saturation (between 0 and 1)
	* @param v value (between 0 and 1)
	* @param r red (between 0 and 1)
	* @param g green (between 0 and 1)
	* @param b blue (between 0 and 1)
	* @ingroup graphics
	*/
	void HSVtoRGB(const double& h, const double& s, const double& v, double &r, double &g, double &b);
}

/////////////////////////////////////////////////////////////////////////////////////////////////
// Gradient class
/////////////////////////////////////////////////////////////////////////////////////////////////
//This class is the base class for the various gradients.
//DO NOT USE pure white in a gradient, since this might be interpreted as a transparent pixel!
//Gradients scale between 0 and 1, but might receive some out of range values for special effects (below sea level, above snow line, etc)
class Gradient_model {
	public:
		Gradient_model() : X(), v_h(), v_s(), v_v() {} //do not use this constructor!
		virtual ~Gradient_model() {}
		Gradient_model(const double& i_min, const double& i_max, const bool& i_autoscale) : X(), v_h(), v_s(), v_v() { (void)i_min; (void)i_max; (void)i_autoscale;}
		//setBgColor()
		//setFgColor()

		//val MUST be between 0 and 1
		virtual void getColor(const double &val, double &r, double &g, double &b) const;
	protected:
		double getInterpol(const double& val, const std::vector<double>& i_X, const std::vector<double>& i_Y) const;
		void HSV2RGB(const double& h, const double& s, const double& v, unsigned char &r, unsigned char &g, unsigned char &b) const;

		std::vector<double> X, v_h, v_s, v_v; ///<control points: vector of X and associated hues, saturations and values. They must be in X ascending order
};

/**
 * @class Gradient
 * @brief This converts numeric values into rgb values.
 * The object is initialized with the range that the gradient should cover and the gradient type. Then each numeric value
 * that is given will be converted into rgb values from the selected gradient. Data out of range are converted to either
 * the minimum or the maximum of the gradient. Special pixels should return a=true to indicate transparency (however, pure
 * white is the transparency color, so do not use it in your gradients!).
 *
 * Some special pixels are recognized:
 * - IOUtils::nodata returns a transparent pixel
 * - legend::bg_color returns an almost white pixel
 * - legend::text_color returns a black pixel
 *
 * The autoscale is handled both by the object and its caller: if "autoscale==true", the gradient might adjust its control points, for
 * example removing sea and snow lines in the terrain gradient. The min and max values are used to scale the gradient: all values less than min
 * will receive the start color while all values greater than max will receive the end color. Therefore true autoscale is acheived by:
 * - setting min/max to the data min/max
 * - passing i_autoscale=true so the gradient might receive some specific adjustments
 *
 * On the other hand, fixed scale is acheived by:
 * - setting min/max to fixed values (so the gradient will be rescaled between these fixed boundaries)
 * - passing i_autoscale=false so the gradient might be able to set so fix points (like sea and snow line)
 *
 * For some interesting discussion on how to define color gradients, see Bernice E. Rogowitz, Lloyd A. Treinish
 * "Why Should Engineers and Scientists Be Worried About Color?"
 * http://www.research.ibm.com/people/l/lloydt/color/color.HTM
 * @ingroup graphics
 * @author Mathias Bavay
 * @date   2012-01-06
 */
class Gradient {
	public:
		/// This enum provides names for possible color gradients
		typedef enum TYPE {
		            none, ///< no type selected
		            terrain, ///< suitable for DEM. if autoscale, then sea and snow line are turned off
		            slope, ///< suitable to represent slope
		            azi, ///< suitable to represent slope azimuth. In autoscale, it becomes a two color gradient
		            heat, ///< the traditional heat gradient (with all its associated shortcomings)
		            freeze, ///< two, two-color gradients with a sharp transition at 0
		            blue_pink, ///< blue to pink gradient, isomorphic gradient
		            pastel, ///< same color scale as "slope" but linear
		            bg_isomorphic, ///< drak-blue to light-green isomorphic gradient
		            bluewhitered, ///< blue to white, then white to red
		            whitetoblk, ///< white to black gradient
		            blktowhite, ///< black to white gradient
		            blue ///< white to slightly violet gradient. This is similar to the one used for the SLF avalanche bulletin
		} Type;

		/**
		* @brief Default Constructor.
		* This should be followed by a call to set() before calling getColor
		*/
		Gradient() : min(0.), max(0.), delta(0.), type(none), model(NULL), nr_unique_cols(0), autoscale(true) {}

		/**
		* @brief Constructor.
		* The object will associate to each numeric value RGB values. See class description for more...
		* @param i_type set the color gradient to use, from the enum Gradient::Type
		* @param min_val start value of the gradient
		* @param max_val end value of the gradient
		* @param i_autoscale do autoscale for setting the colors?
		*/
		Gradient(const Type& i_type, const double& min_val, const double &max_val, const bool& i_autoscale);

		Gradient(const Gradient& c);

		~Gradient() {delete model;}

		/**
		* @brief Setter
		* See class description for more...
		* @param i_type set the color gradient to use, from the enum Gradient::Type
		* @param min_val start value of the gradient
		* @param max_val end value of the gradient
		* @param i_autoscale do autoscale for setting the colors?
		*/
		void set(const Type& i_type, const double& min_val, const double &max_val, const bool& i_autoscale);
		//setBgColor()
		//setFgColor()

		/**
		* @brief Set a reduced number of levels for the gradient
		* The given argument is an upper bound for the number of unique levels in the generated
		* gradient (leading to a reduced number of colors). This is a specially easy and useful way of reducing a file size with
		* no run time overhead (and even a small benefit) and little visible impact if
		* the number of levels/colors remains large enough (say, at least 20-30). This is only applicable to indexed images, that is
		* when getPalette is called an pixels are set using getColor(const double& val, unsigned char& index).
		* @param i_nr_unique_levels maximum number of unique levels
		*/
		void setNrOfLevels(const unsigned char& i_nr_unique_levels);

		/**
		* @brief Get RGB values for a given numeric value
		* See class description for more explanations on the implementation/behavior
		* @param val numerical value to convert
		* @param r red (between 0 and 255)
		* @param g green (between 0 and 255)
		* @param b blue (between 0 and 255)
		* @param a transparent pixel?
		*/
		void getColor(const double &val, unsigned char &r, unsigned char &g, unsigned char &b, bool &a) const;

		/**
		* @brief Get palette index values for a given numeric value
		* See class description for more explanations on the implementation/behavior
		* @param val numerical value to convert
		* @param index palette index for the given value
		*/
		void getColor(const double& val, unsigned char& index) const;

		/**
		* @brief Get palette colors for the selected gradient
		* When building an indexed image, one needs to first retrieve the palette using this method. Afterwards, getColor(val, index)
		* will be called for each pixel in order to retrieve its palette index. The returned colors are interlaced (rgb).
		* @param palette interlaced colors
		* @param nr_colors number of colors in the palette
		*/
		void getPalette(std::vector<unsigned char> &palette, size_t &nr_colors) const;

		Gradient& operator=(const Gradient& source);

		static const unsigned char channel_max_color; ///< nr of colors per channel of the generated gradients
	private:
		void setModel(const Type& i_type);

		double min, max, delta;
		Type type;
		Gradient_model *model;
		unsigned char nr_unique_cols; ///< number of unique colors to generate for indexed images
		static const unsigned char reserved_idx; ///< for indexed gradients, number of reserved indexes
		static const unsigned char reserved_cols; ///< for non-indexed gradients, number of reserved colors
		bool autoscale;
};

class gr_heat : public Gradient_model {
	public:
		gr_heat(const double& i_min, const double& i_max, const bool& i_autoscale) {(void)i_min; (void)i_max; (void)i_autoscale;}
		void getColor(const double &i_val, double &r, double &g, double &b) const;
};

class gr_blue_pink : public Gradient_model {
	public:
		gr_blue_pink(const double& i_min, const double& i_max, const bool& i_autoscale);
};

class gr_freeze : public Gradient_model {
	public:
		gr_freeze(const double& i_min, const double& i_max, const bool& i_autoscale);
		void getColor(const double &val, double &r, double &g, double &b) const;
	private:
		//This gradient is interpolated in RGB color space
		std::vector<double> v_r, v_g, v_b; ///<control points: vector of X and associated r,g,b. They must be in X ascending order
};

class gr_bluewhitered : public Gradient_model {
	public:
		gr_bluewhitered(const double& i_min, const double& i_max, const bool& i_autoscale);
};

class gr_whitetoblk : public Gradient_model {
	public:
		gr_whitetoblk(const double& i_min, const double& i_max, const bool& i_autoscale);
};

class gr_blktowhite : public Gradient_model {
	public:
		gr_blktowhite(const double& i_min, const double& i_max, const bool& i_autoscale);
};

class gr_blue : public Gradient_model {
	public:
		gr_blue(const double& i_min, const double& i_max, const bool& i_autoscale);
};

class gr_bg_isomorphic : public Gradient_model {
	public:
		gr_bg_isomorphic(const double& i_min, const double& i_max, const bool& i_autoscale);
};

class gr_terrain : public Gradient_model {
	public:
		gr_terrain(const double& i_min, const double& i_max, const bool& i_autoscale);
};

class gr_slope : public Gradient_model {
	public:
		gr_slope(const double& i_min, const double& i_max, const bool& i_autoscale);
};

class gr_azi : public Gradient_model {
	public:
		gr_azi(const double& i_min, const double& i_max, const bool& i_autoscale);
};

class gr_pastel : public Gradient_model {
	public:
		gr_pastel(const double& i_min, const double& i_max, const bool& i_autoscale);
};

} //namespace
#endif
