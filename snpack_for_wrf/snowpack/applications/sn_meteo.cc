#include <iostream>
#include <meteoio/MeteoIO.h>

using namespace mio; //The MeteoIO namespace is called mio

int main(int argc, char** argv) {
	std::ofstream fout;
	Date d1, d2;
	std::vector< std::vector<MeteoData> > vecMeteo;

	//check number of arguments
	if(argc!=3) {
		std::cout << argv[0] << " : write meteo data according to a given io.ini file.\nusage:\n";
		std::cout << "give begining and end date in ISO format as arguments 1 and 2. Example:\n";
		std::cout << argv[0] << " 2010-10-01T00:00:00 2010-12-01T00:00:00\n";
		exit(0);
	}

	//we assume that the time given on the command line is in TZ=+1
	d1.setTimeZone(1.);
	IOUtils::convertString(d1,argv[1]);
	IOUtils::convertString(d2,argv[2]);
	const double Tsteps = 1./(24.); //sampling rate = 1/24 day = 1 hour

	Config cfg("io.ini");
	IOHandler raw_io(cfg);
	BufferedIOHandler io(raw_io, cfg);
	io.setBufferDuration(Date(2.0), Date(370.0));

	std::cout << "Reading input data" << std::endl;

	//More elaborate conversion: sample the data to a specific rate
	//by looping over the time and calling readMeteoData for each timestep
	std::vector<MeteoData> Meteo; //we need some intermediate storage, for storing data sets for 1 timestep
	io.readMeteoData(d1, Meteo); //we need to know how many stations will be available
	vecMeteo.insert(vecMeteo.begin(), Meteo.size(), std::vector<MeteoData>()); //allocation for the vectors
	for(; d1<=d2; d1+=Tsteps) { //time loop
		io.readMeteoData(d1, Meteo); //read 1 timestep at once, forcing resampling to the timestep
		for(unsigned int ii=0; ii<Meteo.size(); ii++) {
			vecMeteo.at(ii).push_back(Meteo[ii]); //fill the data manually into the vector of vectors
		}
	}

	//In both case, we write the data out
	std::cout << "Writing output data" << std::endl;
	io.writeMeteoData(vecMeteo);

	std::cout << "Done!!" << std::endl;
	return 0;
}
