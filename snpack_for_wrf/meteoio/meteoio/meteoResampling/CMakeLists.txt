INCLUDE_DIRECTORIES("${PROJECT_SOURCE_DIR}/")

SET(meteoResampling_sources
	meteoResampling/ResamplingAlgorithms.cc
	meteoResampling/Accumulate.cc
	meteoResampling/DailyAverageResampling.cc
	meteoResampling/DailySolarResampling.cc
	meteoResampling/LinearResampling.cc
	meteoResampling/NearestNeighbour.cc
	meteoResampling/NoResampling.cc
	meteoResampling/SolarResampling.cc
)
