INCLUDE_DIRECTORIES("${PROJECT_SOURCE_DIR}/")
LIST(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/tools/cmake/")

SET(core_sources
	snowpackCore/Snowpack.cc
	snowpackCore/Aggregate.cc
	snowpackCore/WaterTransport.cc
	snowpackCore/VapourTransport.cc
	snowpackCore/ReSolver1d.cc
	snowpackCore/Canopy.cc
	snowpackCore/Metamorphism.cc
	snowpackCore/PhaseChange.cc
	snowpackCore/SeaIce.cc
	snowpackCore/Solver.cc
	snowpackCore/SalinityTransport.cc
)
