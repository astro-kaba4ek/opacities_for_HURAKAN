cmake_minimum_required(VERSION 3.6)
project(project_opacity)
enable_language(Fortran)

# find_package(LAPACK)

set(CMAKE_Fortran_COMPILER "/usr/bin/gfortran-13")
set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast -flto")
set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -fsanitize=address -fbounds-check")# -fmax-stack-var-size=65536")

set(CMAKE_Fortran_MODULE_DIRECTORY "./modfiles")

# add_library(regime src/regime.f95)
# if(USE_DOUBLE_PRECISION)
#     message("utilize double precision")
#     target_compile_definitions(regime PUBLIC -DUSE_DOUBLE_PRECISION) 
# endif(USE_DOUBLE_PRECISION)

set(SOURCES
	src/regime.f95
	src/constants.f95
	src/utils.f95

	src/opacity_mod.f95
)
add_executable(main src/main.f95 ${SOURCES})
# target_link_libraries(main LAPACK::LAPACK)
# target_link_libraries(main regime LAPACK::LAPACK)