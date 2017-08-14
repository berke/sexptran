cmake_minimum_required(VERSION 3.5)
project(sexptran)
enable_language(Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008 -fcheck=all -Wall -g -Wimplicit-interface \
                         -Waliasing -Wconversion -Wintrinsic-shadow -march=native -fPIC")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08 -free -implicitnone -check bounds -g")
endif()

add_executable(test_sexptran test_sexptran.f90 sexptran.f90)
add_executable(simplest_example simplest_example.f90 sexptran.f90)
