# Portions adapted from the examples at cmake.org and Jacob William's json-fortran

cmake_minimum_required(VERSION 3.5)

set(PROJECT_DESCRIPTION "A Fortran 2008 library for reading, manipulating and writing S-expressions.")
set(PROJECT_URL "https://github.com/berke/sexptran")

project(sexptran)
enable_language(Fortran)
set(VERSION 1.3.2)

set(LIB_NAME ${CMAKE_PROJECT_NAME})

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008 -fcheck=all -Wall -g -Wimplicit-interface \
                         -cpp -Waliasing -Wconversion -Wintrinsic-shadow -march=native -fPIC")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08 -free -implicitnone -check bounds -g")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Mallocatable=03 -C -g")
endif()

add_executable(test_sexptran test_sexptran.f90 sexptran.f90)
add_executable(simplest_example simplest_example.f90 sexptran.f90)
set(LIB_SRCS sexptran.f90)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

set(MODULE_DIR "${CMAKE_BINARY_DIR}/include")
string(TOLOWER ${CMAKE_PROJECT_NAME}-${CMAKE_Fortran_COMPILER_ID} PACKAGE_NAME)
set(PACKAGE_VERSION "${PACKAGE_NAME}-${VERSION}")
set(INSTALL_MOD_DIR "${CMAKE_INSTALL_INCLUDEDIR}")
set(INSTALL_LIB_DIR "${CMAKE_INSTALL_LIBDIR}")
set(ABS_LIB_INSTALL_DIR "\${CMAKE_INSTALL_FULL_LIBDIR}")
set(EXPORT_INSTALL_DIR "${INSTALL_LIB_DIR}/cmake/${PACKAGE_VERSION}")

add_library(${LIB_NAME} SHARED ${LIB_SRCS})
add_library(${LIB_NAME}-static STATIC ${LIB_SRCS})

set_target_properties(${LIB_NAME}
  PROPERTIES
  OUTPUT_NAME ${LIB_NAME}
  PREFIX lib
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR}
  VERSION ${VERSION}
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
  Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})

set_target_properties(${LIB_NAME}-static
  PROPERTIES
  OUTPUT_NAME ${LIB_NAME}
  PREFIX lib
  VERSION ${VERSION}
  ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
  Fortran_MODULE_DIRECTORY ${MODULE_DIR})

install(TARGETS ${LIB_NAME} ${LIB_NAME}-static
  EXPORT ${PACKAGE_NAME}-targets
  LIBRARY DESTINATION "${INSTALL_LIB_DIR}"
  ARCHIVE DESTINATION "${INSTALL_LIB_DIR}")

install(DIRECTORY "${MODULE_DIR}/" DESTINATION  "${INSTALL_MOD_DIR}")
install(EXPORT ${PACKAGE_NAME}-targets DESTINATION "${EXPORT_INSTALL_DIR}")

write_basic_package_version_file(
  "${CMAKE_BINARY_DIR}/${PACKAGE_NAME}-config-version.cmake"
  VERSION ${VERSION}
  COMPATIBILITY SameMajorVersion)

configure_package_config_file(
  "${CMAKE_SOURCE_DIR}/cmake/pkg/${CMAKE_PROJECT_NAME}-config.cmake.in"
  "${CMAKE_BINARY_DIR}/pkg/${PACKAGE_NAME}-config.cmake"
  INSTALL_DESTINATION "${EXPORT_INSTALL_DIR}"
  PATH_VARS EXPORT_INSTALL_DIR INSTALL_MOD_DIR)

install(FILES
  "${CMAKE_BINARY_DIR}/pkg/${PACKAGE_NAME}-config.cmake"
  "${CMAKE_BINARY_DIR}/${PACKAGE_NAME}-config-version.cmake"
  DESTINATION "${EXPORT_INSTALL_DIR}")
