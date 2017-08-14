# SEXPTRAN

A Fortran 2008 library for reading, manipulating and writing
S-expressions.

* Represents S-expressions as a tree structure
* Handles comments, quoted strings and escaped characters
* Compatible with the OCaml Sexplib library
* Written in modern Fortran style
* Overloaded functions for concise construction of
  S-expressions (within Fortran bounds)
* Conversion functions for logical, integer, allocatable character,
  singlea and double precision reals, integer and real arrays
* Accessor function for record names
* Reference-counted cells prevent memory leaks
* Self-contained with no dependencies
* Detailed syntax error reporting

## Usage

See test\_sexptran.f90 and simplest\_example.f90 for two usage examples.

## Requirements

A good Fortran compiler and CMake.

## Compilation

	mkdir build
	cd build
	cmake ..
	make

For gfortran:

	cmake .. -DCMAKE_Fortran_COMPILER=/usr/bin/gfotran-7

NOTE: gfortran 6 produces spurious warnings such as:

	Warning: Only array FINAL procedures declared for derived type ‘out_channel’ defined at (1), suggest also scalar one [-Wsurprising]

For Intel (untested, my trial license expired):

	cmake .. -DCMAKE_Fortran_COMPILER=/opt/intel/bin/ifort

For PGI:

	cmake .. -DCMAKE_Fortran_COMPILER=/opt/pgi/linux86-64/2017/bin/pgf90

NOTE: SEXPTRAN doesn't work with PGI compiler 17.4.  The "select type" construct seems to be broken, the dynamic type of objects is lost.

## Limitations

* Multi-line comments not implemented yet
* No cycle detection
* Output to formatted units not supported

## License

Placed under the MIT license.

## Author

Oğuz Berke Antoine DURAK <berke.durak@gmail.com>
