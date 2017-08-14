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

## Compilation

  mkdir build
  cd build
  cmake ..
  make

## Limitations

* Multi-line comments not implemented yet
* No cycle detection
* Output to formatted units not supported

## License

Placed under the MIT license.

## Author

Oğuz Berke Antoine DURAK <berke.durak@gmail.com>
