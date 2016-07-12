# mscript

[![CircleCI](https://circleci.com/gh/chrisrink10/mscript/tree/master.svg?style=svg)](https://circleci.com/gh/chrisrink10/mscript/tree/master)

mscript is an interpreted programming language taking inspiration from several
other programming languages such as Go, JavaScript, Python, and MUMPS/M (which
is the source of the M in the name mscript).

Like M and unlike almost any other extant programming language in the
modern arsenal, mscript permits programmers to persist values to the builtin
key-value store database using the same simple syntax used to manipulate 
in-memory variables. This flexible and unique system allows programmers 
unprecedented control over their data structures.

## Getting Started
To install mscript, you can use the following commands (on *NIX platforms):
 
    cd ~/<mscript directory>
    cmake .
    make
    ./bin/mscript
    
Windows users can download [CMake](http://www.cmake.org/download/) directly
and use the GUI to create a Visual Studio project which can be used to
compile the project.

## License
Apache License 2.0