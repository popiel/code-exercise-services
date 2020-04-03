# Swiftly Services Coding Exercise, implemented by Alex Popiel


## Overview

This is an implementation of the ProductRecord coding exercise from Swiftly, completed as part of their inteview process.  The original repository that it is based upon is at https://github.com/Swiftly-Systems/code-exercise-services.

Of particular note, this exercise includes structures representing currencies; following standard practice, all math with currencies are done as INTEGERS to avoid round-off errors due to decimal/binary conversions of fractions.  I contemplated using the Scala standard library BigDecimal type for them instead, but that type is designed for fixed precision, not fixed scale, and is thus a bit of a nuisance to use correctly for currencies.

This also includes some parsing/deserialization.  Normally, I would use a parser-generator for describing the format, but that was specifically forbidden in this exercise, so I do it manually with various string splitting and type conversion primitives.  This is certainly harder to extend in future, but the exercise spec also assert that the format will never change.  (Famous last words...)

When parsing, I _always_ include some error checking... even when told I don't need to.  While building this code, said error checking found a case where I had mistyped one of the format offsets (easy enough to do with a manually created parser like this), so the effort of putting in the error checking paid off with significantly reduced bug-hunting time.

## Running the code

This code can be run against any input file from within a clone of the repository via sbt, e.g.:

"""sbt "run input-sample.txt" """

This will read the input file and print the resulting ProductRecords (using the toString provided automatically by case classes, so not really machine-readable due to lack of string quoting).  However, that should be sufficient to see that it is actually building the data structures correctly.

Unit tests can be run with:

"""sbt test"""

## Continuous Integration

CI is at ... WIP
