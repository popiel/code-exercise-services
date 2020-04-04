# Swiftly Services Coding Exercise, implemented by Alex Popiel

Travis CI status: [![Build Status](https://travis-ci.com/popiel/code-exercise-services.svg?branch=master)](https://travis-ci.com/popiel/code-exercise-services)

## Overview

This is an implementation in Scala of the ProductRecord coding exercise from Swiftly, completed as part of their inteview process.  The original repository that it is based upon is at https://github.com/Swiftly-Systems/code-exercise-services.

Of particular note, this exercise includes structures representing currencies; following standard practice, all math with currencies are done as INTEGERS to avoid round-off errors due to decimal/binary conversions of fractions.  I contemplated using the Scala standard library BigDecimal type for them instead, but that type is designed for fixed precision, not fixed scale, and is thus a bit of a nuisance to use correctly for currencies.

This also includes some parsing/deserialization.  Normally, I would have more error checking in it, but the exercise spec specifically disclaimed the need for strong error checking in this initial version.

## Running the code

This code can be run against any input file from within a clone of the repository via sbt, e.g.:

    sbt "run input-sample.txt"

This will read the input file and print the resulting ProductRecords, which should be sufficient to see that it is actually building the data structures correctly.

Unit tests can be run with:

    sbt test

## Continuous Integration

Unfortunately, AppVeyor doesn't seem to support Scala, so I used Travis CI instead: https://travis-ci.com/github/popiel/code-exercise-services

[![Build Status](https://travis-ci.com/popiel/code-exercise-services.svg?branch=master)](https://travis-ci.com/popiel/code-exercise-services)
