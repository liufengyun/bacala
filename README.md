# Bacala

Bacala is an experimental package dependency manager for Scala projects.

The ultimate goal is to replace *Ivy* in Sbt with *Bacala*, when the latter matures and performs better.

## Project Objective

The objective of this project is:

- Use SAT solvers as the core algorithm
- Improve efficiency of network IO through reactive programming
- Provide well-defined API and command line interface

## Run

`sbt "run data/github-api.xml"`

The input XML is a POM file.

## Test

`sbt test`

## Reference

- [Maven: The Complete Reference](http://books.sonatype.com/mvnref-book/reference/index.html)