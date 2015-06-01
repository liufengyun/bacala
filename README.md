# Bacala

Bacala is an experimental package dependency manager for Scala projects.

## Project Objective

The objective of this project is:

- Use SAT solvers as the core algorithm
- Improve efficiency of network IO through reactive programming
- Provide well-defined API and command line interface

## Run

To resolve a POM file:

    sbt "run data/pom/github-api.xml"

To resolve an Ivy file

    sbt "run [<setting xml>] <ivy xml> [config]"

## Test

`sbt test`

## To do list

- [ ] support POM configuration file
  - [x] support different version formats(including unstandard)
  - [x] support version ranges
  - [x] support scopes in POM file
  - [x] support path properties and variable properties in version constraint
  - [x] support reading version constraint from parent POM file
  - [x] inherit dependencies from parent POM file
  - [x] support aggregating dependencies from multiple mudules
  - [x] support chaining multiple repositories
  - [x] support excludes in POM
  - [ ] support SNAPSHOT versioning
- [x] resolve dependency via SAT solver
  - [x] find an assignment or answer impossible
  - [x] find the minimal conflict set
  - [x] find the optimal solution according to weighted value
- [ ] support Ivy configuration file
  - [x] support configuration
  - [x] support excludes
  - [x] support artifacts
  - [x] support transitive
  - [x] support dynamic version constraints
  - [ ] support force
  - [ ] support conflicts
  - [ ] support overrides
- [ ] support download resources via Futures in parallel

## Reference

- [Maven: The Complete Reference](http://books.sonatype.com/mvnref-book/reference/index.html)
