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

## To do list

- [ ] support POM configuration file
  - [x] support different version formats(including unstandard)
  - [x] support version ranges
  - [x] support scopes in POM file
  - [x] support path properties and variable properties in version constraint
  - [x] support reading version constraint from parent POM file
  - [x] inherit dependencies from parent POM file
  - [x] support aggregating dependencies from multiple mudules
  - [ ] support excludes in POM
  - [ ] support SNAPSHOT versioning
- [ ] resolve dependency via SAT solver
- [ ] support multiple resolvers
- [ ] support Ivy configuration file
  - [ ] support force a specific package
  - [ ] support configuration
  - [ ] support custom conflict management
- [ ] support download resources via Futures in parallel
- [ ] resolve result visualization in html
  - [ ] generate dependency graph
  - [ ] generate logs

## Reference

- [Maven: The Complete Reference](http://books.sonatype.com/mvnref-book/reference/index.html)
