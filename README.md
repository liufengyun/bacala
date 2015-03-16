# Bacala

Bacala is an experimental package dependency manager for Scala projects.

The ultimate goal is to replace *Ivy* in Sbt with *Bacala*, when the latter matures and performs better.

## Project Objective

The objective of this project is:

- Use SAT solvers as the core algorithm
- Improve efficiency of network IO through reactive programming
- Provide well-defined API and command line interface

## Command Line Interface

The program takes in a dependency specifiction XML file, and outputs the solution to the console as a tree. If there's no solution, it indicates where the insolvable conflict happens.

## Development

### Test

Run `sbt test` to execute the test set.

