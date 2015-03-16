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

The input XML is a fragment of POM, e.g.

``` xml
<project>
  <dependencies>
    <dependency>
      <groupId>org.scala-lang</groupId>
      <artifactId>scala-library</artifactId>
      <version>[2.11.3, 2.11.6)</version>
    </dependency>
    <dependency>
      <groupId>com.typesafe</groupId>
      <artifactId>config</artifactId>
      <version>(1.1.1, 1.2.1]</version>
    </dependency>
  </dependencies>
</project>
```

## Test

`sbt test`
