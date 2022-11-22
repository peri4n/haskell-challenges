# Haskell Solutions to Project Euler

If you don't know what Project Euler is, you can have a look at [projecteuler.net](https://projecteuler.net).
It's a coding challenge mostly containing algorithmic and number theory problems.

This repository contains all my attempts to solve the problems in the [Haskell programming language](https://www.haskell.org).

## Running a Problem

For each problem there is an executable that prints the solution to your terminal.
To run the solution to problem 1 run:

```
stack run -- 1
```

To run the solution to problem 2 run:

```
stack run -- 2
```

I hope you get the idea.

## Profiling a problem

To generate a profiling file run:

```
stack exec --profile -- euler-exe 14 +RTS -p
```

You can analyze the generated `euler-exe.prof` with the usual tools.
