# aoc

A programming language inspired by and designed for solving Advent of Code
puzzles.

## Why?

At the time of writing this, Advent of Code has had 5 events, for a total of a
little less than 250 problems. These problems, in my observation, span a
plethora of "domains" many of which are well known or reside in the sphere of
classical computer science problems (e.g. cellular automaton, graphs, etc...).

I have found Advent of Code to be a great learning tool, and I love writing toy
programming languages so I figured, "Why not try tackle some of these domains?"

So, this is really just a toy and shouldn't be taken too seriously, however, to
me at least, it's more than just a toy. It's an opportunity to explore a few
different problem domains, in terms of language design and the challenges
therein.

Advent of Code problems can certainly (and in fact _are_) solved by general
purpose programming languages. They could also be solved by a language in which
one could enter something like the following and get the desired answer

```
solve year 2019, day 22 where my input is at ./input.txt
```

however, that would be quite boring and really just be a trivial DSL for
invoking a more complicated, problem-specific solution. This language aims to be
somewhere in between the two aforementioned approaches.

## Domains This Language Can "Solve"

See the [examples](./examples) directory for amalgamation of working solutions
and examples of what I imagine other domains to look like.

To figure out what actually works, see the [tests](./test) as they actually
solve real AoC problems for my inputs.

For some shallow meaning of "solve"...

### Lists

List domain solutions begin with `list`.

AoC problems that come earlier in the year can be modeled as simple list
processing problems.

Since list processing is a fairly rudimentary form of processing, I suspect this
domain to make appearances in other domains.

### Conway's Game of Life (Cellular Automaton)

Cellular automaton domain solutions begin with `conway`.

Many AoC problems can be modeled in this way, though AoC usually throws in cool
twists that I imagine a lot of folks haven't seen before (at least, I certainly
hadn't).

### Turtles

This is a work in progress...

### Assembly Code

This is a work in progress...

### Graph

This is a work in progress...

### Maze

This is a work in progress...

This may not even be distinct from the `Graph` domain.
