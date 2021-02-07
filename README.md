# aoc

A programming language inspired by and designed for solving [Advent of Code](https://adventofcode.com/)
puzzles.

## Why?

At the time of writing this, Advent of Code has had [6 events][events], for a
total of a little less than 250 problems. These problems, in my observation,
span a plethora of "domains" many of which are well known or reside in the
sphere of classical computer science problems (e.g. cellular automaton, graphs,
etc...).

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

See the [EXAMPLES.md](./EXAMPLES.md) for working solutions.

See the [examples](./examples) directory for future designs.

To figure out what actually works, see the [tests](./test) as they actually
solve real AoC problems for my inputs.

For some shallow meaning of "solve"...

### Lists

List domain solutions begin with `list`.

AoC problems that come earlier in the year can be modeled as simple list
processing problems.

Since list processing is a fairly rudimentary form of processing, I suspect this
domain to make appearances in other domains.

### Conway's Game of Life ([Cellular Automaton](https://en.wikipedia.org/wiki/Cellular_automaton))

Cellular automaton domain solutions begin with `conway`.

Many AoC problems can be modeled in this way, though AoC usually throws in cool
twists that I imagine a lot of folks haven't seen before (at least, I certainly
hadn't).

### Turtles

Turtle domain solutions begin with `turtle`.

You know, like in the [logo programming language](https://en.wikipedia.org/wiki/Logo_%28programming_language%29).

### Assembly Code

Assembly-code-esque domain solutions begin with `program`.

There are a bunch of these. I probably won't be tackling the optimization
problems but that would certainly be a challenge.

### Graph

Graph domain solutions begin with `graph`.

My hunch is that these problems will be a lot of parsing details followed by
a short algorithm plug. Fun!

### Maze

This is a work in progress...

This may not even be distinct from the `Graph` domain.

## TODO

 - Cross-cutting
   - [ ] Runtime errors
   - [ ] Error formatting
   - [ ] Clean up *Problem.hs files
   - [ ] Comments
   - [ ] Application parsing is wonky (and it should be `(value value)` not `(text value)`)
   - [ ] Doc-gen of builtins.
   - [ ] Too many parens necessary in places.
   - [X] Github actions
   - [ ] Hlint
   - [ ] "One free" in solutions should only check when list is the type
   - [ ] Better organization post-remedial-program
   - [ ] Syntactic sugar.
   - [ ] Properly type check `&` composition
   - [ ] Add appropriate error-on-warn settings
   - [ ] Syntax highlighting (emacs)
   - [ ] Pretty printing (currently we just do AST dumps)
 - `conway`
   - [ ] Transitions should have context distinct from `grid`
   - [ ] Allow `any_state` in transitions "from"
   - [ ] `Grid` should store what generation it is 
   - [ ] Optimize (2015, d18 is kinda slow)
   - [ ] Lumberyard part 2
 - `program`
   - [ ] Check no repeated names in instruction specs.
   - [ ] Case (exhaustive/overlap) checking in instructions
   - [ ] Context in later instruction parts (e.g. jump should reference earlier `num`)
   - [ ] Backend: C
   - [ ] Backend: Go
   - [ ] Backend: Haskell
   - [ ] Think more about `Show Program`.
   - [ ] Are types in instruction spec really necessary?
 - `turtle`
   - [ ] Dimensions probably don't matter.


## Problem Notes

### 2015, 3 part 2

`turtle`, conditional walkers, perhaps multiple solvers joining in the end

### 2015, 4

MD5, skipping for a while

### 2015, 5

List filtering, tricky context-sensitive validation (not really "tricky" more unnatural)

### 2015, 6

Grid mass manipulation

 [events]: https://adventofcode.com/2020/events
