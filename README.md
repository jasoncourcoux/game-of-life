# Game of Life

Simple example following an [interactive/spec driven development approach](http://blog.cognitect.com/blog/2016/10/5/interactive-development-with-clojurespec),
to solve [the game of life problem.](https://sites.google.com/site/tddproblems/all-problems-1/game-of-life)

I've tried to document my thinking as I go which can be found within the comments. This was a first attempt with spec, so
please be aware that my approach is probably not the best!

## Problem Description
Develop an algorithm that takes "one step" in the game of life. The behaviour examples may simply be the
rules of the game:
 - Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
 - Any live cell with more than three live neighbours dies, as if by overcrowding.
 - Any live cell with two or three live neighbours lives on to the next generation.
 - Any dead cell with exactly three live neighbours becomes a live cell.

## License

Copyright © 2016 Jason Courcoux

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
