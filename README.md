# game-of-life

I wanted to try out following a sort of spec driven development approach - as presented in the cognitect blog -
http://blog.cognitect.com/blog/2016/10/5/interactive-development-with-clojurespec

A quick google for good TDD problems came up with the following:
https://sites.google.com/site/tddproblems/all-problems-1/game-of-life

Game of life is a so called cellular automaton. You can read up all about it at wikipedia.

Develop an algorithm that takes "one step" in the game of life. The behaviour examples may simply be the
rules of the game:
 - Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
 - Any live cell with more than three live neighbours dies, as if by overcrowding.
 - Any live cell with two or three live neighbours lives on to the next generation.
 - Any dead cell with exactly three live neighbours becomes a live cell.
 - You also have to think of things such as how to represent the board in a test-friendly way, and what "value" cells
 outside the board has. Or maybe the board does not have borders?

 I've tried to document as I go with comments in the file.

## License

Copyright © 2016 Jason Courcoux

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
