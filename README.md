SudokuH
=======

A simple Sudoku solver created to become more familiar with Haskell.

## Current Features
- Puzzles of arbitrary dimensions are theoretically supported. 
- The dimensions of a region can be specified to be different from the standard (3,3).
- `puzzleDisplay (solveForced puzzle (3,3))` will solve a puzzle without making any guesses.
- `puzzleDisplay (dfsSolve (buildPuzzleState puzzle (3,3)))` uses a greedy depth first search.

## Puzzle State Representation
I haven't played around with any Haskell data types other than List and Int.
For that reason, puzzles are represented as [[[Int]]]. For example:

		puzzle = [[[ ],[ ],[ ],[4],[ ],[5],[ ],[ ],[ ]],
		          [[ ],[9],[ ],[8],[ ],[1],[ ],[7],[ ]],
		          [[2],[ ],[ ],[9],[6],[3],[ ],[ ],[8]],
		          [[ ],[ ],[7],[ ],[ ],[ ],[3],[ ],[ ]],
		          [[5],[ ],[8],[ ],[ ],[ ],[4],[ ],[1]],
		          [[ ],[ ],[6],[ ],[ ],[ ],[8],[ ],[ ]],
		          [[7],[ ],[ ],[5],[1],[6],[ ],[ ],[3]],
		          [[ ],[4],[ ],[7],[ ],[2],[ ],[5],[ ]],
		          [[ ],[ ],[ ],[3],[ ],[9],[ ],[ ],[ ]]]

##License: MIT

Copyright (c) 2013 Julian Ceipek

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.