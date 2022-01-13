## Gemini 
A puzzle with 3 overlapping rings and 50 disks in 6 colors. The goal is to line up disks into unbroken sequences of common color.

Project goals:
- Interactive visual emulation of the puzzle
- Ability to program "algos", reusable sets of moves to apply.
- Learn how to solve the puzzle

## Run Dev Server
ghcid --command "stack ghci --main-is gemini" -W -T Main.dev

## UI
- scramble: apply 1000 random moves and clear the history buffer
- undo: undo the last move
- reset button: reset to a solved state
- L: rotate left ring clockwise, 
- L': rotate left ring anticlockwise

## TODO
- Attach image of puzzle to readme
- scripting: record seqeuences of moves, give them names and replay them
- solve button: solve by algorithms
- arrow butons over each ring
- restyle / regroup button toolbar
- undo button
- pop confetti when solved
