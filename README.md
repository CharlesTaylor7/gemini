## Gemini 
A puzzle with 3 overlapping rings and 50 disks in 6 colors. The goal is to line up disks into unbroken sequences of common color.

Project goals:
- Interactive visual emulation of the puzzle
- Ability to program "algos", reusable sets of moves to apply.
- Learn how to solve the puzzle

## Run Dev Server
ghcid --command "stack ghci --main-is gemini" -W -T Main.dev

## TODO
- Attach image of puzzle to readme
- scramble button : apply 1000 random moves and clear the history buffer
- scripting: record seqeuences of moves, give them names and replay them
- solve button: solve by algorithms
- arrow butons over each ring
- restyle / regroup button toolbar
