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
- reset button: reset to a solved state
- recording:
    - start/stop record button
    - display recorded motions as cycle notation

## TODO
- Attach image of puzzle to readme

### Features
- solve button: solve by algorithms
- animate transitions

### Saved Moves sidebar
- on hover over a cycle, highlight positions, and give them a label for their order in the cycle
- fix border & box shadows on buttons
- collapsble panel 

### Gemini Controls
- need explanation in header
- keyboard controls
- click and drag based controls
- arrow buttons above each ring?

### On Solve
- pop confetti 
- show number of moves to solve
