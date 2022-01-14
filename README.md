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

### UI improvements
- guide that explains keyboard controls
- arrow butons over each ring
- restyle / regroup button toolbar
- provide labels for the ring positions

### Saved Moves sidebar
- on hover, highlight positions that are permuted
- highlight each cycle with a different color
- highlight disks that match each cycle

- fix border & box shadows on buttons


### On Solve
- pop confetti 
- show number of moves to solve
