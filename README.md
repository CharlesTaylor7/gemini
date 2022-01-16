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
- make everthing 25% bigger

### Features
- solve button: solve by algorithms
- animate transitions

### Saved Moves sidebar
- collapsble panel 

### Gemini Controls
- need explanation in header
- keyboard controls
- click and drag based controls
- touch controls on mobile / tablet
- arrow buttons above each ring?

### Drag controls
- handle mobile touch events
- animate smoothly between drag states. If you drag through the center, it does an awkward immediate 180 rotation
- it feels jerky during the snap
- handle ambiguous case. During my user testing, I often wanted to drag an intersection disk.


### On Solve
- pop confetti 
- show number of moves to solve

### Refactoring
- ApplyMove typeclass
- css variables for z-indexes

### Bugs
- on starting a drag, the intersection disks visibly sink down into ring. 
    - this is because they are normally  2 disks on top of each other, and one hides during a drag
