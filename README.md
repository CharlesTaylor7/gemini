## Gemini 
A puzzle with 3 overlapping rings and 50 disks in 6 colors. The goal is to line up disks into unbroken sequences of common color.

Project goals:
- Interactive visual emulation of the puzzle
- Ability to program "algos", reusable sets of moves to apply.
- Learn how to solve the puzzle

## Run Dev Server
`./scripts/run-dev.sh`

## Deploy
Fresh builds of the docker image will take about 2 hours. Most of that time is spent compiling jsaddle-dom.
Run `./deploy.sh`

## UI
- scramble: apply 1000 random moves and clear the history buffer
- reset button: reset to a solved state
- recording:
    - start/stop record button
    - display recorded motions as cycle notation

## TODO
### Misc
- Attach image of puzzle to readme
- test performance of a ghcjs build
- solve button: solve by algorithms
- animate transitions

### Saved Moves sidebar
- collapsible panel  on desktop

### Gemini Controls
- need explanation in header
- keyboard controls
- click / touch and drag based controls
- arrow buttons above each ring?

### Drag controls
- animate smoothly between drag states. If you drag through the center, it does an awkward immediate 180 rotation
- it feels jerky during the snap
- user test on mobile


#### Ambiguous case
- lock the choice in early. Only allow changing the rotation ring if the mouse passes through a narrow band
 around the starting location


### On Solve
- show number of moves to solve


### Bugs
- on starting a drag, the intersection disks visibly sink down into ring. 
    - this is because they are normally  2 disks on top of each other, and one hides during a drag
- labels do not rescale to fit disk size
