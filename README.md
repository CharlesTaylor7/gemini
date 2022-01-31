## What is Gemini?
A puzzle with 3 overlapping rings and 50 disks in 6 colors. The goal is to line up disks into unbroken sequences of common color.

## Project Goals
- Interactive visual emulation of the puzzle
- Ability to program "algos", reusable sets of moves to apply.
- Learn how to solve the puzzle

## Scripts Dev Server
- TODO: explain installion of deps

- Run a dev server with hot reloads: `./scripts/run-dev.sh`
- Run the tests in watch mode: `./scripts/watch-dev.sh`
- Build the docker image and deploy to fly.io: `./scripts/deploy.sh`
    - Fresh builds of the docker image will take about 2 hours. Most of that time is spent compiling jsaddle-dom.

## UI Features
- scramble: apply 1000 random moves and clear the history buffer
- reset button: reset to a solved state
- recording:
    - start/stop record button
    - display recorded motions as cycle notation
### Saved Moves sidebar
- available on desktop
- clicking on a move applies to the gemini and to recorded moves
- buffer of recorded moves is show at top of panel

## To Do
### I would be embarrassed not to do
- Attach image of puzzle to readme
- test performance of a ghcjs build

### Gemini Controls
- need explanation in header
- keyboard controls
- click / touch and drag based controls

#### Drag case
- For the ambiguous case, lock the choice in early. 
Only allow changing the rotation ring if the mouse passes through a narrow band
 around the starting location
- animate smoothly between drag states. If you drag through the center, it does an awkward immediate 180 rotation
- it feels jerky during the snap

#### Bugs
- on starting a drag, the intersection disks visibly sink down into ring. 
    - this is because they are normally  2 disks on top of each other, and one hides during a drag
- labels do not rescale to fit disk size

### Future Work

- Implement a search program for efficient algorithms. 
    - Right now I'm shooting in the dark. Knowing efficient algorithms for transpositions or 3 cycles, would allow me to solve on my physical puzzle.
    - The puzzle the puzzle has odd parity since the generators are 18-cycles. 
    - But I haven't been able to generate odd permutations smaller than an 8 cycle. Weird
- animate transitions
- present elapsed time to solve in confetti view
- solve button: watch the computer solve the puzzle via algorithms
