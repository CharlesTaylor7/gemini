## What is Gemini?
Gemini is a simulator for a puzzle I owned. Unlike the Rubik's cube which has numerous guides & tools for understanding, I was hard pressed to find information on this puzzle that I was gifted. 

I received the puzzle from my Brazilian inlaws, and the only information I could find on this puzzle initially was from a website entirely in portugese. 

Later I learned about and realized this puzzle is the same as the "Hungarian rings" puzzle but with an extra disk. The solution techniques for the puzzle is the same even with the extra ring.
Still even after knowing the puzzle's true origin, I found the existing tutorials for the puzzle overcomplicate things and make for very slow solves. So I embarked to document and optimize my algorithm. 

## Puzzle description
A sliding disk puzzle with 3 overlapping rings and 50 disks in 6 colors. The goal is to line up disks into unbroken sequences of common color.
Here's an example of solved puzzle:

![IMG_20220131_144622954 (1)](https://user-images.githubusercontent.com/16541866/152467339-90726a2f-5fbd-4585-b9ef-040bf5b22a51.jpg)


## Project Goals
- Interactive visual emulation of the puzzle
- Ability to program "algos", reusable sets of moves to apply.
- Learn how to solve the puzzle

## Scripts Dev Server
- TODO: explain installation of deps

- Run a dev server with hot reloads: `./scripts/run-dev.sh`
- Run the tests in watch mode: `./scripts/watch-tests.sh`
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

- It's unacceptably slow in production. I need to deploy and test performance of a ghcjs build
- Attach image of puzzle to readme
- Explain what "solving means" and explain controls.
- For the ambiguous case, lock the choice in early. 
Only allow changing the rotation ring if the mouse passes through a narrow band around the starting location
- The saved moves need to be more obviously buttons
- Hovering over a move should describe all cycles simulataneously
- Highlight how a cycle impacts a pair of disks 5 apart?

#### Bugs
- labels do not rescale to fit disk size
- intersection disks flicker if you click and stop a drag quickly.
- animate smoothly between drag states. If you drag through the center, it does an awkward immediate 180 rotation
- it feels jerky during the snap

- Hot reload doesn't work if I try to run another stack build process for typechecking. This interferes somehow with ghcid's loop. I'd rather not pick between hot reloading and typechecker feedback. (Otherwise I have to guess why it didn't reload)

Maybe hls in my editor can help with this. Or maybe ghcid flags can help with this?

ghcid should be reporting typechecker issues to me but it isn't 

### Future Work
- Implement a search program for efficient algorithms. 
    - Right now I'm shooting in the dark. Knowing efficient algorithms for transpositions or 3 cycles, would allow me to solve on my physical puzzle.
    - The puzzle the puzzle has odd parity since the generators are 18-cycles. 
    - But I haven't been able to generate odd permutations smaller than an 8 cycle. Weird
- animate transitions
- present elapsed time to solve in confetti view
- solve button: watch the computer solve the puzzle via algorithms
- explain controls in footer

