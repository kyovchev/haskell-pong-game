# Haskell Pong Game

This repository contains the source code of the **Pong game** written on the **Haskell** language.

The game was developed in the final weeks of the **Functional Programming** course for the **Information Systems** bachelor's program at the Faculty of Mathematics and Informatics of Sofia University "St. Kliment Ohridski".

The final state of the game is contained within the [`./haskell-pong-game/`](./haskell-pong-game/) folder.

## Installation

First, you need to install Haskell with cabal, MSys2, and Stack: https://www.haskell.org/ghcup/

Be careful, when selecting the options, because Stack is not being installed by default. As usual, wait patiently for the installation to complete.

Setup stack for the first time: `stack setup`. Build the project with stack: `stack build`. Run the project with stack: `stack run`.

**Note:** You might get an error for GLUT: `user error (unknown GLUT entry glutInit)`. You can find a specific solution for your Operating System, e.g.:

- Windows: Most likely, you need to pack it: `stack exec -- pacman -S mingw-w64-x86_64-freeglut`.

- Ubuntu: Most likely, you need to `sudo apt update && sudo apt install freeglut3-dev -y`.

- MacOS: Should work out of the box.

The code in the folder [`./begin-stage-01/`](./begin-stage-01/) is tested on Windows 11 Home 24H2, Ubuntu 24.04.2 and MacOS 10.15.7 (Catalina).

## Development stages

During the course the students had to develop the game by going through multiple stages. The start source code for each of the stages is contained within the corresponding `./begin-stage-XX/` folder. The students had to modify the code in order to achieve the desired level of readiness for the next stage. Each directory contains the finalized code from the previous stage.

The goals for each of the stages are as follows.

- **Stage 01**

  - Get to know the Haskell `stack` and the packaging system.
  - Take a look at the Gloss example.
  - Read the documentation for `Graphics.Gloss.Interface.Pure.Game`.
  - Create the basic layout of the Haskell Pong Game.
  - Add two paddles.
  - Add a moving ball.

- **Stage 02**

  - Move the paddles while the buttons are pressed.
  - Constrain the motion of the paddles within the game window.
  - Check for collision between the ball and the paddles.
  - Change the direction of the ball when collision occurs.
  - Return the ball to the center when it goes outside of the window.

- **Stage 03**

  - Create separate modules for game constants, logic, state, input handling and rendering.
  - Add scores to the state.
  - Draw the scores as a text to the screen.

- **Stage 04**

  - Study about monads and the State monad.
  - Add `mtl` to the package dependencies.
  - Put the game state within the State monad.

- **Stage 05**

  - Add a winning condition.
  - Add a game over screen
  - Add a basic AI opponent.
  - Add a menu screen to choose the game type.

- **Stage 06**

  - Get to know the `IO` monad.
  - Get to know the `Graphics.Gloss.Interface.IO.Game`.
  - Get to know `pure`.
  - Use the `IO` to capture the `Esc` key.
  - Add a pause screen.
  - Do some final touches.
