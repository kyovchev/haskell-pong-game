# Gloss example

Get to know Haskell packaging.

First, you need to install Haskell with cabal, MSys2, and Stack: https://www.haskell.org/ghcup/
Be careful, when selecting the options, because Stack is not being installed by default.
As usual, wait patiently for the installation to complete.

Next, take a look at the current Haskell package files and folders.

Setup stack for the first time:

```
stack setup
```

Build the project with stack:

```
stack build
```

Run the project with stack:

```
stack run
```

Note: You might get an error for GLUT: `user error (unknown GLUT entry glutInit)`. You can solve it, depending on your OS:

- Windows: Most likely, you need to pack it: `stack exec -- pacman -S mingw-w64-x86_64-freeglut`.

- Ubuntu: Most likely, `sudo apt update && sudo apt install freeglut3-dev -y`.

- MacOS: Should work out of the box.
