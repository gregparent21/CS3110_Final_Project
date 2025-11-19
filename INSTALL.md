Currently to run on Mac run all the commands below. For Windows devices, you do not need to install or run XQuartz (This is because Xquartz is required to essentially emulate a windows environment).

Installations:
- Must opam install everything in requirements.txt. Note, you must be in OCaml version 4.14.2 for the packages to properly install. 
- If on mac, also install Xquartz
- You might need to run [brew install libpng jpeg giflib libtiff]

To run:
- Run [open -a XQuartz] (only for mac users)
- Run [echo $DISPLAY=:0]
- Run [dune exec bin/main.exe data/Homer_Simpson.png]