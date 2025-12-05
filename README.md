Authors:
- Gregory Parent, gmp89@cornell.edu
- Evan Holly, egh67@cornell.edu
- Julian Meinke, jm2874@cornell.edu
- Milo Schlittgen-Li, ms3626@cornell.edu
- Nate Benz, nab267@cornell.edu

Currently to run on Mac run all the commands below. For Windows devices, you do not need to install or run XQuartz (This is because Xquartz is required to essentially emulate a windows environment): 
- Must opam install everything in requirements.txt. Note, you must be in OCaml version 4.14.2 for the packages to properly install. (also install Xquartz on mac)
- Run [open -a XQuartz] (only for mac users)
- Run [echo $DISPLAY=:0]
- Run [dune exec bin/main.exe data/Homer_Simpson.png]

Notes: 
- Only works with png
