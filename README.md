Authors:
- Gregory Parent, gmp89@cornell.edu
- Evan Holly, egh67@cornell.edu
- Julian Meinke, jm2874@cornell.edu
- Milo Schlittgen-Li, ms3626@cornell.edu
- Nate Benz, nab267@cornell.edu

GenAI Usage:
- Greg: In reading files, I discovered that some files used a CMYK color format rather than RGB. However, unlike other file formats, I was having trouble using the built in libraries to switch to RGB24. I had ChatGPT explain to me and help implement [cmyk_pixel_to_rgb] as I was unaware of some of the calculations involved in switching color types

Currently to run on Mac (from what I've seen online you don't need the first 2 commands on windows, we are essentially emulating a windows environment): 
- Run [open -a XQuartz]
- Run [echo $DISPLAY=:0]
- Run [dune exec bin/main.exe /Users/gregoryparent/Downloads/Cornell/"Semester 3"/CS3110/FinalProject/CS3110_Final_Project/data/Homer_Simpson.png]

Notes: 
- Currently only works with png