Authors:
- Gregory Parent, gmp89@cornell.edu
- Evan Holly, egh67@cornell.edu
- Julian Meinke, jm2874@cornell.edu
- Milo Schlittgen-Li, ms3626@cornell.edu
- Nate Benz, nab267@cornell.edu

GenAI Usage:
- Greg: In reading files, I discovered that some files used a CMYK color format rather than RGB. However, unlike other file formats, I was having trouble using the built in libraries to switch to RGB24. I had ChatGPT explain to me and help implement [cmyk_pixel_to_rgb] as I was unaware of some of the calculations involved in switching color types
- Greg: I was having trouble printing live code of the clicked pixels, as it would only print once the image popup was terminated. I asked ChatGPT and it told me about [flush stdout] which solved this issue.
- Julian: When doing the image shrinking algorithm, I was unsure how to extract RGB values from the Graphics.image type.


Currently to run on Mac (from what I've seen online you don't need the first 2 commands on windows, we are essentially emulating a windows environment): 
- Must opam install everything in requirements.txt (also install Xquartz on mac)
- Run [open -a XQuartz] (only for mac users)
- Run [echo $DISPLAY=:0]
- Run [dune exec bin/main.exe data/Homer_Simpson.png]

Notes: 
- Currently only works with png