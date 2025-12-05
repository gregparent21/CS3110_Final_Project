Nate Benz (nab267), Evan Holly (egh67), Julian Meinke (jm2874), Gregory Parent (gmp89), Milo Schlittgen-Li (ms3626)

GenAI Usage:
- Greg: In reading files, I discovered that some files used a CMYK color format rather than RGB. However, unlike other file formats, I was having trouble using the built in libraries to switch to RGB24. I had ChatGPT explain to me and help implement [cmyk_pixel_to_rgb] as I was unaware of some of the calculations involved in switching color types
- Greg: I was having trouble printing live code of the clicked pixels, as it would only print once the image popup was terminated. I asked ChatGPT and it told me about [flush stdout] which solved this issue.
- Julian: When doing the image shrinking algorithm, I was unsure how to extract RGB values from the Graphics.image type.
- Milo: Had ChatGPT help handling the brightness as a separate reference through the main loop, helping updating it in the correct spots. Also helped with having the slidebar function properly. 
- Greg: When implementing file saving, I had ChatGPT explain to me how to use the [land] binary operation to extract RGB components from packed ints of format 0xRRGGBB
