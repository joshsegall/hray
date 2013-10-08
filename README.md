hray
----

This program is based on Andrew Kensler's [business card raytracer][1] but
translated into Haskell. It would not have been possible without Fabien
Sanglard's promotion and analysis of Andrew's code:
[Decyphering The Business Card Raytracer][2].

Usage
-----

    ghc -O hray.hs
    ./hray > hray.ppm

Opening PPM files seems to be a challenge in general. I was able to do it with
LibreOffice on OS X. You can see a sample of the output as `hray.png` which is
a more accessible format.

Implementation Notes
--------------------

Haskell is surprisingly flexible and fits the business card format better than
I would have expected. I would not say that this is "good" Haskell. In
addition to using some constructions to cram it into a 40x40 character space,
this is also my first Haskell program, so please forgive any awkwardness in
the code. Also, I could have squeezed a few more characters out, but I wanted
to leave it at 1337 bytes like the original.

As a functional language Haskell is a good fit for a mathematical problem like
raytracing, though I did have to make a few changes to the structure of
Andrew's program in order to eliminate side effects (aside from IO to print
the ppm data). For the most part I was faithful to the original program with a
couple of exceptions. The first exception is that I don't have any randomness.
This could probably be remedied at the cost of few more characters. The effect
of the omission means harder shadows and less dithering in general. The second
exception is that it only takes 9 samples per pixel instead of 64. This is
purely for performance reasons. A more skilled person could probably make the
program run with less memory, but as it stands I was maxing out my laptop with
more than 9 samples. Oh, and of course I had to leave my mark by substituting
my initials.

Any improvements or Haskell tips are welcome.

[1]: http://www.cs.utah.edu/~aek/code/card.cpp
[2]: http://fabiensanglard.net/rayTracing_back_of_business_card/
