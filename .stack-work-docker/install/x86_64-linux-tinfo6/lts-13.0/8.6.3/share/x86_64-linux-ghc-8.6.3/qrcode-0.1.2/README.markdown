# Basic Encoding

The `encode` function and the `toArray` function are all you need. See `Example.hs`. You can use ImageMagick to enlarge the symbol and convert to other formats:

    $ runhaskell Example.hs
    $ convert hello.pgm -bordercolor white -border 4 -scale 300x300 -interpolate integer hello.png

Output:

![Example output](/kizzx2/haskell-qrcode/raw/master/doc/hello.png)
