Vindinium Starter Haskell
=========================

Haskell starter for [Vindinium](http://vindinium.org).

I've added some sample code in src/Foo.hs.

To try it out, first build everything using:

    cabal sandbox init
    cabal install --only-dependencies
    cabal build

Then while in the top-level directory, try running:

    dist/build/foo/foo

You should see a grid showing the distance from first hero
to every other occupiable cell on the board.

