# Comonad Life
## Comonadic Implementation of Conway's Game of Life

Generates .gif animations with
`runhaskell Life.hs <width> <height> <number_of_generations> <outfile.gif>`.

The program iterates over a two dimensional grid zipper, applying an individual
cell update rule with a cobind/extend.
