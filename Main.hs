module Main where

import Game (run, example, glider, blinker, toad, spaceship)

-- just type 'main' to run program
-- change 'example' to any of glider, blinker, toad, spaceship
main :: IO()
main = run example