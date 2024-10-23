-- conway's game of life in Haskell
-- runs in terminal
-- size 9x9 to start ig? do infinite later if i have time
-- maybe not too hard to do infinite actually, but may be difficult to change size and randomly set Dead starting cells
-- Dead = '#'
-- dead = ' '

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Game (run, example, glider, blinker, toad, spaceship) where

import Debug.Trace (trace)
import Control.Concurrent (threadDelay)

data Cell = Live | Dead
  deriving (Eq)

instance Show Cell where
  show Live = "#"
  show Dead = " "

data Coord = Coord Int Int
  deriving (Show)

type Grid = [[Cell]]

-- Get cell from grid given coords
-- normalise so works across infinite
get_cell :: Coord -> Grid -> Cell
get_cell (Coord x y) grid = grid !! _x !! _y
  where
    _x = normalise_x x grid
    _y = normalise_y y grid

-- TODO: pass width, height to get_cell - calc when grid is read in to avoid redundant computation
normalise_x :: Int -> Grid -> Int
normalise_x x grid
  | x >= width = x `rem` width
  | x /= 0 && x < 0 = width + (x `rem` width)
  | otherwise = x
  where
    width = get_width grid

normalise_y :: Int -> Grid -> Int
normalise_y y grid
  | y >= height = y `rem` height
  | y /= 0 && y < 0 = height + (y `rem` height)
  | otherwise = y
  where
    height = get_height grid

get_width :: Grid -> Int
get_width (x : xs) = length x

get_height :: Grid -> Int
get_height = length

get_coords :: Grid -> [[Coord]]
get_coords grid = [[Coord x y | (x, _) <- zip [0 ..] row] | (y, row) <- zip [0 ..] grid]

-- (x,y) order (-1, -1) (0, -1) (1, -1)
--             (-1, 0)  (0, 0)  (1, 0)
--             (-1, 1)  (0, 1)  (1, 1)
get_surrounding :: Coord -> Grid -> [Cell]
get_surrounding (Coord x y) grid =
  [ get_cell (Coord (x - 1) (y - 1)) grid,
    get_cell (Coord x (y - 1)) grid,
    get_cell (Coord (x + 1) (y - 1)) grid,
    get_cell (Coord (x - 1) y) grid,
    -- get_cell (Coord y x) grid, -- for testing
    get_cell (Coord (x + 1) y) grid,
    get_cell (Coord (x - 1) (y + 1)) grid,
    get_cell (Coord x (y + 1)) grid,
    get_cell (Coord (x + 1) (y + 1)) grid
  ]

-- update cell based on state and surrounding cells
-- check surrounding 8 cells
-- if live and <2 live touching, kill
-- if live and 2-3 live touching, live
-- if live and >3 live touching, kill
-- if dead and 3 live touching, live
update :: Cell -> Coord -> [Cell] -> Cell
update cell (Coord x y) touching_cells
  | cell == Live && num_live < 2 = Dead
  | cell == Live && (num_live == 2 || num_live == 3) = Live
  | cell == Live && (num_live > 3) = Dead
  | cell == Dead && (num_live == 3) = Live
  | otherwise = cell
  where
    num_live = length $ filter (== Live) touching_cells

run :: Grid -> Int -> IO ()
run grid delay = do
  putStrLn $ unlines $ map (concatMap show) grid
  threadDelay delay
  let newGrid = [ [ update (get_cell (Coord x y) grid) (Coord x y) (get_surrounding (Coord x y) grid)
                  | x <- [0 .. get_width grid - 1] ]
                  | y <- [0 .. get_height grid - 1] ]
  run newGrid delay

-- examples
example =
  [ [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Live, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Live, Dead, Live, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Live, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Live, Live, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Live, Live, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Live, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
  ]

glider =
  [ [Dead, Dead, Dead, Dead, Dead],
    [Dead, Live, Dead, Dead, Dead],
    [Dead, Dead, Live, Dead, Dead],
    [Live, Live, Live, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead]
  ]

-- not sure why this doesn't blink properly...
blinker =
  [ [Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead],
    [Dead, Live, Live, Live, Dead],
    [Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead]
  ]

toad =
  [ [Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Live, Live, Live, Dead],
    [Dead, Live, Live, Live, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead]
  ]

spaceship =
  [ [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Live, Dead, Dead, Live, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Live, Dead, Dead],
    [Dead, Dead, Live, Dead, Dead, Dead, Live, Dead, Dead],
    [Dead, Dead, Dead, Live, Live, Live, Live, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead],
    [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
  ]

-- tests --
test1 = show (get_surrounding (Coord 4 4) example)
test2 = show (get_coords example)
test3 = show (get_cell (Coord 0 0) example)
test4 = show (update (get_cell (Coord 4 4) example) (Coord 4 4) (get_surrounding (Coord 4 4) example))
test5 = show (get_surrounding (Coord 1 0) example)
test6 = show (get_surrounding (Coord 17 17) example)