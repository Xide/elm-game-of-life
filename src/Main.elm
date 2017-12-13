module Main exposing (..)

import Html exposing (text)
import Random exposing (Generator)
import Array2D
import List exposing (range)

type Cell = Alive | Dead


-- randomCells: Int -> Int -> Array2D.Array2D Cell
-- randomCells x y =
--   Array2D.initialize x y
--
-- randomCell: Generator Cell
-- randomCell =
--   map (\b -> if b then Alive else Dead) Random.bool

accessTile: Array2D.Array2D Cell -> Int -> Int -> Cell
accessTile grid x y =
  case Array2D.get x y grid of
    Just x -> x
    Nothing -> Dead

conv: Array2D.Array2D Cell -> Int -> Int -> List Cell
conv grid x y =
  List.map
    (\dx dy -> accessTile grid (x + dx) (y + dy))
    (List.filter (\dx dy -> ((dx /= 0) || (dy /= 0))) (range -1 1, range -1 1))


neighbours: Array2D.Array2D Cell -> List (List Cell)
neighbours grid =
  List.map
    (\x y -> conv grid x y)
    (range 0 Array2D.columns grid, range 0 Array2D.rows grid)


nextGeneration: Array2D.Array2D Cell -> List Int
nextGeneration grid =
  List.map (\states -> (List.length (List.filter (\i -> i == Alive) states))) (neighbours grid)


grid = Array2D.fromList [
  [Dead, Alive, Dead, Dead, Dead],
  [Dead, Dead, Alive, Dead, Dead],
  [Dead, Dead, Dead, Alive, Dead],
  [Dead, Alive, Alive, Alive, Dead],
  [Dead, Dead, Dead, Dead, Dead]]

main =
  grid
    |> nextGeneration
    |> toString
    |> text
