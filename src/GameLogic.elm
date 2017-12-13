module GameLogic exposing (
  nextGeneration,
  gridToString)

import Html exposing (text)
import Random exposing (Generator)
import Array2D
import List exposing (range)

type Cell = Alive | Dead

zip = List.map2 (,)

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


recDiffTuple: List (Int, Int) -> List (Int, Int)
recDiffTuple l =
  case  List.length l of
    9 -> l
    x ->
      Debug.log (toString l)
      recDiffTuple ((x % 3, x // 3) :: l)

diffTuple: List (Int, Int)
diffTuple =
  recDiffTuple []

nonZeroDiffTuple: List (Int, Int)
nonZeroDiffTuple =
  List.filter
    (\tpl -> (((Tuple.first tpl) /= 0) || ((Tuple.second tpl) /= 0)))
    diffTuple

conv: Array2D.Array2D Cell -> Int -> Int -> List Cell
conv grid x y =
  Debug.log (toString (x, y))
  List.map
    (\tpl ->
      accessTile
        grid
        (x + Tuple.first tpl)
        (y + Tuple.second tpl)
    )
    nonZeroDiffTuple


neighbours: Array2D.Array2D Cell -> List (List Cell)
neighbours grid =
  List.map
    (\idx -> conv grid (idx % (Array2D.columns grid)) (idx // Array2D.rows grid))
    (range 0 ((Array2D.columns grid) * (Array2D.rows grid)))


nextGeneration: Array2D.Array2D Cell -> List Int
nextGeneration grid =
  Debug.log (toString (neighbours grid))
  List.map (\states -> (List.length (List.filter (\i -> i == Alive) states))) (neighbours grid)

recGridToString: Array2D.Array2D -> Int -> String
recGridToString arr idx =
  case Array2D.get
    (idx % (Array2D.columns arr))
    (idx // (Array2D.rows arr))
    arr
  of
    Nothing -> ""
    Just Dead -> "_" ++ recGridToString arr (idx + 1)
    Just Alive -> "X" ++ recGridToString arr (idx + 1)

gridToString: Array2D.Array2D -> String
gridToString arr =
  recGridToString arr 0
