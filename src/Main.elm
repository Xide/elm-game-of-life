module Main exposing (..)
import GameLogic exposing (gridToString, nextGeneration)
import Array2D

grid = Array2D.fromList [
  [Dead, Alive, Dead, Dead, Dead],
  [Dead, Dead, Alive, Dead, Dead],
  [Dead, Dead, Dead, Alive, Dead],
  [Dead, Alive, Alive, Alive, Dead],
  [Dead, Dead, Dead, Dead, Dead]]


type alias Model = {
  grid: Array2D.Array2D
}

type Action =
  NoOp |
  ResetBoard |
  NextGeneration

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    ResetBoard ->
      {
        model | grid =
          Array2D.repeat
            (Array2D.rows model.grid)
            (Array2D.columns model.grid)
            0
      }
    NextGeneration ->
      {model | grid = nextGeneration model.grid}


main =
  -- |> nextGeneration
  -- |> toString
  grid
    |> gridToString
    |> text
