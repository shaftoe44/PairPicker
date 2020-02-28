module Pairer exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


get_random : Int -> Random.Generator (List Int)
get_random n =
  Random.list n (Random.int 0 Random.maxInt)


shuffle_list : List String -> List Int -> List String
shuffle_list li rand =
  List.map2 Tuple.pair rand li
  |> List.sortBy Tuple.first
  |> List.map Tuple.second


pair_up : List String -> List (String, Maybe(String))
pair_up li = 
  let 
    helper acc ll = 
      case ll of
        h1 :: h2 :: tl -> 
          helper ((h1, Just h2) :: acc) tl

        hd :: [] -> 
          (hd, Nothing) :: acc

        [] -> 
          acc
  in helper [] li


pair_text : List (String, Maybe(String)) -> List (String)
pair_text li =
  let
    helper pair =
      case Tuple.second pair of
        Just p ->
          Tuple.first pair ++ " is with " ++ p

        Nothing ->
          Tuple.first pair ++ " is on their own"
  in
  List.map helper li


-- MODEL


type alias Model =
  { people : List String
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model []
  , Cmd.none
  )


-- UPDATE


type Msg
  = Roll
  | Shuffle (List Int)
  | People String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model,
        Random.generate Shuffle (get_random (List.length model.people))
      )

    Shuffle li ->
      ({model | people = shuffle_list model.people li}
      , Cmd.none
      )

    People s ->
      let people = String.split "\n" s |> List.filter (String.isEmpty >> not)
      in ({model | people = people}
      ,Random.generate Shuffle (get_random (List.length people)))


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Precise Pair Programming Pair Pairer" ]
    , textarea [ onInput People, placeholder "Enter line-seperated names here"] (List.map (\p -> text (p ++ "\n")) model.people)
    , div [] (List.map (\person -> p [] [text person]) (pair_up model.people |> pair_text))
    , button [ onClick Roll ] [ text "Randomise pairs" ]
    ]
