module IdSetExample exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import IdSet exposing (IdSet)
import IntIdSet exposing (IntIdSet)

main : Program Never Model Msg
main =  Html.program
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }

type alias Model =
  { input: String
  , items: IntIdSet Item
  }

type alias Id = Int
type alias Item = String

type Msg
  = AddItem
  | Remove Int
  | OnInput String

init : (Model, Cmd Msg)
init =
  ( Model
      ""
      IntIdSet.empty
  , Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddItem ->
      { model
      | items = IdSet.insert model.input model.items
      , input = ""
      } ! []
    Remove id ->
      { model | items = IdSet.remove id model.items } ! []
    OnInput input ->
      { model | input = input } ! []

view : Model -> Html Msg
view model =
  div
    []
    [ itemInput model.input
    , itemList (IdSet.toList model.items)
    ]


itemInput : String -> Html Msg
itemInput inputValue =
  input
    [ type_ "text"
    , onInput OnInput
    , value inputValue
    , on "keypress" <| Json.map (always AddItem) enter
    ]
    []

itemList : List (Id, Item) -> Html Msg
itemList items =
  table
    []
    (List.map (uncurry item) items)

item : Id -> Item -> Html Msg
item id item =
  tr
    []
    [ td
        []
        [ text (toString id) ]
    , td
        []
        [ text item ]

    , td
        []
        [ button
            [ onClick <| Remove id
            ]
            [ text "Remove" ]
        ]
    ]
enter : Json.Decoder ()
enter =
  let
    checkEnter keyCode =
      case keyCode of
        13 -> Json.succeed ()
        _ -> Json.fail "Not enter"
  in
    Json.field "keyCode" Json.int |> Json.andThen checkEnter
