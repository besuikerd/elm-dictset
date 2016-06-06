module DictSetExample exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import DictSet exposing (DictSet)

main : Program Never
main =  App.program
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }

type alias Model =
  { input: String
  , uid: Int
  , items: DictSet Int Item
  }

type alias Item =
  { id: Int
  , name : String
  }

type Msg
  = AddItem
  | Remove Item
  | OnInput String

init : (Model, Cmd Msg)
init =
  ( Model
      ""
      0
      (DictSet.empty .id)
  , Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddItem ->
      { model
      | items = DictSet.insert (Item model.uid model.input) model.items
      , input = ""
      , uid = model.uid + 1
      } ! []
    Remove item ->
      { model | items = DictSet.remove item model.items } ! []
    OnInput input ->
      { model | input = input } ! []

view : Model -> Html Msg
view model =
  div
    []
    [ itemInput model.input
    , itemList (DictSet.values model.items)
    ]


itemInput : String -> Html Msg
itemInput inputValue =
  input
    [ type' "text"
    , onInput OnInput
    , value inputValue
    , on "keypress" <| Json.map (always AddItem) enter
    ]
    []

itemList : List Item -> Html Msg
itemList items =
  table
    []
    (List.map item items)

item : Item -> Html Msg
item i =
  tr
    []
    [ td
        []
        [ text (toString i.id) ]
    , td
        []
        [ text i.name ]

    , td
        []
        [ button
            [ onClick <| Remove i
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
    "keyCode" := Json.int
      `Json.andThen` checkEnter
