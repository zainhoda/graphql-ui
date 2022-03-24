module Main exposing (..)

import Browser
import Html exposing (Html, text)

-- TYPES
type alias Config = String

type alias Model = { config : Config }


-- MAIN

main : Program Config Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- INIT

init : Config -> ( Model, Cmd Msg )
init config =
  ( { config = config }
  , Cmd.none
  )


-- UPDATE

type Msg = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
  ( model, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
  text (model.config)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none