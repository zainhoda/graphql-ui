module Main exposing (..)

import Browser
import Html exposing (Html, text)
import Http
import Json.Decode

-- TYPES
type alias ConfigURL = String

type alias Config = 
  { graphqlEndpoint: String
  }

type alias Model = 
  { config: Maybe Config
  }

type Msg
  = NoOp
  | GotConfig (Result Http.Error Config)

-- MAIN

main : Program ConfigURL Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- INIT

init : ConfigURL -> ( Model, Cmd Msg )
init configURL =
  ( { config = Nothing
    }
  , getConfig configURL
  )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    GotConfig config ->
      ( { model 
        | config = Result.toMaybe config
        }
      , Cmd.none
      )


-- VIEW

view : Model -> Html Msg
view model =
  text (Debug.toString model)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- CMD

getConfig : ConfigURL -> Cmd Msg
getConfig configURL =
  Http.get {url = configURL, expect = Http.expectJson GotConfig configDecoder}

configDecoder : Json.Decode.Decoder Config
configDecoder =
  Json.Decode.field "graphql_endpoint" Json.Decode.string
  |> Json.Decode.map Config