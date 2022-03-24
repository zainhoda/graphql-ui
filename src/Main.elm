module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import GraphQl
import GraphQl.Http

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
  | HitEndpoint

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
    
    HitEndpoint ->
      case model.config of
        Nothing ->
          (model, Cmd.none)
        
        Just config ->
          (model, sendRequest config.graphqlEndpoint)


-- VIEW

view : Model -> Html Msg
view model =
  div []
      [ text (Debug.toString model)
      , button [Html.Events.onClick HitEndpoint ] [text "Hit Endpoint"]
      ]


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

-- {
--  __schema {
  --   types {
  --     name
  --   }
  -- }
-- }
typesRequest : GraphQl.Operation GraphQl.Query GraphQl.Anonymous
typesRequest =
  GraphQl.object
    [ GraphQl.field "__schema"
      |> GraphQl.withSelectors
        [ GraphQl.field "types"
          |> GraphQl.withSelectors
            [ GraphQl.field "name"              
            ]
        ]
    ]
  
sendRequest url =
  GraphQl.query typesRequest
    |> GraphQl.Http.send { url = url, headers = [] } 
      (\result -> let _ = Debug.log "result" result in NoOp)
      (Json.Decode.succeed "42")