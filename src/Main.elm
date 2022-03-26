module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import GraphQl
import GraphQl.Http
import Graphql.Parser as Parser
import Graphql.Generator.Types exposing (ApiInteractions)
import Html.Attributes exposing (title)
import Graphql.Parser.Type as Type exposing (TypeDefinition)
import Graphql.Parser.CamelCaseName as CamelCaseName
import Graphql.Parser.CamelCaseName exposing (CamelCaseName)

-- TYPES
type alias ConfigURL = String

type alias Config = 
  { graphqlEndpoint: String
  }

type alias Model = 
  { config: Maybe Config
  , introspection: Result Http.Error ApiInteractions
  }

type Msg
  = NoOp
  | GotConfig (Result Http.Error Config)
  | HitEndpoint
  | GotIntrospection (Result Http.Error ApiInteractions)

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
    , introspection = Result.Err <| Http.BadUrl "Not Asked Yet -- TODO: Change this to another type"
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
          (model, runIntrospectionQuery config.graphqlEndpoint)

    GotIntrospection apiInteractionsResult ->
      ( { model | introspection = apiInteractionsResult}, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  div []
      [ text (Debug.toString model.config)
      , button [Html.Events.onClick HitEndpoint ] [text "Hit Endpoint"]
      , resultView apiView model.introspection
      ]

resultView : (a -> Html msg) -> Result Http.Error a -> Html msg
resultView contentsView result  = 
  case result of
      Err error ->
        case error of
            Http.BadUrl str ->
              text <| "Bad Url: " ++ str
            
            Http.Timeout ->
              text "Timeout"

            Http.NetworkError ->
              text "Network Error"

            Http.BadStatus statusCode ->
              text <| "Bad Status. Status Code: " ++ (String.fromInt statusCode)
            
            Http.BadBody str ->
              text <| "Bad Body: " ++ str

      Ok contents ->
        contentsView contents

apiView : ApiInteractions -> Html Msg
apiView apiInteractions = 
  let queries =
        apiInteractions.queries
        |> List.map typeView
      mutations =
        apiInteractions.mutations
        |> List.map Debug.toString
        |> List.map (\x -> li [] [ text x])

      baseTypes =
        apiInteractions.baseTypes
        |> List.map Debug.toString
        |> List.map (\x -> li [] [ text x])

  in
    div []
      [ h1 [] [text "Queries"]
      , ul [] queries
      , h1 [] [text "Mutations"]
      , ul [] mutations
      , h1 [] [text "Base Types"]
      , ul [] baseTypes
      ]

typeView : TypeDefinition -> Html Msg
typeView (Type.TypeDefinition name definableType description) =
  case definableType of
    Type.ScalarType ->
      text "ScalarType"
    
    Type.ObjectType listOfField ->
      listOfField
        |> List.map objectFieldToString
        |> List.map (\x -> li [] [text x])
        |> ul []

    _ ->
      text "Not Implemented"
    -- | InterfaceType (List Field) (List ClassCaseName)
    -- | UnionType (List ClassCaseName)
    -- | EnumType (List EnumValue)
    -- | InputObjectType (List Field)

objectFieldToString : Type.Field -> String
objectFieldToString field =
   (nameToString field.name)
   ++ " - "
   ++ (typeRefToString field.typeRef)
   ++ " : "
   ++ (List.map argToString field.args |> String.join ", ")

nameToString : CamelCaseName -> String
nameToString camelCaseName =
  CamelCaseName.raw camelCaseName

typeRefToString : Type.TypeReference -> String
typeRefToString (Type.TypeReference referrableType isNullable) =
  (case referrableType of
      Type.Scalar scalar ->
        (Debug.toString scalar)
      _ ->
        "TODO: TypeReference"
  ) ++ " " ++ (nullableToString isNullable)

nullableToString : Type.IsNullable -> String
nullableToString isNullable =
  case isNullable of
      Type.Nullable ->
        "(Nullable)"
      
      Type.NonNullable ->
        "(Non-Nullable)"

argToString : Type.Arg -> String
argToString arg =
  (nameToString arg.name)
  ++ " - "
  ++ (typeRefToString arg.typeRef)

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
            , GraphQl.field "fields"
              |> GraphQl.withSelectors
                [ GraphQl.field "name"
                ]          
            ]
        ]
    ]
  
sendRequest url =
  GraphQl.query typesRequest
    |> GraphQl.Http.send { url = url, headers = [] } 
      (\result -> let _ = Debug.log "result" result in NoOp)
      (Json.Decode.succeed "42")

runIntrospectionQuery url =
  Http.post 
    { url = url
    , body = Http.stringBody "application/json" introspectionQuery 
    , expect = Http.expectJson GotIntrospection (Json.Decode.field "data" Parser.decoder)
    }


-- CONSTS

introspectionQuery : String
introspectionQuery = """
{"query":"query IntrospectionQuery {    __schema {      queryType {        name      }      mutationType {        name      }      subscriptionType {        name      }      types {        ...FullType      }    }  }  fragment FullType on __Type {    kind    name    description    fields(includeDeprecated: false) {      name      description      args {        ...InputValue      }      type {        ...TypeRef      }      isDeprecated      deprecationReason    }    inputFields {      ...InputValue    }    interfaces {      ...TypeRef    }    enumValues(includeDeprecated: false) {      name      description      isDeprecated      deprecationReason    }    possibleTypes {      ...TypeRef    }  }  fragment InputValue on __InputValue {    name    description    type { ...TypeRef }    defaultValue  }  fragment TypeRef on __Type {    kind    name    ofType {      kind      name      ofType {        kind        name        ofType {          kind          name          ofType {            kind            name            ofType {              kind              name              ofType {                kind                name                ofType {                  kind                  name                }              }            }          }        }      }    }  }","variables":null,"operationName":"IntrospectionQuery"}
"""