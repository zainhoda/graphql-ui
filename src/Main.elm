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
import Html.Attributes exposing (class)
import Graphql.Parser.Type exposing (IsNullable)
import Graphql.Parser.Type exposing (IsNullable(..))
import Dict
import Dict.Extra
import Graphql.Parser.Type exposing (Field)

-- TYPES
type alias ConfigURL = String

type alias Config = 
  { graphqlEndpoint: String
  }

type alias Model = 
  { config: Maybe Config
  , introspection: Result Http.Error ApiInteractions
  , formInput: Dict.Dict String (Dict.Dict String String)
  }

type Msg
  = NoOp
  | GotConfig (Result Http.Error Config)
  | HitEndpoint
  | GotIntrospection (Result Http.Error ApiInteractions)
  | UpdateFormInput String String String

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
    , formInput = Dict.empty
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

    UpdateFormInput formName formField formValue ->
      ( { model 
        | formInput = 
            let newFormFieldDict = model.formInput 
                  |> Dict.get formName
                  |> Maybe.withDefault Dict.empty
                  |> Dict.insert formField formValue
            in
              Dict.insert formName newFormFieldDict model.formInput
        } 
      , Cmd.none
      )


-- VIEW

view : Model -> Html Msg
view model =
  div [Html.Attributes.class "container"]
      [ pre [] [text (Debug.toString model.config)]
      , pre [] [text (Debug.toString model.formInput)]
      , button [Html.Attributes.class "button is-large is-success" , Html.Events.onClick HitEndpoint ] [text "Introspect!"]
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
        |> List.map formView
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
      [ h1 [Html.Attributes.class "title"] [text "Queries"]
      , ul [] queries
      , h1 [Html.Attributes.class "title"] [text "Mutations"]
      , ul [] mutations
      , h1 [Html.Attributes.class "title"] [text "Base Types"]
      , ul [] baseTypes
      ]

formView : Type.TypeDefinition -> Html Msg
formView (Type.TypeDefinition name definableType description) = 
  case definableType of
    Type.ScalarType ->
      text "ScalarType"
    
    Type.ObjectType listOfField ->
      listOfField
        |> List.map fieldTypeToForm
        |> div []

    _ ->
      text "Not Implemented"

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

fieldTypeToForm : Type.Field -> Html Msg
fieldTypeToForm fieldType =
  div [] 
    (
      ( h2 [Html.Attributes.class "subtitle"] [text (nameToString fieldType.name) ] ) ::
      (List.map (argToFormField (nameToString fieldType.name)) fieldType.args) ++
      [ button [ Html.Attributes.class "button is-small is-success" ] [text "Submit"] ]
    )

argToFormField : String -> Type.Arg -> Html Msg
argToFormField formName arg =
  div
    [ Html.Attributes.class "field" ]
    [ label [Html.Attributes.class "label"] [text (nameToString arg.name) ] 
    , div 
      [ Html.Attributes.class "control" ] 
      [ input [ Html.Attributes.class "input"
              , Html.Attributes.type_ "text"
              , Html.Attributes.placeholder (arg.description |> Maybe.withDefault "") 
              , Html.Events.onInput (UpdateFormInput formName (nameToString arg.name))
              ] [] 
      ]
    ]
--   <div class="field">
--   <label class="label">Name</label>
--   <div class="control">
--     <input class="input" type="text" placeholder="Text input">
--   </div>
-- </div>

nullableField : Type.IsNullable -> Html Msg
nullableField isNullable =
  case isNullable of
    NonNullable ->
      text ""
    
    Nullable ->
      div 
        [ Html.Attributes.class "control" ]
        [ label 
          [Html.Attributes.class "radio"] 
          [ input 
            [Html.Attributes.type_ "radio", Html.Attributes.name "nullable"] 
            [text "Null"]
          ]
        , label 
          [Html.Attributes.class "radio"] 
          [ input 
            [Html.Attributes.type_ "radio", Html.Attributes.name "nullable"] 
            [text "Value"]
          ]
        ]
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