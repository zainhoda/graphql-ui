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
import Html.Attributes
import Graphql.Parser.Type as Type exposing (TypeDefinition)
import Graphql.Parser.CamelCaseName as CamelCaseName
import Graphql.Parser.CamelCaseName exposing (CamelCaseName)
import Graphql.Parser.ClassCaseName
import Html.Attributes
import Graphql.Parser.Type exposing (IsNullable)
import Graphql.Parser.Type exposing (IsNullable(..))
import Dict
import Html.Attributes exposing (class)

-- TYPES
type alias ConfigURL = String

type alias Config = 
  { graphqlEndpoint: String
  }

type alias Model = 
  { config: Maybe Config
  , introspection: Result Http.Error ApiInteractions
  , queries: Dict.Dict String Type.Field
  , mutations: Dict.Dict String Type.Field
  , types: Dict.Dict String Type.TypeDefinition
  , formInput: Dict.Dict String (Dict.Dict String String)
  , activeForm: Maybe String
  }

type Msg
  = NoOp
  | GotConfig (Result Http.Error Config)
  | HitEndpoint
  | GotIntrospection (Result Http.Error ApiInteractions)
  | UpdateFormInput String String String
  | SubmitForm String
  | SetActiveForm (Maybe String)

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
    , queries = Dict.empty
    , mutations = Dict.empty
    , types = Dict.empty
    , formInput = Dict.empty
    , activeForm = Nothing
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
      ( { model 
        | introspection = apiInteractionsResult
        , queries = apiInteractionsToFieldDict apiInteractionsResult .queries
        , mutations = apiInteractionsToFieldDict apiInteractionsResult .mutations
        , types = apiInteractionsToTypeDict apiInteractionsResult
        }
      , Cmd.none
      )

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
    
    SubmitForm formName ->
      ( model, submitForm model formName)

    SetActiveForm maybeForm ->
      ( {model | activeForm = maybeForm }, Cmd.none)

apiInteractionsToFieldDict : Result Http.Error ApiInteractions -> (ApiInteractions -> List Type.TypeDefinition) -> Dict.Dict String Type.Field
apiInteractionsToFieldDict res queryOrMutation =
  case res of
      Err _ ->
        Dict.empty
      
      Ok contents ->
        contents
        |> queryOrMutation
        |> List.head
        |> Maybe.map 
          (\x ->
            case x of
                -- This type is "Query" and it should only contain an ObjectType with a list of fields
                Type.TypeDefinition _ definableType _ -> 
                  case definableType of
                    Type.ObjectType listOfField ->
                      listOfField
                      |> List.map 
                          (\y ->
                            (nameToString y.name, y)
                          )
                      |> Dict.fromList
                    _ ->
                      Dict.empty
          )
        |> Maybe.withDefault Dict.empty

apiInteractionsToTypeDict : Result Http.Error ApiInteractions -> Dict.Dict String Type.TypeDefinition
apiInteractionsToTypeDict res =
  case res of
      Err _ ->
        Dict.empty
      
      Ok contents ->
        contents.baseTypes
        |> List.map 
            (\x -> 
              case x of
                Type.TypeDefinition classCaseName _ _ ->
                  (Graphql.Parser.ClassCaseName.raw classCaseName, x)
            )
        |> Dict.fromList

-- VIEW

view : Model -> Html Msg
view model =
  div [Html.Attributes.class "container"]
      [ pre [] [text (Debug.toString model.config)]
      , pre [] [text (Debug.toString model.formInput)]
      , button [Html.Attributes.class "button is-large is-success" , Html.Events.onClick HitEndpoint ] [text "Introspect!"]
      , errorView model.introspection
      , apiView model
      ]

errorView : Result Http.Error a -> Html msg
errorView result  = 
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
        text ""

apiView : Model -> Html Msg
apiView model = 
  let queries =
        model.queries
        |> Dict.toList
        |> List.map (\(_, queryField) -> fieldTypeToButton queryField)

      maybeForm =
        model.activeForm
        |> Maybe.map 
          (\activeForm ->
            model.queries
            |> Dict.get activeForm
            |> Maybe.map formModal
            |> Maybe.withDefault (text "Something went wrong -- the activeForm wasn't found in the queries")
          )
        |> Maybe.withDefault (text "")
      mutations =
        model.mutations
        |> Dict.toList
        |> List.map Debug.toString
        |> List.map (\x -> li [] [ text x])

      baseTypes =
        model.types
        |> Dict.toList
        |> List.map Debug.toString
        |> List.map (\x -> li [] [ text x])

  in
    div []
      [ maybeForm
      , h1 [Html.Attributes.class "title"] [text "Queries"]
      , ul [] queries
      , h1 [Html.Attributes.class "title"] [text "Mutations"]
      , ul [] mutations
      , h1 [Html.Attributes.class "title"] [text "Base Types"]
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

fieldTypeToButton : Type.Field -> Html Msg
fieldTypeToButton fieldType =
  let formName = (nameToString fieldType.name)
  in
    button 
      [ class "button"
      , Html.Events.onClick (SetActiveForm (Just formName))
      ] 
      [text formName]

fieldTypeToForm : Type.Field -> Html Msg
fieldTypeToForm fieldType =
  div [] 
    (
      (List.map (argToFormField (nameToString fieldType.name)) fieldType.args) ++
      [ pre [] [text (Debug.toString fieldType.typeRef)] ]
    )

formModal : Type.Field -> Html Msg
formModal fieldType = 
  div [ class "modal is-active" ]
      [ div [ class "modal-background" ]
          []
      , div [ class "modal-card" ]
          [ header [ class "modal-card-head" ]
              [ p [ class "modal-card-title" ]
                  [ text (nameToString fieldType.name) ]
              , button [ class "delete", Html.Events.onClick (SetActiveForm Nothing) ]
                  []
              ]
          , section [ class "modal-card-body" ]
              [ fieldTypeToForm fieldType ]
          , footer [ class "modal-card-foot" ]
              [ button [ Html.Attributes.class "button is-success", Html.Events.onClick (SubmitForm (nameToString fieldType.name)) ] [text "Run Query"]
              , button [ class "button", Html.Events.onClick (SetActiveForm Nothing) ]
                  [ text "Cancel" ]
              ]
          ]
      ]

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
  
-- sendRequest : String -> Request -> Cmd Msg
sendRequest url request =
  GraphQl.query request
    |> GraphQl.Http.send { url = url, headers = [] } 
      (\result -> let _ = Debug.log "result" result in NoOp)
      (Json.Decode.succeed "42")

runIntrospectionQuery : String -> Cmd Msg
runIntrospectionQuery url =
  Http.post 
    { url = url
    , body = Http.stringBody "application/json" introspectionQuery 
    , expect = Http.expectJson GotIntrospection (Json.Decode.field "data" Parser.decoder)
    }

submitForm : Model -> String -> Cmd Msg
submitForm model formName =
  let formValues = Dict.get formName model.formInput
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
      
      addArguments x = 
        List.foldl (\(fieldName, fieldValue) -> \y -> y |> GraphQl.withArgument fieldName (GraphQl.string fieldValue) ) x formValues
      request = 
        GraphQl.object 
          [ GraphQl.field formName
            |> addArguments
            |> GraphQl.withSelectors [GraphQl.field "id"]
          ]
  in
    case model.config of
      Nothing ->
        Cmd.none
      
      Just config ->
        sendRequest config.graphqlEndpoint request

-- Get fields from query
-- formName -> model -> typeRef
-- typeRef -> model -> GraphQl.Field a -> GraphQl.Field a 
--    i.e. this will do nested GraphQl.withSelectors and GraphQl.field

-- getTypeRefFromQuery : Model -> String -> Maybe Type.TypeReference
-- getTypeRefFromQuery model formName =
--   case model.introspection of
--       Err _ ->
--         Nothing
      
--       Just apiInteractions ->
--         apiInteractions.queries
--         |> List.map
--             (\(Type.TypeDefinition _ definableType _ ) -> 
--               case definableType of
--                   Type.ObjectType listOfField ->
--                     listOfField
--                     |> List.filterMap 
--                       (\x -> 
--                         if nameToString x.name == formName then
--                           Just x.typeRef
--                         else
--                           Nothing
--                       )
--                     |> List.head

--                   _ ->
--                     Nothing
--             ) 

-- CONSTS

introspectionQuery : String
introspectionQuery = """
{"query":"query IntrospectionQuery {    __schema {      queryType {        name      }      mutationType {        name      }      subscriptionType {        name      }      types {        ...FullType      }    }  }  fragment FullType on __Type {    kind    name    description    fields(includeDeprecated: false) {      name      description      args {        ...InputValue      }      type {        ...TypeRef      }      isDeprecated      deprecationReason    }    inputFields {      ...InputValue    }    interfaces {      ...TypeRef    }    enumValues(includeDeprecated: false) {      name      description      isDeprecated      deprecationReason    }    possibleTypes {      ...TypeRef    }  }  fragment InputValue on __InputValue {    name    description    type { ...TypeRef }    defaultValue  }  fragment TypeRef on __Type {    kind    name    ofType {      kind      name      ofType {        kind        name        ofType {          kind          name          ofType {            kind            name            ofType {              kind              name              ofType {                kind                name                ofType {                  kind                  name                }              }            }          }        }      }    }  }","variables":null,"operationName":"IntrospectionQuery"}
"""