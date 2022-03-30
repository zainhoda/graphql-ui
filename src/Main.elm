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
import Generic
import Generic.Json
import GenericDict
import RemoteData
import Time

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
  , response: Dict.Dict String (RemoteData.WebData Generic.Value)
  , activeResponse: Maybe String
  }

type Msg
  = NoOp
  | GotConfig (Result Http.Error Config)
  | HitEndpoint
  | GotIntrospection (Result Http.Error ApiInteractions)
  | UpdateFormInput String String String
  | SubmitForm String Type.TypeReference
  | SetActiveForm (Maybe String)
  | GotQueryResponse String (Result Http.Error String)
  | SetActiveResponse (Maybe String)

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
    , response = Dict.empty
    , activeResponse = Nothing
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
    
    SubmitForm formName typeRef ->
      ( { model 
        | response = model.response |> Dict.insert formName (RemoteData.Loading)
        , activeForm = Nothing
        , activeResponse = Just formName
        } 
      , submitForm model formName typeRef
      )

    SetActiveForm maybeForm ->
      ( {model | activeForm = maybeForm }, Cmd.none)

    SetActiveResponse maybeResponse ->
      ( {model | activeResponse = maybeResponse }, Cmd.none)

    GotQueryResponse key result ->
      let genericValue = Debug.log "genericValue" (Result.andThen (\x -> Result.mapError Http.BadBody (Generic.Json.decode x)) result ) |> RemoteData.fromResult
      in
        ( { model | response = model.response |> Dict.insert key genericValue} , Cmd.none)

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
      , button [Html.Attributes.class "button is-large is-success" , Html.Events.onClick HitEndpoint ] [text "Introspect!"]
      , h1 [Html.Attributes.class "title"] [text "Responses"]
      , tabView model.activeResponse model.response
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
      , div [Html.Attributes.class "buttons"] queries
      , h1 [Html.Attributes.class "title"] [text "Mutations"]
      , ul [] mutations
      , h1 [Html.Attributes.class "title"] [text "Base Types"]
      , ul [] baseTypes
      ]

tabView : Maybe String -> Dict.Dict String (RemoteData.WebData Generic.Value) -> Html Msg
tabView maybeActiveTab dict =
  let activeTab = Maybe.withDefault "" maybeActiveTab
      tabList = Dict.toList dict
      maybeTabContents = Dict.get activeTab dict
  in
    div []
    [ div 
      [Html.Attributes.class "tabs"]
      [ ul []
        ( tabList 
          |> List.map
            (\(tabTitle, _) ->
              li 
                [Html.Attributes.class (if tabTitle == activeTab then "is-active" else "")] 
                [a [Html.Events.onClick (SetActiveResponse (Just tabTitle))] [text tabTitle]]
            )
        
        ) 
      ]
    , 
    case maybeTabContents of
      Just tabContents ->
        webDataView (genericView False) tabContents
      
      Nothing ->
        text ""
    ]

webDataView : (a -> Html Msg) -> RemoteData.WebData a -> Html Msg
webDataView successView remoteData  =
  case remoteData of
      RemoteData.NotAsked ->
        text "Not Asked"

      RemoteData.Loading ->
        text "Loading..."
      
      RemoteData.Failure e ->
        text (Debug.toString e)

      RemoteData.Success a ->
        successView a

toUtcString : Time.Posix -> String
toUtcString time =
  String.fromInt (Time.toYear Time.utc time)
  ++ "-" ++
  (
  case (Time.toMonth Time.utc time) of
    Time.Jan -> "01"
    Time.Feb -> "02"
    Time.Mar -> "03"
    Time.Apr -> "04"
    Time.May -> "05"
    Time.Jun -> "06"
    Time.Jul -> "07"
    Time.Aug -> "08"
    Time.Sep -> "09"
    Time.Oct -> "10"
    Time.Nov -> "11"
    Time.Dec -> "12"
  )
  ++ "-" ++
  String.fromInt (Time.toDay Time.utc time)

genericView : Bool -> Generic.Value -> Html Msg
genericView displayAsTable genericValue =
  case genericValue of
      Generic.Null ->
        text "Null"
      
      Generic.Bool b ->
        text (if b then "✅" else "❌")
      
      Generic.Int i ->
        text (String.fromInt i)

      Generic.Float f ->
        text (String.fromFloat f)
      
      Generic.String str ->
        text str

      Generic.List listValue ->
        genericTableView listValue

      Generic.Set everySet ->
        text "TODO: No clue yet how to deal with this set type"

      Generic.Date posix ->
        text (toUtcString posix)

      Generic.DateTime posix ->
        text (toUtcString posix)

      Generic.Dict dictValueValue ->
        if displayAsTable then
          genericFieldView dictValueValue
        else
          genericFieldView dictValueValue


genericTableView : List Generic.Value -> Html Msg
genericTableView listValue =
  let headers =
        listValue
        |> List.head
        |> Maybe.map
          (\value -> 
            case value of

              Generic.Dict dictValueValue ->
                GenericDict.keys dictValueValue
                |> List.map (genericView False)
    
              _ ->
                []
          )
        |> Maybe.withDefault [(text "")]
      contents =
        listValue
        |> List.map
          (\value -> 
            case value of

              Generic.Dict dictValueValue ->
                GenericDict.values dictValueValue
                |> List.map (genericView False)
    
              _ ->
                []
          )
  in
    div 
      [Html.Attributes.class "table-container"]
      [ table [Html.Attributes.class "table is-striped"]
        [ thead []
          [tr []
            (headers |> List.map (\x -> th [] [x]))
          ]
        , tbody []
          ( contents 
            |> List.map 
                (\x -> 
                  tr [] 
                  (x |> List.map (\y -> td [] [y]))
                
                )
          )
        ]
      ]
genericFieldView : GenericDict.Dict Generic.Value Generic.Value -> Html Msg
genericFieldView dict =
  let keyValuePairs = GenericDict.toList dict
  in
    div 
      [Html.Attributes.class "table-container"]
      [ table [Html.Attributes.class "table is-striped"]
        [tbody 
          [] 
          ( keyValuePairs
            |> List.map 
              (\(k, v) -> 
                tr [] 
                  [ th [] [genericView True k]
                  , td [] [genericView True v]
                  ]
              )
          )
        ]
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
      [ class "button is-info is-light"
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
              [ button [ Html.Attributes.class "button is-success", Html.Events.onClick (SubmitForm (nameToString fieldType.name) fieldType.typeRef) ] [text "Run Query"]
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
-- sendRequest url request =
--   GraphQl.query request
--     |> GraphQl.Http.send { url = url, headers = [] } 
--       (\result -> let _ = Debug.log "result" result in NoOp)
--       (Generic.Json.decode)

sendRequest : String -> String -> GraphQl.Operation GraphQl.Query GraphQl.Anonymous -> Cmd Msg
sendRequest url formName operation =
  Http.post 
    { url = url
    , body = Http.jsonBody (operation |> GraphQl.query |> GraphQl.toJson) 
    , expect = Http.expectString (GotQueryResponse formName)
    }

runIntrospectionQuery : String -> Cmd Msg
runIntrospectionQuery url =
  Http.post 
    { url = url
    , body = Http.stringBody "application/json" introspectionQuery 
    , expect = Http.expectJson GotIntrospection (Json.Decode.field "data" Parser.decoder)
    }

submitForm : Model -> String -> Type.TypeReference -> Cmd Msg
submitForm model formName typeRef =
  let formValues = Dict.get formName model.formInput
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
      
      addArguments x = 
        List.foldl (\(fieldName, fieldValue) -> \y -> y |> GraphQl.withArgument fieldName (GraphQl.string fieldValue) ) x formValues
      request = 
        GraphQl.object 
          [ GraphQl.field formName
            |> addArguments
            |> typeRefToSelectors 0 model.types typeRef
          ]
  in
    case model.config of
      Nothing ->
        Cmd.none
      
      Just config ->
        sendRequest config.graphqlEndpoint formName request

typeRefToSelectors: Int -> Dict.Dict String Type.TypeDefinition -> Type.TypeReference -> GraphQl.Field a -> GraphQl.Field a
typeRefToSelectors depth dictTypeDef (Type.TypeReference referrableType isNullable) gqlField =
  gqlField
  |> case referrableType of
      Type.Scalar scalar -> -- Scalar doesn't need to be defined. It just returns the scalar value (bool, string, etc)
        \x -> x
      Type.List typeRef -> -- In GraphQL, the fact that it's a list doesn't matter when constructing a query
        typeRefToSelectors depth dictTypeDef typeRef
      Type.EnumRef className -> -- For all intents and purposes, enum can be treated as just another scalar
        \x -> x
      Type.ObjectRef objectName ->
        dictTypeDef
        |> Dict.get objectName
        |> Maybe.map
            (typeDefToSelectors depth dictTypeDef)
        |> Maybe.withDefault -- We should never get here because the referenced type should exist in the Dict
            (\x -> x)
      Type.UnionRef unionName ->
        GraphQl.withSelectors [ GraphQl.field "__typename"] -- TODO: Need to expand this. Lookup the union and do the proper ... on query
      _ ->
        GraphQl.withSelectors [ GraphQl.field "id" ]

typeDefToSelectors: Int -> Dict.Dict String Type.TypeDefinition -> Type.TypeDefinition -> GraphQl.Field a -> GraphQl.Field a
typeDefToSelectors depth dictTypeDef (Type.TypeDefinition classCaseName definableType _) gqlField =
  gqlField
  |> case definableType of
      Type.ScalarType ->
        (\x -> x)
      
      Type.ObjectType listOfField ->
        GraphQl.withSelectors 
          (listOfField 
            |> List.filterMap (typeFieldToGraphQlField (depth+1) dictTypeDef)
          )

      _ ->
        GraphQl.withSelectors [ GraphQl.field "id" ]

maxDepth : Int
maxDepth = 1
typeFieldToGraphQlField: Int -> Dict.Dict String Type.TypeDefinition -> Type.Field -> Maybe (GraphQl.Field a)
typeFieldToGraphQlField depth dictTypeDef typeField =
    if depth < maxDepth || (depth <= maxDepth && (not <| typeRefIsNested typeField.typeRef)) then
      GraphQl.field (nameToString typeField.name)
        |> typeRefToSelectors depth dictTypeDef typeField.typeRef
        |> Just
    else
      Nothing

typeRefIsNested : Type.TypeReference -> Bool
typeRefIsNested (Type.TypeReference referrableType isNullable) =
  case referrableType of 
    Type.Scalar _ ->
      False

    Type.List typeRef ->
      typeRefIsNested typeRef

    Type.EnumRef _ ->
      False

    Type.ObjectRef _ ->
      True

    Type.InputObjectRef _ ->
      True

    Type.UnionRef _ ->
      True

    Type.InterfaceRef _ ->
      True

-- Get fields from query
-- formName -> model -> typeRef
-- typeRef -> model -> GraphQl.Field a -> GraphQl.Field a 
--    i.e. this will do nested GraphQl.withSelectors and GraphQl.field

-- getTypeRefFromQuery : Model -> String -> Maybe Type.TypeReference
-- getTypeRefFromQuery model formName =
--   case model.introspection of
--       Err _ ->
--         Nothing
      
--       Ok apiInteractions ->
--         apiInteractions.queries
--         |> List.filterMap
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
--         |> List.head

-- CONSTS

introspectionQuery : String
introspectionQuery = """
{"query":"query IntrospectionQuery {    __schema {      queryType {        name      }      mutationType {        name      }      subscriptionType {        name      }      types {        ...FullType      }    }  }  fragment FullType on __Type {    kind    name    description    fields(includeDeprecated: false) {      name      description      args {        ...InputValue      }      type {        ...TypeRef      }      isDeprecated      deprecationReason    }    inputFields {      ...InputValue    }    interfaces {      ...TypeRef    }    enumValues(includeDeprecated: false) {      name      description      isDeprecated      deprecationReason    }    possibleTypes {      ...TypeRef    }  }  fragment InputValue on __InputValue {    name    description    type { ...TypeRef }    defaultValue  }  fragment TypeRef on __Type {    kind    name    ofType {      kind      name      ofType {        kind        name        ofType {          kind          name          ofType {            kind            name            ofType {              kind              name              ofType {                kind                name                ofType {                  kind                  name                }              }            }          }        }      }    }  }","variables":null,"operationName":"IntrospectionQuery"}
"""