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
import Graphql.Parser.Scalar exposing (Scalar(..))
import GraphQl exposing (query)

-- TYPES
type alias Flags = Json.Decode.Value

type alias Config = 
  { graphqlEndpoint: String
  }

type alias Model = 
  { config: Maybe Config
  , introspection: Result Http.Error ApiInteractions
  , queries: Dict.Dict String Type.Field
  , mutations: Dict.Dict String Type.Field
  , types: Dict.Dict String Type.TypeDefinition
  , formInput: Dict.Dict String (Dict.Dict String QueryArgument)
  , activeForm: Maybe String
  , response: Dict.Dict String (RemoteData.WebData Generic.Value)
  , activeResponse: Maybe String
  }

type Msg
  = NoOp
  | UpdateEndpoint String
  | HitEndpoint
  | GotIntrospection (Result Http.Error ApiInteractions)
  | UpdateFormAt String ArgumentType (Maybe String)
  | SubmitForm String Type.TypeReference
  | SetActiveForm (Maybe String)
  | GotQueryResponse String (Result Http.Error String)
  | SetActiveResponse (Maybe String)

type QueryArgument -- TODO: Need to handle lists here
  = QueryLeaf String ArgumentType
  | QueryNested (Dict.Dict String QueryArgument)

type ArgumentType
  = ArgumentString
  | ArgumentEnum

-- MAIN

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- INIT

init : Flags -> ( Model, Cmd Msg )
init flags =
  ( { config = flagsToMaybeConfig flags 
    , introspection = Result.Err <| Http.BadUrl "Not Asked Yet -- TODO: Change this to another type"
    , queries = Dict.empty
    , mutations = Dict.empty
    , types = Dict.empty
    , formInput = Dict.empty
    , activeForm = Nothing
    , response = Dict.empty
    , activeResponse = Nothing
    }
  , Cmd.none
  )

flagsToMaybeConfig : Flags -> Maybe Config
flagsToMaybeConfig flags =
  Json.Decode.decodeValue
  (Json.Decode.field "graphql_endpoint" Json.Decode.string |> Json.Decode.map Config)
  flags
  |> Result.toMaybe

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case Debug.log "msg" msg of
    NoOp ->
      ( model, Cmd.none )

    UpdateEndpoint url ->
      let maybeConfig = model.config
          newConfig = case maybeConfig of
                        Nothing ->
                          Just { graphqlEndpoint = url }
                        
                        Just config ->
                          Just {config | graphqlEndpoint = url}
      in
        ( { model 
          | config = newConfig
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

    UpdateFormAt path argumentType maybeFormValue ->
      ( { model 
        | formInput = updateFormAt path argumentType maybeFormValue model.formInput
        } 
      , Cmd.none
      )

    SubmitForm formName typeRef ->
      ( { model 
        | response = model.response |> Dict.insert formName (RemoteData.Loading)
        , activeForm = Nothing
        , activeResponse = Just formName
        } 
      , let maybeQuery = Dict.get formName model.queries 
            maybeMutation = Dict.get formName model.mutations 
        in
          case maybeQuery of
            Nothing ->
              case maybeMutation of
                  Nothing ->
                    Cmd.none -- We should never get here
                  Just mutation ->
                    submitForm False model formName typeRef
            Just query ->
              submitForm True model formName typeRef
      )

    SetActiveForm maybeForm ->
      ( {model | activeForm = maybeForm }, Cmd.none)

    SetActiveResponse maybeResponse ->
      ( {model | activeResponse = maybeResponse }, Cmd.none)

    GotQueryResponse key result ->
      let genericValue = Debug.log "genericValue" (Result.andThen (\x -> Result.mapError Http.BadBody (Generic.Json.decode x)) result ) |> RemoteData.fromResult
      in
        ( { model | response = model.response |> Dict.insert key genericValue} , Cmd.none)

getFormAt : String -> (Dict.Dict String (Dict.Dict String QueryArgument)) -> Maybe String
getFormAt path formDict =
  let pathList = String.split "." path -- List String
      formName = Maybe.withDefault "" (List.head pathList) -- String
      pathToGet = List.tail pathList |> Maybe.withDefault [] -- List String
      baseQueryArgument  = formDict                          -- QueryArgument
                            |> Dict.get formName
                            |> Maybe.withDefault Dict.empty
                            |> QueryNested
  in
    pathToGet
    |> List.foldl
        (\str -> \queryArgument ->
          case queryArgument of
              QueryLeaf _ _ ->
                queryArgument

              QueryNested dict ->
                dict
                |> Dict.get str
                |> Maybe.withDefault (QueryNested Dict.empty)
        )
        baseQueryArgument
    |> \queryArgument ->
        case queryArgument of
            QueryLeaf value argumentType ->
              Just value
            
            QueryNested _ ->
              Nothing


updateFormAt : String -> ArgumentType -> Maybe String -> (Dict.Dict String (Dict.Dict String QueryArgument)) -> (Dict.Dict String (Dict.Dict String QueryArgument))
updateFormAt path argumentType maybeFormValue formDict =
  let pathList = String.split "." path -- List String
      formName = Maybe.withDefault "" (List.head pathList) -- String
      pathToUpdate = List.tail pathList |> Maybe.withDefault [] -- List String
        
  in 
    formDict
    |> Dict.update formName
      (\maybeDict ->
        maybeDict
        |> Maybe.withDefault Dict.empty
        |> nestedQueryArgumentDictUpdate pathToUpdate argumentType maybeFormValue
        |> Just
      )

nestedQueryArgumentDictUpdate : List String -> ArgumentType -> Maybe String -> (Dict.Dict String QueryArgument) -> (Dict.Dict String QueryArgument)
nestedQueryArgumentDictUpdate path argumentType maybeFormValue queryArgumentDict =
  case Debug.log "List.head path" <| List.head path of
      Nothing ->
        queryArgumentDict

      Just head ->
        let tail = path |> List.tail |> Maybe.withDefault []
        in
          if List.length tail == 0 then
              queryArgumentDict
              |> Dict.update head
                  (\_ ->
                    maybeFormValue |> Maybe.map (\x -> QueryLeaf x argumentType)
                  )
          else
              queryArgumentDict
              |> Dict.update head
                  (\maybeQueryArgument ->
                    case maybeQueryArgument of
                        Nothing -> -- Insert using empty Dict
                          Just (QueryNested (nestedQueryArgumentDictUpdate tail argumentType maybeFormValue Dict.empty))
                        
                        Just queryArgument ->
                          case queryArgument of
                            QueryLeaf _  _ -> -- THIS SHOULD BE AN IMPOSSIBLE PATH BECAUSE there's more to update
                              Just (QueryNested Dict.empty)
                            
                            QueryNested dictToUpdate ->
                              Just (QueryNested (nestedQueryArgumentDictUpdate tail argumentType maybeFormValue dictToUpdate))
                  )

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
      [ configView model.config
      , pre [] [text (Debug.toString model.formInput)]
      , h1 [Html.Attributes.class "title"] [text "Responses"]
      , tabView model.activeResponse model.response
      , errorView model.introspection
      , apiView model
      ]

configView : Maybe Config -> Html Msg
configView maybeConfig =
  div
    [Html.Attributes.class "field is-horizontal"]
    [ div [Html.Attributes.class "field-label is-normal"] [label [Html.Attributes.class "label"] [text "GraphQL Endpoint"]]
    , div 
      [ Html.Attributes.class "field-body field has-addons" ]
      [ div 
        [ Html.Attributes.class "control" ] 
        [ input 
          [ Html.Attributes.class "input"
          , Html.Attributes.type_ "text"
          , Html.Attributes.value (maybeConfig |> Maybe.map .graphqlEndpoint |> Maybe.withDefault "")
          , Html.Events.onInput UpdateEndpoint
          ]
          [] 
        ]
      , div 
        [ Html.Attributes.class "control" ] 
        [ button [Html.Attributes.class "button is-success" 
        , Html.Events.onClick HitEndpoint ] [text "Introspect!"]
        ]
      ]
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
            (Dict.union model.queries model.mutations)
            |> Dict.get activeForm
            |> Maybe.map (formModal model.types model.formInput)
            |> Maybe.withDefault (text "Something went wrong -- the activeForm wasn't found in the queries")
          )
        |> Maybe.withDefault (text "")
      mutations =
        model.mutations
        |> Dict.toList
        |> List.map (\(_, queryField) -> fieldTypeToButton queryField)

      baseTypes =
        model.types
        |> Dict.toList
        |> List.map Debug.toString
        |> List.map (\x -> li [] [ pre [] [text x]])

  in
    div []
      [ maybeForm
      , h1 [Html.Attributes.class "title"] [text "Queries"]
      , div [Html.Attributes.class "buttons"] queries
      , h1 [Html.Attributes.class "title"] [text "Mutations"]
      , div [Html.Attributes.class "buttons"] mutations
      , h1 [Html.Attributes.class "title"] [text "Available Types"]
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
        progress [Html.Attributes.class "progress is-large is-info", Html.Attributes.max "100"] [text ""]
      
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

fieldArgumentsToForm : String -> Dict.Dict String Type.TypeDefinition -> Dict.Dict String (Dict.Dict String QueryArgument) -> Type.Field -> Html Msg
fieldArgumentsToForm path dictTypeDef formDict fieldType =
  div
    [ Html.Attributes.class "table-container" ]
    [ table 
      [ Html.Attributes.class "table is-hoverable is-narrow is-bordered"]
      [ tbody 
        []
        (List.map (argToFormField path dictTypeDef formDict) fieldType.args)
      ] 
    ]

formModal : Dict.Dict String Type.TypeDefinition -> Dict.Dict String (Dict.Dict String QueryArgument) -> Type.Field -> Html Msg
formModal dictTypeDef formDict fieldType = 
  div [ class "modal is-active" ]
      [ div [ class "modal-background" ]
          []
      , div [ class "modal-card", Html.Attributes.style "width" "calc(100vw - 30px)"] 
          [ header [ class "modal-card-head" ]
              [ p [ class "modal-card-title" ]
                  [ text (nameToString fieldType.name) ]
              , button [ class "delete", Html.Events.onClick (SetActiveForm Nothing) ]
                  []
              ]
          , section [ class "modal-card-body" ]
              [ text (Maybe.withDefault "" fieldType.description)
              , fieldArgumentsToForm (nameToString fieldType.name) dictTypeDef formDict fieldType 
              ]
          , footer [ class "modal-card-foot" ]
              [ button [ Html.Attributes.class "button is-success", Html.Events.onClick (SubmitForm (nameToString fieldType.name) fieldType.typeRef) ] [text "Run Query"]
              , button [ class "button", Html.Events.onClick (SetActiveForm Nothing) ]
                  [ text "Cancel" ]
              ]
          ]
      ]

argToFormField : String -> Dict.Dict String Type.TypeDefinition -> Dict.Dict String (Dict.Dict String QueryArgument) -> Type.Arg  -> Html Msg
argToFormField pathPrefix dictTypeDef formDict arg =
    tr
      []
      [ th [] [text (nameToString arg.name), br [] [], text (Maybe.withDefault "" arg.description)]
      , td [] 
        [ inputFromTypeRef (pathPrefix ++ "." ++ (nameToString arg.name)) dictTypeDef formDict arg.typeRef
        ]
      ]
    -- TODO: Show object nesting
    -- TODO: Switch on types, including object types
    -- TODO: The input value needs to be available in this context for pre-population as well as deciding to set/unset nullable values
    -- TODO: Nullable types should have a "+" button when null, and an "x" button to set null

typeDefToForm : String -> Dict.Dict String Type.TypeDefinition -> Dict.Dict String (Dict.Dict String QueryArgument) -> Type.TypeDefinition -> Html Msg
typeDefToForm path dictTypeDef formDict (Type.TypeDefinition classCaseName definableType maybeDescription) =
  case definableType of
    Type.InputObjectType listOfField ->
      List.map (fieldToRowInput (path) dictTypeDef formDict) listOfField
      |> div []
    _ ->
      text "TODO: Handle Other Types of Type Definitions in the Input"
        
fieldToRowInput : String -> Dict.Dict String Type.TypeDefinition -> Dict.Dict String (Dict.Dict String QueryArgument) -> Type.Field -> Html Msg
fieldToRowInput path dictTypeDef formDict fieldType =
  tr 
    []
    [ td [] 
      [ b [] [text (nameToString fieldType.name)]
      , br [] []
      , text (Maybe.withDefault "" fieldType.description)
      ]
    , td [] [inputFromTypeRef (path++"."++(nameToString fieldType.name)) dictTypeDef formDict fieldType.typeRef]
    ]

inputFromTypeRef : String -> Dict.Dict String Type.TypeDefinition -> Dict.Dict String (Dict.Dict String QueryArgument) -> Type.TypeReference -> Html Msg
inputFromTypeRef path dictTypeDef formDict typeRef =
  let currentValue = getFormAt path formDict
      inputHtml refType =
        case refType of
          Type.Scalar _ ->          
            input [ Html.Attributes.class "input"
                  , Html.Attributes.type_ "text"
                  , Html.Events.onInput (\x -> UpdateFormAt path (typeRefToArgumentType typeRef) (Just x))
                  , Html.Attributes.value ( Maybe.withDefault "" currentValue)
                  ] []

          Type.EnumRef classCaseName ->
            let availableValues =
                  dictTypeDef
                  |> Dict.get (Graphql.Parser.ClassCaseName.raw classCaseName)
                  |> Maybe.map
                      (\x ->
                        case x of
                            Type.TypeDefinition _ definableType maybeDescription ->
                              case definableType of
                                  Type.EnumType listEnumValue ->
                                    listEnumValue |> List.map (\y -> Graphql.Parser.ClassCaseName.raw y.name)
                                  _ ->
                                    []
                      )
                  |> Maybe.withDefault []
            in
              div [Html.Attributes.class "select"]
              [ select [Html.Events.onInput (\a -> UpdateFormAt path (typeRefToArgumentType typeRef) (Just a))]
                ( availableValues
                |> List.map (\z -> option [Html.Attributes.value z] [text z])
                )
              ]

          Type.InputObjectRef objectClassCaseName ->
            let objectName = (Graphql.Parser.ClassCaseName.raw objectClassCaseName)
            in
              dictTypeDef
              |> Dict.get objectName
              |> Maybe.map (typeDefToForm path dictTypeDef formDict)
              |> Maybe.withDefault (text (objectName ++ " not found in the Dict of all objects"))

          _ ->
            text "TODO: Implement this input type"
      
  in
    case typeRef of
        Type.TypeReference referrableType isNullable ->
          case isNullable of
            Type.Nullable ->
              div
                [Html.Attributes.class "field has-addons"]
                [ div 
                  [ Html.Attributes.class "control" ] 
                  [ case currentValue of
                      Nothing -> -- No value set for this nullable type
                        button 
                          [ Html.Attributes.class "button is-info"
                          , Html.Events.onClick (UpdateFormAt path (typeRefToArgumentType typeRef) (Just "")) -- TODO: Do we need to send a better type here for nested inputs?
                          ] 
                          [ text "Add Optional Value" 
                          ]
                      Just _ ->
                        inputHtml referrableType
                  ]
                , case currentValue of
                    Nothing ->
                      text ""
                
                    Just _ ->
                      div [Html.Attributes.class "control"] 
                        [ a [class "button is-warning"
                        , Html.Events.onClick (UpdateFormAt path (typeRefToArgumentType typeRef) Nothing)
                        ] [text "Clear"]
                        ]
                ]
            Type.NonNullable ->
              inputHtml referrableType

typeRefToArgumentType : Type.TypeReference -> ArgumentType
typeRefToArgumentType (Type.TypeReference referrableType isNullable) =
  case referrableType of
      Type.Scalar _ ->
        ArgumentString
      
      Type.EnumRef _ ->
        ArgumentEnum
      
      _ -> -- TODO: Fill out for all types
        ArgumentString 

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
            [Html.Attributes.type_ "radio", Html.Attributes.name "nullable"] []
          , text "Null"
          ]
        , label 
          [Html.Attributes.class "radio"] 
          [ input 
            [Html.Attributes.type_ "radio", Html.Attributes.name "nullable"] []
          , text "Value"
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

sendQueryRequest : String -> String -> GraphQl.Operation GraphQl.Query GraphQl.Anonymous -> Cmd Msg
sendQueryRequest url formName operation =
  Http.post 
    { url = url
    , body = Http.jsonBody (operation |> GraphQl.query |> GraphQl.toJson) 
    , expect = Http.expectString (GotQueryResponse formName)
    }

sendMutationRequest : String -> String -> GraphQl.Operation GraphQl.Mutation GraphQl.Anonymous -> Cmd Msg
sendMutationRequest url formName operation =
  Http.post 
    { url = url
    , body = Http.jsonBody (operation |> GraphQl.mutation |> GraphQl.toJson) 
    , expect = Http.expectString (GotQueryResponse formName)
    }

runIntrospectionQuery : String -> Cmd Msg
runIntrospectionQuery url =
  Http.post 
    { url = url
    , body = Http.stringBody "application/json" introspectionQuery 
    , expect = Http.expectJson GotIntrospection (Json.Decode.field "data" Parser.decoder)
    }

submitForm : Bool -> Model -> String -> Type.TypeReference -> Cmd Msg
submitForm isQuery model formName typeRef =
  let formValues = Dict.get formName model.formInput
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
      
      addArguments x = 
        List.foldl (\(fieldName, fieldValue) -> \y -> y |> GraphQl.withArgument fieldName (queryArgumentToGraphQlAgument fieldValue) ) x formValues
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
        if isQuery then
          sendQueryRequest config.graphqlEndpoint formName request
        else
          sendMutationRequest config.graphqlEndpoint formName request

queryArgumentToGraphQlAgument : QueryArgument -> GraphQl.Argument
queryArgumentToGraphQlAgument queryArgument =
  case queryArgument of
      QueryLeaf str argumentType ->
        case argumentType of
            ArgumentString ->
              GraphQl.string str
            
            ArgumentEnum ->
              GraphQl.type_ str

      QueryNested dictStringQueryArgument ->
        dictStringQueryArgument
        |> Dict.map (\_ -> \v -> queryArgumentToGraphQlAgument v)
        |> Dict.toList
        |> GraphQl.input

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
      _ -> -- TODO: Are other types even semantically possible in this context?
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