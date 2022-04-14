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
import Json.Decode exposing (field)
import Set
import List.Unique

-- TYPES
type alias Flags = Json.Decode.Value

type alias Config = 
  { graphqlEndpoint: String
  , debugMode: Bool
  , buttonConfig: ButtonConfig
  }

type alias ButtonConfig = List SingleButtonConfig
type alias SingleButtonConfig =
  { displayName: String -- "Add Product"
  , context: String -- products.data.products
  , fields: List FieldMapping -- mapping from field within the context to the form field
  , formToDisplay: String -- addProduct
  }

type alias FieldMapping =
  { inputField: String -- dataset
  , formField: String -- addProduct.input.dataset
  }

type alias Model = 
  { config: Result Json.Decode.Error Config
  , introspection: RemoteData.WebData ApiInteractions
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
  | ConfigurableButtonClick SingleButtonConfig (GenericDict.Dict Generic.Value Generic.Value) 
  | UpdateFormAt String ArgumentType (Maybe String)
  | SubmitForm String Type.TypeReference
  | SetActiveForm (Maybe String)
  | GotQueryResponse String (Result Http.Error String)
  | SetActiveResponse (Maybe String)

type QueryArgument
  = QueryLeaf String ArgumentType
  | QueryNested (Dict.Dict String QueryArgument)
  | QueryList (List QueryArgument)

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
    , introspection = RemoteData.NotAsked
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

flagsToMaybeConfig : Flags -> Result Json.Decode.Error Config
flagsToMaybeConfig flags =
  Json.Decode.decodeValue
  ( Json.Decode.map3 Config
    (Json.Decode.field "graphqlEndpoint" Json.Decode.string)
    (Json.Decode.field "debugMode" Json.Decode.bool)
    (Json.Decode.field "buttonConfig" decodeButtonConfig)
  )
  flags

decodeButtonConfig : Json.Decode.Decoder ButtonConfig
decodeButtonConfig =
  Json.Decode.list
  ( Json.Decode.map4 SingleButtonConfig
    (Json.Decode.field "displayName" Json.Decode.string)
    (Json.Decode.field "context" Json.Decode.string)
    (Json.Decode.field "fields" decodeFieldMapping)
    (Json.Decode.field "formToDisplay" Json.Decode.string)
  )

decodeFieldMapping : Json.Decode.Decoder (List FieldMapping)
decodeFieldMapping =
  Json.Decode.list
  ( Json.Decode.map2 FieldMapping
    (Json.Decode.field "inputField" Json.Decode.string)
    (Json.Decode.field "formField" Json.Decode.string)
  )

defaultConfig : String -> Config
defaultConfig endpoint =
  Config
    endpoint
    True
    []

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    UpdateEndpoint url ->
      let maybeConfig = model.config
          newConfig = case maybeConfig of
                        Err _ ->
                          Ok (defaultConfig url)
                        
                        Ok config ->
                          Ok {config | graphqlEndpoint = url}
      in
        ( { model 
          | config = newConfig
          }
        , Cmd.none
        )
    
    HitEndpoint ->
      case model.config of
        Err _ ->
          (model, Cmd.none)
        
        Ok config ->
          (model, runIntrospectionQuery config.graphqlEndpoint)

    GotIntrospection apiInteractionsResult ->
      ( { model 
        | introspection = RemoteData.fromResult apiInteractionsResult
        , queries = apiInteractionsToFieldDict apiInteractionsResult .queries
        , mutations = apiInteractionsToFieldDict apiInteractionsResult .mutations
        , types = apiInteractionsToTypeDict apiInteractionsResult
        }
      , Cmd.none
      )

    ConfigurableButtonClick singleButtonConfig dictValueValue ->
      ( { model
        | activeForm = Just singleButtonConfig.formToDisplay
        , formInput = updateFormFromConfig singleButtonConfig dictValueValue model.types model.formInput
        }
      , Cmd.none
      )

    UpdateFormAt path argumentType maybeFormValue ->
      let _ = Debug.log "UpdateFormAt path" path
      in
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
              
              QueryList _ ->
                queryArgument
        )
        baseQueryArgument
    |> queryArgumentToMaybeString


isDebug : Result Json.Decode.Error Config -> Bool
isDebug resultConfig =
  resultConfig
  |> Result.map (\x -> x.debugMode)
  |> Result.withDefault True

queryArgumentToMaybeString : QueryArgument -> Maybe String
queryArgumentToMaybeString queryArgument =
        case queryArgument of
            QueryLeaf value argumentType ->
              Just value
            
            QueryNested _ ->
              Nothing
            
            QueryList queryList ->
              queryList
              |> List.map (\x -> queryArgumentToMaybeString x |> Maybe.withDefault "")
              |> String.join ","
              |> Just


updateFormFromConfig : SingleButtonConfig -> (GenericDict.Dict Generic.Value Generic.Value) -> Dict.Dict String Type.TypeDefinition -> (Dict.Dict String (Dict.Dict String QueryArgument)) -> (Dict.Dict String (Dict.Dict String QueryArgument))
updateFormFromConfig singleButtonConfig context types formDict =
  -- For each field in the config
  --  Get the value from the context
  --  Set the value based on the path
  -- How do we get the ArgumentType though?
  --    typeRefToArgumentType : Type.TypeReference -> ArgumentType
  --    model.types : Dict.Dict String Type.TypeDefinition
  -- Need a function that takes path and model.types and returns ArgumentType
  singleButtonConfig.fields
  |> List.map 
      (\field ->
        ( Debug.log "getValueAt" <| getValueAt field.inputField context -- Maybe String
        , field.formField -- String (i.e. addProduct.input.dataset)
        , getArgumentTypeAt (singleButtonConfig.context ++ "." ++ field.inputField) types -- ArgumentType
        )
      )
  |> List.foldl 
      (\(maybeFormValue, formField, argumentType) -> \dict ->
        updateFormAt formField argumentType maybeFormValue dict
      )
      formDict
  
--   formDict

getValueAt : String -> GenericDict.Dict Generic.Value Generic.Value -> Maybe String
getValueAt field context =
  let fieldWithoutDot = String.replace "." "" field -- in case there's an errant dot, remove it
  in
    GenericDict.get genericValueToStringWithQuotes (Generic.String (fieldWithoutDot)) context
    |> Maybe.map genericValueToStringWithoutQuotes


getArgumentTypeAt : String -> Dict.Dict String Type.TypeDefinition -> ArgumentType
getArgumentTypeAt path types =
  ArgumentString -- TODO: Just a placeholder. Look up the real type.


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
                        Nothing -> -- Insert using empty Dict. TODO: This should insert a list if the path contains a [
                          Just (QueryNested (nestedQueryArgumentDictUpdate tail argumentType maybeFormValue Dict.empty))
                        
                        Just queryArgument ->
                          case queryArgument of
                            QueryLeaf _  _ -> -- THIS SHOULD BE AN IMPOSSIBLE PATH BECAUSE there's more to update
                              Just (QueryNested Dict.empty)
                            
                            QueryNested dictToUpdate ->
                              Just (QueryNested (nestedQueryArgumentDictUpdate tail argumentType maybeFormValue dictToUpdate))

                            QueryList listQueryArgument ->
                              Just (QueryList []) -- TODO: We should actually have a [] in the path and branch on the appropriate array index
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
  div [Html.Attributes.class "container", Html.Attributes.style "max-width" "calc(100vw - 30px)"]
      [ case model.introspection of
          RemoteData.NotAsked ->
            configView model.config
          
          _ ->
            webDataView (\_ -> text "") model.introspection
      , if isDebug model.config then
          pre [] [text (Debug.toString model.formInput)]
        else
          text ""
      , h1 [Html.Attributes.class "title"] [text "Responses"]
      , case model.config of
          Ok config ->
            tabView config.buttonConfig model.activeResponse model.response
          
          Err _ ->
            text "Unable to parse config"
      , apiView model
      ]

configView : Result Json.Decode.Error Config -> Html Msg
configView resultConfig =
  div [ class "modal is-active" ]
      [ div [ class "modal-background" ]
          []
      , div [ class "modal-card", Html.Attributes.style "width" "calc(100vw - 30px)"] 
          [ header [ class "modal-card-head" ]
              [ p [ class "modal-card-title" ]
                  [ text "App Configuration" ]
              , button [ class "delete", Html.Events.onClick (SetActiveForm Nothing) ]
                  []
              ]
          , section [ class "modal-card-body" ]
              [   div
                  [Html.Attributes.class "field is-horizontal"]
                  [ div [Html.Attributes.class "field-label is-normal"] [label [Html.Attributes.class "label"] [text "GraphQL Endpoint"]]
                  , div 
                    [ Html.Attributes.class "field-body field has-addons" ]
                    [ div 
                      [ Html.Attributes.class "control" ] 
                      [ input 
                        [ Html.Attributes.class "input"
                        , Html.Attributes.type_ "text"
                        , Html.Attributes.value (resultConfig |> Result.toMaybe |> Maybe.map .graphqlEndpoint |> Maybe.withDefault "")
                        , Html.Events.onInput UpdateEndpoint
                        ]
                        [] 
                      ]
                    ]
                  ]
              , case resultConfig of
                  Err e ->
                    pre [] [text <| Json.Decode.errorToString e]
                  
                  Ok config ->
                    pre [] [text (Debug.toString config)]
              ]
          , footer [ class "modal-card-foot" ]
              [ button [Html.Attributes.class "button is-success" 
                      , Html.Events.onClick HitEndpoint ] [text "Introspect!"]
              ]
          ]
      ]


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
      , if isDebug model.config then
          div []
          [ h1 [Html.Attributes.class "title"] [text "Available Types"]
          , ul [] baseTypes
          ]
        else
          text ""
      ]

tabView : ButtonConfig -> Maybe String -> Dict.Dict String (RemoteData.WebData Generic.Value) -> Html Msg
tabView buttonConfig maybeActiveTab dict =
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
        webDataView (genericView buttonConfig activeTab False) tabContents
      
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
      
      RemoteData.Failure error ->
        httpErrorView error

      RemoteData.Success a ->
        successView a

httpErrorView : Http.Error -> Html Msg
httpErrorView error =
  (case error of
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
  ) |>
    \x ->
      div
        [ Html.Attributes.class "notification is-danger" ]
        [ x ]

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

genericValueToStringWithoutQuotes : Generic.Value -> String
genericValueToStringWithoutQuotes genericValue =
  case genericValue of
      Generic.String str ->
        str
      
      _ ->
        "TODO: Unhandled type"


genericValueToStringWithQuotes : Generic.Value -> String
genericValueToStringWithQuotes genericValue =
  case genericValue of
      Generic.String str ->
        "\"" ++ str ++ "\""
      
      _ ->
        "TODO: Unhandled type"

displayButton : ButtonConfig -> String -> GenericDict.Dict Generic.Value Generic.Value -> List (Html Msg)
displayButton buttonConfig path context =
  let _ = Debug.log "context" context
  in
    buttonConfig
    |> List.filter (\x -> x.context == path)
    |> List.map (
        \x -> 
          button 
          [ Html.Attributes.class "button is-primary" 
          , Html.Events.onClick (ConfigurableButtonClick x context)
          ] 
          [ text x.displayName ]
        )

genericView : ButtonConfig -> String -> Bool -> Generic.Value -> Html Msg
genericView buttonConfig path displayAsTable genericValue =
    case genericValue of
        Generic.Null ->
          span [class "tag is-warning"] [text "Null"]
        
        Generic.Bool b ->
          text (if b then "✅" else "❌")
        
        Generic.Int i ->
          text (String.fromInt i)

        Generic.Float f ->
          text (String.fromFloat f)
        
        Generic.String str ->
          text str

        Generic.List listValue ->
          genericTableView buttonConfig path listValue

        Generic.Set everySet ->
          text "TODO: No clue yet how to deal with this set type"

        Generic.Date posix ->
          text (toUtcString posix)

        Generic.DateTime posix ->
          text (toUtcString posix)

        Generic.Dict dictValueValue ->
          if displayAsTable then
            genericFieldView buttonConfig path dictValueValue
          else
            genericFieldView buttonConfig path dictValueValue


genericTableView : ButtonConfig -> String -> List Generic.Value -> Html Msg
genericTableView buttonConfig path listValue =
  let buttons = displayButton buttonConfig path
      allFields = 
        listValue 
        |> List.map 
            (\value ->
                case value of
                    Generic.Dict dictValueValue ->
                      GenericDict.keys dictValueValue
                    
                    _ ->
                      []
            )
        |> List.concat
        |> List.Unique.filterDuplicates
      headers =
        allFields
        |> List.map (genericView buttonConfig path False)
        -- listValue
        -- |> List.head
        -- |> Maybe.map
        --   (\value -> 
        --     case value of

        --       Generic.Dict dictValueValue ->
        --         GenericDict.keys dictValueValue
        --         |> List.map (genericView buttonConfig path False)
    
        --       _ ->
        --         []
        --   )
        -- |> Maybe.withDefault [(text "")]
      contents =
        listValue
        |> List.map
          (\value -> 
            case value of

              Generic.Dict dictValueValue ->
                -- GenericDict.toList dictValueValue
                -- |> List.map (\(k, v) -> genericView buttonConfig (path++"."++(genericValueToString k)) False v)
                -- |> \x -> x++(buttons dictValueValue)
                allFields
                |> List.map 
                    (\k ->
                      dictValueValue
                      |> GenericDict.get genericValueToStringWithQuotes k
                      |> Maybe.map (\v -> genericView buttonConfig (path++"."++(genericValueToStringWithoutQuotes k)) False v)
                      |> Maybe.withDefault (span [Html.Attributes.class "tag is-light"] [text "N/A"])
                    )
                |> \x -> x++(buttons dictValueValue)
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
genericFieldView : ButtonConfig -> String -> GenericDict.Dict Generic.Value Generic.Value -> Html Msg
genericFieldView buttonConfig path dict =
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
                  [ th [] [genericView buttonConfig (path++"."++(genericValueToStringWithoutQuotes k)) True k]

                  , td [] [genericView buttonConfig (path++"."++(genericValueToStringWithoutQuotes k)) True v]
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
      [ Html.Attributes.class "table is-fullwidth is-hoverable is-narrow is-bordered"]
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

          Type.List listTypeRef ->
            inputFromTypeRef (path++"[") dictTypeDef formDict listTypeRef -- TODO: Probably need to add [] to the path

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
      Err _ ->
        Cmd.none
      
      Ok config ->
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

      QueryList listQueryArgument ->
        listQueryArgument
        |> List.map
           (\x ->
              case x of
                  QueryLeaf str _ -> -- TODO: Handle the non-string values
                    str
                  
                  _ ->
                    "TODO" -- TODO: Handle the nested types
          )
        |> List.map (\x -> "\"" ++ x ++ "\"")
        |> String.join ", "
        |> (\x -> "[ " ++ x ++ " ]")
        |> GraphQl.type_

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
        dictTypeDef
        |> Dict.get unionName
        |> Maybe.map
            (\typeDef ->
              case typeDef of
                Type.TypeDefinition _ definableType _ ->
                  case definableType of
                      Type.UnionType listUnionClassCaseName ->
                        GraphQl.withSelectors 
                          ( (GraphQl.field "__typename")
                          ::
                          ( listUnionClassCaseName
                            |> List.map
                                (\unionClassCaseName ->
                                    GraphQl.field ("... on " ++ Graphql.Parser.ClassCaseName.raw unionClassCaseName)
                                    |> typeRefToSelectors depth dictTypeDef (Type.TypeReference (Type.ObjectRef (Graphql.Parser.ClassCaseName.raw unionClassCaseName)) Type.NonNullable) -- Treat this as an object ref. TODO: Is this ok for scalars?? We don't enforce that ObjectRef must go to ObjectType so it might be ok?
                                )
                          )   
                          )
                      
                      _ ->
                        GraphQl.withSelectors [ GraphQl.field "__typename"]                      
            )
        |> Maybe.withDefault (GraphQl.withSelectors [ GraphQl.field "__typename"])
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