module Graphql.Generator.Types exposing (..)

import Graphql.Parser.Type as Type
import Graphql.Parser.ClassCaseName
import Graphql.Generator.Context exposing (Context)
import Dict exposing (Dict)
import Html exposing (..)

-- generate : IntrospectionData -> List String
generate { typeDefinitions, queryObjectName, mutationObjectName, subscriptionObjectName } =
    let
        context : Context
        context =
            { query = Graphql.Parser.ClassCaseName.build queryObjectName
            , mutation = mutationObjectName |> Maybe.map Graphql.Parser.ClassCaseName.build
            , subscription = subscriptionObjectName |> Maybe.map Graphql.Parser.ClassCaseName.build
            , interfaces = interfacePossibleTypesDict typeDefinitions
            }

        definitionsWithExclusions =
            typeDefinitions
                |> excludeBuiltIns
                |> excludeQuery context
                |> excludeMutation context
                |> excludeSubscription context
    in
        typeDefinitions
            |> excludeBuiltIns
            |> List.map (toViews context)
                -- (\x -> 
                --     case x of
                --         Type.TypeDefinition classCaseName _ maybeDescription ->
                --             Graphql.Parser.ClassCaseName.raw classCaseName 
                -- )

type alias IntrospectionData =
    { typeDefinitions : List Type.TypeDefinition
    , queryObjectName : String
    , mutationObjectName : Maybe String
    , subscriptionObjectName : Maybe String
    }


sortedIntrospectionData : List Type.TypeDefinition -> String -> Maybe String -> Maybe String -> IntrospectionData
sortedIntrospectionData typeDefinitions queryObjectName mutationObjectName subscriptionObjectName =
    { typeDefinitions = typeDefinitions |> List.sortBy typeDefName
    , queryObjectName = queryObjectName
    , mutationObjectName = mutationObjectName
    , subscriptionObjectName = subscriptionObjectName
    }


typeDefName : Type.TypeDefinition -> String
typeDefName (Type.TypeDefinition name definableType description) =
    Graphql.Parser.ClassCaseName.normalized name

interfacePossibleTypesDict : List Type.TypeDefinition -> Dict String (List Graphql.Parser.ClassCaseName.ClassCaseName)
interfacePossibleTypesDict typeDefs =
    typeDefs
        |> List.filterMap
            (\(Type.TypeDefinition typeName definableType description) ->
                case definableType of
                    Type.InterfaceType fields possibleTypes ->
                        Just ( Graphql.Parser.ClassCaseName.raw typeName, possibleTypes )

                    _ ->
                        Nothing
            )
        |> Dict.fromList

excludeBuiltIns : List Type.TypeDefinition -> List Type.TypeDefinition
excludeBuiltIns typeDefinitions =
    typeDefinitions
        |> List.filter
            (\(Type.TypeDefinition name definableType description) ->
                not (Graphql.Parser.ClassCaseName.isBuiltIn name)
            )


excludeQuery : Context -> List Type.TypeDefinition -> List Type.TypeDefinition
excludeQuery { query } typeDefinitions =
    typeDefinitions
        |> List.filter (\(Type.TypeDefinition name definableType description) -> name /= query)


excludeMutation : Context -> List Type.TypeDefinition -> List Type.TypeDefinition
excludeMutation { mutation } typeDefinitions =
    case mutation of
        Just mutationObjectName ->
            typeDefinitions
                |> List.filter (\(Type.TypeDefinition name definableType description) -> name /= mutationObjectName)

        Nothing ->
            typeDefinitions


excludeSubscription : Context -> List Type.TypeDefinition -> List Type.TypeDefinition
excludeSubscription { subscription } typeDefinitions =
    case subscription of
        Just subscriptionObjectName ->
            typeDefinitions
                |> List.filter (\(Type.TypeDefinition name definableType description) -> name /= subscriptionObjectName)

        Nothing ->
            typeDefinitions

toViews context ((Type.TypeDefinition name definableType description) as definition) =
    case definableType of
        Type.ObjectType fields ->
            if name == context.query then
                "query: " ++ name
            else
                "object: " ++ name
                -- Graphql.Generator.Query.generate context moduleName fields
                --     |> Just

        --     else if Just name == context.mutation then
        --         Graphql.Generator.Mutation.generate context moduleName fields
        --             |> Just

        --     else if Just name == context.subscription then
        --         Graphql.Generator.Subscription.generate context moduleName fields
        --             |> Just

        --     else
        --         Graphql.Generator.Object.generate context name moduleName fields
        --             |> Just

        -- Type.ScalarType ->
        --     Nothing

        -- Type.EnumType enumValues ->
        --     Graphql.Generator.Enum.generate name moduleName enumValues description
        --         |> Just

        -- Type.InterfaceType fields possibleTypes ->
        --     Graphql.Generator.Interface.generate context (ClassCaseName.raw name) moduleName fields
        --         |> Just

        -- Type.UnionType possibleTypes ->
        --     Graphql.Generator.Union.generate context name possibleTypes
        --         |> Just

        -- Type.InputObjectType fields ->
        --     Nothing

        _ ->
            name

