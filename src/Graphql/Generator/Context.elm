module Graphql.Generator.Context exposing (Context, stub)

import Dict exposing (Dict)
import Graphql.Parser.ClassCaseName as ClassCaseName exposing (ClassCaseName)


context :
    { query : String
    , mutation : Maybe String
    , subscription : Maybe String
    , interfaces : InterfaceLookup
    }
    -> Context
context { query, mutation, interfaces, subscription } =
    { query = ClassCaseName.build query
    , mutation = mutation |> Maybe.map ClassCaseName.build
    , subscription = subscription |> Maybe.map ClassCaseName.build
    , interfaces = interfaces
    }


type alias Context =
    { query : ClassCaseName
    , mutation : Maybe ClassCaseName
    , subscription : Maybe ClassCaseName
    , interfaces : InterfaceLookup
    }


stub : Context
stub =
    { query = ClassCaseName.build "RootQueryObject"
    , mutation = Nothing
    , subscription = Nothing
    , interfaces = Dict.empty
    }


type alias InterfaceLookup =
    Dict String (List ClassCaseName)