module Graphql.Generator.Types exposing (..)

import Graphql.Parser.Type
import Graphql.Parser.ClassCaseName

generate : (List Graphql.Parser.Type.TypeDefinition) -> List String
generate types =
    types
        |> List.map 
            (\x -> 
                case x of
                    Graphql.Parser.Type.TypeDefinition classCaseName _ maybeDescription ->
                        Graphql.Parser.ClassCaseName.raw classCaseName 
            )