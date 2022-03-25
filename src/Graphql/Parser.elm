module Graphql.Parser exposing (decoder)

import Dict exposing (Dict)
import Graphql.Parser.Type as Type
import Graphql.Generator.Types
import Json.Decode as Decode exposing (Decoder)


typesDecoder : Decode.Decoder (List Type.TypeDefinition)
typesDecoder =
    (Type.decoder
            |> Decode.list
            |> Decode.at [ "__schema", "types" ]
        )

decoder =
    Decode.map4 Graphql.Generator.Types.sortedIntrospectionData
        (Type.decoder
            |> Decode.list
            |> Decode.at [ "__schema", "types" ]
        )
        (Decode.at [ "__schema", "queryType", "name" ] Decode.string)
        (Decode.maybe (Decode.at [ "__schema", "mutationType", "name" ] Decode.string))
        (Decode.maybe (Decode.at [ "__schema", "subscriptionType", "name" ] Decode.string))
        |> Decode.map Graphql.Generator.Types.generate
    -- typesDecoder
    -- |> Decode.map Graphql.Generator.Types.generate

-- decoder : { apiSubmodule : List String, scalarCodecsModule : Maybe ModuleName } -> Decoder (Dict String String)
-- decoder options =
--     Decode.map4 sortedIntrospectionData
--         (Type.decoder
--             |> Decode.list
--             |> Decode.at [ "__schema", "types" ]
--         )
--         (Decode.at [ "__schema", "queryType", "name" ] Decode.string)
--         (Decode.maybe (Decode.at [ "__schema", "mutationType", "name" ] Decode.string))
--         (Decode.maybe (Decode.at [ "__schema", "subscriptionType", "name" ] Decode.string))
--         |> Decode.map (Graphql.Generator.Group.generateFiles options)
