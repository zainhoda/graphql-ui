module Graphql.Parser exposing (decoder)

import Dict exposing (Dict)
import Graphql.Parser.Type as Type
import Json.Decode as Decode exposing (Decoder)


decoder =
    (Type.decoder
            |> Decode.list
            |> Decode.at [ "__schema", "types" ]
        )

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
