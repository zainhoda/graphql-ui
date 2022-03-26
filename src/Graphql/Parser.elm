module Graphql.Parser exposing (decoder)

import Dict exposing (Dict)
import Graphql.Parser.Type as Type
import Graphql.Generator.Types
import Json.Decode as Decode exposing (Decoder)


decoder : Decoder Graphql.Generator.Types.ApiInteractions
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

