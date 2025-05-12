module RiveredTypes exposing (Question, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline
import Json.Encode as Encode



-- QUESTION TYPE


type alias Question =
    { id : Int
    , text : String

    -- , tags : List String
    -- , note : String
    , asked : Bool
    }



-- DECODER


decoder : Decoder Question
decoder =
    Decode.succeed Question
        |> Json.Decode.Pipeline.required "id" Decode.int
        |> Json.Decode.Pipeline.required "text" Decode.string
        -- |> Json.Decode.Pipeline.required "tags" (Decode.list Decode.string)
        -- |> Json.Decode.Pipeline.required "note" Decode.string
        |> Json.Decode.Pipeline.hardcoded False


encoder : Question -> Encode.Value
encoder q =
    Encode.object
        [ ( "id", Encode.int q.id )
        , ( "text", Encode.string q.text )

        -- , ( "tags", Encode.list Encode.string q.tags )
        -- , ( "note", Encode.string q.note )
        ]
