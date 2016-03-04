module Fs where

import Json.Encode
import Json.Decode exposing (..)
import Json.Decode.Extra exposing ((|:))

type alias File =
    { name: String
    , extension: String
    , contents: String
    }

default : File
default =
    { name = ""
    , extension = ""
    , contents = ""
    }

decoder : Decoder File
decoder =
    succeed File
    |: ("name" := string)
    |: ("extension" := string)
    |: ("contents" := string)

decode : Json.Encode.Value -> File
decode value =
    (decodeValue decoder) value
    |> Result.withDefault default
