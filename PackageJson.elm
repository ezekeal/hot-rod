module PackageJson where

import Json.Decode exposing (..)
import Json.Decode.Extra exposing ((|:), lazy)
import String

-- Types

type alias PackageJson =
    { name : String
    , version : String
    , description : Maybe String
    , keywords : Maybe (List String)
    , homepage : Maybe String
    , bugs : Maybe String
    , license : Maybe String
    , author : Maybe Person
    , contributors : Maybe (List Person)
    , files : Maybe (List String)
    , main : Maybe String
    , bin : Maybe Bin
    }

type alias Person =
    { name : String
    , email : Maybe String
    , url : Maybe String
    }

type alias Bin =
    { name : String
    , version : Maybe String
    , bin : String
    }

bin : String -> Maybe String -> String -> Bin
bin name version bin =
    { name = name
    , version = version
    , bin = bin
    }


-- Default

default : PackageJson
default =
    { name = ""
    , version = ""
    , description = Nothing
    , keywords = Nothing
    , homepage = Nothing
    , bugs = Nothing
    , license = Nothing
    , author = Nothing
    , contributors = Nothing
    , files = Nothing
    , main = Nothing
    , bin = Nothing
    }

defaultPerson : Person
defaultPerson =
    { name = ""
    , email = Nothing
    , url = Nothing
    }

-- Decoders

packageJsonDecoder : Decoder PackageJson
packageJsonDecoder =
    succeed PackageJson
    |: ("name" := string)
    |: ("version" := string)
    |: maybe ("description" := string)
    |: maybe ("keywords" := list string)
    |: maybe ("homepage" := string)
    |: maybe ("bugs" := string)
    |: maybe ("license" := string)
    |: maybe ("author" := personDecoder)
    |: maybe ("contributors" := list personDecoder) --Start testing here
    |: maybe ("files" := list string)
    |: maybe ("main" := string)
    |: maybe ("bin" := binDecoder)

personDecoder : Decoder Person
personDecoder =
    let
        person =
            succeed Person
            |: ("name" := string)
            |: maybe ("email" := string)
            |: maybe ("url" := string)

        personString str =
            { defaultPerson | name = str }
    in
        oneOf
        [ person
        , string |> map personString
        ]

decodeBinString : Decoder Bin
decodeBinString =
    string
    |> map (\n -> bin (baseName n) Nothing n)

decodeBinPair : Decoder Bin
decodeBinPair =
    keyValuePairs string
    |> map (\n -> Maybe.withDefault ("", "") (List.head n))
    |> map (\(key,value) -> bin key Nothing value)

decodeBinObject : Decoder Bin
decodeBinObject =
    object3 bin
        ("name" := string)
        (maybe ("version" := string))
        ("bin" := string)

binDecoder : Decoder Bin
binDecoder =
  oneOf [ decodeBinString
        , decodeBinObject
        , decodeBinPair
        ]

decode : String -> PackageJson
decode value =
    (decodeString packageJsonDecoder) value
    |> Result.withDefault default


-- Utils

baseName : String -> String
baseName str =
    String.split "/" str
    |> List.reverse
    |> List.head
    |> Maybe.withDefault ""
