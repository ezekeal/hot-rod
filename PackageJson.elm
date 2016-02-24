module PackageJson where

import Json.Decode exposing (..)
import Json.Decode.Extra exposing ((|:))
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
    }

type alias Person =
    { name : String
    , email : Maybe String
    , url : Maybe String
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
