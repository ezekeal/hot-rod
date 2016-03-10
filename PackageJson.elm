module PackageJson (..) where

import Json.Decode exposing (..)
import Json.Decode.Extra exposing ((|:), lazy)
import String
import Html exposing (..)


-- Types


type alias PackageJson =
  { name : Maybe String
  , version : Maybe String
  , description : Maybe String
  , keywords : Maybe (List String)
  , homepage : Maybe String
  , bugs : Maybe String
  , license : Maybe String
  , author : Maybe Person
  , contributors : Maybe (List Person)
  , files : Maybe (List String)
  , main : Maybe String
  , bin : Maybe ( String, String )
  , man : Maybe (List String)
  , directories : Maybe Directories
  , repository : Maybe ( String, String )
  , scripts : Maybe (List ( String, String ))
  , config : Maybe (List ( String, String ))
  , dependencies : Maybe (List ( String, String ))
  , devDependencies : Maybe (List ( String, String ))
  , peerDependencies : Maybe (List ( String, String ))
  , bundledDependencies : Maybe (List String)
  , bundleDependencies : Maybe (List String)
  , optionalDependencies : Maybe (List ( String, String ))
  , engines : Maybe (List ( String, String ))
  , os : Maybe (List String)
  , cpu : Maybe (List String)
  , preferGlobal : Maybe Bool
  , private : Maybe Bool
  , publishConfig : Maybe (List ( String, String ))
  }


type alias Person =
  { name : Maybe String
  , email : Maybe String
  , url : Maybe String
  }


type alias Directories =
  { bin : Maybe String
  , doc : Maybe String
  , lib : Maybe String
  , man : Maybe String
  , example : Maybe String
  , tests : Maybe String
  }



-- Default


default : PackageJson
default =
  { name = Nothing
  , version = Nothing
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
  , man = Nothing
  , directories = Nothing
  , repository = Nothing
  , scripts = Nothing
  , config = Nothing
  , dependencies = Nothing
  , devDependencies = Nothing
  , peerDependencies = Nothing
  , bundledDependencies = Nothing
  , bundleDependencies = Nothing
  , optionalDependencies = Nothing
  , engines = Nothing
  , os = Nothing
  , cpu = Nothing
  , preferGlobal = Nothing
  , private = Nothing
  , publishConfig = Nothing
  }


defaultPerson : Person
defaultPerson =
  { name = Nothing
  , email = Nothing
  , url = Nothing
  }



-- Decoders


packageJsonDecoder : Decoder PackageJson
packageJsonDecoder =
  succeed PackageJson
    |: maybe ("name" := string)
    |: maybe ("version" := string)
    |: maybe ("description" := string)
    |: maybe ("keywords" := list string)
    |: maybe ("homepage" := string)
    |: maybe ("bugs" := string)
    |: maybe ("license" := string)
    |: maybe ("author" := person)
    |: maybe ("contributors" := list person)
    |: maybe ("files" := list string)
    |: maybe ("main" := string)
    |: maybe ("bin" := stringOrKeyValue)
    |: maybe ("man" := stringOrListString)
    |: maybe ("directories" := directoriesDecoder)
    |: maybe ("repository" := repositoryDecoder)
    |: maybe ("scripts" := keyValuePairs string)
    |: maybe ("config" := keyValuePairs string)
    |: maybe ("dependencies" := keyValuePairs string)
    |: maybe ("devDependencies" := keyValuePairs string)
    |: maybe ("peerDependencies" := keyValuePairs string)
    |: maybe ("bundledDependencies" := list string)
    |: maybe ("bundleDependencies" := list string)
    |: maybe ("optionalDependencies" := keyValuePairs string)
    |: maybe ("engines" := keyValuePairs string)
    |: maybe ("os" := list string)
    |: maybe ("cpu" := list string)
    |: maybe ("perferGlobal" := bool)
    |: maybe ("private" := bool)
    |: maybe ("publishConfig" := keyValuePairs string)


person : Decoder Person
person =
  let
    personObj =
      succeed Person
        |: maybe ("name" := string)
        |: maybe ("email" := string)
        |: maybe ("url" := string)

    personString str =
      { defaultPerson | name = Just str }
  in
    oneOf
      [ string |> map personString
      , personObj
      ]


directoriesDecoder : Decoder Directories
directoriesDecoder =
  succeed Directories
    |: maybe ("bin" := string)
    |: maybe ("doc" := string)
    |: maybe ("lib" := string)
    |: maybe ("man" := string)
    |: maybe ("example" := string)
    |: maybe ("test" := string)


repositoryDecoder : Decoder ( String, String )
repositoryDecoder =
  oneOf
    [ string |> map (\n -> ( "git", n ))
    , object2
        (,)
        ("type" := string)
        ("url" := string)
    ]


stringOrKeyValue : Decoder ( String, String )
stringOrKeyValue =
  oneOf
    [ string |> map (\n -> ( baseName n, n ))
    , (keyValuePairs string) |> map (\n -> Maybe.withDefault ( "", "" ) (List.head n))
    ]


stringOrListString : Decoder (List String)
stringOrListString =
  oneOf
    [ (string |> map (\n -> n :: []))
    , list string
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

parseSemVer : String -> Html
parseSemVer semver =
    span [ ] [ ]
-- TODO convert semver

-- Reference

semverSymbols : List (String, String)
semverSymbols =
    [ ("", "exactly")
    , (">", "above")
    , ("<", "below")
    , (">=", "at least")
    , ("<=", "at most")
    , ("~", "approximately")
    , ("^", "compatible with")
    , ("*", "any version")
    , ("-", "to")
    , ("||", "or")
    ]
