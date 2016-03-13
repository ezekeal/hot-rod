module PackageJson (..) where

import Json.Decode exposing (..)
import Json.Decode.Extra exposing ((|:), lazy)
import String
import Char


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

parseSemVer : String -> List (String, String)
parseSemVer semver =
    let
        svList =
            String.split " " semver
        assignType str =
            (getSvType str, str)
    in
        if String.length semver == 0 then
            [("any", "")]
        else
            List.map assignType svList
            |> List.map parseVersions
            |> List.concat

parseVersions : (String, String) -> List (String, String)
parseVersions (vType, v) =
    let
        verlist i ver =
            case i of
                0 ->
                    ("major", ver)
                1 ->
                    ("minor", ver)
                2 ->
                    ("patch", ver)
                _->
                    ("?",ver)

        verSign str =
            List.filter (\(sym, txt) -> str == sym) semverSymbols
            |> List.head
            |> Maybe.withDefault ("?","?")
            |> snd

        versionSign =
            String.filter (\c -> not (Char.isDigit c) && c /= '.' && c/= 'x') v
            |> (\s -> [(verSign s, s)])

        versionNumber =
            String.filter (\c -> Char.isDigit c || c == '.' || c == 'x') v
            |> String.split "."
            |> List.indexedMap verlist
    in
        if vType == "version" then
            List.concat [versionSign, versionNumber]
        else
            [(vType, v)]

getSvType : String -> String
getSvType str =
    let
        first n =
            String.left n str
        isHttp =
            first 4 == "http"
        isGit =
            first 3 == "git"
        isPath =
            [ (first 3, "../")
            , (first 2, "./")
            , (first 2, "~/")
            , (first 1, "/")
            ]
            |> List.any (\(a,b) -> a == b)
        isOr =
            first 2 == "||"
        isTo =
            first 1 == "-"
        isFile =
            first 4 == "file"
        isTag =
            String.all (\c -> not (Char.isDigit c)) str
    in
        if isHttp then
            "http"
        else if isGit then
            "git"
        else if isPath then
            "path"
        else if isOr then
            "or"
        else if isTo then
            "to"
        else if isFile then
            "file"
        else if isTag then
            "tag"
        else
            "version"

-- Reference

semverSymbols : List (String, String)
semverSymbols =
    [ (">", "above")
    , ("<", "below")
    , (">=", "at least")
    , ("<=", "at most")
    , ("~", "approximately")
    , ("^", "compatible with")
    , ("*", "any version")
    , ("-", "to")
    , ("||", "or")
    , ("", "exactly")
    ]
