module MountPoint exposing
    ( MountPoint(..)
    , fromString
    , router
    , toString
    )


import Json.Decode as D
import Url.Parser exposing (Parser, (</>), s, top)


type MountPoint
  = Root
  | Subpath (List String)


router : MountPoint -> Parser a a
router mountPoint =
  case mountPoint of
    Root ->
      top
    Subpath [] ->
      top
    Subpath (path :: []) ->
      s path
    Subpath rest ->
      List.map s rest
      |> List.foldr (</>) top


fromString : String -> MountPoint
fromString path =
  case (String.toList path) of
    [] -> Root
    '/' :: [] -> Root
    '/' :: rest -> Subpath (splitPath rest)
    rest -> Subpath (splitPath rest)


toString : MountPoint -> String
toString mountPoint =
  case mountPoint of
    Root ->
      "/"
    Subpath [] ->
      "/"
    Subpath (path :: []) ->
      "/" ++ path
    Subpath rest ->
      "/" ++ (String.join "/" rest)


splitPath : List Char -> List String
splitPath subpath =
  String.fromList subpath
  |> String.split "/"
  |> List.filter (\segment -> not (String.isEmpty segment))
