module Href exposing
  ( join
  , toProject
  , toVersion
  , toModule
  )


import Elm.Version as V
import MountPoint exposing(MountPoint(..))
import Url.Builder as Url



-- HREFS


join : MountPoint -> String -> String
join mountPoint path =
    case mountPoint of
        Root -> path
        Subpath subpaths -> "/" ++ (String.join "/" subpaths) ++ path


toProject : MountPoint -> String -> String -> String
toProject mount author project =
  Url.absolute [ "packages", author, project, "" ] []
      |> join mount


toVersion : MountPoint -> String -> String -> Maybe V.Version -> String
toVersion mount author project version =
  Url.absolute [ "packages", author, project, vsnToString version, ""] []
      |> join mount


toModule : MountPoint -> String -> String -> Maybe V.Version -> String -> Maybe String -> String
toModule mount author project version moduleName maybeValue =
  Url.custom Url.Absolute [ "packages", author, project, vsnToString version, String.replace "." "-" moduleName ] [] maybeValue
      |> join mount



-- HELPERS


vsnToString : Maybe V.Version -> String
vsnToString maybeVersion =
  case maybeVersion of
    Nothing ->
      "latest"

    Just version ->
      V.toString version
