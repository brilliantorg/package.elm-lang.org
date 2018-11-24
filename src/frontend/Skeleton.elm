module Skeleton exposing
  ( Details
  , Warning(..)
  , view
  , Segment
  , helpSegment
  , authorSegment
  , projectSegment
  , versionSegment
  , moduleSegment
  )


import Browser
import Elm.Version as V
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Href
import Json.Decode as D
import MountPoint exposing (MountPoint)
import Utils.Logo as Logo



-- NODE


type alias Details msg =
  { title : String
  , header : List Segment
  , warning : Warning
  , attrs : List (Attribute msg)
  , kids : List (Html msg)
  }


type Warning
  = NoProblems
  | NewerVersion String V.Version



-- SEGMENT


type Segment
  = Text String
  | Link String String


helpSegment : Segment
helpSegment =
  Text "help"


authorSegment : String -> Segment
authorSegment author =
  Text author


projectSegment : MountPoint -> String -> String -> Segment
projectSegment mount author project =
  Link (Href.toProject mount author project) project


versionSegment : MountPoint -> String -> String -> Maybe V.Version -> Segment
versionSegment mount author project version =
  Link (Href.toVersion mount author project version) (vsnToString version)


moduleSegment : MountPoint -> String -> String -> Maybe V.Version -> String -> Segment
moduleSegment mount author project version moduleName =
  Link (Href.toModule mount author project version moduleName Nothing) moduleName


vsnToString : Maybe V.Version -> String
vsnToString maybeVersion =
  case maybeVersion of
    Nothing ->
      "latest"

    Just version ->
      V.toString version



-- VIEW


view : (a -> msg) -> MountPoint -> Details a -> Browser.Document msg
view toMsg mount details =
  { title =
      details.title
  , body =
      [ viewHeader mount details.header
      , lazy viewWarning details.warning
      , Html.map toMsg <|
          div (class "center" :: details.attrs) details.kids
      , viewFooter
      ]
  }



-- VIEW HEADER


viewHeader : MountPoint -> List Segment -> Html msg
viewHeader mount segments =
  div
    [ style "background-color" "#eeeeee"
    ]
    [ div [class "center"]
        [ h1 [ class "header" ] <|
            (viewLogo mount) :: List.intersperse slash (List.map viewSegment segments)
        ]
    ]



slash : Html msg
slash =
  span [ class "spacey-char" ] [ text "/" ]


viewSegment : Segment -> Html msg
viewSegment segment =
  case segment of
    Text string ->
      text string

    Link address string ->
      a [ href address ] [ text string ]



-- VIEW WARNING


viewWarning : Warning -> Html msg
viewWarning warning =
  div [ class "header-underbar" ] <|
    case warning of
      NoProblems ->
        []

      NewerVersion url version ->
        [ p [ class "version-warning" ]
            [ text "Warning! The latest version of this package is "
            , a [ href url ] [ text (V.toString version) ]
            ]
        ]



-- VIEW FOOTER


viewFooter : Html msg
viewFooter =
  div [class "footer"]
    [ text "All code for this site is open source and written in Elm. "
    , a [ class "grey-link", href "https://github.com/elm/package.elm-lang.org/" ] [ text "Check it out" ]
    , text "! — © 2012-present Evan Czaplicki"
    ]



-- VIEW LOGO


viewLogo : MountPoint -> Html msg
viewLogo mount =
  a [ href (MountPoint.toString mount)
    , style "text-decoration" "none"
    ]
    [
      div
        [ style "display" "-webkit-display"
        , style "display" "-ms-flexbox"
        , style "display" "flex"
        ]
        [ Logo.logo 30
        , div
            [ style "color" "black"
            , style "padding-left" "8px"
            ]
            [ div [ style "line-height" "20px" ] [ text "elm" ]
            , div
                [ style "line-height" "10px"
                , style "font-size" "0.5em"
                ]
                [ text "packages" ]
            ]
        ]
    ]
