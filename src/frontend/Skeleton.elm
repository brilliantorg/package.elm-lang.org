module Skeleton exposing
  ( Details
  , Warning(..)
  , view
  , Segment
  , authorSegment
  , projectSegment
  , versionSegment
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
  | WarnOld
  | WarnMoved String String
  | WarnNewerVersion String V.Version



-- SEGMENT


type Segment
  = Text String
  | Link String String


authorSegment : String -> Segment
authorSegment author =
  Text author


projectSegment : MountPoint -> String -> String -> Segment
projectSegment mount author project =
  Link (Href.toProject mount author project) project


versionSegment : MountPoint -> String -> String -> Maybe V.Version -> Segment
versionSegment mount author project version =
  Link (Href.toVersion mount author project version) (vsnToString version)


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
      , lazy (viewWarning mount) details.warning
      , Html.map toMsg <|
          div (class "center" :: style "flex" "1" :: details.attrs) details.kids
      , viewFooter
      ]
  }



-- VIEW HEADER


viewHeader : MountPoint -> List Segment -> Html msg
viewHeader mount segments =
  div [class "header"]
    [ div [class "nav"]
        [ viewLogo mount
        , case segments of
            [] -> text ""
            _  -> h1 [] (List.intersperse slash (List.map viewSegment segments))
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


viewWarning : MountPoint -> Warning -> Html msg
viewWarning mount warning =
  div [ class "header-underbar" ] <|
    case warning of
      NoProblems ->
        []

      WarnOld ->
        [ p [ class "version-warning" ]
            [ text "NOTE — this package is not compatible with Elm 0.19.1"
            ]
        ]

      WarnMoved author project ->
        [ p [ class "version-warning" ]
            [ text "NOTE — this package moved to "
            , a [ href (Href.toVersion mount author project Nothing) ]
                [ text (author ++ "/" ++ project)
                ]
            ]
        ]

      WarnNewerVersion url version ->
        [ p [ class "version-warning" ]
            [ text "NOTE — the latest version is "
            , a [ href url ] [ text (V.toString version) ]
            ]
        ]



-- VIEW FOOTER


viewFooter : Html msg
viewFooter =
  div [class "footer"]
    [ a [ class "grey-link", href "https://github.com/elm/package.elm-lang.org/" ] [ text "Site Source" ]
    , text " — © 2012-2020 Evan Czaplicki"
    ]



-- VIEW LOGO


viewLogo : MountPoint -> Html msg
viewLogo mount =
  a [ href (MountPoint.toString mount)
    , style "text-decoration" "none"
    , style "margin-right" "32px"
    , style "display" "flex"
    , style "align-items" "center"
    ]
    [ Logo.logo 32
    , div
        [ style "padding-left" "8px" ]
        [ div
            [ style "line-height" "24px"
            , style "font-size" "30px"
            ]
            [ text "elm" ]
        , div
            [ style "font-size" "12px"
            ]
            [ text "packages" ]
        ]
    ]
