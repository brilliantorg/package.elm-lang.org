module Flags exposing (Flags, init)


import Json.Decode as D


type alias Flags =
    { mountedAt : String }


init : D.Value -> Flags
init value =
  case D.decodeValue decoder value of
    Ok flags ->
      flags
    Err error ->
      -- Fallback and assume mounted at root
      { mountedAt = "/" }


decoder : D.Decoder Flags
decoder =
  D.map Flags
    (D.field "mountedAt" D.string)
