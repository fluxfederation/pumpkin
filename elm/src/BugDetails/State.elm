module BugDetails.State exposing (..)

import BugDetails.Types exposing (..)
import BugDetails.Rest exposing (..)

update : Msg -> (Maybe Details, Cmd Msg)
update message =
  case message of
    RequestDetails bugId ->
      (Nothing, BugDetails.Rest.loadDetails bugId)

    LoadedDetails (result) ->
      case result of
        Err err ->
          -- TODO Actually handle errors
          Debug.log "error loading Bug details"
          (Nothing, Cmd.none)
        Ok bugDetails ->
          (Just bugDetails, Cmd.none)
