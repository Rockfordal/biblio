module Main where

import Prelude ((<<<), bind)
import Pux (start, renderToDOM)

import Pux (start, fromSimple, renderToDOM)
import Pux.Router (sampleUrl)
import Signal ((~>))

import RoutingExample.Routes (match)
import RoutingExample.App (Action(PageView), init, update, view)
-- import AjaxExample.Todos (init, update, view)

main = do
  urlSignal <- sampleUrl
  -- app <- start
  let routeSignal = urlSignal ~> (PageView <<< match)

  app <- start
    { initialState: init
    , update: fromSimple update
    -- , update:       update
    , view: view
    , inputs: [routeSignal]
    -- , inputs:       []
    }

  renderToDOM "#app" app.html
