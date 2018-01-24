module Sandbox.DevData exposing (..)

import Card


foo = Card.rank "foo"

deck = List.map
        (Card.card foo)
        (List.range 1 27)