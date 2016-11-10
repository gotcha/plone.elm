module Plone.Css exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)


type CssClasses
    = NavBar


type CssIds
    = Page


css =
    (stylesheet << namespace "plone")
        [ (.) NavBar
            [ backgroundColor (rgb 200 128 64)
            ]
        ]
