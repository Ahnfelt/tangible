module Tangible.Syntax where

data Value
    = Function String [(String, Bool, Value)]
    | Parameter String -- TODO: Should use a number instead
    | Range Double Double Double -- value minimum maximum
    | TypeVariable String

