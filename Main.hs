import Haste
import Haste.Graphics.Canvas
import Control.Monad
import Data.List
import Tangible.Syntax

data Widget = Widget Double (Picture ())

test1 = Range 0.5 0 1

test2 = Function "max" [
    ("x", False, Range 0.5 0 1),
    ("y", False, Range 0.7 0 1),
    ("max", False, Range 0.7 0 1)
    ]

test3 = Function "sin" [
    ("x", True, Range 0.8 0 1),
    ("sin", False, Range 0.2 0 1)
    ]

test4 = Function "max" [
    ("a", False, Range 0.7 0 1),
    ("b", False, Function "sin" [
        ("x", False, Parameter "a"),
        ("sin", False, Range 0.2 0 1)
    ]),
    ("max", False, Range 0.7 0 1)
    ]

test5 = Function "max" [
    ("a", False, Range 0.7 0 1),
    ("b", False, Function "sin" [
        ("x", True, Parameter "a"),
        ("sin", False, Range 0.2 0 1)
    ]),
    ("max", False, Range 0.7 0 1)
    ]

test6 = Function "max" [
    ("a", False, Range 0.7 0 1),
    ("b", True, Function "sin" [
        ("x", True, Parameter "a"),
        ("sin", False, Range 0.2 0 1)
    ]),
    ("max", False, Range 0.7 0 1)
    ]

test7 = Function "max" [
    ("a", False, Range 0.7 0 1),
    ("b", False, TypeVariable "a"),
    ("max", False, Range 0.7 0 1)
    ]
    
main :: IO ()
main = do
    Just canvas <- getCanvasById "canvas"
    let Widget height picture = drawValue 300 test4
    render canvas picture

drawValue :: Double -> Value -> Widget
drawValue widgetWidth widgetValue = case widgetValue of

    TypeVariable name -> do
        Widget 35 $ do
            let rectangle = rect (5, 5) (widgetWidth - 5, 32)
            color (RGB 240 240 240) $ fill $ rectangle
            color (RGB 240 240 240) $ stroke $ rectangle
            color (RGB 200 200 200) $ font ("italic 20px Bitstream Vera") $ text (15, 25) ("Type")
            color (RGB 150 150 150) $ font ("bold 20px Bitstream Vera") $ text (65, 25) (name)

    Parameter name -> do
        Widget 25 $ color (RGB 150 150 150) $ do
            font ("bold 20px Bitstream Vera") $ text (5, 21) ("=")
            font ("italic 20px Bitstream Vera") $ text (25, 20) (name)

    Range value minimum maximum -> do
        let size = maximum - minimum
        let fraction = (value - minimum) / size
        Widget 25 $ do
            let offset = 15
            color (RGB 180 180 180) $ stroke $ line (5, offset) (widgetWidth - 5, offset)
            let innerWidth = widgetWidth - 10
            color (RGB 150 150 150) $ fill $ rect (5 + fraction * innerWidth - 2, offset - 6) (5 + fraction * innerWidth + 2, offset + 6)

    Function functionName xs ->
        let innerWidth = widgetWidth - 40 in
        let f (Widget widgetHeight picture) ((name, fused, innerValue), returnValue) =
                let labelColor = if fused then RGB 150 150 150 else RGB 100 100 100 in
                let labelStyle = if returnValue then "bold " else "italic " in
                let label = color labelColor $ font (labelStyle ++ "20px Bitstream Vera") $ text (10, 20) name in
                let Widget innerHeight innerPicture = if fused then Widget 0 (return ()) else drawValue innerWidth innerValue in
                let labeledPicture = label >> translate (10, 25) innerPicture in
                Widget (widgetHeight + 25 + innerHeight + 5) (picture >> translate (0, widgetHeight) labeledPicture) in
        let Widget innerHeight picture = foldl' f (Widget 0 (return ())) (xs `zip` [i == length xs | i <- [1..]]) in
        Widget (innerHeight + 25) $ do
            color (RGB 100 100 100) $ stroke $ rect (5, 10) (widgetWidth - 5, innerHeight + 15)
            translate (10, 15) picture
