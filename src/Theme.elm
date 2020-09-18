module Theme exposing (Theme, dark, light, themed, themedProperty)

import Css exposing (Color, Style)
import Css.Media


type alias Theme =
    { purple : String
    , background : String
    , softBackground : String
    , mediumBackground : String
    , softText : String
    , contrastBackground : String
    , shadow : String
    , text : String
    , highlight : String
    }


light : Theme
light =
    { purple = "402945"
    , background = "fff"
    , softBackground = "eee"
    , mediumBackground = "ccc"
    , softText = "555"
    , contrastBackground = "cbc8d6"
    , shadow = "ddd"
    , text = "333"
    , highlight = "d0f2fe"
    }


dark : Theme
dark =
    { purple = "402945"
    , background = "111"
    , softBackground = "222"
    , mediumBackground = "444"
    , softText = "999"
    , contrastBackground = "4f4c5c"
    , shadow = "000"
    , text = "eee"
    , highlight = "204a62"
    }


themed : List ( Color -> Style, Theme -> String ) -> Style
themed styles =
    Css.batch
        [ Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
            (List.map
                (\( s, t ) ->
                    s (Css.hex (t dark))
                )
                styles
            )
        , Css.Media.withMediaQuery [ "(prefers-color-scheme: light)" ]
            (List.map
                (\( s, t ) ->
                    s (Css.hex (t light))
                )
                styles
            )
        ]


themedProperty : String -> (Theme -> String) -> Style
themedProperty name style =
    Css.batch
        [ Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
            [ Css.property name ("#" ++ style dark) ]
        , Css.Media.withMediaQuery [ "(prefers-color-scheme: light)" ]
            [ Css.property name ("#" ++ style light) ]
        ]
