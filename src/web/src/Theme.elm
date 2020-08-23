module Theme exposing (Theme, light, dark)

type alias Theme =
  { purple : String
  , background : String
  , softBackground : String
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
  , softText = "555"
  , contrastBackground = "cbc8d6"
  , shadow = "ddd"
  , text = "333"
  , highlight = "97f1e2"
  }


dark : Theme
dark =
  { purple = "402945"
  , background = "000"
  , softBackground = "1f1f1f"
  , softText = "999"
  , contrastBackground = "4f4c5c"
  , shadow = "111"
  , text = "eee"
  , highlight = "1b544b"
  }
