module Theme exposing (Theme, light, dark)

type alias Theme =
  { purple : String
  , background : String
  , softBackground : String
  , contrastBackground : String
  , shadow : String
  , text : String
  }


light : Theme
light =
  { purple = "402945"
  , background = "fff"
  , softBackground = "eee"
  , contrastBackground = "cbc8d6"
  , shadow = "eee"
  , text = "333"
  }


dark : Theme
dark =
  { purple = "402945"
  , background = "000"
  , softBackground = "1f1f1f"
  , contrastBackground = "4f4c5c"
  , shadow = "111"
  , text = "eee"
  }
