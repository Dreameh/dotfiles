-- docs: http://projects.haskell.org/xmobar/
Config {
    font = "xft:Source Code Pro-12",
    bgColor = "#2e3440",
    fgColor = "#eceff4",
    position = Top,
    border = BottomB,
    borderColor = "#434c5e",
    lowerOnStart = True,
    commands =
      [ Run Memory
          [ "--template", "Mem: <usedratio>%"
          , "--High",     "8192"
          , "--Low",      "4096"
          , "--high",     "#f2777a"
          , "--normal",   "#6699cc"
          , "--low",      "#99cc99"
          ] 20
      , Run Swap
          [ "--template", "Swap: <usedratio>%"
          , "--High",     "1024"
          , "--Low",      "512"
          , "--high",     "#f2777a"
          , "--normal",   "#6699cc"
          , "--low",      "#99cc99"
          ] 20
      , Run Date "<fc=#FFF>(%a) <fc=#ee9a00>%T</fc></fc>" "date" 10
      , Run StdinReader
      ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{  %memory%  %date% "
}
