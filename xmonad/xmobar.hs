Config { 
    font = "xft:Droid Sans Mono:size=9:bold:antialias=true"
    bgColor = "#000000",
    fgColor = "#ffffff",
    position = Static { xpos = 0, ypos = 0, width = 1920, height = 16 },
    lowerOnStart = True,
    commands = [
         Run Kbd [("us", "US"), ("ru", "RU")]
        ,Run Date "%Y.%m.%d %H:%M:%S" "date" 10
        ,Run StdinReader
	,Run CommandReader "~/Projects/pymodoro/pymodoro.py" "pomodoro"
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %pomodoro% | %kbd% | <fc=#FFFFCC>%date%</fc>   "
}
