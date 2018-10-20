Config { 
    font = "xft:Droid Sans Mono:size=9:bold:antialias=true"
    bgColor = "#000000",
    fgColor = "#ffffff",
    position = Top,
    lowerOnStart = True,
    commands = [
         Run Kbd [("us", "US"), ("ru", "RU")]
        ,Run Date "%Y.%m.%d %H:%M:%S" "date" 10
        ,Run BatteryN ["BAT0"] ["-t", "<left>% <timeleft>"] 60 "battery"
        ,Run StdinReader
        ,Run CommandReader "~/Projects/pymodoro/pymodoro.py" "pomodoro"
        ,Run CommandReader "~/.local/bin/obs-status" "rec"
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %rec% | %pomodoro% | %battery% | %kbd% | <fc=#FFFFCC>%date%</fc>   "
}
