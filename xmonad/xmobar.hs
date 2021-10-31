Config {
    font = "xft:M+ 2m:size=10:bold:antialias=true"
    bgColor = "#000000",
    fgColor = "#ffffff",
    position = Top,
    lowerOnStart = True,
    commands = [
         Run Kbd [("us", "US"), ("ru", "RU")]
        ,Run Date "%Y.%m.%d %H:%M:%S" "date" 10
        ,Run BatteryN ["BAT0"] ["-t", "<left>% <timeleft>"] 60 "battery"
        ,Run StdinReader
        ,Run CommandReader "~/Projects/pymodoro/pymodoro.py 35 10" "pomodoro"
        ,Run CommandReader "~/.local/bin/obs-status" "rec"
        ,Run CommandReader "~/.local/bin/pandora-playing" "pandora"
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %pandora% | %rec% | %pomodoro% | %battery% | %kbd% | <fc=#FFFFCC>%date%</fc>   "
}
