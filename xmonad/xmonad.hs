import System.IO
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.BoringWindows
import XMonad.Layout.Fullscreen
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)


-- Colors and Borders
xmobarTitleColor = "#FFB6B0" -- Current window title
xmobarCurrentWorkspaceColor = "#CEFFAC"  -- Current workspace

defaults = defaultConfig {
    modMask = mod4Mask, -- use the Windows button as mod
    terminal = "urxvt",
    manageHook = composeAll [
			    className =? "mpv" --> doFloat,
			    className =? "obs" --> doShift "9",
			    manageDocks,
			    isFullscreen --> doFullFloat,
			    manageHook defaultConfig],
    layoutHook = avoidStruts $ smartBorders $ minimize $ boringWindows $ layoutHook desktopConfig,
    handleEventHook    = fullscreenEventHook,
    startupHook = setWMName "LG3D"
} `additionalKeys` myKeys

myKeys = [
          ((mod4Mask, xK_m), withFocused minimizeWindow),
          ((mod4Mask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin),
          ((mod4Mask, xK_apostrophe), sendMessage ToggleStruts),
          ((mod4Mask, xK_Tab), focusDown),
          ((mod4Mask .|. shiftMask, xK_Tab), focusUp),
          ((mod4Mask, xK_w), spawn "~/Projects/wall-changer/main.py"),
          ((mod4Mask .|. shiftMask, xK_w), spawn "~/Projects/wall-changer/main.py -c"),
          ((mod4Mask, xK_n), spawn "touch ~/.pomodoro_session"),
          ((mod4Mask .|. shiftMask, xK_n), spawn "rm ~/.pomodoro_session")
	 ]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
    xmonad . docks $ defaults {
      logHook =  dynamicLogWithPP $ defaultPP {
            ppOutput = System.IO.hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
          , ppSep = "   "
          , ppWsSep = " "
          , ppLayout  = (\ x -> case x of
              "Spacing 6 Mosaic"                      -> "[:]"
              "Spacing 6 Mirror Tall"                 -> "[M]"
              "Spacing 6 Hinted Tabbed Simplest"      -> "[T]"
              "Spacing 6 Full"                        -> "[ ]"
              _                                       -> x )
          , ppHiddenNoWindows = showNamedWorkspaces
      } 
} where showNamedWorkspaces wsId = if any (`elem` wsId) ['a'..'z']
                                       then pad wsId
                                       else ""
