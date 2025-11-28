local WindowManagement = require("window-management")
local HyperKey = require("hyper-key")

-- use super key (cmd+ctrl+alt) for window management
local super = HyperKey.new({"cmd", "ctrl", "alt"})

-- maximize and center
super:bind("f"):toFunction("Maximize window", WindowManagement.maximizeWindow)
super:bind("c"):toFunction("Center window", WindowManagement.centerOnScreen)

-- halves
super:bind("h"):toFunction("Left half", WindowManagement.leftHalf)
super:bind("l"):toFunction("Right half", WindowManagement.rightHalf)
super:bind("k"):toFunction("Top half", WindowManagement.topHalf)
super:bind("j"):toFunction("Bottom half", WindowManagement.bottomHalf)

-- corners
super:bind("u"):toFunction("Top left corner", WindowManagement.topLeft)
super:bind("i"):toFunction("Top right corner", WindowManagement.topRight)
super:bind("n"):toFunction("Bottom left corner", WindowManagement.bottomLeft)
super:bind("m"):toFunction("Bottom right corner", WindowManagement.bottomRight)

-- thirds
super:bind("d"):toFunction("Left third", WindowManagement.leftThird)
super:bind("e"):toFunction("Center third", WindowManagement.centerThird)
super:bind("g"):toFunction("Right third", WindowManagement.rightThird)

-- two thirds
super:bind("s"):toFunction("Left two thirds", WindowManagement.leftTwoThirds)
super:bind("t"):toFunction("Right two thirds", WindowManagement.rightTwoThirds)

-- move between displays
super:bind("q"):toFunction("Throw to left display", WindowManagement.throwLeft)
super:bind("w"):toFunction("Throw to right display", WindowManagement.throwRight)
super:bind("a"):toFunction("Throw to display above", WindowManagement.throwUp)
super:bind("z"):toFunction("Throw to display below", WindowManagement.throwDown)

-- resize adjustments (using shift modifier with super)
hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "h", WindowManagement.shrinkLeft)
hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "l", WindowManagement.growRight)
hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "k", WindowManagement.shrinkUp)
hs.hotkey.bind({"cmd", "ctrl", "alt", "shift"}, "j", WindowManagement.growDown)

-- nudge position (using option modifier with super)
hs.hotkey.bind({"cmd", "ctrl", "alt", "option"}, "h", WindowManagement.nudgeLeft)
hs.hotkey.bind({"cmd", "ctrl", "alt", "option"}, "l", WindowManagement.nudgeRight)
hs.hotkey.bind({"cmd", "ctrl", "alt", "option"}, "k", WindowManagement.nudgeUp)
hs.hotkey.bind({"cmd", "ctrl", "alt", "option"}, "j", WindowManagement.nudgeDown)

return {}
