-- luacheck ignore: hammerspoon runtime
return Leader("c", "CleanShot", {
  Bind("a", "All-in-one capture", { url = "cleanshot://all-in-one" }),
  Bind("c", "Capture area", { url = "cleanshot://capture-area" }),
  Bind("f", "Capture fullscreen", { url = "cleanshot://capture-fullscreen" }),
  Bind("w", "Capture window", { url = "cleanshot://capture-window" }),
  Bind("s", "Scrolling capture", { url = "cleanshot://scrolling-capture" }),
  Bind("r", "Record screen", { url = "cleanshot://record-screen" }),
  Bind("o", "OCR text", { url = "cleanshot://capture-text" }),
  Bind("h", "Capture history", { url = "cleanshot://open-history" }),
  Bind("d", "Toggle desktop icons", { url = "cleanshot://toggle-desktop-icons" }),
})
