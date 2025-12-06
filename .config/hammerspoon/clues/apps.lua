local function focusChromeProfile(profileName)
  return function()
    local chrome = hs.application.get("Google Chrome")
    if not chrome then
      hs.application.launchOrFocus("Google Chrome")
      return
    end

    -- Find window with matching profile (case insensitive)
    local pattern = profileName:lower()
    for _, win in ipairs(chrome:allWindows()) do
      local title = win:title()
      if title and title:lower():match(pattern) then
        win:focus()
        chrome:activate()
        return
      end
    end

    -- If no window found, just focus Chrome
    chrome:activate()
  end
end

local function focusGmail()
  hs.application.launchOrFocus("Gmail")
end

local function focusGcal()
  hs.application.launchOrFocus("Google Calendar")
end

return Leader("a", "Apps", {
  Bind("g", "Ghostty", { focus = "Ghostty" }),
  Bind("c", "Chrome (personal)", { fn = focusChromeProfile("gmail") }),
  Bind("w", "Chrome (stripe)", { fn = focusChromeProfile("stripe") }),
  Bind("s", "Slack", { focus = "Slack" }),
  Bind("z", "Zoom", { focus = "zoom.us" }),
  Bind("m", "Gmail", { fn = focusGmail }),
  Bind("l", "Calendar", { fn = focusGcal }),
})
