Clue{
  name = "Test alert",
  desc = "Show a simple test alert",
  key = { "hyper", "t", "a" },
  group = "test",
  action = { fn = function() hs.alert("Test alert executed!") end }
}

Clue{
  name = "Test notify",
  desc = "Show a test notification",
  key = { "hyper", "t", "n" },
  group = "test",
  action = { fn = function()
    hs.notify.new({
      title = "Clue System",
      informativeText = "Test notification from clue loader"
    }):send()
  end }
}

Clue{
  name = "Test console",
  desc = "Print test message to console",
  key = { "hyper", "t", "c" },
  group = "test",
  action = { fn = function()
    print("Test clue executed from console")
    hs.alert("Check console for output")
  end }
}
