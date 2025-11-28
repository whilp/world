local QuickSwitch = {}

function QuickSwitch.setup(hyper)
  hyper:bind("return"):toApplication("Ghostty")
  hyper:bind("c"):toApplication("Google Chrome")
  hyper:bind("s"):toApplication("Spotify")
  hyper:bind("1"):toApplication("1Password")
end

return QuickSwitch
