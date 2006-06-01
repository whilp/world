--
-- Ion mod_sp configuration file
--

defbindings("WScreen", {
    bdoc("Toggle scratchpad called 'scratch'."),
    -- kpress(MOD1.."J", "toggle_named_scratchpad(_, 'scratch')"),
    kpress(MOD1.."Shift+1", "toggle_named_scratchpad(_, 'tunes')"),

    bdoc("Toggle scratchpad called 'tunes'."),
    -- kpress(MOD1.."space", "toggle_named_scratchpad(_, 'tunes')"),
    kpress(MOD1.."Shift+2", "toggle_named_scratchpad(_, 'scratch')"),
    
    -- A more ideal key for toggling the scratchpad would be the key left of
    -- the key for numeral 1. Unfortunately the symbols mapped to this key
    -- vary by the keyboard layout, and to be fully portable to different 
    -- architechtures and fancy keyboards, we can't rely on keycodes either. 
    -- However, on standard Finnish/Swedish (and other Nordic) keyboard 
    -- layouts the following should work:
    --kpress(MOD1.."section", "mod_sp.set_shown_on(_, 'toggle')"),
    -- and on UK and US layouts this should work:
    --kpress(MOD1.."grave", "mod_sp.set_shown_on(_, 'toggle')"),
})

