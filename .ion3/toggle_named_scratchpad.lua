-- Toggle (and create) a named scratchpad.
--
-- Example: This will create a scratchpad named example_sp
--          kpress(MOD4.."space", "toggle_named_scratchpad(_, 'example_sp')")
--
-- If you use this script you will also want to add the following binding:
--
-- kpress(META.."space", "toggle_named_scratchpad(_, '*scratchpad*')")
--
-- to your WScreen bindings section so that META+space still gets you the
-- default scratchpad, if you don't it will get you one of the scratchpads
-- created by this script instead.
--
-- Etan Reisner <deryni@gmail.com>

function toggle_named_scratchpad(reg, name)
	local named_sp

	local scr = reg:screen_of()
	local geom_scr = scr:geom()

	local geom_loc = {
	    x = math.floor(geom_scr.w / 4),
	    y = math.floor(geom_scr.h / 4),
	    h = math.floor(geom_scr.h / 2),
	    w = math.floor(geom_scr.w / 2),
	}

	named_sp = ioncore.lookup_region(name, "WFrame")

	if not named_sp then
	    named_sp = scr:attach_new({
		type='WFrame',
		layer=2,
		name=name,
		sizepolicy=3,
		passive=false,
		frame_style='frame-scratchpad',
		geom=geom_loc})
	    -- I'm not sure why this is necessary but if I don't have this the first call doesn't show the scratchpad
	    mod_sp.set_shown(named_sp, 'unset')
	end

	mod_sp.set_shown(named_sp, 'toggle')
end
