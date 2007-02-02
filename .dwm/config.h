/* (C)opyright MMVI Anselm R. Garbe <garbeam at gmail dot com>
 * See LICENSE file for license details.
 * ex: set ts=8:
 */

#define TAGS \
const char *tags[] = { "META", "MAIN", "SH", "WWW", "INFO", NULL };

#define BORDERPX		2
#define DEFMODE			dotile		/* dofloat */
#define FLOATSYMBOL		"F"
#define TILESYMBOL		"T"

#define FONT			"-sgi-screen-bold-*-*-*-16-*-*-*-*-*-*-*"
#define NORMBORDERCOLOR		"#333"
#define NORMBGCOLOR		"#222"
#define NORMFGCOLOR		"#ccc"
#define SELBORDERCOLOR		"#999"
#define SELBGCOLOR		"#555"
#define SELFGCOLOR		"#fff"

#define MASTER			600		/* per thousand */
#define MODKEY			Mod1Mask
#define NMASTER			2		/* clients in master area */
#define SNAP			20		/* pixel */

#define KEYS \
static Key key[] = { \
	/* modifier			key		function	argument */ \
	{ NULL,		                XK_F2,	        spawn,		{ .cmd = "exec term" } }, \
	{ MODKEY,		        XK_F2,	        spawn,		{ .cmd = "exec term -T META -e ssh -t will@meta.lfod.us screen -RD meta" } }, \
	{ NULL,		                XK_F3,	        spawn,		{ .cmd = "exec `dmenu -b < ~/.dwm/menu`" } }, \
	{ NULL,		                XK_F4,	        spawn,		{ .cmd = "exec browser" } }, \
	{ MODKEY,	                XK_F4,	        spawn,		{ .cmd = "exec firefox" } }, \
	{ NULL,		                XK_F5,	        spawn,		{ .cmd = "exec tunes" } }, \
	{ MODKEY,	                XK_F5,	        spawn,		{ .cmd = "exec tunes stop" } }, \
	{ NULL,		                XK_F6,	        spawn,		{ .cmd = "exec vol -d" } }, \
	{ MODKEY,	                XK_F6,	        spawn,		{ .cmd = "exec tunes prev" } }, \
	{ NULL,		                XK_F7,	        spawn,		{ .cmd = "exec vol -u" } }, \
	{ MODKEY,	                XK_F7,	        spawn,		{ .cmd = "exec tunes next" } }, \
	{ NULL,		                XK_F8,	        spawn,		{ .cmd = "exec vol -t" } }, \
	{ MODKEY,	                XK_F8,	        spawn,		{ .cmd = "exec tunes stop" } }, \
	{ NULL,		                XK_F10,	        spawn,		{ .cmd = "sleep 1 && exec /home/will/bin/lock" } }, \
	{ NULL,		                XK_F12,		quit,		{ 0 } }, \
	{ MODKEY,			XK_Tab,		focusnext,	{ 0 } }, \
	{ MODKEY|ShiftMask,		XK_Tab,		focusprev,	{ 0 } }, \
	{ MODKEY,			XK_Return,	zoom,		{ 0 } }, \
	{ MODKEY,			XK_g,		resizemaster,	{ .i = 15 } }, \
	{ MODKEY,			XK_s,		resizemaster,	{ .i = -15 } }, \
	{ MODKEY,			XK_i,		incnmaster,	{ .i = 1 } }, \
	{ MODKEY,			XK_d,		incnmaster,	{ .i = -1 } }, \
	{ MODKEY|ShiftMask,		XK_0,		tag,		{ .i = -1 } }, \
	{ MODKEY|ShiftMask,		XK_1,		tag,		{ .i = 0 } }, \
	{ MODKEY|ShiftMask,		XK_2,		tag,		{ .i = 1 } }, \
	{ MODKEY|ShiftMask,		XK_3,		tag,		{ .i = 2 } }, \
	{ MODKEY|ShiftMask,		XK_4,		tag,		{ .i = 3 } }, \
	{ MODKEY|ShiftMask,		XK_5,		tag,		{ .i = 4 } }, \
	{ MODKEY|ControlMask|ShiftMask,	XK_1,		toggletag,	{ .i = 0 } }, \
	{ MODKEY|ControlMask|ShiftMask,	XK_2,		toggletag,	{ .i = 1 } }, \
	{ MODKEY|ControlMask|ShiftMask,	XK_3,		toggletag,	{ .i = 2 } }, \
	{ MODKEY|ControlMask|ShiftMask,	XK_4,		toggletag,	{ .i = 3 } }, \
	{ MODKEY|ControlMask|ShiftMask,	XK_5,		toggletag,	{ .i = 4 } }, \
	{ MODKEY,		        XK_c,		killclient,	{ 0 } }, \
	{ MODKEY,			XK_space,	togglefloat,	{ 0 } }, \
	{ MODKEY,			XK_0,		view,		{ .i = -1 } }, \
	{ MODKEY,			XK_1,		view,		{ .i = 0 } }, \
	{ MODKEY,			XK_2,		view,		{ .i = 1 } }, \
	{ MODKEY,			XK_3,		view,		{ .i = 2 } }, \
	{ MODKEY,			XK_4,		view,		{ .i = 3 } }, \
	{ MODKEY,			XK_5,		view,		{ .i = 4 } }, \
	{ MODKEY|ControlMask,		XK_1,		toggleview,	{ .i = 0 } }, \
	{ MODKEY|ControlMask,		XK_2,		toggleview,	{ .i = 1 } }, \
	{ MODKEY|ControlMask,		XK_3,		toggleview,	{ .i = 2 } }, \
	{ MODKEY|ControlMask,		XK_4,		toggleview,	{ .i = 3 } }, \
	{ MODKEY|ControlMask,		XK_5,		toggleview,	{ .i = 4 } }, \
};

/* Query class:instance:title for regex matching info with following command:
 * xprop | awk -F '"' '/^WM_CLASS/ { printf("%s:%s:",$4,$2) }; /^WM_NAME/ { printf("%s\n",$2) }' */
#define RULES \
static Rule rule[] = { \
	/* class:instance:title regex	tags regex	isfloat */ \
	{ "^XTerm:.*LOCAL.:.*META.$",	"META",		False }, \
	{ "^XTerm:.*LOCAL.:.*MAIN.$",	"MAIN",		False }, \
	{ "^XTerm:.*LOCAL.:.*SHELLS.$",	"SH",		False }, \
	{ "Firefox.*",			"WWW",		False }, \
	{ "Konqueror.*",		"WWW",		False }, \
	{ "MPlayer.*",			NULL,		True }, \
	{ "KPDF.*",			NULL,		True }, \
};
/*
	{ "Gimp.*",			NULL,		True }, \
*/
