/* (C)opyright MMVI Anselm R. Garbe <garbeam at gmail dot com>
 * See LICENSE file for license details.
 * ex: set ts=8:
 */

/* appearance */
#define BORDERPX		2
#define FONT			"-sgi-screen-bold-*-*-*-16-*-*-*-*-*-*-*"
#define NORMBORDERCOLOR		"#333"
#define NORMBGCOLOR		"#222"
#define NORMFGCOLOR		"#ccc"
#define SELBORDERCOLOR		"#999"
#define SELBGCOLOR		"#555"
#define SELFGCOLOR		"#fff"
#define TOPBAR			True		/* False */

/* tagging */
#define TAGS \
const char *tags[] = { "META", "MAIN", "SH", "WWW", "X", NULL };
#define RULES \
static Rule rule[] = { \
	/* class:instance:title regex	tags regex	isfloating */ \
	{ "^XTerm:.*LOCAL.:.*META.$",	"META",		False }, \
	{ "^XTerm:.*LOCAL.:.*MAIN.$",	"MAIN",		False }, \
	{ "^XTerm:.*LOCAL.:.*SHELLS.$",	"SH",		False }, \
	{ "Firefox.*",			"WWW",		False }, \
	{ "Konqueror.*",		"WWW",		False }, \
	{ ".Copying",			NULL,		True }, \
	{ "Warning - Konqueror",	NULL,		True }, \
	{ "MPlayer",			NULL,		True }, \
	{ "KPDF.*",			NULL,		True }, \
	{ "Gimp",			NULL,		True }, \
};

/* layout(s) */
#define LAYOUTS \
static Layout layout[] = { \
	/* symbol		function */ \
	{ "T",			tile }, /* first entry is default */ \
	{ "F",			floating }, \
};
#define MASTERWIDTH		600		/* per thousand */
#define NMASTER			2		/* clients in master area */
#define SNAP			32		/* snap pixel */

/* key definitions */
#define MODKEY			Mod1Mask
#define KEYS \
static Key key[] = { \
	/* modifier			key		function	argument */ \
	{ NULL,				XK_F2,		spawn,		"exec term" }, \
	{ MODKEY,			XK_F2,		spawn,		"exec term -T META -e ssh -t will@meta.lfod.us screen -RD meta" }, \
	{ NULL,				XK_F3,		spawn,		"exec `dmenu -b < ~/.dwm/menu`" }, \
	{ NULL,				XK_F4,		spawn,		"exec browser" }, \
	{ MODKEY,			XK_F4,		spawn,		"exec firefox" }, \
	{ NULL,				XK_F5,		spawn,		"exec tunes" }, \
	{ MODKEY,			XK_F5,		spawn,		"exec tunes stop" }, \
	{ NULL,				XK_F6,		spawn,		"exec vol -d" }, \
	{ MODKEY,			XK_F6,		spawn,		"exec tunes prev" }, \
	{ NULL,				XK_F7,		spawn,		"exec vol -u" }, \
	{ MODKEY,			XK_F7,		spawn,		"exec tunes next" }, \
	{ NULL,				XK_F8,		spawn,		"exec vol -t" }, \
	{ MODKEY,			XK_F8,		spawn,		"exec tunes stop" }, \
	{ NULL,				XK_F10,		spawn,		"sleep 1 && exec /home/will/bin/lock" }, \
	{ NULL,				XK_F12,		quit,		NULL }, \
	{ MODKEY|ShiftMask,		XK_space,	setlayout,	NULL }, \
	{ MODKEY,			XK_h,		incmasterw,	"-32" }, \
	{ MODKEY,			XK_l,		incmasterw,	"32" }, \
	{ MODKEY,		        XK_j,		incnmaster,	"1" }, \
	{ MODKEY,		        XK_k,		incnmaster,	"-1" }, \
	{ MODKEY,			XK_n,		focusclient,	"1" }, \
	{ MODKEY,			XK_p,		focusclient,	"-1" }, \
	{ MODKEY,			XK_m,		togglemax,	NULL }, \
	{ MODKEY,			XK_Return,	zoom,		NULL }, \
	{ MODKEY,			XK_space,	togglefloating,	NULL }, \
	{ MODKEY,		        XK_c,		killclient,	NULL }, \
	{ MODKEY,			XK_0,		view,		NULL }, \
	{ MODKEY,			XK_1,		view,		"0" }, \
	{ MODKEY,			XK_2,		view,		"1" }, \
	{ MODKEY,			XK_3,		view,		"2" }, \
	{ MODKEY,			XK_4,		view,		"3" }, \
	{ MODKEY,			XK_5,		view,		"4" }, \
	{ MODKEY|ControlMask,		XK_1,		toggleview,	"0" }, \
	{ MODKEY|ControlMask,		XK_2,		toggleview,	"1" }, \
	{ MODKEY|ControlMask,		XK_3,		toggleview,	"2" }, \
	{ MODKEY|ControlMask,		XK_4,		toggleview,	"3" }, \
	{ MODKEY|ControlMask,		XK_5,		toggleview,	"4" }, \
	{ MODKEY|ControlMask,		XK_6,		toggleview,	"5" }, \
	{ MODKEY|ShiftMask,		XK_0,		tag,		NULL }, \
	{ MODKEY|ShiftMask,		XK_1,		tag,		"0" }, \
	{ MODKEY|ShiftMask,		XK_2,		tag,		"1" }, \
	{ MODKEY|ShiftMask,		XK_3,		tag,		"2" }, \
	{ MODKEY|ShiftMask,		XK_4,		tag,		"3" }, \
	{ MODKEY|ShiftMask,		XK_5,		tag,		"4" }, \
	{ MODKEY|ShiftMask,		XK_6,		tag,		"5" }, \
	{ MODKEY|ControlMask|ShiftMask,	XK_1,		toggletag,	"0" }, \
	{ MODKEY|ControlMask|ShiftMask,	XK_2,		toggletag,	"1" }, \
	{ MODKEY|ControlMask|ShiftMask,	XK_3,		toggletag,	"2" }, \
	{ MODKEY|ControlMask|ShiftMask,	XK_4,		toggletag,	"3" }, \
	{ MODKEY|ControlMask|ShiftMask,	XK_5,		toggletag,	"4" }, \
	{ MODKEY|ControlMask|ShiftMask,	XK_6,		toggletag,	"5" }, \
};
