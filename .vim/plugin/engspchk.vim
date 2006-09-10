" engspchk.vim: Vim syntax file
" Language:    English
" Author:      Dr. Charles E. Campbell, Jr. <NdrOchip@ScampbellPfamilyA.Mbiz> - NOSPAM
" Last Change: Jul 12, 2005
" Version:     62
" License:     GPL (Gnu Public License)
"
" GetLatestVimScripts: :AutoInstall: 195 1 engspchk.vim
" Help: {{{1
" Environment Variables: {{{2
"
"  $CVIMSYN         : points to a directory holding the engspchk dictionaries
"                     ie., <engspchk.dict>, <engspchk.usr>, <engspchk.rare>
"
"  g:cvimsyn        : Vim variable, settable in your <.vimrc>, that points to
"                     a directory holding the user word database.
"
"  g:spchklang      : override name-of-file prefix with desired language
"                     prefix/filename (ie. gerspchk.vim ger frspchk.vim fr etc)
"
"  g:spchkautonext  : if this variable exists, then \es and \et will also
"                     automatically jump to the next spelling error (\en).
"                     \ea, if a word is selected, will also do a \en.
"
"  g:spchkdialect   : pick a dialect (no effect if spchklang not "eng")
"                     = "usa" : pick United States dialect
"                     = "uk"  : pick United Kingdom dialect
"                     = "can" : pick Canadian dialect
"
"  g:spchknonhl     : apply engspchk'ing to all non-syntax-highlighted text
"                     (done if variable exists)
"
"  g:spchkpunc =0   : default, no new behavior
"              =1   : check for some simple English punctuation problems
"                     non-capitalized word after ellipse (... a)
"                     non-capitalized word after sentence ending character
"                     ([.?!])
"  g:spchksilent= 0 : default
"               = 1 : usual Sourcing... and Loading messages suppressed
"
"  If you make a Dialect highlighting group, it will be used instead
"
" Finding Dictionaries: {{{2
"      If        g:cvimsyn exists, it is tried
"      Otherwise $CVIMSYN is tried
"      Otherwise each path on the runtimepath is tried
"      Otherwise quit with an error message
"
"      "Trying" involves checking if the spelling dictionary is
"      filereadable(); if not, then if filereadable(expand())
"      works.  If a combination works, that path is set into
"      g:cvimsyn.
"
"      Note that the "eng" prefix can be changed via setting
"      g:spchklang or renaming <engspchk.vim>.  Then engspchk
"      will load:  (elide the [])
"
"         [eng]spchk.dict  Main word dictionary
"         [eng]spchk.usr   User's personal word dictionary
"         [eng]spchk.rare  English only -- Webster's 1913 dictionary extra words
"                          and unusual words culled from previous
"                          <engspchk.dict> wordlists.
"
" Included Maps:  maps use <mapleader>, which by default is \ {{{2
"  \ec : load engspchk
"  \et : add  word under cursor into database (temporarily - ie. just this file)
"  \es : save word under cursor into usr  database (permanently)
"  \ej : save word under cursor into proj database (permanently)
"  \en : move cursor to the next     spelling error
"  \ep : move cursor to the previous spelling error
"  \ea : look for alternative spellings of word under cursor
"  \ed : toggle Dialect highlighting (Warning/Error)
"  \ee : end engspchk
"  \eT : make word under cursor a BadWord (temporarily, opposite of \et)
"  \eS : make word under cursor a BadWord (permanently, opposite of \es)
"        and removes the word from the user dictionary
"  \eJ : remove word under cursor from project dictionary (permanently)
"
" Maps for Alternatives Window Only:
"  <cr> : on alternatives line, will put word under cursor in
"         searchword's stead
"  <tab>: like <cr>, but does a global substitute changing all such
"         mispelled words to the selected alternate word.
"  q    : will quit the alternate-word window
"  :q   : will quit the alternate-word window
"
" Usage: {{{2
"  Simply source the file in.  It does *not* do a "syntax clear", so that means
"  that you can usually just source it in on top of other highlighting.
"  NOTE: not all alphas of 6.0 support plugins, <silent>, etc.
"        engspchk can't check for them; all their versions are 600.
"        Besides, 6.1 is out nowadays.
"
" Non English Languages: {{{2
"  There are versions of this script for languages other than English.
"  I've tried to make this script work for non-English languages by
"
"    (a) allowing one to rename the script with a different prefix
"    (b) using that prefix to load the non-English language dictionary
"
"  If you come up with a version for another language, please let me
"  know where on the web it is so that I can help make it known.
"
"    Dutch     : http://www.thomer.com/thomer/vi/nlspchk.vim.gz
"    German    : http://jeanluc-picard.de/vim/gerspchk/gerspchk.vim.gz
"    Hungarian : http://vim.sourceforge.net/scripts/script.php?script_id=22
"    Polish    : http://strony.wp.pl/wp/kostoo/download.htm#vim
"    Yiddish   : http://www.cs.uky.edu/~raphael/yiddish/vim.tar.gz

"------------------------------------------------------------------------------
let s:keepcpo= &cpo
set cpo&vim

" Determining current language (based on name of this file) {{{2
"                    -or- if it previously exists
"   ie. engspchk gerspchk nlspchk hngspchk yidspchk etc
"       eng      ger      nl      hng      yid
"
"  b:spchklang: dictionary language prefix
"  b:spchkfile: prefix based on name of this file
if exists("g:spchklang")
 let b:spchklang= substitute(g:spchklang,'spchk\.vim',"","e")
 let b:spchkfile= substitute(expand("<sfile>:t"),'spchk\.vim',"","e")
" call Decho("g:spchklang<".g:spchklang."> exists, setting up b:spchklang<".b:spchklang."> and b:spchkfile<".b:spchkfile.">")
else
 let b:spchklang= substitute(expand("<sfile>:t"),'spchk\.vim',"","e")
 let b:spchkfile= b:spchklang
" call Decho("g:spchklang !exists, setting up b:spchklang<".b:spchklang."> and b:spchkfile<".b:spchkfile.">")
 let g:spchklang = b:spchklang
 let g:spchkfile = b:spchkfile
endif
let s:spchkfile= b:spchkfile
let b:Spchklang=substitute(b:spchklang,'\(.\)\(.*\)$','\u\1\2','')

if !exists("g:spchklang")
 let g:spchklang = b:spchklang
endif
if !exists("g:spchkfile")
 let g:spchkfile = b:spchkfile
endif
if !exists("g:Spchklang")
 let g:Spchklang= b:Spchklang
endif

if exists("mapleader") && mapleader != ""
 let s:usermaplead= mapleader
else
 let s:usermaplead= '\'
endif
let s:mapleadstring= escape(s:usermaplead,'\ ')

" Quick load:
if !exists("s:loaded_".s:spchkfile."spchk")
" call Decho("Quick load: s:loaded_".s:spchkfile."spchk doesn't exist yet")
 let s:spchkversion             = "v62"
 let s:loaded_{b:spchkfile}spchk= s:spchkversion
 let s:engspchk_loadcnt         =  0

 " ===========================
 " Pre-Loading Interface: {{{1
 " ===========================
 "       \ec invokes <Plug>LoadSpchk which invokes <SID>LoadSpchk() {{{2
 if !hasmapto('<Plug>LoadSpchk')
  nmap <unique> <Leader>ec <Plug>LoadSpchk
 endif
 nmap <silent> <script> <Plug>LoadSpchk :call <SID>LoadSpchk()<CR>

 " \ev: set up a visual-block for spellchecking {{{2
 if !hasmapto('<Plug>Spchkev')
  vmap <unique> <Leader>ev <Plug>Spchkev
 endif
 vmap <silent> <script> <Plug>Spchkev    :<c-u>call <SID>SpchkVisBlock(0)<CR>

" ---------------------------------------------------------------------
 " LoadSpchk: set up and actually load <engspchk.vim> {{{2
 silent! fun! <SID>LoadSpchk()
"   call Dfunc("LoadSpchk()")
   " prevent unnecessary re-loading of engspchk
   if exists("b:engspchk_loaded")
"   	call Dret("LoadSpchk : preventing unnecessary re-load of engspchk (b:engspchk_loaded=".b:engspchk_loaded.")")
    return
   endif
   let b:engspchk_loaded  = 1
   let s:engspchk_loadcnt = s:engspchk_loadcnt + 1
   let b:hidden           = &bh
   set bh=hide
"   call Decho("b:engspchk_loaded=".b:engspchk_loaded." s:engspchk_loadcnt=".s:engspchk_loadcnt)

   if !exists("b:spchkfile")
   	let b:spchkfile= g:spchkfile
   endif
   if !exists("b:spchklang")
   	let b:spchklang= g:spchklang
   endif
   if !exists("b:Spchklang")
   	let b:Spchklang= g:Spchklang
   endif

   let b:ch_keep = &ch
   let s:errid   = synIDtrans(hlID("Error"))
"   call Decho("setting spchkfile=".b:spchkfile)
"   call Decho("setting spchklang=".b:spchklang)
"   call Decho("setting Spchklang=".b:Spchklang)
"   call Decho("setting errid=".s:errid." (Error syntax id)")

   set ch=8
   exe 'runtime plugin/'.b:spchkfile.'spchk.vim'
   let &ch  = b:ch_keep
   unlet b:ch_keep
"   call Dret("LoadSpchk")
 endfun

" ---------------------------------------------------------------------

 " Pre-Loading DrChip menu support:

  " set up b:spchklang and b:Spchklang {{{2
  if exists("g:spchklang")
   let b:spchklang= substitute(g:spchklang,'spchk\.vim',"","e")
   let b:spchkfile= s:spchkfile
  else
   let b:spchklang= s:spchkfile
   let b:spchkfile= s:spchkfile
  endif
  let b:Spchklang = substitute(b:spchklang,'\(.\)\(.*\)$','\u\1\2','')

  " ---------------------------------------------------------------------
  " SpchkSetCvimsyn: default value for path to dictionaries {{{2
  fun! s:SpchkSetCvimsyn()
"  	call Dfunc("SpchkSetCvimsyn()")

    if !exists("g:cvimsyn") || g:cvimsyn == ""
     if exists("$CVIMSYN")
      let g:cvimsyn= $CVIMSYN
"      call Decho("setting g:cvimsyn to $CVIMSYN<".g:cvimsyn.">")

     else
      " Try looking along the rtp
      let rtpdirlist = &rtp
      while rtpdirlist != ""
       let rtpdir     = substitute(rtpdirlist,',.*','','e')
       let rtpdirlist = substitute(rtpdirlist,'.\{-},','','e')
"       call Decho("Trying <".rtpdir."> from <".rtpdirlist.">")
       if isdirectory(rtpdir."/CVIMSYN")
       	break
       endif
       if stridx(rtpdirlist,',') == -1
       	let rtpdir= ""
       	break
       endif
      endwhile
      let g:cvimsyn= rtpdir."/CVIMSYN"
     endif
    endif
    
    " if g:cvimsyn still doesn't exist...
    if !exists("g:cvimsyn") || g:cvimsyn == ""
"	 call Decho("g:cvimsyn still not good, checking presets")
     if expand('$HOME') != ""
      if has("win32") || has("win64") || has("win95")
       let g:cvimsyn= expand('$HOME').'/vimfiles/CVIMSYN'
      else
       let g:cvimsyn= expand('$HOME')."/.vim/CVIMSYN"
	  endif
	 else
      if has("win32") || has("win64") || has("win95")
       let g:cvimsyn= 'c:/vimfiles/CVIMSYN'
	  endif
     endif
    
     if !isdirectory(g:cvimsyn)
      echoerr 'Unable to find dictionaries (see :help cvimsyn)'
      call inputsave()|call input("Press <cr> to continue")|call inputrestore()
	  let &cpo= s:keepcpo
	  unlet s:keepcpo
      finish
     endif
     if glob(g:cvimsyn."/*spchk.dict") == ""
      echoerr 'Dictionaries are missing from <'.g:cvimsyn.'> -- please check path!'
      call inputsave()|call input("Press <cr> to continue")|call inputrestore()
	  let &cpo= s:keepcpo
	  unlet s:keepcpo
      finish
     endif
    endif

"  	call Dret("SpchkSetCvimsyn : g:cvimsyn<".g:cvimsyn.">")
  endfun

  " ---------------------------------------------------------------------
  " AltLangMenus: read cvimsyn directory for alternate languages {{{2
  "    domenu=2 : make menu entries and loading by language commands
  "          =1 : set up loading-by-language commands (:Engspchk :Nlspchk etc)
  "          =0 : unmenu the entries
  fun! s:AltLangMenus(domenu)
"    call Dfunc("AltLangMenus(domenu=".a:domenu.")")

  	if a:domenu >= 1
	 call s:SpchkSetCvimsyn()
	 let b:cvimsyn= g:cvimsyn
"	 call Decho("b:cvimsyn set to <".b:cvimsyn.">")
     if b:cvimsyn != ""
      let dictfiles= glob(b:cvimsyn."/*spchk.dict")
      let dictfile = ""
"      call Decho("dictfiles<".dictfiles.">")
      while dictfiles != dictfile
       let pat      = '^\(.\{-}\)\n\(.*$\)'
       let dictfile = substitute(dictfiles,pat,'\1','e')
       let dictfiles= substitute(dictfiles,pat,'\2','e')
       let lang     = substitute(dictfile,'^.*[/\\]\(.*\)spchk.dict','\1','e')
"       call Decho("lang<".lang."> dictfile<".dictfile."> dictfiles<".dictfiles.">")
       let Lang   = substitute(lang,'\(.\)\(.*\)','\u\1\2','e')
       if lang != b:spchklang && a:domenu == 2
	   	" make alternate-language menu entry only if its not the default language
        exe 'menu '.g:DrChipTopLvlMenu.'Load\ AltLang\ Spelling\ Checker.Load\ As\ '.Lang.'spchk :call <SID>SpchkAltLang("'.lang.'")'."<cr>"
       endif
"	   call Decho('setting up '.Lang.'spchk command')
	   exe "com! ".Lang."spchk exe 'normal ".s:usermaplead."ee' | let g:spchklang= '".lang."' | normal ".s:usermaplead."ec"
      endwhile
     endif
	else
	 exe 'silent! unmenu '.g:DrChipTopLvlMenu.'Load\ AltLang\ Spelling\ Checker'
	endif

"    call Dret("AltLangMenus")
  endfun

  " ---------------------------------------------------------------------
  " SpchkAltLang: initial loading of an alternate dictionary {{{2
  fun! s:SpchkAltLang(lang)
"  	call Dfunc("SpchkAltLang(lang<".a:lang.">")

  	let g:spchklang= a:lang
  	let g:Spchklang= substitute(a:lang,'^\(.\)\(.*\)$','\u\1\2','e')
	call s:LoadSpchk()
	call s:AltLangMenus(0)

"	call Dret("SpchkAltLang")
  endfun


  " ---------------------------------------------------------------------
  " SpchkVisBlock: restrict spellchecking to a specific block selected by visual mode {{{2
  fun! s:SpchkVisBlock(mode)
"    call Dfunc("SpchkVisBlock(mode=".a:mode.")")

    " clear out any previous out-of-visual-block de-Error highlighting
    silent! syn clear OutOfBlock
    if a:mode == 1
     " disable visual-block selected spellchecking
"    call Dret("SpchkVisBlock")
	 return
    endif

    " enable visual-block selected spellchecking
	" load spellchecker if not already loaded
	call s:LoadSpchk()

    " visual block only spellchecking support: {{{2
    let l1      = line("'<")
    let l2      = line("'>")
    let c1      = col("'<")
    let c2      = col("'>")
    let vismode = visualmode()
    if s:incluster
     let incluster= " contained "
     syn cluster Spell add=OutOfBlock
    else
     let incluster= ""
    endif
    exe 'syn match OutOfBlock '.incluster.'"\%<'.l1.'l"'
    exe 'syn match OutOfBlock '.incluster.'"\%>'.l2.'l"'
    if vismode == "\<c-v>"
     exe 'syn match OutOfBlock '.incluster.'"\%<'.c1.'v"'
     exe 'syn match OutOfBlock '.incluster.'"\%>'.c2.'v"'
"     call Decho("OutOfBlock: (visual-block) lines[".l1.",".l2."] col[".c1.",".c2."]")
    else
"     call Decho("OutOfBlock: (visual only) lines[".l1.",".l2."]")
    endif

"    call Dret("SpchkVisBlock")
  endfun

  " ---------------------------------------------------------------------
  " set up DrChipTopLevelMenu {{{2
 if exists("did_install_default_menus") && has("menu") && has("gui_running") && &go =~ 'm'
  if !exists("g:DrChipTopLvlMenu") || g:DrChipTopLvlMenu == ""
   let g:DrChipTopLvlMenu= "DrChip."
  endif
  exe 'menu '.g:DrChipTopLvlMenu.'Load\ Spelling\ Checker<tab>'.s:mapleadstring.'ec	<Leader>ec'
"  call Decho("installed menu item: Load Spelling Checker")
  call s:AltLangMenus(2)
 else
  call s:AltLangMenus(1)
 endif
" call Decho("---- END INITIAL PRE-LOADING OF ENGSPCHK ----")

 let &cpo= s:keepcpo
 unlet s:keepcpo
 finish  " end pre-load
endif

" ================================
" Begin Main Loading of Engspchk {{{1
" ================================
"call Decho("---- BEGIN ENGSPCHK MAIN LOADING ----")

" ---------------------------------------------------------------------
"  SpchkModeline: analyzes current line for a spchk: modeline {{{2
fun! s:SpchkModeline()
  let curline= getline(".")
"  call Dfunc("SpchkModeline() <".curline.">")
  let curline = substitute(curline,'^.*spchk:\s*','','')
  while curline != ""
"   call Decho("curline<".curline.">")
   let curopt  = "b:spchk".substitute(curline,'^\([bg]:\)\=\(spchk\)\=\(\a\+\)=.*$','\3','e')
"   call Decho("curopt<".curopt.">")
   if curopt == "b:spchkacronym" || curopt == "b:spchkaltright" || curopt == "b:spchkautonext" || curopt == "b:spchkcvimsyn" || curopt == "b:spchkdialect" || curopt == "b:spchkDrChipTopLvlMenu" || curopt == "b:spchklang" || curopt == "b:spchkmouse" || curopt == "b:spchknonhl" || curopt == "b:spchkproj" || curopt == "b:spchkpunc" || curopt == "b:spchksilent"
    let curval  = substitute(curline,'^\a\+=\(\S*\).*$','\1','')
"	call Decho("setting ".curopt."=".curval)
    exe "let ".curopt.'="'.curval.'"'
   endif
   let curline= substitute(curline,'^[^: \t]\+:\=\s*\(.*\)$','\1','e')
  endwhile
"  call Dret("SpchkModeline")
endfun

" ---------------------------------------------------------------------
" SpchkSavePosn: save current cursor position {{{2
fun! s:SpchkSavePosn()
"  call Dfunc("SpchkSavePosn()")
  let swline    = line(".")
  let swcol     = col(".")
  let swwline   = winline() - 1
  let swwcol    = virtcol(".") - wincol()
  let b:spchksaveposn = "silent! ".swline
  let b:spchksaveposn = b:spchksaveposn."|silent norm! z\<cr>"
  let b:spchksaveposn = b:spchksaveposn.":silent! norm! zO\<cr>"
  if swwline > 0
   let b:spchksaveposn= b:spchksaveposn.":silent norm! ".swwline."\<c-y>\<cr>"
  endif
  let b:spchksaveposn = b:spchksaveposn.":silent call cursor(".swline.",".swcol.")\<cr>"
  if swwcol > 0
   let b:spchksaveposn= b:spchksaveposn.":silent norm! ".swwcol."zl\<cr>"
  endif
"  call Decho("SpchkSavePosn: swline=".swline." swcol=".swcol." swwline=".swwline." swwcol=".swwcol)
"  call Dret("SpchkSavePosn : saveposn<".b:spchksaveposn.">")
endfun

" ---------------------------------------------------------------------
" SpchkRestorePosn: restore cursor position {{{2
fun! s:SpchkRestorePosn()
"  call Dfunc("SpchkRestorePosn() restoring cursor position")
  if exists("b:spchksaveposn")
"   call Decho("spchksaveposn<".b:spchksaveposn.">")
   exe b:spchksaveposn
   " seems to be something odd: vertical motions after RWP
   " cause jump to first column.  Following fixes that
   if wincol() > 1
    silent norm! hl
   elseif virtcol(".") < virtcol("$")
    silent norm! lh
   endif
  endif
"  call Dret("SpchkRestorePosn")
endfun

" ---------------------------------------------------------------------
" Convert global to local variables {{{2
let b:spchkaltright    = exists("g:spchkaltright")?    g:spchkaltright    : 0
let b:spchkacronym     = exists("g:spchkacronym")?     g:spchkacronym     : 0
call s:SpchkSetCvimsyn()
let b:cvimsyn          = g:cvimsyn
let b:DrChipTopLvlMenu = exists("g:DrChipTopLvlMenu")? g:DrChipTopLvlMenu : ""
let b:spchkautonext    = exists("g:spchkautonext")?    g:spchkautonext    : 0
let b:spchkdialect     = exists("g:spchkdialect")?     g:spchkdialect     : "usa"
let b:spchknonhl       = exists("g:spchknonhl")?       g:spchknonhl       : 0
let b:spchkmouse       = exists("g:spchkmouse")?       g:spchkmouse       : 0
let b:spchkpunc        = exists("g:spchkpunc")?        g:spchkpunc        : 0
let b:spchksilent      = exists("g:spchksilent")?      g:spchksilent      : 0
let b:spchkproj        = exists("g:spchkproj")?        g:spchkproj        : ""

" ---------------------------------------------------------------------
" Process modelines from top and bottom of file, if any {{{2
if &mls > 0
 call s:SpchkSavePosn()
 let lastline    = line("$")
 let lastlinemls = (lastline < &mls)? lastline : &mls

 " look for spchk: lines at top-of-file
 exe "silent 1,".lastlinemls.'g/\<spchk:/call s:SpchkModeline()'

 let lastlinemls = lastline - &mls
 let lastlinemls = (lastlinemls < &mls)?     &mls + 1 : lastlinemls
 let lastlinemls = (lastlinemls > lastline)? lastline : lastlinemls

 " look for spchk: lines at bottom-of-file
 exe "silent ".lastlinemls.",".lastline.'g/\<spchk:/call s:SpchkModeline()'
 call s:SpchkRestorePosn()
endif

" ---------------------------------------------------------------------

" report on sourcing of (which language)spchk.vim: {{{2
if !b:spchksilent
 echomsg "Sourcing <".b:spchklang."spchk.vim>  (version ".s:spchkversion.")"
endif

" remove "Load Spelling Checker" from menu {{{2
if exists("did_install_default_menus") && has("menu") && has("gui_running") && &go =~ 'm'
 " remove \ec from DrChip menu
 exe 'silent! unmenu '.b:DrChipTopLvlMenu.'Load\ Spelling\ Checker'
" call Decho("uninstalled menu item: Load Spelling Checker")
endif

" check if syntax highlighting is on and, if it isn't, enable it {{{2
if !exists("syntax_on")
 if !has("syntax")
  echomsg "Your version of vim doesn't have syntax highlighting support"
  let &cpo= s:keepcpo
  unlet s:keepcpo
  finish
 endif
 echomsg "Enabling syntax highlighting"
 syn enable
endif

" ---------------------------------------------------------------------

" HLTest: tests if a highlighting group has been set up {{{2
fun! s:HLTest(hlname)
"  call Dfunc("HLTest(hlname<".a:hlname.">)")

  let id_hlname= hlID(a:hlname)
"  call Decho("hlID(".a:hlname.")=".id_hlname)
  if id_hlname == 0
"   call Dret("HLTest 0")
   return 0
  endif

  let id_trans = synIDtrans(id_hlname)
"  call Decho("id_trans=".id_trans)
  if id_trans == 0
"   call Dret("HLTest 0")
   return 0
  endif

  let fg_hlname= synIDattr(id_trans,"fg")
  let bg_hlname= synIDattr(id_trans,"bg")
"  call Decho("fg_hlname<".fg_hlname."> bg_hlname<".bg_hlname.">")

  if fg_hlname == -1 && bg_hlname == -1
"   call Dret("HLTest 0")
   return 0
  endif
"  call Dret("HLTest 1")
  return 1
endfun

" ---------------------------------------------------------------------
" SpchkHighlight: define Warning Dialect Notice and RareWord as needed {{{2
fun! s:SpchkHighlight()
  " check if user has specified a Dialect highlighting group.
  " If not, this script will highlight-link it to a Warning highlight group.
  " If that hasn't been defined, then this script will define it.
  if !s:HLTest("Dialect")
   if !s:HLTest("Warning")
    hi Warning term=NONE cterm=NONE gui=NONE ctermfg=black ctermbg=yellow guifg=black guibg=yellow
   endif
   hi link Dialect Warning
  endif
  
  " check if user has specified a RareWord highlighting group
  " If not, this script will highlight-link it to a Warning highlight group.
  " If that hasn't been defined, then this script will define it.
  if  !<SID>HLTest("RareWord")
   if !<SID>HLTest("Notice")
    hi Notice term=NONE cterm=NONE gui=NONE ctermfg=black ctermbg=cyan guifg=black guibg=cyan
   endif
   hi link RareWord Notice
  endif
endfun
call <SID>SpchkHighlight()
if v:version < 700
 au CursorHold,FocusGained	*	silent call <SID>SpchkHighlight()
else
 au ColorScheme,FocusGained	*	silent call <SID>SpchkHighlight()
endif

" ---------------------------------------------------------------------

" SaveMap: this function sets up a buffer-variable (b:spchk_restoremap) {{{2
"          which will be used by StopDrawIt to restore user maps
"          mapchx: either <something>  which is handled as one map item
"                  or a string of single letters which are multiple maps
"                  ex.  mapchx="abc" and maplead='\': \a \b and \c are saved
fun! <SID>SaveMap(mapmode,maplead,mapchx)
"  call Dfunc("SaveMap(mapmode<".a:mapmode."> maplead<".a:maplead."> mapchx<".a:mapchx.">)")
"  call Decho("in savemap: bufnr#".bufnr("%")." name<".bufname("%").">")
"  call Decho("is single map? strpart(mapchx<".a:mapchx.">,0,1) == ".strpart(a:mapchx,0,1))
  if strpart(a:mapchx,0,1) == '<'
   " save single map <something>
   if maparg(a:mapchx,a:mapmode) != ""
"     call Decho("saving single map ".a:mapchx)
     let b:spchk_restoremap= a:mapmode."map ".a:mapchx." ".maparg(a:mapchx,a:mapmode)."|".b:spchk_restoremap
     exe a:mapmode."unmap ".a:mapchx
    endif
  else
   " save multiple maps
   let i= 1
   while i <= strlen(a:mapchx)
    let amap=a:maplead.strpart(a:mapchx,i-1,1)
"	call Decho("multimaps: maparg(amap<".amap.">,mapmode<".a:mapmode.">)=".maparg(amap,a:mapmode))
    if maparg(amap,a:mapmode) != ""
"     call Decho("saving multiple maps: <".amap.">")
     let b:spchk_restoremap= a:mapmode."map ".amap." ".maparg(amap,a:mapmode)."|".b:spchk_restoremap
     exe a:mapmode."unmap ".amap
    endif
    let i= i + 1
   endwhile
  endif
"  call Dret("SaveMap")
endfunction

" ---------------------------------------------------------------------
"  User Interface: {{{1
let b:spchk_restoremap= ""
"call Decho("starting savemap: bufnr#".bufnr("%")." name<".bufname("%").">")
call <SID>SaveMap("n",s:usermaplead.'e',"a")
call <SID>SaveMap("n",s:usermaplead.'e',"d")
call <SID>SaveMap("n",s:usermaplead.'e',"e")
call <SID>SaveMap("n",s:usermaplead.'e',"j")
call <SID>SaveMap("n",s:usermaplead.'e',"J")
call <SID>SaveMap("n",s:usermaplead.'e',"n")
call <SID>SaveMap("n",s:usermaplead.'e',"p")
call <SID>SaveMap("n",s:usermaplead.'e',"t")
call <SID>SaveMap("n",s:usermaplead.'e',"T")
call <SID>SaveMap("n",s:usermaplead.'e',"s")
call <SID>SaveMap("n",s:usermaplead.'e',"S")
call <SID>SaveMap("v",s:usermaplead.'e',"V")

" Maps to facilitate entry of new words {{{2
"  use  temporarily (\et)   remove temporarily (\eT)
"  save permanently (\es)   remove permanently (\eS)
if !hasmapto('<Plug>Spchket')
 nmap <unique> <Leader>et <Plug>Spchket
endif
nmap <silent> <script> <Plug>Spchket :syn case ignore<CR>:exe "syn keyword GoodWord transparent	" . expand("<cword>")<CR>:syn case match<CR>:if b:spchkautonext<BAR>call <SID>SpchkNxt(0)<BAR>endif<CR>

if !hasmapto('<Plug>SpchkeT')
 nmap <unique> <Leader>eT <Plug>SpchkeT
endif
nmap <silent> <script> <Plug>SpchkeT :syn case ignore<CR>:exe "syn keyword BadWord "	  . expand("<cword>")<CR>:syn case match<CR>

" \es: saves a new word to a user dictionary (b:cvimsyn/engspchk.usr). {{{2
"      Uses vim-only functions to do save, thereby avoiding external programs
" \eS: remove new word from user dictionary
if !hasmapto('<Plug>Spchkes')
 nmap <unique> <Leader>es <Plug>Spchkes
endif
nmap <silent> <script> <Plug>Spchkes    :call <SID>SpchkSave(expand("<cword>"),"usr")<CR>
 
if !hasmapto('<Plug>SpchkeS')
 nmap <unique> <Leader>eS <Plug>SpchkeS
endif
nmap <silent> <script> <Plug>SpchkeS    :call <SID>SpchkRemove(expand("<cword>"),"usr")<CR>

" \ej: saves a new word to a project dictionary (b:cvimsyn/engspchk.proj). {{{2
" \eJ: remove new word from project dictionary
if !hasmapto('<Plug>Spchkej')
 nmap <unique> <Leader>ej <Plug>Spchkej
endif
nmap <silent> <script> <Plug>Spchkej    :call <SID>SpchkSave(expand("<cword>"),"proj")<CR>

if !hasmapto('<Plug>SpchkeJ')
 nmap <unique> <Leader>eJ <Plug>SpchkeJ
endif
nmap <silent> <script> <Plug>SpchkeJ    :call <SID>SpchkRemove(expand("<cword>"),"proj")<CR>

" \ed: toggle between Dialect->Warning/Error {{{2
" \ee: end engspchk
if !hasmapto('<Plug>Spchked')
 nmap <unique> <Leader>ed <Plug>Spchked
endif
nmap <silent> <script> <Plug>Spchked	:call <SID>SpchkToggleDialect()<CR>
 
if !hasmapto('<Plug>Spchker')
 nmap <unique> <Leader>er <Plug>Spchker
endif
nmap <silent> <script> <Plug>Spchker	:call <SID>SpchkToggleRareWord()<CR>
 
if !hasmapto('<Plug>Spchkee')
 nmap <unique> <Leader>ee <Plug>Spchkee
endif
nmap <silent> <script> <Plug>Spchkee	:call <SID>SpchkEnd()<CR><bar>:redraw!<CR>
 
" \eV: unset visual-block spellchecking (visual mode)
if !hasmapto('<Plug>SpchkeVvis')
 vmap <unique> <Leader>eV <Plug>SpchkeVvis
endif
vmap <silent> <script> <Plug>SpchkeVvis  :<c-u>call <SID>SpchkVisBlock(1)<CR>

" \eV: unset visual-block spellchecking (normal mode)
if !hasmapto('<Plug>SpchkeVnrml')
 nmap <unique> <Leader>eV <Plug>SpchkeVnrml
endif
nmap <silent> <script> <Plug>SpchkeVnrml :call <SID>SpchkVisBlock(1)<CR>

" mouse stuff: {{{2
if b:spchkmouse > 0
 if &mouse !~ '[na]'
  " insure that the mouse will work with Click-N-Fix
  set mouse+=n
 endif
 call <SID>SaveMap("n","","<leftmouse>")
 call <SID>SaveMap("n","","<middlemouse>")
 call <SID>SaveMap("n","","<rightmouse>")
 nnoremap <silent> <leftmouse>    <leftmouse>:call <SID>SpchkMouse(0)<CR>
 nnoremap <silent> <middlemouse>  <leftmouse>:call <SID>SpchkMouse(1)<CR>
 nnoremap <silent> <rightmouse>   <leftmouse>:call <SID>SpchkRightMouse()<CR>
 if has("clipboard")
  " insure that the paste buffer has nothing in it
  let @*=""
 endif
endif

" ---------------------------------------------------------------------
" DrChip Menu {{{2
if exists("did_install_default_menus") && has("menu") && has("gui_running") && &go =~ 'm'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Alternative\ spellings<tab>'.s:mapleadstring.'ea		'.s:usermaplead.'ea'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Move\ to\ next\ spelling\ error<tab>'.s:mapleadstring.'en	'.s:usermaplead.'en'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Move\ to\ previous\ spelling\ error<tab>'.s:mapleadstring.'ep	'.s:usermaplead.'ep'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Save\ word\ to\ user\ dictionary\ (temporarily)<tab>'.s:mapleadstring.'et	'.s:usermaplead.'et'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Save\ word\ to\ user\ dictionary\ (permanently)<tab>'.s:mapleadstring.'es	'.s:usermaplead.'es'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Save\ word\ to\ proj\ dictionary\ (permanently)<tab>'.s:mapleadstring.'ej	'.s:usermaplead.'ej'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Remove\ word\ from\ user\ dictionary\ (temporarily)<tab>'.s:mapleadstring.'eT	'.s:usermaplead.'eT'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Remove\ word\ from\ user\ dictionary\ (permanently)<tab>'.s:mapleadstring.'eS	'.s:usermaplead.'eS'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Remove\ word\ from\ proj\ dictionary\ (permanently)<tab>'.s:mapleadstring.'eJ	'.s:usermaplead.'eJ'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Dialect:\ toggle\ Warning/Error\ highlighting<tab>'.s:mapleadstring.'ed	'.s:usermaplead.'ed'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.RareWord:\ toggle\ Warning/Error\ highlighting<tab>'.s:mapleadstring.'er	'.s:usermaplead.'er'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Load\ '.b:Spchklang.'spchk<tab>'.s:mapleadstring.'ec		'.s:usermaplead.'ec'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.End\ '.b:Spchklang.'spchk<tab>'.s:mapleadstring.'ee		'.s:usermaplead.'ee'
 exe 'menu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Help<tab>\ 		:help engspchk<cr>'
" call Decho("installed menu items: spchk.Save, Remove, Dialect, Load, End, etc")
endif

" ---------------------------------------------------------------------

" IGNORE CASE
syn case ignore

" Language Specials {{{2
" Ignore upper/lower case
" For non-English, allow accented (8-bit) characters as keywords
if b:spchklang =~? "^eng"
" call Decho("Language Special<eng> : b:spchkpunc=".b:spchkpunc)
 if b:spchkpunc != 0
  " These patterns are thanks to Steve Hall
  " Flag as error a non-capitalized word after ellipses
  syn match GoodWord	"\.\.\. \{0,2}\l\@="
  " but not non-capitalized word after ellipses plus period
  syn match BadWord "\.\.\.\. \{0,2}\l"
 
  " non-lowercased end-of-word problems
  " required: period/question-mark/exclamation-mark
  " optional: double/single quote
  " required: return/return-linefeed/space/two spaces
  " required: lowercase letter
  syn match BadWord "[.?!][\"']\=[\r\n\t ]\+\l"
 endif
elseif b:spchklang =~? "^fr"
" call Decho("Language Special<".b:spchklang."> isk has no -")
 setlocal isk=48-57,_,65-90,_,97-122,128-255
else
" call Decho("Language Special<".b:spchklang."> isk has -")
 setlocal isk=45,48-57,_,65-90,_,97-122,128-255
endif

" ---------------------------------------------------------------------
" Find Dictionary Path: {{{2

if b:cvimsyn == ""
" call Decho('Please set either g:cvimsyn (vim) or $CVIMSYN (environment)')
 echoerr 'Please set either g:cvimsyn or the CVIMSYN environment variable (:help cvimsyn)'
 call inputsave()|call input("Press <cr> to continue")|call inputrestore()
 let &cpo= s:keepcpo
 unlet s:keepcpo
 finish
endif

"call Decho("attempt to find dictionaries: spchklang<".b:spchklang.">")
"call Decho("trying <".b:cvimsyn."/".b:spchklang."spchk.dict>")
if !filereadable(b:cvimsyn."/".b:spchklang."spchk.dict")
 let b:cvimsyn= expand(b:cvimsyn)

" call Decho("trying <".b:cvimsyn."/".b:spchklang."spchk.dict>")
 if !filereadable(b:cvimsyn."/".b:spchklang."spchk.dict")
  let rtp= &rtp

  " search runtimepath
  while rtp != ""
   " get leftmost path from rtp
   let b:cvimsyn= substitute(rtp,',.*$','','')."/CVIMSYN"
"   call Decho("setting b:cvimsyn<".b:cvimsyn.">")

   " remove leftmost path from rtp
   if stridx(rtp,',') == -1
    let rtp= ""
   else
    let rtp= substitute(rtp,'.\{-},','','e')
   endif

   " see if dictionary is readable
"   call Decho("trying <".b:cvimsyn."/".b:spchklang."spchk.dict>")
   if filereadable(b:cvimsyn."/".b:spchklang."spchk.dict")
    break
   else
    " attempt to expand and see if dictionary is readable then
    let b:cvimsyn= expand(b:cvimsyn)
"    call Decho("trying <".b:cvimsyn."/".b:spchklang."spchk.dict>")
    if filereadable(b:cvimsyn."/".b:spchklang."spchk.dict")
     break
	else
	 let b:cvimsyn= ""
    endif
   endif
  endwhile
 endif

 " sanity check
 if !exists("b:cvimsyn") || b:cvimsyn == ""
"  call Decho("Unable to find ".b:spchklang."spchk.dict dictionary (:help cvimsyn)")
  echoerr "Unable to find ".b:spchklang."spchk.dict dictionary (try :help cvimsyn)"
  call inputsave()|call input("Press <cr> to continue")|call inputrestore()
  let &cpo= s:keepcpo
  unlet s:keepcpo
  finish
 endif
endif
"call Decho("final b:cvimsyn<".b:cvimsyn.">")

" ---------------------------------------------------------------------

" ========================
" Loading The Dictionaries {{{1
" ========================

" ---------------------------------------------------------------------
" SpchkLoadDictionary: this function loads the specified dictionary {{{2
"     reqd=0          not required      (no error message if not present)
"         =1          check if writable (not required)
"         =2          required          (gives error message if not present)
"     lang="eng" etc  language of dictionary
"     dict="dict"     main dictionary
"         ="rare"     rarewords dictionary
"         ="dialect"  dialect dictionary
"         ="usr"      user's personal dictionary
"         ="proj"     project-specific dictionary
"         ="match"    loads after BadWord is defined, hence
"                     syn match GoodWord "..pattern.." will take priority
"                     
"
"         This function will first set "syn case ignore".  However,
"         the dictionary file itself may override this with a
"         leading "syn case match".
fun! s:SpchkLoadDictionary(reqd,lang,dict)
"  call Dfunc("SpchkLoadDictionary(reqd=".a:reqd." lang<".a:lang."> dict<".a:dict.">)")
"  let loadtime= localtime()		" Decho

  " set up short and long names
  let shortname= a:lang."spchk.".a:dict
  let fullname = b:cvimsyn."/".shortname
"  call Decho("shortname<".shortname.">")
"  call Decho("fullname <".fullname.">")

  " preferentially load from current directory
  if filereadable(shortname)
   let fullname= shortname
"   call Decho("will load <".fullname."> from current directory")
  endif

  " check if dictionary is readable
  if filereadable(fullname) > 0
   if !b:spchksilent
    echomsg "Loading  <".shortname.">"
   endif
   syn case ignore
   if s:incluster == 1
    com! -nargs=+ GoodWord syn keyword GoodWord transparent contained <args>
   else
    com! -nargs=+ GoodWord syn keyword GoodWord transparent <args>
   endif
   exe "so ".fullname
   delcommand GoodWord
   if a:reqd == 1
   	" check if writable
    if !filewritable(fullname)
     echomsg "***warning*** ".fullname." isn't writable"
    endif
   endif

  elseif a:reqd == 2
   if !filereadable(b:cvimsyn)
    echomsg "***warning*** ".b:cvimsyn."/ directory is not readable"
   else
    echomsg "***warning*** cannot read <".fullname.">"
   endif
  endif

"  let difftime= localtime() - loadtime	" Decho
"  call Dret("SpchkLoadDictionary ".shortname." : took ".difftime." seconds")
endfun

" ---------------------------------------------------------------------
" SpchkInCluster: {{{2
fun! s:SpchkInCluster()
"  call Dfunc("SpchkInCluster()")

  " Detect whether BadWords should be detected/highlighted inside comments.
  " This can be done only for those syntax files' comment blocks that
  " contains=@cluster.  The code below adds GoodWord and BadWord to various
  " clusters.  If your favorite syntax comments are not included, send a note
  if exists("b:spchk_incluster")
   let s:incluster= b:spchk_incluster
  else
   let s:incluster= 0
   if     &ft == "amiga"
"   " call Decho("amiga: GoodWord, BadWord added to Spell cluster")
    syn cluster Spell		add=GoodWord,BadWord
    let s:incluster=1
   elseif &ft == "bib"
"   " call Decho("bib: GoodWord, BadWord added to Spell cluster")
    syn cluster bibVarContents     	contains=GoodWord,BadWord
    syn cluster bibCommentContents 	contains=GoodWord,BadWord
    let s:incluster=1
   elseif &ft == "c" || &ft == "cpp"
"   " call Decho("c: GoodWord, BadWord added to Spell cluster")
    syn cluster Spell		add=GoodWord,BadWord
    let s:incluster=1
   elseif &ft == "csh"
"   "  call Decho("csh: GoodWord, BadWord added to Spell cluster")
    syn cluster Spell		add=GoodWord,BadWord
    let s:incluster=1
   elseif &ft == "dcl"
"   " call Decho("dcl: GoodWord, BadWord added to Spell cluster")
    syn cluster Spell		add=GoodWord,BadWord
    let s:incluster=1
   elseif &ft == "fortran"
"   " call Decho("fortran: GoodWord, BadWord added to Spell cluster")
    syn cluster fortranCommentGroup	add=GoodWord,BadWord
    syn match   fortranGoodWord contained	"^[Cc]\>"
    syn cluster fortranCommentGroup	add=fortranGoodWord
    hi link fortranGoodWord fortranComment
    let s:incluster=1
   elseif &ft == "html"
"   " call Decho("html: GoodWord, BadWord added to Spell cluster")
    syn cluster Spell			add=GoodWord,BadWord
    let s:incluster=2
   elseif &ft == "mail"
"   " call Decho("mail: GoodWord, BadWord added to Spell cluster")
    syn cluster Spell			add=GoodWord,BadWord
    let s:incluster=2
   elseif &ft == "sh"
"   " call Decho("sh: GoodWord, BadWord added to Spell cluster")
    syn cluster Spell			add=GoodWord,BadWord
    let s:incluster=1
   elseif &ft == "tex"
"   " call Decho("tex: GoodWord, BadWord added to Spell cluster")
    syn cluster Spell			add=GoodWord,BadWord
    syn cluster texMatchGroup	add=GoodWord,BadWord
    let s:incluster=2
   elseif &ft == "vim"
"   " call Decho("vim: GoodWord, BadWord added to Spell cluster")
    syn cluster Spell			add=GoodWord,BadWord
    let s:incluster=1
   endif
  endif
"  call Decho("s:incluster=".s:incluster." ft=".&ft)
  
  " attempt to infer spellcheck use - is the Spell cluster included somewhere?
  if s:incluster == 0
   fun! <SID>ChkForCluster(cname)
"    call Dfunc("ChkForCluster(cname<".a:cname.">)")
    let keep_rega= @a
    redir @a
    exe "syn list @".a:cname
    redir END
    if match(@a,"E392") != -1
     let has_cluster= 0
    elseif match(@a,"No Syntax items defined") != -1
     let has_cluster= 0
    else
     let has_cluster= 1
    endif
    let @a= keep_rega
"    call Dret("ChkForCluster has_cluster<".a:cname.">=".has_cluster)
    return has_cluster
   endfun
  
   silent! let has_cluster= s:ChkForCluster("Spell")
   if has_cluster
"    call Decho("inferred @Spell: add GoodWord,BadWord to Spell cluster; setting s:incluster=".s:incluster)
    syn cluster Spell				add=GoodWord,BadWord
    let s:incluster=1
   else
    " @Spell cluster not used since the syntax writer didn't use @Spell.
"    " call Decho(&ft." doesn't have @Spell.  Using containedin...")
    echomsg "***warning*** syntax <".&ft."> doesn't support spell-checking"
   endif
   silent! has_cluster= ChkForCluster("texMatchGroup")
   if has_cluster
    syn cluster texMatchGroup		add=GoodWord,BadWord
   endif
   unlet has_cluster
   delfun <SID>ChkForCluster
  endif

"  call Dret("SpchkInCluster")
endfun

" ---------------------------------------------------------------------

" Load dictionaries {{{2
"  in reverse order of priority
"call Decho("load dictionaries in reverse priority order")
call s:SpchkInCluster()
call s:SpchkLoadDictionary(0,b:spchklang,"proper")
call s:SpchkLoadDictionary(0,b:spchklang,"rare")
call s:SpchkLoadDictionary(0,b:spchklang,"dialect")
call s:SpchkLoadDictionary(0,b:spchklang,"proj")
call s:SpchkLoadDictionary(2,b:spchklang,"dict")
call s:SpchkLoadDictionary(1,b:spchklang,"usr")

" Resume Case Sensitivity
syn case match

" ---------------------------------------------------------------------

" The Raison D'Etre! Highlight the BadWords {{{2
" I've allowed '`- in non-English words
"
"    s:incluster
"        0       BadWords matched outside normally highlighted sections
"        1       BadWords matched inside @Spell, etc highlighting clusters
"        2       both #0 and #1
if s:incluster == 0 || s:incluster == 2 || b:spchknonhl
" call Decho("s:incluster=".s:incluster.": BadWords match outside syntax")
 if b:spchklang == "eng"
  syn match BadWord	"\<[^[:punct:][:space:][:digit:]]\{2,}\>"	 contains=OutOfBlock,RareWord,Dialect
 else
  syn match BadWord	"\<[^[!@#$%^&*()_+=[\]{};:",<>./?\\|[:space:][:digit:]]\{2,}\>" contains=OutOfBlock,RareWord,Dialect
 endif
endif
if s:incluster == 1 || s:incluster == 2
" call Decho("s:incluster=".s:incluster.": BadWords match inside syntax with @Spell (contained)")
 if b:spchklang == "eng"
  syn match BadWord contained	"\<[^[:punct:][:space:][:digit:]]\{2,}\>"	 contains=OutOfBlock,RareWord,Dialect
  syn cluster Spell add=Dialect,RareWord
 else
  syn match BadWord contained	"\<[^[!@#$%^&*()_+=[\]{};:",<>./?\\|[:space:][:digit:]]\{2,}\>" contains=OutOfBlock,RareWord,Dialect
 endif
endif

" Allows <engspchk.vim> to work better with LaTeX {{{2
if &ft == "tex"
"  call Decho("ft==tex: more GoodWords")
  syn match GoodWord	"{[a-zA-Z|@]\+}"lc=1,me=e-1
  syn match GoodWord	"\[[a-zA-Z]\+]"lc=1,me=e-1
  syn match texGoodWord	"\\[a-zA-Z]\+"lc=1,me=e-1	contained
  hi link texGoodWord texComment
  syn cluster texCommentGroup	add=texGoodWord
endif

" Ignore web addresses and \n for newlines {{{2
syn match GoodWord transparent	"\<http://www\.\S\+"
syn match GoodWord transparent	"\\n"

" Load the "contract" and "match" dictionaries {{{2
" these are intended to allow match patterns to be
" loaded after the BadWord (like English's contractions above).
" The "contraction" dictionary is the standard one; the "match"
" one is for the user to modify
call s:SpchkLoadDictionary(0,b:spchklang,"contraction")
call s:SpchkLoadDictionary(0,b:spchklang,"match")


" Acronymns {{{2
if b:spchkacronym
 " Pan Shizhu suggested that two or more capitalized letters
 " should be treated as an abbreviation and accepted.  You
 " may put "let b:spchkacronym= 1" in [lang]spchk.match
 syn match GoodWord	"\<\u\{2,}\>"
endif

" BadWords are highlighted with Error highlighting (by default) {{{2
"   Colorschemes, such as navajo-night, may define BadWord to
"   be something other than Error.  Hence engspchk will clear
"   that setting first.
hi clear BadWord
hi link BadWord Error

" ==================================================
" Support Functions: {{{1
" ==================================================
" SpchkSave: {{{2
fun! <SID>SpchkSave(newword,dict)
"  call Dfunc("SpchkSave(newword<".a:newword.">,dict<".a:dict.">)")
  silent 1sp
  if a:dict == "usr"
   exe "silent e ".b:cvimsyn."/".b:spchklang."spchk.".a:dict
  else
   exe "silent e ".b:spchklang."spchk.".a:dict
  endif
  $put='GoodWord	'.a:newword
  let un= bufnr(".")
  silent wq
  if bufexists(un)
   exe "silent bw ".un
  endif
  syn case ignore
  if s:incluster == 1
   exe "syn keyword GoodWord contained transparent ".a:newword
  else
   exe "syn keyword GoodWord transparent ".a:newword
  endif
  syn case match
  if b:spchkautonext
   call s:SpchkNxt(0)
  endif
"  call Dret("SpchkSave")
endfun

" ---------------------------------------------------------------------
" SpchkRemove: implements \eS : depends on SpchkSave's putting one {{{2
"              user word per line in <*spchk.usr>.  This function
"              actually will delete the entire line containing the
"              new BadWord.
fun! <SID>SpchkRemove(killword,dict)
"  call Dfunc("SpchkRemove(killword<".a:killword.">,dict<".a:dict.">)")
  silent 1sp
  if a:dict == "usr"
   exe "silent e ".b:cvimsyn."/".b:spchklang."spchk.".a:dict
  else
   exe "silent e ".b:spchklang."spchk.".a:dict
  endif
  exe "silent g/".a:killword."/d"
  silent wq
  syn case ignore
  exe "syn keyword BadWord ".a:killword
  syn case match
"  call Dret("SpchkRemove")
endfun

if !hasmapto('<Plug>SpchkNxt')
 nmap <unique> <Leader>en	<Plug>SpchkNxt
endif
nmap <silent> <script> <Plug>SpchkNxt	:call <SID>SpchkNxt(1)<CR>

if !hasmapto('<Plug>SpchkPrv')
 nmap <unique> <Leader>ep	<Plug>SpchkPrv
endif
nmap <silent> <script> <Plug>SpchkPrv	:call <SID>SpchkPrv(1)<CR>

" ignores people's middle-name initials
syn match   GoodWord	"\<[A-Z]\."

" -------------------------------------------------------------------
" SpchkNxt: calls this function to search for next spelling error (\en) {{{2
fun! <SID>SpchkNxt(autofix)
"  call Dfunc("SpchkNxt(autofix=".a:autofix.")")
  if a:autofix == 1 && exists("b:spchksaveposn")
   unlet b:spchksaveposn
  endif
"  call Decho("SpchkNxt(".a:autofix.")")
  let lastline = line("$")
  let curcol   = 0
  let fenkeep  = &fen
  set nofen
  silent! norm! w

  " skip words until we find next error
  while synIDtrans(synID(line("."),col("."),1)) != s:errid
    silent! norm! w
    if line(".") == lastline
      let prvcol = curcol
      let curcol = col(".")
      if curcol == prvcol
	   echo "at end-of-file"
"	   call Decho("at end-of-file")
	   if exists("b:spchksaveposn")
"	   	call Decho("unlet b:spchksaveposn")
	    unlet b:spchksaveposn
	   endif
       break
      endif
    endif
  endwhile

  " cleanup
  let &fen= fenkeep
  if foldlevel(".") > 0
   norm! zO
  endif
  unlet curcol
  unlet lastline
  if exists("prvcol")
    unlet prvcol
  endif
"  call Dret("SpchkNxt : <".expand("<cword>").">")
endfunction

" -------------------------------------------------------------------
" SpchkPrv: calls this function to search for previous spelling error (\ep) {{{2
fun! <SID>SpchkPrv(autofix)
"  call Dfunc("SpchkPrv(autofix=".a:autofix.")")
  if a:autofix == 1 && exists("b:spchksaveposn")
   unlet b:spchksaveposn
  endif
  let curcol  = 0
  let fenkeep = &fen
  set nofen

  silent! norm! ge

  " skip words until we find previous error
  while synIDtrans(synID(line("."),col("."),1)) != s:errid
"call Decho("SpchkPrv: word<".expand("<cword>")."> hl=".synIDtrans(synID(line("."),col("."),1))." errid=".s:errid)
    norm! b
    if line(".") == 1
      let prvcol = curcol
      let curcol = col(".")
      if curcol == prvcol
	   echo "at beginning-of-file"
       break
      endif
    endif
  endwhile
"call Decho("SpchkPrv: word<".expand("<cword>")."> hl=".synIDtrans(synID(line("."),col("."),1))." errid=".s:errid)

  " cleanup
  let &fen= fenkeep
  if foldlevel(".") > 0
   norm! zO
  endif
  unlet curcol
  if exists("prvcol")
    unlet prvcol
  endif
"  call Dret("SpchkPrv : <".expand("<cword>").">")
endfunction

if !hasmapto('<Plug>SpchkAlternate')
 nmap <silent> <Leader>ea <Plug>SpchkAlternate
endif
nmap <silent> <script> <Plug>SpchkAlternate :call <SID>SpchkAlternate(expand("<cword>"))<CR>

" -----------------------------------------------------------------

" Prevent error highlighting while one is typing {{{2
" Chase Tingley implemented \%# which is used to forestall
" Error highlighting of words while one is typing them.
syn match GoodWord "\<\k\+\%#\>"
syn match GoodWord "\<\k\+'\%#"

" -----------------------------------------------------------------

" BuildWordlist: Build the <engspchk.wordlist>
fun! s:BuildWordlist(cvimsyn,lang,dict)
"  call Dfunc("BuildWordlist(lang<".a:lang."> dict<".a:dict.">")

  " set up short and long names
  let shortname= a:lang."spchk.".a:dict
  let fullname = a:cvimsyn."/".shortname

  " preferentially load from current directory
  if filereadable(shortname)
   let fullname= shortname
"   call Decho("will build wordlist with <".fullname.">")
  endif
  if filereadable(fullname)
"   call Decho("<".fullname.". is readable; now appending to wordlist")
   $
   exe "silent r ".fullname
   $
"   call Dret("BuildWordlist 1")
   return 1
  endif

  " unable to load requested dictionary
  $
"  call Dret("BuildWordlist 0")
  return 0
endfun

" ---------------------------------------------------------------------
" SpchkAlternate: handle words that are close in spelling {{{2
fun! <SID>SpchkAlternate(wrd)
"  call Dfunc("SpchkAlternate(wrd<".a:wrd.">)")
   call s:SpchkSavePosn()

  " can't provide alternative spellings without agrep
  if !executable("agrep")
   echoerr "engspchk: needs agrep for alternative spellings support (:help engspchk-agrep)"
   call inputsave()|call input("Press <cr> to continue")|call inputrestore()
"   call Dret("SpchkAlternate : needs agrep")
   return
  endif
"  call Decho("agrep is executable")

  if !exists("b:spchklang")
   echomsg "Reloading engspchk"
   silent! call <SID>LoadSpchk()
  endif
  " because SpchkAlternate switches buffers to an "alternate spellings"
  " window, the various b:... variables will no longer be available, but
  " they're still needed!
  let spchklang     = b:spchklang
  let cvimsyn       = b:cvimsyn
  let spchkaltright = b:spchkaltright
  let spchkdialect  = b:spchkdialect
  let s:iskkeep     = &isk
"  call Decho("options: lang<".spchklang."> cvimsyn<".cvimsyn."> altright=".spchkaltright." dialect<".spchkdialect.">")

  silent! set isk-=#
  norm! "_yiw
  if exists("g:spchkwholeword")
"   call Decho("using g:spchkwholeword=".g:spchkwholeword)
   exe "match PreProc '\\%".line(".")."l\\%".col('.')."c\\k\\+'"
  else
"   call Decho("not using g:spchkwholeword=".g:spchkwholeword)
   exe "match PreProc '\\%".line(".")."l\\%".col('.')."c'"
  endif

  if exists("s:spchkaltwin")
"	call Decho("| re-use wordlist in alternate window")
    let s:winnr= winnr()
    " re-use wordlist in bottom window
	if spchkaltright > 0
     exe "norm! \<c-w>bG"
	 %d
	 put! ='Alternates for'
	 2d
	 put =' <'.a:wrd.'>'
	 exe "silent! norm! \<Esc>"
	else
     exe "norm! \<c-w>bG0DAAlternates<".a:wrd.">: \<Esc>"
	endif

  elseif filereadable(cvimsyn."/".spchklang."spchk.wordlist")
    " utilize previously generated <engspchk.wordlist>
"	call Decho("| utilize previously generated ".spchklang."spchk.wordlist")

    " Create a window to hold dictionaries during conversion
    let s:winnr= winnr()
	if spchkaltright > 0
     exe "vertical bo ".spchkaltright."new"
	else
     bo 1new
    endif
    let s:spchkaltwin= bufnr("%")
    setlocal lz bt=nofile nobl noro noswapfile
	if spchkaltright > 0
	 wincmd b
	 %d
	 put! ='Alternates for'
	 2d
	 put =' <'.a:wrd.'>'
	else
	 setlocal winheight=1
     exe "norm! \<c-w>bG0DAAlternates<".a:wrd.">: \<Esc>"
	endif

  else
    " generate <engspchk.wordlist> from dictionaries
"	call Decho("| build wordlist")
    echo "Building <".spchklang."spchk.wordlist>"
    echo "This may take awhile, but it is a one-time only operation."
    echo "Please be patient..."

    " following avoids a problem on Macs with ffs="mac,unix,dos"
    let ffskeep= &ffs
    set ffs="unix,dos"

    " Create a one line window to hold dictionaries during conversion
    let s:winnr= winnr()
	if spchkaltright > 0
"	 call Decho("creating ".spchkaltright."-column window on right")
     exe "vertical bo ".spchkaltright."new"
	else
"	 call Decho("creating one-line alternate-spelling window on bttm")
     bo 1new
	 setlocal winheight=1
    endif
    let s:spchkaltwin= bufnr("%")
    setlocal lz bt=nofile noswapfile nobl noro

    " for quicker operation
    "   turn off undo
    "   turn on lazy-update
    "   make a temporary one-line window
    let ulkeep= &ul
    let gdkeep= &gd
    set ul=-1 nogd

	call s:BuildWordlist(cvimsyn,spchklang,"dict")
	call s:BuildWordlist(cvimsyn,spchklang,"usr")
	call s:BuildWordlist(cvimsyn,spchklang,"proj")
	put
	let firstline   = line("$")
	let diddialect  = s:BuildWordlist(cvimsyn,spchklang,"dialect")
	let lastline    = line("$")
	let didrareword = s:BuildWordlist(cvimsyn,spchklang,"rare") 
	call s:BuildWordlist(cvimsyn,spchklang,"proper")

    " Remove non-dictionary lines and make it one word per line
	"   Keep RareWords
	"   Remove Dialect, comments, etc
"	call Decho("doing conversion")
    echo "Doing conversion..."

	" remove non-selected dialect
	if diddialect
"	 call Decho("remove non-selected dialect<".spchkdialect."> in lines".firstline."-".lastline)
	 exe firstline.';/? "'.spchkdialect.'"/d'
	 /^\(elseif\|endif\)/
	 exe '.,'.lastline."d"
	endif

	if didrareword
	 silent! %s/^syn\s*keyword\s*RareWord/GoodWord/
	endif

    silent v/^GoodWord\>/d
    %s/^GoodWord\s\+//
"	call Decho("make it one word per line")
    silent! exe '%s/\s\+/\r/g'
	if executable("sort") && has("unix")
	 " if sort is available, run wordlist through it as a filter
	 echo "Sorting wordlist"
	 exe '%!sort'
	endif
    echo "Writing ".cvimsyn."/".spchklang."spchk.wordlist"
    exe "w! ".cvimsyn."/".spchklang."spchk.wordlist"
	" re-use same buffer for Alternate spellings
	" building wordlist
    silent %d
	put! ='Alternates for'
	2d
	put =' <'.a:wrd.'>'
	norm! 0
    let &ul = ulkeep
    let &ffs= ffskeep
    let &gd = gdkeep
  endif

  " set up local-to-alternate-window-only maps for <CR> and <tab> to invoke SpchkChgWord
  nnoremap <buffer> <silent> <CR>  :call <SID>SpchkChgWord(0)<CR>
  nnoremap <buffer> <silent> <tab> :call <SID>SpchkChgWord(1)<CR>

  " keep initial settings
  let s:keep_mod = &mod
  let s:keep_wrap= &wrap
  let s:keep_ic  = &ic
  let s:keep_lz  = &lz
  cnoremap  <silent> <buffer> q      :call <SID>SpchkExitChgWord()<CR>
  nnoremap  <silent> <buffer> q      :call <SID>SpchkExitChgWord()<CR>
  nnoremap  <silent> <buffer> :      :call <SID>SpchkExitChgWord()<CR>
  nnoremap  <silent> <buffer> <c-w>c :call <SID>SpchkExitChgWord()<CR>
  setlocal nomod nowrap ic nolz

  " let's add a wee bit of color...
  set lz
  syn match altLeader	"^Alternates\="
  syn match altAngle	"[<>]"
  hi def link altLeader	Statement
  hi def link altAngle	Delimiter

  " set up path+wordlist
  let wordlist= cvimsyn."/".spchklang."spchk.wordlist"
  if &term == "win32" && !filereadable(wordlist)
   let wordlist= substitute(wordlist,'/','\\','g')
  else
   let wordlist= substitute(wordlist,'\\','/','g')
  endif

  " Build patterns based on permutations of up to 3 letters
  exe "silent norm! \<c-w>b"
  if (spchklang ==? "eng" || spchklang==? "ger") && strlen(a:wrd) > 2
   let agrep_opt= "-S2 "
  else
   let agrep_opt= " "
  endif
  if &term == "win32"
   let agrep_opt= agrep_opt."-V0 "
  else
   let agrep_opt= agrep_opt." "
  endif
  " if German and first char is capitalized: case sensitive search
  if spchklang !=? "ger" || match(a:wrd, '\u') == -1
   let agrep_opt= agrep_opt." -i "
  endif
  if strlen(a:wrd) > 2
   " agrep options:  -2  max qty of errors permitted in finding approx match
   "                 -i  case insensitive search enabled
   "                 -w  search for pattern as a word (surrounded by non-alpha)
   "                 -S2 set cost of a substitution to 2
"    call Decho("running: agrep -2 -w ".agrep_opt."\"".a:wrd."\" \"".wordlist."\"")
    exe  "silent r! agrep -2 -w ".agrep_opt."\"".a:wrd."\" \"".wordlist."\""
  else
   " handle two-letter words
"   call Decho("running: agrep -1 -w ".agrep_opt."\"".a:wrd."\" \"".wordlist."\"")
   exe "silent r! agrep -1 -w ".agrep_opt."\"".a:wrd."\" \"".wordlist."\""
  endif
  if spchkaltright > 0
   3
  else
   silent %j
   silent norm! 04w
  endif
  setlocal nomod
  set nolz
"  call Dret("SpchkAlternate")
endfun

" ---------------------------------------------------------------------

" SpchkChgWord: {{{2
fun! <SID>SpchkChgWord(allfix)
"  call Dfunc("SpchkChgWord(allfix=".a:allfix.")")
  let reg0keep= @0
  norm! yiw
  let goodword= @0
"  call Decho("| goodword<".goodword.">")
  exe s:winnr."wincmd w"
  norm! yiw
  let badword=@0
  if col(".") == 1
   exe "norm! ea#\<Esc>b"
  else
   exe "norm! hea#\<Esc>b"
  endif
  norm! yl
  if match(@@,'\u') == -1
   " first letter not capitalized
   exe "norm! de\<c-w>blbye".s:winnr."\<c-w>wPlxb"
  else
   " first letter *is* capitalized
   exe "norm! de\<c-w>blbye".s:winnr."\<c-w>wPlxb~h"
  endif
  exe "silent norm! \<c-w>b:silent q!\<cr>"
  exe "silent! ".s:winnr."winc w"
  if a:allfix == 1
   let gdkeep= &gd
   set nogd
   let g:keep="bad<".badword."> good<".goodword.">"
   exe "silent! %s/".badword."/".goodword."/ge"
   norm! ``
   let &gd= gdkeep
  endif
  unlet s:spchkaltwin
  let &wrap = s:keep_wrap
  let &ic   = s:keep_ic
  let &lz   = s:keep_lz
  let @0    = reg0keep
  let &isk  = s:iskkeep
  unlet s:keep_mod s:keep_wrap s:keep_ic s:keep_lz s:iskkeep
  if b:spchkautonext
   call s:SpchkNxt(0)
  endif
  match
"  call Dret("SpchkChgWord")
endfun

" ---------------------------------------------------------------------

" SpchkExitChgWord: restore options and exit from change-word window {{{2
fun! <SID>SpchkExitChgWord()
"  call Dfunc("SpchkExitChgWord()")
  unlet s:spchkaltwin
  let &mod  = s:keep_mod
  let &wrap = s:keep_wrap
  let &ic   = s:keep_ic
  let &lz   = s:keep_lz
  unlet s:keep_mod s:keep_wrap s:keep_ic s:keep_lz
  q!
  let &isk  = s:iskkeep
  unlet s:iskkeep
  match
  redraw!
"  call Dret("SpchkExitChgWord")
endfun

" ---------------------------------------------------------------------
" SpchkMouse: {{{2
"    mode==0: leftmouse
"        ==1:  middlemouse
"        ==2:  rightmouse
fun! <SID>SpchkMouse(mode)
"  call Dfunc("SpchkMouse(mode=".a:mode.")")

  if exists("s:spchkaltwin") && bufnr("%") == s:spchkaltwin
   " leftmouse and alternate window exists: (cursor must be in alternate-words window)
   " change word and open alternate-spelling window on new word
"   call Decho("SpchkMouse(mode=".a:mode.") in alternate-words window")
   if a:mode == 0 || a:mode == 2
    call s:SpchkChgWord(0)
    if synIDtrans(synID(line("."),col("."),1)) == s:errid
     call s:SpchkAlternate(expand("<cword>"))
    endif
   elseif a:mode == 1
   	" if in alternate-spellings window, and the middlemouse was clicked:
	"   exit alternate-words window
	"   restore cursor position
	"   put word into temporary dictionary
	"   goto next spelling error (if any)
    call s:SpchkExitChgWord()
    call s:SpchkRestorePosn()
	norm \et
    if synIDtrans(synID(line("."),col("."),1)) == s:errid
     call s:SpchkAlternate(expand("<cword>"))
	endif
   endif

  else " cursor in non-alternate-words window (ie. the text window)
"   call Decho("SpchkMouse(mode=".a:mode.") in normal window")
   if exists("s:spchkaltwin")
"   	call Decho("but s:spchkaltwin exists")
	" move cursor to bottom/right window (ie. the alternate-words window)
   	wincmd b
    call s:SpchkExitChgWord()
   endif

   if     a:mode == 0
   	" leftmouse : go to next spelling error in sequence, irregardless of
	"             mouse-specified cursor position
"    call Decho("a:mode=".a:mode.": leftmouse")
    call s:SpchkRestorePosn()
	if exists("b:spchksaveposn")
     norm! w
	endif
	" following sequence safely puts cursor at beginning of word
	norm! "_yiw
    if synIDtrans(synID(line("."),col("."),1)) != s:errid
"	 call Decho("currently not on Error, use SpchkNxt(0)")
     call s:SpchkNxt(0)
    endif

   elseif a:mode == 1
   	" middlemouse : go to next spelling error from current cursor position
	"               as specified by the mouseclick
"    call Decho("a:mode=".a:mode.": middlemouse")
	norm! "_yiw
    if synIDtrans(synID(line("."),col("."),1)) != s:errid
"	 call Decho("currently not on Error, use SpchkNxt(0)")
     call s:SpchkNxt(0)
    endif

   elseif a:mode == 2
   	" rightmouse : go to previous spelling error
"    call Decho("a:mode=".a:mode.": rightmouse")
    call s:SpchkRestorePosn()
	norm! "_yiw
    call s:SpchkPrv(0)

   endif

   " call SpchkAlternate() if already on Error word
   if synIDtrans(synID(line("."),col("."),1)) == s:errid
    call s:SpchkAlternate(expand("<cword>"))
   endif
  endif

"  call Dret("SpchkMouse")
endfun

" ---------------------------------------------------------------------

" SpchkRightMouse: click with rightmouse while in the alternate-words window {{{2
"                  and all similarly misspelled words will be replaced with
"                  the selected alternate word
fun! <SID>SpchkRightMouse()
"  call Dfunc("SpchkRightMouse()")
  if exists("s:spchkaltwin") && bufnr("%") == s:spchkaltwin
   " rightmouse - cursor in alternate-words window, so change
   "              all words with current spelling error and
   "              alternate word
   call s:SpchkChgWord(1)
   if b:spchkautonext && synIDtrans(synID(line("."),col("."),1)) == s:errid
    call s:SpchkAlternate(expand("<cword>"))
   endif
  else
   call s:SpchkMouse(2)
  endif
"  call Dret("SpchkRightMouse")
endfun

" ---------------------------------------------------------------------

" SpchkToggleDialect: toggles Dialect being mapped to Warning/Error {{{2
fun! <SID>SpchkToggleDialect()
"  call Dfunc("SpchkToggleDialect()")
  let dialectid= synIDtrans(hlID("Dialect"))
  let warningid= synIDtrans(hlID("Warning"))

  if dialectid == warningid
   hi link Dialect Error
  elseif dialectid == s:errid
   hi link Dialect NONE
  else
   hi link Dialect Warning
  endif
"  call Dret("SpchkToggleDialect")
endfun

" ---------------------------------------------------------------------

" SpchkToggleRareWord: toggles RareWord being mapped to Warning/Error {{{2
fun! <SID>SpchkToggleRareWord()
"  call Dfunc("SpchkToggleRareWord()")
  let rarewordid = synIDtrans(hlID("RareWord"))
  let noticeid   = synIDtrans(hlID("Notice"))

  if rarewordid == noticeid
   hi link RareWord Error
"   call Decho("RareWord switching to Error-highlighting")
  elseif rarewordid == s:errid
   hi link RareWord NONE
"   call Decho("RareWord switching to no-highlighting")
  else
   hi link RareWord Notice
"   call Decho("RareWord switching to Notice-highlighting")
  endif
"  call Dret("SpchkToggleRareWord")
endfun

" ---------------------------------------------------------------------

" SpchkEnd: end engspchk highlighting for the current buffer {{{2
fun! <SID>SpchkEnd()
"  call Dfunc("SpchkEnd()")

  " prevent \ee from "unloading" a buffer where \ec wasn't run
  if !exists("b:engspchk_loaded")
"   call Dret("SpchkEnd : b:engspchk_loaded doesn't exist, so \\ec hasn't run yet")
   return
  endif

  " restore normal highlighting for the current buffer
  " Thanks to Gary Johnson: filetype detect occurs prior
  " to "unlet'ing" b:engspchk_loaded so that any filetype
  " plugins that attempt to load engspchk see that it
  " is still loaded at this point.
  syn clear
  filetype detect

  let &bh                = b:hidden
  let s:engspchk_loadcnt = s:engspchk_loadcnt - 1
  unlet b:engspchk_loaded b:hidden
  call s:SpchkVisBlock(1)

  " remove engspchk maps
  if s:engspchk_loadcnt <= 0
   let s:engspchk_loadcnt= 0

   nunmap <Leader>ea
   nunmap <Leader>ed
   nunmap <Leader>ee
   nunmap <Leader>ej
   nunmap <Leader>eJ
   nunmap <Leader>en
   nunmap <Leader>ep
   nunmap <Leader>et
   nunmap <Leader>eT
   nunmap <Leader>es
   nunmap <Leader>eS
   nunmap <Leader>eV
   if b:spchkmouse > 0
   	nunmap <leftmouse>
   	nunmap <middlemouse>
   	nunmap <rightmouse>
   endif

   " restore user map(s), if any
"   call Decho("restoration: bufnr#".bufnr("%")." bufname<".bufname("%").">")
   if exists("b:spchk_restoremap") && b:spchk_restoremap != ""
"   	call Decho("restoring user maps, if any")
    exe b:spchk_restoremap
    silent! unlet b:spchk_restoremap
   endif

   " remove menu entries
   if has("gui_running") && has("menu") && &go =~ 'm'
"   	call Decho("remove menu entries")
    exe 'menu '.b:DrChipTopLvlMenu.'Load\ Spelling\ Checker<tab>'.s:mapleadstring.'ec	<Leader>ec'

    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Alternative\ spellings<tab>'.s:mapleadstring.'ea		'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Move\ to\ next\ spelling\ error<tab>'.s:mapleadstring.'en	'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Move\ to\ previous\ spelling\ error<tab>'.s:mapleadstring.'ep	'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Save\ word\ to\ user\ dictionary\ (temporarily)<tab>'.s:mapleadstring.'et	'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Save\ word\ to\ user\ dictionary\ (permanently)<tab>'.s:mapleadstring.'es	'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Save\ word\ to\ proj\ dictionary\ (permanently)<tab>'.s:mapleadstring.'ej	'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Remove\ word\ from\ user\ dictionary\ (temporarily)<tab>'.s:mapleadstring.'eT	'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Remove\ word\ from\ user\ dictionary\ (permanently)<tab>'.s:mapleadstring.'eS	'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Remove\ word\ from\ proj\ dictionary\ (permanently)<tab>'.s:mapleadstring.'eJ	'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Dialect:\ toggle\ Warning/Error\ highlighting<tab>'.s:mapleadstring.'ed	'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.RareWord:\ toggle\ Warning/Error\ highlighting<tab>'.s:mapleadstring.'er	'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Load\ '.b:Spchklang.'spchk<tab>'.s:mapleadstring.'ec		'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.End\ '.b:Spchklang.'spchk<tab>'.s:mapleadstring.'ee		'
    exe 'silent! unmenu '.b:DrChipTopLvlMenu.b:Spchklang.'spchk.Help<tab>\ '
	call s:AltLangMenus(1)
"    call Decho("uninstalled menu items: spchk.Save, Remove, Dialect, Load, End, etc")
"    call Decho("installed menu item: Load Spelling Checker")
   endif

   " enable subsequent re-loading of engspchk
   let s:loaded_{b:spchkfile}spchk= 1
  endif

  " restore syntax (as suggested by Bram Moolenaar)
  let &syntax= &syntax

"  call Dret("SpchkEnd")
endfun

" ---------------------------------------------------------------------

if !exists("s:spchklangfunc")
 " SpchkLang: implements changing language {{{2
 fun! <SID>SpchkLang(newlang)
   call s:SpchkEnd()
   let b:spchklang= substitute(a:newlang,"spchk.*$","","e")
   call s:LoadSpchk()
 endfun
 let s:spchklangfunc= 1
 com! -nargs=1 SpchkLang call <SID>SpchkLang(<f-args>)
endif

" ---------------------------------------------------------------------

" Modeline highlighting - ignore spchk: lines {{{1
syn match spchkModeline 	"^.*\<spchk:.*$"	contains=spchkML,spchkMLsetting
syn match spchkML			"\<spchk\>"			contained
syn match spchkMLsetting	"\<\k\+=\k\+"		contains=spchkMLop,spchkMLoption
syn match spchkVimModeline	"^\s*%\s*vim:.*$"
syn keyword spchkMLoption contained cvimsyn               acronym       autonext      mouse      punc        lang
syn keyword spchkMLoption contained DrChipTopLvlMenu      altright      dialect       nonhl      silent      proj
syn keyword spchkMLoption contained spchkcvimsyn          spchkacronym  spchkautonext spchkmouse spchkpunc   spchklang
syn keyword spchkMLoption contained spchkDrChipTopLvlMenu spchkaltright spchkdialect  spchknonhl spchksilent spchkproj
hi link spchkML				PreProc
hi link spchkMLop			Operator
hi link spchkMLoption		Identifier
hi link spchkModeline		Comment
hi link spchkVimModeline	Comment

"  Done Loading Message:   {{{1
if !b:spchksilent
" call Decho("Done Loading <".b:spchklang."spchk.vim>")
 echo "Done Loading <".b:spchklang."spchk.vim>"
endif

let &cpo= s:keepcpo
unlet s:keepcpo
" ---------------------------------------------------------------------
" vim: ts=4 fdm=marker
