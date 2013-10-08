c = get_config()

c.TerminalIPythonApp.display_banner = False
c.TerminalInteractiveShell.color_info = False
c.TerminalInteractiveShell.colors = 'LightBG'
c.TerminalInteractiveShell.confirm_exit = False
c.InteractiveShell.autoindent = True
c.PromptManager.in_template  = '>>> '
c.PromptManager.in2_template = '   .\D.: '
c.PromptManager.out_template = ''
c.PromptManager.justify = True
