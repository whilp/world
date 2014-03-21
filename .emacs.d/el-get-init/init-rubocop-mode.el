(require 'flycheck)

(add-hook 'ruby-mode-hook 'rubocop-mode)
(flycheck-add-next-checker 'chef-foodcritic 'ruby-rubocop)
