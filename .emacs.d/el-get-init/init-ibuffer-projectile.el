(remove-hook 'ibuffer-hook (first ibuffer-hook))
(add-hook 'ibuffer-hook
          (lambda ()
            (setq ibuffer-filter-groups
                  (append
                   ibuffer-filter-groups
                   (ibuffer-projectile-generate-filter-groups)))
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
