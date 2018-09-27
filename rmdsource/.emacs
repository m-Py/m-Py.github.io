;; This file is in the public domain

;; add markdown mode

(add-to-list 'load-path "~/git/markdown-mode") 

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; add polymode

(setq load-path
      (append '("/home/martin/git/polymode/"
                "/home/martin/git/poly-markdown/"
                "/home/martin/git/poly-R/"
                "/home/martin/git/poly-noweb/")
              load-path))
              
(require 'poly-R)
(require 'poly-noweb)
(require 'poly-markdown)

;; deactivate fill paragraph (M-q) for inline R (`r ... `). Adapted from
;; https://stackoverflow.com/questions/23755506/emacs-fill-mode-for-python-that-doesnt-break-quoted-strings-causing-errors

(defun odd-number-of-graves-this-paragraph-so-far ()
  (oddp (how-many "`" (save-excursion (backward-paragraph) (point)) (point))))
(add-to-list 'fill-nobreak-predicate 'odd-number-of-graves-this-paragraph-so-far)

;; customizations for markdown mode:

;; activate syntax highlighting for LaTex equations:
(setq markdown-enable-math t)

;; activate spell checker

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; default dictionary: American English
(setq ispell-dictionary "american") 
