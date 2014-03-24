(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lib")

(add-to-list 'load-path "~/.emacs.d/site-lib/python-mode.el-6.0.12") 

(and (= emacs-major-version 23) (defun server-ensure-safe-dir (dir) "Noop" t))
(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;;; Requires
(require 'emacs-type)
(require 'cl)

(load "~/.emacs.d/site-lib/color-theme-molokai.el")
(color-theme-molokai)

;;;; External librarys
(load-library "dochoncho")

;;;; Emacs window customization
(modify-frame-parameters nil '((wait-for-wm . nil)))

(setq function-map 
	  '(("LADYADAS" home-setup) 
		("work" work-setup)))

(process-location 
 (getenv "COMPUTERNAME") 
 function-map
 'default-setup)
 
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;;; Keymappings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
;(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-a" `kill-ring-save-buffer)

(global-set-key "\M-\C-m" 'switch-to-previous-buffer)
; (global-set-key "\C-z" 'undo)

(global-set-key "\C-c\C-g" 'goto-line)
(global-set-key "\C-c\C-r" 'search-backward-regexp)
(global-set-key "\C-c\C-s" 'search-forward-regexp)

(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(ergoemacs-mode 1)

;;;; Extra mode setup
(add-to-list 'auto-mode-alist '("\\.yml$" .  yaml-mode))

;;;; Aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'perl-mode 'cperl-mode)

;;;; Settings
(defun defaults-fn ()
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq-default c-basic-offset 4)
  (setq tab-stop-list 
        '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
  (setq make-backup-files nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq inhibit-startup-message t)
  (iswitchb-mode t)
  (set-buffer-file-coding-system 'utf-8-unix))

(defun my-javascript-mode-fn ()
  (require 'fly-jshint-wsh)
  (flymake-mode 1)
  (setq tab-width 2)
  (setq c-basic-offset 4))

;;;; Hooks
(defaults-fn)
(add-hook 'sql-mode-hook 'my-sql-mode-hook)
(add-hook 'kill-emacs-query-functions 'kill-emacs-did-you-really-mean-that)
(add-hook 'javascript-mode-hook 'my-javascript-mode-fn)

;; Stuff emacs added
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((c-mode . "k&r") (java-mode . "java") (awk-mode . "awk") (other . "k&r"))))
 '(ergoemacs-mode-used "5.13.11"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
