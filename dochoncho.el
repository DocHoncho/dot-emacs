;;; dochoncho.el --- Personal functions

;; Author: Joel Madigan <joelm@annarbordist.com>
;; Created: 01 Oct 2010
;; Version: 1.1
;; 
;; Change History
;;
;; Version 1.0 - Initial version
;; Version 1.1 - Added setup for Wendy's laptop

(defun kill-ring-save-buffer ()
  "Mark entire buffer without moving point"
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Saved buffer to kill ring"))

(defun my-sql-mode-hook ()
  "Custom settings for sql-mode"
  (message "Smile, you're entering SQL mode")
  (setq tab-width 4)
  (define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop))

(defun kill-emacs-did-you-really-mean-that ()
  "Make sure you really want to quit."
  (yes-or-no-p "Really quit Emacs? "))

;; Thanks to Dolda2000 @ Slashdot
(defun get-previous-buffer (numbufs blist)
  (if (not blist) (signal 'no-such-buffer ()))
  (if (not (buffer-file-name (car blist)))
      (get-previous-buffer numbufs (cdr blist))
    (if (> numbufs 0)
    (get-previous-buffer (1- numbufs) (cdr blist))
      (car blist))))

(defun switch-to-previous-buffer (numbufs)
  "Switches to the previous file-associated buffer in the buffer
list."
  (interactive "p")
  (switch-to-buffer (get-previous-buffer numbufs (buffer-list))))

(defun default-setup ()
  (message "Default hit"))

(defun work-setup ()	  
  (setq default-frame-alist
		'((top . 0) (left . 0)
		  (width . 80) (height . 51)))
  (setq initial-frame-alist '((top . 0) (left . 0)))
  (set-default-font 
   "-outline-Consolas-normal-r-normal-normal-19-142-96-96-c-*-iso8859-1"))

(defun my-csharp-mode-fn ()
  "function that runs when csharp-mode is initialized for a buffer."
  (local-set-key "\M-\\"   'cscomp-complete-at-point)
  (local-set-key "\M-\."   'cscomp-complete-at-point-menu)
  )

(defun home-setup ()
  (setq default-frame-alist
        '((top . 0) (left . 0)
          (width . 80) (height . 42))
  (setq initial-frame-alist '((top . 0) (left . 0)))
  (set-default-font
   "-outline-Consolas-normal-r-normal-normal-19-142-96-96-c-*-iso8859-1")
  (setq load-path (cons 
				   "C:/Program Files (x86)/erl5.8.1.1/lib/tools-2.6.6.1/emacs"
				   load-path))
  (setq erlang-root-dir "C:/Program Files (x86)/erl5.8.1.1")
  (setq exec-path (cons "C:/Program Files (x86)/erl5.8.1.1/bin" exec-path))
  (require 'erlang-start)
  (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
  (setq auto-mode-alist
		(append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
  (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)
  ))
(defun wendy-setup ()
  (setq default-frame-alist
        '((top . 0) (left . 0)
          (width . 80) (height . 42)))
  (setq initial-frame-alist '((top . 0) (left . 0)))
  (set-default-font
   "-outline-Consolas-normal-r-normal-normal-16-142-96-96-c-*-iso8859-1"))

(defun process-location (location function-map default-function)
  (let ((hit nil))
    (dolist (item function-map)
      (let ((name (car item))
	    (func (cdr item)))
	(if (string= location name)
	    (progn
	      (eval func)
	      (setq hit t)))))))

(setq function-map 
	  '(("LADYADA" home-setup)
	    ("WENDY17-PC" wendy-setup)
	    ("work" work-setup)))

(process-location (getenv "COMPUTERNAME") function-map 'default-setup)
