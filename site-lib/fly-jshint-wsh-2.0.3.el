;;; fly-jshint-wsh.el --- use flymake with jshint on js code, on Windows
;;
;; Author     : Dino Chiesa
;; Version    : 2.0.3
;; Keywords   : javascript js jslint jshint flymake languages jscript
;; URL        : http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=fly-jshint-wsh.el
;; License    : New BSD
;; Last-saved : <2012-March-29 16:59:19>
;; Package-Requires: ((flymake "0.3"))
;;

;;; Commentary
;;
;; This module performs flymake on-the-fly syntax checking of Javascript
;; source files, on Windows, using JSLINT or JSHINT, via the builtin
;; Cscript.exe.
;;
;; This module started from
;; http://www.emacswiki.org/emacs-en/FlymakeJavaScript
;; and got modifications for Windows.
;;
;;
;; Usage: Add something like this in your .emacs file:
;;
;;   (defun my-javascript-mode-fn ()
;;     (require 'fly-jshint-wsh)
;;     (flymake-mode 1)
;;      ...
;;      )
;;
;; By default, this module uses JSHINT. If you prefer jslint, then
;;
;;   (defun my-javascript-mode-fn ()
;;     (require 'fly-jshint-wsh)
;;     (setq flyjs-checker 'jslint) ;; jshint is the default
;;     (flymake-mode 1)
;;      ...
;;      )
;;
;; You then need to put your custom hook onto the mode hook list.
;; You need only one of the following, whichever is right for you.
;;
;;   (add-hook 'javascript-mode-hook 'my-javascript-mode-fn)
;;   (add-hook 'espresso-mode-hook   'my-javascript-mode-fn)
;;   (add-hook 'js2-mode-hook        'my-javascript-mode-fn)
;;   (add-hook 'js-mode-hook         'my-javascript-mode-fn)
;;
;;
;; There are multiple versions of the flymake for JSLINT/JSHINT
;; capability for windows. This is the latest, as of Thu, 29 Mar 2012.
;; They all are based on the idea of running JSHINT or JSLINT from a
;; command line, via CScript.exe.  They all can use either JSHINT or
;; JSLINT, but in either case need some boilerplate logic appended to
;; the standard JSLINT or JSHINT distribution, in order to function
;; properly.
;;
;; In prior versions, there was a requirement for the user to manually
;; download JSLINT or JSHINT, then perform the modifications and set the
;; location of the modified verion of the script.  This version of the
;; module has been updated to do that for you. It uses Powershell - a
;; feature of Windows since Windows XPSP2 - to download the bare JSLINT
;; or JSHINT script, then it appends the necessary boilerplate, and
;; saves the resulting script in the user's temporary directory. It then
;; runs the script from there to do syntax checks. This simplifies
;; installation, but it does require an internet connection, the first
;; time through.
;;
;; Pre-requisites to use this module include: Windows and CScript.exe;
;; the latter is built-in to Windows since Windows 95.  Also, on the
;; first run only, Powershell 1.0 and an internet connection are
;; required, in order to download the JSLINT or JSHINT script.
;;
;; With this feature, the first time you open a .js file, emacs will
;; download the necessary script. This can take several seconds, so you
;; will see a delay in opening the buffer. This is a one-time delay.
;;

;;; Copyright
;;
;; Copyright (c) 2010-2012, Dino Chiesa
;; All rights reserved.
;;

;;; License
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; Neither the name of the author or any contributors, nor the names of
;; any organizations they belong to, may be used to endorse or promote
;; products derived from this software without specific prior written
;; permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;


;;; Bugs:
;;
;;  - Not integrated with the flymake-for-jslint.el which is used on
;;    Linux. Really there shouldn't be separate .el files for Windows
;;    and Linux.  This should be combined with the Linux-oriented
;;    flymake-for-jslint.js ; it will then be one module generalized
;;    to support both Linux and windows.
;;
;;  - Does not check for updates of JSLINT or JSHINT. It should, and then
;;    should download only as necessary.
;;


(require 'flymake)

(defgroup flyjs nil
  "Run flymake on Javascript code, with JSLINT or JSHINT, via Windows CScript.exe")

(defcustom flyjs-checker 'jshint
  "The syntax checker to use for Javascript. Legal values are
'jslint and 'jshint.  Use this in your emacs:

   (require 'fly-jshint-wsh)
   (setq flyjs-checker 'jslint)

Or you can customize this variable.
"
  :group 'flyjs)

(defvar flyjs-jshint-src "https://raw.github.com/jshint/jshint/master/jshint.js"
  "Source URL for JSHint")

(defvar flyjs-jslint-src "https://raw.github.com/douglascrockford/JSLint/master/jslint.js"
  "Source URL for JSLint")

(defcustom flyjs-try-use-chakra t
  "Whether to try to use the Chakra engine on Windows.

It is possible to use the \"Chakra\" engine from IE9, from a
cscript.exe command. The benefit is, Javascript programs run
faster.

When this variable is non-nil, `fly-jshint-wsh.el' will
look for the Chakra Javascript engine, and will use it if it is
available. If this variable is nil, or if the Chakra engine is
not available, then fly-jshint-wsh will use the
default JScript engine (not at version 5.8) when running the
jslint program.

In most cases you should just leave this variable as t.

To make it possible to use Chakra, you will need at least IE9
installed on your computer, and you will need to modify your
registry to expose the Chakra engine to cscript.exe.

To do the registry mod, save the following into a .reg file, and
run it:

    Windows Registry Editor Version 5.00

    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\CLSID\\{16d51579-a30b-4c8b-a276-0ff4dc41e755}\\ProgID]
    @=\"Chakra\"

    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Wow6432Node\\CLSID\\{16d51579-a30b-4c8b-a276-0ff4dc41e755}\\ProgID]
    @=\"Chakra\"

    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Chakra]
    @=\"JScript Language\"

    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Chakra\\CLSID]
    @=\"{16d51579-a30b-4c8b-a276-0ff4dc41e755}\"

    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Chakra\\OLEScript]
")


(defvar flyjs--language-name nil
  "This is the name of the language that will be requested on the
cscript.exe command line when running the jslint program.

`fly-jshint-wsh.el' tries to infer the name by examining the
registry, the first time you use the module.  It selects Chakra
if it is available and selects Jscript otherwise.

You can force this module to always use JScript by setting
`flyjs-try-use-chakra' to nil, before calling require on this
module.  You can also override what the module selects, by
setting this variable manually in your .emacs.  Take care to set
it to only JScript, Javascript, or a legal Javascript-compatible
language string that can be understood by cscript.exe's //E option.")


(defvar flyjs-reg-exe
  (concat (getenv "windir") "\\system32\\reg.exe"))

(defvar flyjs-cscript-exe
  (concat (getenv "windir") "\\system32\\cscript.exe"))

(defvar flyjs-powershell-exe
  (concat (getenv "windir")
   "\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"))

(defvar flyjs-script-location nil
  "Filename for the jslint.js or jshint.js script. This gets
downloaded as necessary the first time `fly-jshint-wsh.el' runs.")

;; consider several options for the .js extension  on the flymake list
(defvar flyjs--js-keys '("\\.js\\'"  "\\.js\\$" ".+\\.js\\'" ".+\\.js\\$"))


(defvar flyjs-wsh-boilerplate "(function () {'use strict';var filename='stdin', content='', fso, fs, i, e, line, linter, label,\noptions={curly:false,\nwsh:true,\nwhite:false,\nplusplus:false,\npassfail:false};if (WScript.Arguments.length > 0) {filename=WScript.Arguments(0);fso=new ActiveXObject('Scripting.FileSystemObject');fs=fso.OpenTextFile(filename, 1);content=fs.ReadAll();fs.Close();fso=null;fs=null;} else {content=WScript.StdIn.ReadAll();}if (typeof JSHINT === 'function') {linter=JSHINT;label='JSHINT';}else if (typeof JSLINT === 'function') {linter=JSLINT;label='JSLINT';}else {throw 'no lint tool found.';}if (!linter(content, options)) {WScript.StdErr.WriteLine(label);for (i=0; i < linter.errors.length; i++) {e=linter.errors[i];if (e !== null) {line=(typeof e.line === 'undefined') ? '0':e.line;WScript.StdErr.WriteLine(filename + '(' + line + ',' + e.character +\n') ' + label + ': ' + e.reason);WScript.StdErr.WriteLine('    ' + (e.evidence || '').replace(/^\\s*(\\S*(\\s+\\S+)*)\\s*$/, '$1'));}}}}());")


(defun flyjs-script-location ()
  "gets the location of the checker script."
  (if (and (not (eq flyjs-checker 'jslint))
           (not (eq flyjs-checker 'jshint)))
      (setq flyjs-checker 'jshint))
  (setq flyjs-script-location
        (concat
         (file-name-as-directory temporary-file-directory)
         "emacs.flyjs."
         (symbol-name flyjs-checker)
         ".js")))


(defun flyjs--reg-read (regpath)
  "read a path in the Windows registry"
  (let (tokens last-token)
    (setq reg-value (shell-command-to-string
                     (concat flyjs-reg-exe " query " regpath))
          tokens (split-string reg-value nil t)
          last-token (nth (1- (length tokens)) tokens))
    (and (not (string= last-token "value.")) last-token)))


(defun flyjs-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

This won't always work; it will fail if the source module
refers to relative paths.
"
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                prefix "-"
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "-"))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))


(defun flyjs-choose-script-url ()
  "Choose which URL to download from, to get the script.
User may set the preference in `flyjs-checker'. Legal values
are 'jshint and 'jslint .
"
  (cond
   ((eq flyjs-checker 'jslint)
    flyjs-jslint-src)
   ((eq flyjs-checker 'jshint)
    flyjs-jshint-src)
   (t
    (setq flyjs-checker 'jshint)
    flyjs-jshint-src)))


(defun flyjs-download-script ()
  "Download the jslint or jshint script from the intertubes."
    (flyjs-wget-script-via-powershell (flyjs-choose-script-url)
                                      (flyjs-script-location))
    (with-temp-buffer
      (insert-file-contents flyjs-script-location)
      (goto-char (point-max))
      (insert flyjs-wsh-boilerplate)
      (insert "\n")
      (write-region (point-min) (point-max) flyjs-script-location)))


(defun flyjshint-init ()
  "Called each time a flymake check occurs."
  (let ((work-file (flymake-init-create-temp-buffer-copy
                    'flyjs-create-temp-intemp)))
    ;; download the script file as necessary
    (if (or (not (file-exists-p (flyjs-script-location)))
            (not (file-readable-p flyjs-script-location)))
        (flyjs-download-script))

    (if (or (not (file-exists-p flyjs-script-location))
            (not (file-readable-p flyjs-script-location)))
        (error "Please set flyjs-script-location to an actual location.")
      (list flyjs-cscript-exe (list
                               flyjs-script-location
                               work-file
                               (concat "//E:" flyjs--language-name))))))


(defun flyjs-wget-script-via-powershell (url target-f)
  "get the contents of a URL into a file named TARGET-F
via Windows Powershell.

"
  (let* ((ps-cmd (concat
                  "(new-object System.Net.WebClient).DownloadFile("
                  (replace-regexp-in-string (char-to-string 34)
                                            (char-to-string 39)
                                            (pp-to-string url))
                  ","
                  (replace-regexp-in-string
                   "/"
                   "\\\\"
                   (replace-regexp-in-string (char-to-string 34)
                                             (char-to-string 39)
                                             (pp-to-string target-f)))
                  ")"))
         (shell-command
          (format "%s -Command %s"
                  flyjs-powershell-exe
                  (concat "\"& {" ps-cmd "}\""))))

    (shell-command-on-region (point) (point)
                             shell-command
                             nil nil nil)))



(defun flyjs-install (&optional force-download)
  "installs fly-jshint-wsh logic into flymake. This needs to be done
just once, per instance of emacs. It is done automatically when
your .emacs file includes this statement:

    (require 'fly-jshint-wsh)

This fn does several things:

  - If FORCE-DOWNLOAD is non-nil, download jshint.js or
    jslint.js, and combine it with some WSH boilerplate (cmd line
    handling) and save it into script cache location.

  - select the Javascript engine to use with cscript.exe. (See
    `flyjs-try-use-chakra')

  - modify `flymake-err-line-patterns' to include a pattern for
    JSLINT or JSHINT

  - modify `flymake-allowed-file-name-masks' to make sure the
    jslint-for-wsh command is invoked by flymake.


If the script does not exist (either jshint or jslint) in the
well-known place this module expects it, and FORCE-DOWNLOAD is
nil, then this module will download the scriptat the time flymake
first runs. There may be a delay.

If you want to avoid that delay, you can invoke this method
explicitly, at some point after the require statement, like so:

    (require 'fly-jshint-wsh)
    (flyjs-install t)

You can precede it with a setq to explicitly specify which
checker script to use, like so:

    (require 'fly-jshint-wsh)
       ....
    (setq flyjs-checker 'jslint) ;; jshint is the default
    (flyjs-install t) ;; force download of jslint
    (flymake-mode 1)

Consider carefully whether to download the jslint or jshint
script every time you start emacs. This module will re-use a
previous download of the script, if it is available. This can be
good, because it saves time. It can also be bad, because you
won't get the latest checker script when updates are made
available on github. This could still be good, though, because
some updates are destabliziing.

This is the typical way people will use `fly-jshint-wsh.el'.

    (require 'fly-jshint-wsh)
    (flymake-mode 1)


"

  (if force-download
      (flyjs-download-script))

  (setq flyjs--language-name
        (if (and flyjs-try-use-chakra
            (string=
             (upcase
              (flyjs--reg-read "HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Chakra\\CLSID\\"))
             "{16D51579-A30B-4C8B-A276-0FF4DC41E755}"))
            "Chakra" "JScript")

        flymake-err-line-patterns
        (cons '("^[ \t]*\\([-A-Za-z.0-9_:/ ]+\\)(\\([0-9]+\\)[,]\\( *[0-9]+\\))\\( Microsoft JScript runtime error\\| JSLINT\\| JSHINT\\): \\(.+\\)$"
                1 2 3 5)
              flymake-err-line-patterns))

  ;; fixup the various keys for javascript files in the flymake alists
  (let ((keys flyjs--js-keys)
        key
        found
        jsentry)
    (while (and keys (not found))
      (setq key (car keys)
            jsentry (assoc key flymake-allowed-file-name-masks))
      (if jsentry
          (progn
            (setcdr jsentry '(flyjshint-init
                              flymake-simple-cleanup
                              flymake-get-real-file-name))
            (setq found t)))
      (setq keys (cdr keys)))
    (if (not found)
        (add-to-list
         'flymake-allowed-file-name-masks
         (list (car flyjs--js-keys)
               'flyjshint-init 'flymake-simple-cleanup ;;'flymake-get-real-file-name
               )))))

(flyjs-install)

(provide 'fly-jshint-wsh)

;;; fly-jshint-wsh.el ends here
