;;; Misc
(set-default-font "PragmataPro-14")

;; Make sure that all the packages are installed
;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-js
                      starter-kit-ruby
                      yasnippet)
  "A list of packages to ensure are installed at launch.")

;;This not work with osx emacs because the version of the packages
;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))

;; Deft
(add-to-list 'load-path "~/.emacs.d/vendors/deft")
(require 'deft)
(setq dropbox-everything-dir "~/Dropbox/Everything")
(setq deft-extension "org")
(setq deft-directory dropbox-everything-dir)
(setq deft-text-mode 'org-mode)
(global-set-key [f8] 'deft)

;; Scala 
(add-to-list 'load-path "~/.emacs.d/vendors/scala-mode")
(add-to-list 'load-path "~/.emacs.d/vendors/ensime/elisp")
(require 'scala-mode-auto)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas-minor-mode)))

;; Yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"            ;; personal snippets
        "~/.emacs.d/elpa/yasnippet-0.8.0/snippets"    ;; the default collection
        ))

;; Copy the path from the shell
;; https://github.com/purcell/exec-path-from-shell/blob/master/exec-path-from-shell.el
(defun exec-path-from-shell-getenv (name)
  (with-temp-buffer
    (call-process (getenv "SHELL") nil (current-buffer) nil
                  "--login" "-i" "-c" (concat "echo __RESULT=$" name))
    (when (re-search-backward "__RESULT=\\(.*\\)" nil t)
      (match-string 1))))

(defun exec-path-from-shell-copy-env (name)
  "Set the environment variable with `NAME' to match the value seen in the user's shell."
  (interactive "sCopy value of which environment variable from shell? ")
  (setenv name (exec-path-from-shell-getenv name)))

(defun exec-path-from-shell-initialize ()
  "Set the PATH environment variable and `exec-path' to match that seen in the user's shell."
  (interactive)
  (let ((path-from-shell (exec-path-from-shell-getenv "PATH")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
