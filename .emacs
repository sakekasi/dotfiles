(require 'cl)

(setq default-tab-width 2)

;;;CUSTOM STUFF IN EMACS ITSELF
(if (string-match "XEmacs\\|Lucid" emacs-version)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; XEmacs
  ;;; ------
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (progn
     (if (file-readable-p "~/.xemacs/init.el")
	 (load "~/.xemacs/init.el" nil t))
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; GNU-Emacs
  ;;; ---------
  ;;; load ~/.gnu-emacs or, if not exists /etc/skel/.gnu-emacs
  ;;; For a description and the settings see /etc/skel/.gnu-emacs
  ;;;   ... for your private ~/.gnu-emacs your are on your one.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if (file-readable-p "~/.gnu-emacs")
      (load "~/.gnu-emacs" nil t)
    (if (file-readable-p "/etc/skel/.gnu-emacs")
	(load "/etc/skel/.gnu-emacs" nil t)))

  ;; Custom Settings
  ;; ===============
  ;; To avoid any trouble with the customization system of GNU emacs
  ;; we set the default file ~/.gnu-emacs-custom
  (setq custom-file "~/.gnu-emacs-custom")
  (load "~/.gnu-emacs-custom" t t)
;;;
)
;;;



;;; GLOBAL STUFF
;;; ============

;;UTILITY FUNCTIONS
(defun concat-with-space (a b)
  (concat a " " b))


;;APPEARANCE

(defun fullscreen (&optional f)
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(global-linum-mode)

(add-to-list 'exec-path "~/.cabal/bin")


(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
  '(lambda() (set-fill-column 80)))

(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "C-x C-u") 'undo)
 
;(set-face-attribute 'default nil :font "Inconsolata Medium 12")
(set-face-attribute 'default nil :font "Luculent 10")

;allow system copy/paste to interface with emacs copy/paste
(setq x-select-enable-clipboard t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)

;; PACKAGES
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(setq packages '(auto-complete magit autopair markdown-mode tuareg ghc haskell-mode clojure-mode))

(defun check-and-install (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(mapc 'check-and-install packages)

(setq inhibit-startup-screen t)
(setq visible-bell t)

(setq auto-mode-alist
  (cons (cons "\\.pl" 'prolog-mode)
     auto-mode-alist))

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m" . octave-mode) auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.hs" . haskell-mode) auto-mode-alist))
(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))

;;autocomplete
;(add-to-list 'load-path "~/.emacs.d")    ; This may not be appeared if you have already added.
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")


;;; HASKELL
;;; =======

(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode")

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(eval-after-load "haskell-mode"
  '(progn
     (setq haskell-stylish-on-save t)
     (setq haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans"
               "--with-ghc=ghci-ng"))

     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
     (define-key haskell-mode-map (kbd "C-c v c") 'haskell-cabal-visit-file)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "s-s") 'flymake-start-syntax-check)

;     (setq haskell-font-lock-symbols t)

     ;; Do this to get a variable in scope 
     (auto-complete-mode)

     ;; from http://pastebin.com/tJyyEBAS
     (ac-define-source ghc-mod
       '((depends ghc)
         (candidates . (ghc-select-completion-symbol))
         (symbol . "s")
         (cache)))

     (defun my-ac-haskell-mode ()
       (setq ac-sources '(ac-source-words-in-same-mode-buffers
                          ac-source-dictionary
                          ac-source-ghc-mod)))
     (add-hook 'haskell-mode-hook 'my-ac-haskell-mode)


     (defun my-haskell-ac-init ()
       (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
         (auto-complete-mode t)
         (setq ac-sources '(ac-source-words-in-same-mode-buffers
                            ac-source-dictionary
                            ac-source-ghc-mod))))
     (add-hook 'find-file-hook 'my-haskell-ac-init)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'haskell-mode))

(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(add-to-list 'load-path "~/.emacs.d/structured-haskell-mode/elisp")
(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)


;;; C/C++
;;; =====

;; Make a non-standard key binding.  We can put this in
;; c-mode-base-map because c-mode-map, c++-mode-map, and so on,
;; inherit from it.
(defun my-c-initialization-hook ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

(defun my-c-hook ()
  (c-toggle-auto-newline 1)
  (c-set-style "linux")
  (setq tab-width 2
	indent-tabs-mode nil)
  (local-set-key (kbd "C-c C-u") 'uncomment-region)
  (local-set-key (kbd "C-c C-k") 'compile)
  (hs-minor-mode))
(add-hook 'c-mode-common-hook 'my-c-hook)

;;;CODE TO REGENERATE TAGS FILE UPON SAVE
(setq extension-regen-tags
      (list ".cc"
	    ".c"
	    ".cpp"
	    ".hh"
	    ".h"))

(add-hook 
 'after-save-hook 
 (lambda ()
   (if (member (file-name-extension buffer-file-name) extension-regen-tags)
       (progn
	 (shell-command 
	  (concat "ctags -e R " 
		  (file-name-directory buffer-file-name)
		  " "
		  (reduce 'concat-with-space extension-regen-tags)))))))

(defun my-c++-hook ()
  (c-toggle-auto-newline 1)
  (c-set-style "stroustrup")
  (setq tab-width 2
	indent-tabs-mode nil)
  (local-set-key (kbd "C-c C-u") 'uncomment-region)
  (local-set-key (kbd "C-c C-k") 'compile)
  (hs-minor-mode))
(add-hook 'c++-mode-hook 'my-c++-hook)


(add-hook 'makefile-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-s") 'save-buffer)
	    (local-set-key (kbd "C-c C-k") 'compile)))

(add-hook 'compilation-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-s") 'save-buffer)
	    (local-set-key (kbd "C-c C-k") 'compile)
	    (when (not (get-buffer-window "*compilation*"))
	      (save-selected-window
		(save-excursion
		  (let* ((w (split-window-vertically))
			 (h (window-height w)))
		    (select-window w)
		    (switch-to-buffer "*compilation*")
		    (shrink-window (- h 10))))))))

;;; PYTHON
;;; ======

(require 'autopair)
(setq autopair-autowrap t) ;; attempt to wrap selection

;; this mode-hook is taken straight from the comments in autopair.el
(add-hook 'python-mode-hook
	  #'(lambda ()
	      (setq autopair-handle-action-fns
		    (list #'autopair-default-handle-action
			  #'autopair-python-triple-quote-action))))

(add-hook 'python-mode-hook
	  #'(lambda ()
	      (hs-minor-mode)))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; JAVA
;;; ====

(add-hook 'java-mode-hook
	  (lambda ()
	    "Treat Java 1.5 @-style annotations as comments."
	    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
	    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 4
                                      tab-width 4
                                      indent-tabs-mode nil)))

(add-hook 'javascript-mode-hook (lambda ()
				  (setq indent-tabs-mode nil
					tab-width 2
					js-indent-level 2)))
				  

