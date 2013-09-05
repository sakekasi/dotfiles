(require 'cl)

(global-linum-mode)

(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "C-x C-u") 'undo)

;allow system copy/paste to interface with emacs copy/paste
(setq x-select-enable-clipboard t)

(setq inhibit-startup-screen t)
(setq visible-bell t)

;(require 'color-theme)
;(load "~/.emacs.d/themes/color-theme-desert/color-theme-desert.el")
;(color-theme-desert)

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

(defun concat-with-space (a b)
  (concat a " " b))

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


(defun fullscreen (&optional f)
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;;; haskell mode
