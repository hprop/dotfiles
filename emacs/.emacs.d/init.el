;; MELPA y Marmalade repos
(require 'package)

;; https://github.com/jorgenschaefer/elpy
;; Para elpy hace falta instalar con pip:
;; # Either of these
;; pip install rope
;; pip install jedi
;; # flake8 for code checks
;; pip install flake8
;; # importmagic for automatic imports
;; pip install importmagic
;; # and autopep8 for automatic PEP8 formatting
;; pip install autopep8
;; # and yapf for code formatting
;; pip install yapf
(setq package-list '(recentf expand-region iy-go-to-char key-chord yasnippet projectile
			     nose elpy haskell-mode yaml-mode))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives 
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; no startup msg  
(setq inhibit-startup-message t)

;; Básicos
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-,") 'repeat)

;; Abrir ficheros recientes
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 200)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)


;; Navegar entre buffers
(global-set-key (kbd "C-s-j") 'other-window)
(defun other-window-back ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-s-k") 'other-window-back)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "<C-return>") 'er/expand-region)

;; Ace-jump mode
(define-key global-map (kbd "C-ñ") 'ace-jump-mode)

;; iy-go-to-char
(require 'iy-go-to-char)
(setq iy-go-to-char-key-forward ?\,)
(setq iy-go-to-char-key-backward ?\;)
(global-set-key (kbd "C-c f") 'iy-go-up-to-char)
(global-set-key (kbd "C-c F") 'iy-go-up-to-char-backward)

;; Key chords
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "DF" 'iy-go-up-to-char)
(key-chord-define-global "SD" 'iy-go-up-to-char-backward)


;; IDO mode
(require 'ido)
(ido-mode t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Python mode
(setq python-shell-interpreter "python2")

(elpy-enable)
(setq elpy-rpc-backend "jedi")
(setq elpy-modules '(elpy-module-sane-defaults
		     elpy-module-company
		     elpy-module-eldoc
		     elpy-module-highlight-indentation
		     elpy-module-pyvenv
		     elpy-module-yasnippet))
(setq elpy-test-runner 'elpy-test-nose-runner)
(setq elpy-test-nose-runner-command '("nosetests2" "-s"))

(require 'nose)
(add-hook 'python-mode-hook (lambda () (nose-mode t)))
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key nose-mode-map "\C-ca" 'nosetests-all)
	    (define-key nose-mode-map "\C-cm" 'nosetests-module)
	    (define-key nose-mode-map "\C-c." 'nosetests-one)
	    (define-key nose-mode-map "\C-cc" 'nosetests-again)
	    (define-key nose-mode-map "\C-cpa" 'nosetests-pdb-all)
	    (define-key nose-mode-map "\C-cpm" 'nosetests-pdb-module)
	    (define-key nose-mode-map "\C-cp." 'nosetests-pdb-one)))
(setq nose-global-name "nosetests2 -s")

(require 'gud)
(define-key gud-minor-mode-map (kbd "<f5>") 'gud-break)
(define-key gud-minor-mode-map (kbd "<f7>") 'gud-step)
(define-key gud-minor-mode-map (kbd "<C-f7>") 'gud-up)
(define-key gud-minor-mode-map (kbd "<f8>") 'gud-next) 
(define-key gud-minor-mode-map (kbd "<f9>") 'gud-cont)
(define-key gud-minor-mode-map (kbd "<f2>") 'gud-print)


;; pdb setup
(setq pdb-path '/usr/lib/python2.7/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
	 		    (file-name-nondirectory buffer-file-name)))))

;; OCTAVE mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; Apariencia
(load-theme 'misterioso t)
(set-frame-font "Droid Sans Mono-11" nil t)
(tool-bar-mode -1)
(setq column-number-mode t) ; posición row/col del cursor
(set-cursor-color "#f58ce0") 


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
