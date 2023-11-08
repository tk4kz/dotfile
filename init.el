;;;;; init.el

;;; debug
;;(setq debug-on-error t)

;;; exec path
(add-to-list 'exec-path "/usr/local/bin")

;;; my lisp load path
(add-to-list 'load-path "~/.emacs.d/mylisp")

;;; package
(when (require 'package nil t)
  (add-to-list 'package-archives
    '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (package-initialize))

;;; GUI config
(when window-system
  ;; mozc
  (require 'mozc)
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc")
  (prefer-coding-system 'utf-8)
  (global-set-key [henkan]
		  (lambda () (interactive)
		    (when (null current-input-method) (toggle-input-method))))
  (global-set-key [muhenkan]
		  (lambda () (interactive)
		    (inactivate-input-method)))
  ;; 全角半角キーと無変換キーのキーイベントを横取りする
  (defadvice mozc-handle-event (around intercept-keys (event))
    (if (member event (list 'zenkaku-hankaku 'muhenkan))
	(progn
	  (mozc-clean-up-session)
	  (toggle-input-method))
      (progn
	ad-do-it)))
  (ad-activate 'mozc-handle-event)
  
  ;; font
  (add-to-list 'default-frame-alist '(font . "Cica-14"))

  ;; GUI themeはSolarized
  (load-theme 'solarized-dark t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#002b36"))
)

;;; Terminal config
(unless (display-graphic-p)
  (corfu-terminal-mode +1)
  ;; Terminal themeはkaolin
  (load-theme 'kaolin-mono-dark t))

;;; GUI/emacsclient共通
;; 起動時の Welcome 画面無し
(setq inhibit-splash-screen t)
;; scratch buffer のメッセージ非表示
(setq initial-scratch-message "")
;; 一時マークモードの自動有効化
(setq-default transient-mark-mode t)
;; C-x C-u が何もしないように変更する (undo の typo 時誤動作防止)
(global-unset-key "\C-x\C-u")
;; 括弧の対応をハイライト.
(show-paren-mode 1) 
;; バッファ末尾に余計な改行コードを防ぐための設定
(setq next-line-add-newlines nil) 
;; メニューバーを消す
(menu-bar-mode -1)
;; ツールバー非表示
(tool-bar-mode 0)
;; scroll bar 非表示
(scroll-bar-mode -1)
;; C-h でカーソルの左にある文字を消す
;;(define-key global-map "\C-h" 'delete-backward-char)
;; C-h に割り当てられている関数 help-command を C-x C-h に割り当てる
;;(define-key global-map "\C-x\C-h" 'help-command)
;; The local variables list in .emacs と言われるのを抑止
(add-to-list 'ignored-local-variables 'syntax) 

;;;;; markdown
(use-package markdown-mode
  :bind
  (:map markdown-mode-map
	("\C-x m" . hydra-markdown/body))
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md\\'" . markdown-mode)
  :config
;;  (setq markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css") markdown-command "pandoc -t html5")

  ;; githib css
;;  (setq markdown-preview-stylesheets (list "https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"))

;;  (set-face-attribute 'markdown-code-face nil
;;                      :inherit 'outline-3)
;;  (set-face-attribute 'markdown-pre-face nil
;;                      :inherit 'outline-3)

;;; hydra-markdown
(defhydra hydra-markdown (:hint nil :exit t)
  "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code
Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4
Lists             C-c C-x    _m_: insert item   
Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down
Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference
Preview           C-c C-c p  _v_: Preview
"
  
  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("c" markdown-insert-code)

  ("h" markdown-insert-header-dwim) 
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("l" markdown-promote)
  ("r" markdown-demote)
  ("d" markdown-move-down)
  ("u" markdown-move-up)  

  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("R" markdown-insert-reference-link-dwim :color blue)

  ("v" markdown-preview :exit t)))

;;;;; markdown end ;;;;;

;;;;; mode line
(require 'moody)
(setq x-underline-at-descent-line t)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)
;; minions
(require 'minions)
(minions-mode)
(setq minions-mode-line-lighter "[+]")
(setq minions-prominent-modes '(overwrite-mode))
;; display column
(column-number-mode)

;;;;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'magit-ido-completing-read)

;;;;; recentf.el
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)

;;;;; X clipboard integration
(setq x-select-enable-clipboard t)
(defun xsel-cut-function (text &optional push)
  ;; Insert text to temp-buffer, and "send" content to xsel stdin
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max)
			 "xsel" nil 0 nil "--clipboard" "--input")))
(defun xsel-paste-function()
  (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
    (unless (string= (car kill-ring) xsel-output)
      xsel-output )))
(setq interprogram-cut-function 'xsel-cut-function)
(setq interprogram-paste-function 'xsel-paste-function)
;;;;; end of X clipboard integration 

;;;;; yasnippet
;;(use-package yasnippet
;;  :ensure t
;;  :diminish yas-minor-mode
;;  :bind (:map yas-minor-mode-map
;;              ("C-x i i" . yas-insert-snippet)
;;              ("C-x i n" . yas-new-snippet)
;;              ("C-x i v" . yas-visit-snippet-file)
;;              ("C-x i l" . yas-describe-tables)
;;              ("C-x i g" . yas-reload-all))              
;;  :config
;;  (yas-global-mode 1)
;;  (setq yas-prompt-functions '(yas-ido-prompt))
;;  )

;;;;; east-asian-ambiguous
;;(require 'eaw)
;;(eaw-fullwidth)

;;;;; keyboard macro
;;;; commented pry in Ruby script
;;(fset 'comment-pry
;;   "\C-[xreplace-regexp\C-mbinding.pry\C-m#binding.pry\C-m")

;;;; reload init.el
;;(fset 'loadinitel
;;   [?\M-x ?l ?o ?a ?d ?- ?f ?i ?l ?e return ?/ ?h ?o ?m ?e ?/ ?t ?a ?k ?e ?/ ?. ?e ?m ?a ?c ?s ?. ?d ?/ ?i ?n ?i ?t ?. ?e ?l return])

;;;;; espy
(require 'espy)
(setq espy-password-file "~/.etc/passwd/passwd.org.gpg")

;;;;; eglot
;;(require 'eglot)
;;;; ruby-mode
;;(add-hook 'ruby-mode-hook 'eglot-ensure)
;;;; python-mode
;;(add-hook 'python-mode-hook 'eglot-ensure)
;;(add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
;;(define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
;;(define-key eglot-mode-map (kbd "C-c e n") 'eglot-rename)

;;; flymake
;;(require 'flymake-diagnostic-at-point)
;;;;(require 'package-lint)
;;(with-eval-after-load 'flymake
;;  (add-hook 'flymake-mode-hook 'flymake-diagnostic-at-point-mode))

;;;;; js-mode
;;(add-hook 'js-mode-hook
;;          (lambda ()
;;            (make-local-variable 'js-indent-level)
;;            (setq js-indent-level 2)))

;;;;; amx
;;(use-package amx)

;;;;; which-key
(use-package which-key
    :diminish which-key-mode
    :hook (after-init . which-key-mode))

;;;;; Tree-sitter
;;(use-package treesit-auto
;;  :ensure t
;;  :config
;;  (setq treesit-auto-install t)
;;  (global-treesit-auto-mode))
;;
;;(use-package treesit
;;  :config
;;  (setq treesit-font-lock-level 4))

;;;;; backup file
(setq make-backup-files nil)

;;;;; default-text-scale : Easily adjust the font size
;;;;    default keymap
;;;;      (define-key map (kbd "C-M-=") 'default-text-scale-increase)
;;;;      (define-key map (kbd "C-M--") 'default-text-scale-decrease)
;;;;      (define-key map (kbd "C-M-0") 'default-text-scale-reset)
;;(default-text-scale-mode 1)

;;; treemcs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;;(use-package treemacs-evil
;;  :after (treemacs evil)
;;  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;;;;(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;;;  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;;;  :ensure t
;;;;  :config (treemacs-set-scope-type 'Perspectives))
;;
;;;;(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;;;  :after (treemacs)
;;;;  :ensure t
;;;;  :config (treemacs-set-scope-type 'Tabs))
;;
;;;;; treemacs end ;;;

;;;;; winum
(require 'winum)
(winum-mode)

;;; projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-mode-line-prefix " Prj")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;;; flycheck + textlint
;;(use-package flycheck
;;  :ensure t
;;  :init
;;  ;;(global-flycheck-mode t)
;;  :config
;;  (flycheck-define-checker textlint
;;			   "A linter for prose."
;;			   :command ("textlint" "--format" "unix"
;;				     source-inplace)
;;			   :error-patterns
;;			   ((warning line-start (file-name) ":" line ":" column ": "
;;				     (id (one-or-more (not (any " "))))
;;				     (message (one-or-more not-newline)
;;					      (zero-or-more "\n" (any " ") (one-or-more not-newline)))
;;				     line-end))
;;			   :modes (text-mode markdown-mode gfm-mode))
;;  (add-to-list 'flycheck-checkers 'textlint)
;;
;;  (with-eval-after-load 'markdown-mode
;;    (add-hook 'gfm-mode-hook
;;              (lambda ()
;;		(flycheck-mode)
;;		(add-node-modules-path)))
;;    (add-hook 'markdown-mode-hook
;;              (lambda ()
;;		(flycheck-mode)
;;		(add-node-modules-path))))
;;  )
;;
;;(use-package flycheck-inline
;;  :init
;;  (with-eval-after-load 'flycheck
;;    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))

;;; custome-file
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p (expand-file-name custom-file))
    (load-file (expand-file-name custom-file)))

;;; fido-mode
;;(savehist-mode +1)
;;(fido-vertical-mode +1)

;;; vertico
(use-package vertico
  :ensure t
  :init
  ;; 補完候補を最大15行まで表示する
  (setq vertico-count 15)
  (vertico-mode))

;;; marginalia
(require 'marginalia)
(marginalia-mode +1)

;;; orderless
(require 'orderless)
(setq completion-styles '(orderless))

;;; embark-consult
;;(with-eval-after-load 'consult
;;  (with-eval-after-load 'embark
;;    (require 'embark-consult)))

;;; corfu
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `global-corfu-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(setq corfu-cycle t)
(setq corfu-auto t)
(setq corfu-quit-no-match 'separator)
(setq corfu-quit-at-boundary nil)

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;;; consult
(require 'consult)
(global-set-key (kbd "C-x C-r") #'consult-recent-file)

;;; migemo
(require 'migemo)
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
(setq migemo-command "/usr/bin/cmigemo")
(setq migemo-options '("-q" "--emacs"))
(migemo-init)

;;;;; end of init.el ;;;;;

;;;;; company color config
;;   AndroidStudioの色をモチーフ
;;     選択: #2E64C8
;;     非選択: #3B3E40
;;     文字色: #AFB1B3
;;(set-face-attribute 'company-tooltip nil
;;                    :foreground "#AFB1B3" :background "#3B3E40")
;;(set-face-attribute 'company-tooltip-common nil
;;                    :foreground "#AFB1B3" :background "#3B3E40")
;;(set-face-attribute 'company-tooltip-common-selection nil
;;                    :foreground "#AFB1B3" :background "#2E64C8")
;;(set-face-attribute 'company-tooltip-selection nil
;;                    :foreground "#AFB1B3" :background "#2E64C8")
;;(set-face-attribute 'company-preview-common nil
;;                    :background nil :foreground "#3B3E40" :underline t)
;;(set-face-attribute 'company-scrollbar-fg nil
;;                    :background "orange")
;;(set-face-attribute 'company-scrollbar-bg nil
;;                    :background "gray40")
