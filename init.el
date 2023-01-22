;;;;; init.el

;; debug
(setq debug-on-error t)

;; load path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; exec path
(add-to-list 'exec-path "/usr/local/bin")

;;; package
(require 'package)
;; MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; GUI config
(when window-system
  ;; mozc
  (require 'mozc-im)
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc-im")
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

  ;;  Solarized theme
  ;;    GUIのthemeはSolarized
    (load-theme 'solarized-dark t)
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line          nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :underline  line)
      (set-face-attribute 'mode-line          nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :background "#002b36"))
)

;;; emacsclient
(setq myGraphicModeHash (make-hash-table :test 'equal :size 2))
(puthash "gui" t myGraphicModeHash)
(puthash "term" t myGraphicModeHash)
(defun emacsclient-setup-theme-function (frame)
  (let ((gui (gethash "gui" myGraphicModeHash))
	(ter (gethash "term" myGraphicModeHash)))
    (progn
      (select-frame frame)
      (when (or gui ter) 
	(progn
	  ;; emacsclientのtheme
	  (load-theme 'kaolin-mono-dark t))
	  (if (display-graphic-p)
	      (puthash "gui" nil myGraphicModeHash)
	    (puthash "term" nil myGraphicModeHash))))
      (when (not (and gui ter))
	(remove-hook 'after-make-frame-functions 'emacsclient-setup-theme-function))))
(if (daemonp)
    (add-hook 'after-make-frame-functions 'emacsclient-setup-theme-function)
;;  (progn
;;    (load-theme 'kaolin-mono-dark t))
)

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
(define-key global-map "\C-h" 'delete-backward-char)
;; C-h に割り当てられている関数 help-command を C-x C-h に割り当てる
(define-key global-map "\C-x\C-h" 'help-command)
;; The local variables list in .emacs と言われるのを抑止
(add-to-list 'ignored-local-variables 'syntax) 

;;; cperl-mode
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)
(setq cperl-indent-level 4)
(setq cperl-label-offset -4)
(setq cperl-continued-statement-offset 4)
(add-hook 'cperl-mode-hook
	  '(lambda () 
	     (define-key cperl-mode-map "\C-cc" 'cperl-check-syntax)
	     (setq indent-tabs-mode nil)))

;;; markdown
(use-package markdown-mode
  :bind
  (:map markdown-mode-map
	("C-x m" . hydra-markdown/body))
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css") markdown-command "pandoc -t html5")

  ;; githib css
  (setq markdown-preview-stylesheets (list "https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"))
  
  (set-face-attribute 'markdown-code-face nil
                      :inherit 'outline-3)
  (set-face-attribute 'markdown-pre-face nil
                      :inherit 'outline-3)

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

;;; mode line
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

;;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'magit-ido-completing-read)

;;; company
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(global-set-key (kbd "TAB") 'company-indent-or-complete-common)
(setq company-auto-expand t)
(setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
(setq company-idle-delay 0) ; 遅延なしにすぐ表示
(setq company-minimum-prefix-length 1) ; 何文字打つと補完動作を行うか設定
(setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
;; C-n, C-p で補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-s で絞り込む
(define-key company-active-map [tab] 'company-complete-selection) ;; TAB で候補を設定
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-i で company-mode の補完を使う

;; company-quickhelp
(setq company-quickhelp-color-foreground "white")
(setq company-quickhelp-color-background "dark slate gray")
(setq company-quickhelp-max-lines 5)
(company-quickhelp-mode)

;; color config
;;   AndroidStudioの色をモチーフ
;;     選択: #2E64C8
;;     非選択: #3B3E40
;;     文字色: #AFB1B3
(set-face-attribute 'company-tooltip nil
                    :foreground "#AFB1B3" :background "#3B3E40")
(set-face-attribute 'company-tooltip-common nil
                    :foreground "#AFB1B3" :background "#3B3E40")
(set-face-attribute 'company-tooltip-common-selection nil
                    :foreground "#AFB1B3" :background "#2E64C8")
(set-face-attribute 'company-tooltip-selection nil
                    :foreground "#AFB1B3" :background "#2E64C8")
(set-face-attribute 'company-preview-common nil
                    :background nil :foreground "#3B3E40" :underline t)
(set-face-attribute 'company-scrollbar-fg nil
                    :background "orange")
(set-face-attribute 'company-scrollbar-bg nil
                    :background "gray40")

;;; company end ;;;


;;; recentf.el
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 1000)            ;; recentf に保存するファイルの数
(setq recentf-exclude '(".recentf"))           ;; .recentf 自体は含まない
;;(setq recentf-auto-cleanup 'never)             ;; 保存する内容を整理
(run-with-idle-timer 30 t '(lambda ()          ;; 30 秒ごとに .recentf を保存
			     (with-suppressed-message (recentf-save-list))))
(require 'recentf-ext)
;; キーバインド
(global-set-key (kbd "C-c o") 'recentf-open-files)

;;; X clipboard integration
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
;;; end of X clipboard integration 

;;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)

;;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("C-x i i" . yas-insert-snippet)
              ("C-x i n" . yas-new-snippet)
              ("C-x i v" . yas-visit-snippet-file)
              ("C-x i l" . yas-describe-tables)
              ("C-x i g" . yas-reload-all))              
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-ido-prompt))
  )

;;; east-asian-ambiguous
(require 'eaw)
(eaw-fullwidth)

;;; slime-indentation
(slime-setup '(slime-fancy slime-indentation))

;;; keyboard macro
;; commented pry in Ruby script
(fset 'comment-pry
   "\C-[xreplace-regexp\C-mbinding.pry\C-m#binding.pry\C-m")

;; reload init.el
(fset 'loadinitel
   [?\M-x ?l ?o ?a ?d ?- ?f ?i ?l ?e return ?/ ?h ?o ?m ?e ?/ ?t ?a ?k ?e ?/ ?. ?e ?m ?a ?c ?s ?. ?d ?/ ?i ?n ?i ?t ?. ?e ?l return])

;;; espy
(setq espy-password-file "~/.etc/passwd/passwd.org.gpg")

;;; eglot
(require 'eglot)
;; ruby-mode
(add-hook 'ruby-mode-hook 'eglot-ensure)
;; python-mode
(add-hook 'python-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(python-mode . ("pyls")))
(define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
(define-key eglot-mode-map (kbd "C-c e n") 'eglot-rename)

(defun project-root (project)
  (car (project-roots project)))

;;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(x))
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize)
  (setq exec-path-from-shell-check-startup-files nil))

;;; dumb-jump
(dumb-jump-mode)
(setq dumb-jump-selector 'ivy)

;;; smart-jump
(smart-jump-setup-default-registers)

;;; ivy
(when (require 'ivy nil t)
  ;; M-o を ivy-hydra-read-action に割り当てる．
  (when (require 'ivy-hydra nil t)
    (setq ivy-read-action-function #'ivy-hydra-read-action))
  ;; `ivy-switch-buffer' (C-x b) のリストに recent files と bookmark を含める．
  (setq ivy-use-virtual-buffers t)
  ;; ミニバッファでコマンド発行を認める
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1)) ;; 何回層入ったかプロンプトに表示．
  ;; ESC 連打でミニバッファを閉じる
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  ;; プロンプトの表示が長い時に折り返す（選択候補も折り返される）
  (setq ivy-truncate-lines nil)
  ;; リスト先頭で `C-p' するとき，リストの最後に移動する
  ;;  (setq ivy-wrap t)

  (setq ivy-height 20)
  (setq ivy-extra-directories nil)
  
  ;; アクティベート
  (ivy-mode 1))

;;; counsel
(when (require 'counsel nil t)
  ;; key bind
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-M-z") 'counsel-fzf)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-M-f") 'counsel-ag)
  ;;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;;(setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))
  (counsel-mode 1))

;;; swiper
(when (require 'swiper nil t)
  ;;(global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))
  (global-set-key (kbd "C-s") 'swiper-thing-at-point))

;;; flymake
(require 'flymake-diagnostic-at-point)
;;(require 'package-lint)
(with-eval-after-load 'flymake
  (add-hook 'flymake-mode-hook 'flymake-diagnostic-at-point-mode))

;;; js-mode
(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;;; amx
(use-package amx)

;;; which-key.el
(require 'which-key)

;;; Change mode-line color in view-mode
(use-package viewer)
(setq viewer-modeline-color-view "dark red")
(viewer-change-modeline-color-setup)

;;; Tree-sitter
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;;; super-save
(use-package super-save
  :defer 1
  :diminish
  :config
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 5)
  (super-save-mode +1))

;;; backup file
(setq make-backup-files nil)

;;; default-text-scale : Easily adjust the font size
(default-text-scale-mode 1)

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

;;(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;  :ensure t
;;  :config (treemacs-set-scope-type 'Perspectives))

;;(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;  :after (treemacs)
;;  :ensure t
;;  :config (treemacs-set-scope-type 'Tabs))

;;; treemacs end ;;;

;;; winum
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
(use-package flycheck
  :ensure t
  :init
  ;;(global-flycheck-mode t)
  :config
  (flycheck-define-checker textlint
			   "A linter for prose."
			   :command ("textlint" "--format" "unix"
				     source-inplace)
			   :error-patterns
			   ((warning line-start (file-name) ":" line ":" column ": "
				     (id (one-or-more (not (any " "))))
				     (message (one-or-more not-newline)
					      (zero-or-more "\n" (any " ") (one-or-more not-newline)))
				     line-end))
			   :modes (text-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'textlint)

  (with-eval-after-load 'markdown-mode
    (add-hook 'gfm-mode-hook
              (lambda ()
		(flycheck-mode)
		(add-node-modules-path)))
    (add-hook 'markdown-mode-hook
              (lambda ()
		(flycheck-mode)
		(add-node-modules-path))))
  )

(use-package flycheck-inline
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))


;;;;; end of init.el ;;;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(custom-safe-themes
   '("c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "dea4b7d43d646aa06a4f705a58f874ec706f896c25993fcf73de406e27dc65ba" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(fci-rule-color "#073642")
 '(flymake-error-bitmap nil)
 '(flymake-note-bitmap nil)
 '(flymake-warning-bitmap nil)
 '(frame-background-mode 'dark)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100)))
 '(package-selected-packages
   '(flycheck-inline add-node-modules-path adoc-mode srcery-theme kaolin-themes minions moody yaml-mode winum projectile-ripgrep treemacs default-text-scale super-save tree-sitter tree-sitter-langs viewer python-mode hide-mode-line which-key deadgrep pangu-spacing mozc-im amx flycheck-pycheckers flymake-ruby flycheck-posframe package-lint-flymake package-lint flymake-diagnostic-at-point posframe counsel ivy company-quickhelp company-quickhelp-terminal exec-path-from-shell smart-jump projectile eglot espy mozc use-package markdown-mode bind-key hydra markdown-preview-mode slime helm-c-yasnippet robe w3m yasnippet flycheck ruby-block helm-ag recentf-ext company magit helm zenburn-theme total-lines solarized-theme smart-mode-line molokai-theme madhat2r-theme gruvbox-theme dakrone-theme calmer-forest-theme))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#b58900")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#859900")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#2aa198")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markup-code-face ((t nil)))
 '(markup-meta-face ((t (:inherit font-lock-function-name-face))))
 '(markup-meta-hide-face ((t (:inherit markup-meta-face))))
 '(markup-secondary-text-face ((t (:foreground "firebrick"))))
 '(markup-title-0-face ((t (:inherit markup-gen-face))))
 '(markup-title-1-face ((t (:inherit markup-gen-face))))
 '(markup-title-2-face ((t (:inherit markup-gen-face))))
 '(markup-title-3-face ((t (:inherit markup-gen-face))))
 '(markup-title-4-face ((t (:inherit markup-gen-face))))
 '(markup-title-5-face ((t (:inherit markup-gen-face)))))
