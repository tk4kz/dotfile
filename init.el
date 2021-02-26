;;;;; init.el

;; load path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; exec path
(add-to-list 'exec-path "/usr/local/bin")

;;; package
(require 'package)
;; MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; MELPA-stable
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; Marmalade
;;(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; Org
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Solarized theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/emacs-color-theme-solarized")

;; Window(非ターミナル)の設定
;;(when (equal window-system 'x)
(when window-system
  ;; GUI の設定
  ;; mozc
  ;;(require 'mozc)                                 ; mozc の読み込み
  (require 'mozc-im)
  (set-language-environment "Japanese")           ; 言語環境を"japanese"に
  ;;(setq default-input-method "japanese-mozc")     ; IME を japanes-mozc に
  (setq default-input-method "japanese-mozc-im")
  (prefer-coding-system 'utf-8)                   ; デフォルトの文字コードを UTF-8 に
  (global-set-key [henkan]
		  (lambda () (interactive)
		    (when (null current-input-method) (toggle-input-method))))
  (global-set-key [muhenkan]
		  (lambda () (interactive)
		    (inactivate-input-method)))
  ;; 全角半角キーと無変換キーのキーイベントを横取りする
  (defadvice mozc-handle-event (around intercept-keys (event))
    "Intercept keys muhenkan and zenkaku-hankaku, before passing keys to mozc-server (which the function mozc-handle-event does), to properly disable mozc-mode."
    (if (member event (list 'zenkaku-hankaku 'muhenkan))
	(progn
	  (mozc-clean-up-session)
	  (toggle-input-method))
      (progn ;(message "%s" event) ;debug
	ad-do-it)))
  (ad-activate 'mozc-handle-event)
  ;; 変換候補をミニバッファに表示
  (setq mozc-candidate-style 'echo-area)

  
  ;; font
  (set-face-attribute 'default nil
		      :family "DejaVu Sans Mono"
		      :height 120) ;; 12pt
  (set-fontset-font nil 'japanese-jisx0208
		    (font-spec :family "IPAGothic" ))
  ;; フォントの横幅を調整
  (add-to-list 'face-font-rescale-alist '(".*IPA.*" . 1.25))

  ;; Server 設定
  (require 'server)
  (unless (eq (server-running-p) 't)
    (server-start)

    (defun iconify-emacs-when-server-is-done ()
      (unless server-clients (iconify-frame)))

    ;; C-x C-c に割り当てる(好みに応じて)
    (global-set-key (kbd "C-x C-c") 'server-edit)
    ;; M-x exit で Emacs を終了できるようにする
    (defalias 'exit 'save-buffers-kill-emacs)
    ;; 起動時に最小化する
    (add-hook 'after-init-hook 'iconify-emacs-when-server-is-done)

    ;; 終了時に yes/no の問い合わせ
    (setq confirm-kill-emacs 'yes-or-no-p)
    )
  
  ;; Solarized theme
  ;;(load-theme 'solarized t)
)

;; Solarized theme
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
	  (load-theme 'solarized t))
	  (if (display-graphic-p)
	      (puthash "gui" nil myGraphicModeHash)
	    (puthash "term" nil myGraphicModeHash))))
      (when (not (and gui ter))
	(remove-hook 'after-make-frame-functions 'emacsclient-setup-theme-function))))
(if (daemonp)
    (add-hook 'after-make-frame-functions 'emacsclient-setup-theme-function)
  (progn 
    (load-theme 'solarized t)
    ))

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

;; C-x l で goto-line を実行
;;(define-key ctl-x-map "l" 'goto-line) 

;; 時間を表示
;;(display-time) 

;; 列数表示
;;(column-number-mode 1) 

;; メニューバーを消す
(menu-bar-mode -1)

;; ツールバー非表示
(tool-bar-mode 0)

;; C-h でカーソルの左にある文字を消す
(define-key global-map "\C-h" 'delete-backward-char)

;; C-h に割り当てられている関数 help-command を C-x C-h に割り当てる
(define-key global-map "\C-x\C-h" 'help-command)

;; C-o に動的略語展開機能を割り当てる
;;(define-key global-map "\C-o" 'dabbrev-expand)
;;(setq dabbrev-case-fold-search nil) ; 大文字小文字を区別

;; BS で選択範囲を消す
;;(delete-selection-mode 1)

;; The local variables list in .emacs と言われるのを抑止
(add-to-list 'ignored-local-variables 'syntax) 

;; カーソル位置へのペースト
;(setq mouse-yank-at-point t)

;; I never use C-x C-c
;;(defalias 'exit 'save-buffers-kill-emacs)

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

;;; view-mode
;;(require 'viewer)
;;(viewer-stay-in-setup)
;; 色の指定
;;(setq viewer-modeline-color-unwritable "tomato")
;;(setq viewer-modeline-color-view "orange")
;;(viewer-change-modeline-color-setup)
;;(viewer-aggressive-setup 'force)

;; migemo
;;(require 'migemo)
;;(setq migemo-command "/usr/local/bin/cmigemo")
;;(setq migemo-options '("-q" "--emacs"))
;;(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
;;(setq migemo-user-dictionary nil)
;;(setq migemo-regex-dictionary nil)
;;(setq migemo-coding-system 'utf-8-unix)
;;(load-library "migemo")
;;(migemo-init)

;;; helm
;;(require 'helm-config)
;;(helm-mode 1)
;;
;;(define-key global-map (kbd "M-x")     'helm-M-x)
;;(define-key global-map (kbd "C-x C-f") 'helm-find-files)
;;(define-key global-map (kbd "C-x C-r") 'helm-recentf)
;;(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
;;(define-key global-map (kbd "C-c i")   'helm-imenu)
;;(define-key global-map (kbd "C-x b")   'helm-buffers-list)
;;
;;(define-key helm-map (kbd "C-h") 'delete-backward-char)
;;(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
;;;;(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-actio)
;;;;(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;;
;;;;; Disable helm in some functions
;;(add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
;;
;;;;; Emulate `kill-line' in helm minibuffer
;;(setq helm-delete-minibuffer-contents-from-point t)
;;(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
;;  "Emulate `kill-line' in helm minibuffer"
;;  (kill-new (buffer-substring (point) (field-end))))
;;
;;(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
;;  "Execute command only if CANDIDATE exists"
;;  (when (file-exists-p candidate)
;;    ad-do-it))
;;
;;(defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
;;  "Transform the pattern to reflect my intention"
;;  (let* ((pattern (ad-get-arg 0))
;;         (input-pattern (file-name-nondirectory pattern))
;;         (dirname (file-name-directory pattern)))
;;    (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
;;    (setq ad-return-value
;;          (concat dirname
;;                  (if (string-match "^\\^" input-pattern)
;;                      ;; '^' is a pattern for basename
;;                      ;; and not required because the directory name is prepended
;;                      (substring input-pattern 1)
;;                    (concat ".*" input-pattern))))))
;;
;;;;; helm-ag
;;(require 'helm-ag)
;;(setq helm-ag-base-command "rg -S --vimgrep --no-heading")
;;(global-set-key (kbd "C-c s") 'helm-ag)
;;

;;; markdown
(use-package markdown-mode
  :bind
  (:map markdown-mode-map
	("C-c m" . hydra-markdown/body))
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq
   ;;markdown-command "github-markup"
   ;;markdown-command-needs-filename t
   ;;markdown-content-type "application/xhtml+xml"
   markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css")
   markdown-command "pandoc -t html5")

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

;;; smart mode line
(defvar sml/no-confirm-load-theme t)
(defvar sml/shorten-directory -1)
(setq sml/hidden-modes '(" Helm" " AC" " ElDoc" " company" " RBlock" " yas" " counsel" " ivy" " Projectile" " P"))
(sml/setup)
(column-number-mode t)
(line-number-mode t)

;;; total-line
(require 'total-lines)
(global-total-lines-mode t)
(defun my-set-line-numbers ()
  (setq-default mode-line-front-space
		(append mode-line-front-space
			'((:eval (format " (%d)" (- total-lines 1)))))))
(add-hook 'after-init-hook 'my-set-line-numbers)

;;; magit
(global-set-key (kbd "C-x g") 'magit-status)

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
;; color
(set-face-attribute 'company-tooltip nil
		    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common nil
		    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common-selection nil
		    :foreground "white" :background "steelblue")
(set-face-attribute 'company-tooltip-selection nil
		    :foreground "black" :background "steelblue")
(set-face-attribute 'company-preview-common nil
		    :background nil :foreground "lightgrey" :underline t)
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
    ;; I prefer using the "clipboard" selection (the one the
    ;; typically is used by c-c/c-v) before the primary selection
    ;; (that uses mouse-select/middle-button-click)
    (call-process-region (point-min) (point-max)
			 "xsel" nil 0 nil "--clipboard" "--input")))
(defun xsel-paste-function()
  ;; Find out what is current selection by xsel. If it is different
  ;; from the top of the kill-ring (car kill-ring), then return
  ;; it. Else, nil is returned, so whatever is in the top of the
  ;; kill-ring will be used.
  (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
    (unless (string= (car kill-ring) xsel-output)
      xsel-output )))
(setq interprogram-cut-function 'xsel-cut-function)
(setq interprogram-paste-function 'xsel-paste-function)
;; end of X clipboard integration 

;;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)

;;; flycheck
(require 'flycheck)
;;(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
;;(dolist (hook (list
;;               'ruby-mode-hook
;;               'python-mode-hook
;;               ))
;;  (add-hook hook '(lambda () (flycheck-mode 1))))
;;
;; Use posframe as flycheck UI.
;;(with-eval-after-load 'flycheck
;;  (require 'flycheck-posframe)
;;  (add-hook 'flycheck-mode-hook 'flycheck-posframe-mode)
;;  (setq flycheck-posframe-position 'window-bottom-left-corner)
;;  (set-face-attribute 'flycheck-posframe-error-face
;;                    nil
;;                    :inherit nil
;;                    :foreground "red")
;;
;;  (set-face-attribute 'flycheck-posframe-warning-face
;;                    nil
;;                    :foreground "orange")
;;
;;  (set-face-attribute 'flycheck-posframe-info-face
;;                    nil
;;                    :foreground "cyan")
;;
;;  (set-face-attribute 'flycheck-posframe-border-face
;;                    nil
;;                    :foreground "#dc752f")
;;)

;;; yasnippet
(with-eval-after-load 'yasnippet
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"))
;; キーバインド
  (bind-keys :map yas-minor-mode-map	
	     ("C-<tab>" . yas-expand)
	     ("C-c C-s" . yas-insert-snippet)
	     ("<tab>" . nil) ; 他プラグイン(入力補完など)との競合回避
	     )
;; スニペット読込
  (yas-reload-all)
;; 有効にするモード
  (add-hook 'ruby-mode-hook 'yas-minor-mode))

;;; helm-c-yasnippet
;;(require 'helm-c-yasnippet)
;;(setq helm-yas-space-match-any-greedy t)
;;(global-set-key (kbd "C-c y") 'helm-yas-complete)
;;(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)

;;; east-asian-ambiguous
(require 'eaw)
(eaw-fullwidth)

;;; w3m
;;(require 'w3m)
;;(defun w3m-browse-url-other-window (url &optional newwin)
;;  (let ((w3m-pop-up-windows t))
;;    (if (one-window-p) (split-window))
;;    (other-window 1)
;;    (w3m-browse-url url newwin)))

;;; inf-ruby
;;(define-key inf-ruby-minor-mode-map (kbd "C-c C-s") nil)
;;(define-key global-map (kbd "C-c C-i") 'inf-ruby)
;;(with-eval-after-load 'inf-ruby
;; キーバインド
;;  (bind-keys :map inf-ruby-minor-mode-map
;;	     ("C-c C-s" . nil)
;;	     ("C-c C-i" . inf-ruby)))

;;; robe
;;(add-hook 'ruby-mode-hook 'robe-mode)

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

;; Bridge projectile and project together so packages that depend on project
;; like eglot work
(use-package projectile
  :ensure t)
(require 'projectile)
(defun my-projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))
(projectile-mode t)
(with-eval-after-load 'project
  (add-to-list 'projectile-project-root-files ".projectroot")
  (add-to-list 'project-find-functions 'my-projectile-project-find-function))

;;; eglot
(require 'eglot)
;; ruby-mode
(add-hook 'ruby-mode-hook 'eglot-ensure)

;; python-mode
(add-hook 'python-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(python-mode . ("pyls")))

;;(define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
;;(define-key eglot-mode-map (kbd "C-c e n") 'eglot-rename)

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

;; dumb-jump
(dumb-jump-mode)
(setq dumb-jump-selector 'ivy)

;; smart-jump
(smart-jump-setup-default-registers)

;; ivy
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
  (setq ivy-wrap t)

  (setq ivy-height 20)
  (setq ivy-extra-directories nil)
  
  ;; アクティベート
  (ivy-mode 1))

;; counsel
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

  ;; アクティベート
  (counsel-mode 1))

;; swiper
(when (require 'swiper nil t)
  ;;(global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))
  (global-set-key (kbd "C-s") 'swiper-thing-at-point))

;;; flymake
;;(require 'flymake-diagnostic-at-point)
;;(require 'package-lint)
;;(with-eval-after-load 'flymake
;;  (add-hook 'flymake-mode-hook 'flymake-diagnostic-at-point-mode)
;;  (add-hook 'emacs-lisp-mode-hook 'package-lint-setup-flymake)
;;  (set-face-attribute 'popup-tip-face nil
;;		      :background "dark slate gray" :foreground "white" :underline nil))
;;(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

;; flymake-posframe
;;(defvar flymake-posframe-hide-posframe-hooks
;;  '(pre-command-hook post-command-hook focus-out-hook)
;;  "The hooks which should trigger automatic removal of the posframe.")
;;
;;(defun flymake-posframe-hide-posframe ()
;;  "Hide messages currently being shown if any."
;;  (posframe-hide " *flymake-posframe-buffer*")
;;  (dolist (hook flymake-posframe-hide-posframe-hooks)
;;    (remove-hook hook 'flymake-posframe-hide-posframe t)))
;;
;;(defun my/flymake-diagnostic-at-point-display-popup (text)
;;  "Display the flymake diagnostic TEXT inside a posframe."
;;  (posframe-show " *flymake-posframe-buffer*"
;;		 :string (concat flymake-diagnostic-at-point-error-prefix
;;				 (flymake--diag-text
;;				  (get-char-property (point) 'flymake-diagnostic)))
;;		 :position (point)
;;		 :foreground-color "cyan"
;;		 :internal-border-width 2
;;		 :internal-border-color "red"
;;		 :poshandler 'posframe-poshandler-window-bottom-left-corner)
;; (dolist (hook flymake-posframe-hide-posframe-hooks)
;;    (add-hook hook 'flymake-posframe-hide-posframe nil t)))
;;(advice-add 'flymake-diagnostic-at-point-display-popup :override 'my/flymake-diagnostic-at-point-display-popup)
;; https://www.ncaq.net/2019/02/08/19/36/52/
;;(with-eval-after-load 'flymake
;;  (custom-set-variables
;;   '(flymake-error-bitmap nil)
;;   '(flymake-note-bitmap nil)
;;   '(flymake-warning-bitmap nil)
;;   )
;;  (set-face-underline 'flymake-error nil)
;;  (set-face-underline 'flymake-note nil)
;;  (set-face-underline 'flymake-warning nil)
;;  )
;
;;(require 'flymake-ruby)
;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;;; js-mode
(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;;; amx
(use-package amx)

;;; pangu-spacing.el
(require 'pangu-spacing)
(setq pangu-spacing-real-insert-separtor t)
;; 特定のモードで使う
;;(add-hook 'text-mode-hook 'pangu-spacing-mode)
;; すべてのメジャーモードで使う
(global-pangu-spacing-mode 1)


;;;;; end of init.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(flymake-error-bitmap nil)
 '(flymake-note-bitmap nil)
 '(flymake-warning-bitmap nil)
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (pangu-spacing mozc-im amx flycheck-pycheckers flymake-ruby flycheck-posframe package-lint-flymake package-lint flymake-diagnostic-at-point posframe counsel ivy company-quickhelp company-quickhelp-terminal exec-path-from-shell smart-jump python-mode projectile eglot espy mozc use-package markdown-mode bind-key hydra markdown-preview-mode slime helm-c-yasnippet robe w3m yasnippet flycheck ruby-block helm-ag recentf-ext company magit helm zenburn-theme total-lines solarized-theme smart-mode-line molokai-theme madhat2r-theme gruvbox-theme dakrone-theme calmer-forest-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
