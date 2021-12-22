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

;; Solarized theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/emacs-color-theme-solarized")

;;; Window の設定
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
  ;; 変換候補をミニバッファに表示
  ;;(setq mozc-candidate-style 'echo-area)
  
  ;; font
  ;;
  ;; 1234567890
  ;; abcdefghij
  ;; あいうえお
  ;; 
  (add-to-list 'default-frame-alist '(font . "Cica-14"))

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

    ;; Solarized theme適用
    (load-theme 'solarized-dark t)
    )
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
	("C-c m" . hydra-markdown/body))
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

;;; smart mode line
(setq sml/theme 'respectful)
(setq sml/no-confirm-load-theme t)
(setq sml/shorten-directory -1)
(setq sml/hidden-modes '(" Helm" " AC" " ElDoc" " company" " RBlock" " yas" " counsel" " ivy" " Projectile" " WK" " Ρ"))
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
;; companyで使うcolor
;;(set-face-attribute 'company-tooltip nil
;;		    :foreground "black" :background "lightgrey")
;;(set-face-attribute 'company-tooltip-common nil
;;		    :foreground "black" :background "lightgrey")
;;(set-face-attribute 'company-tooltip-common-selection nil
;;		    :foreground "white" :background "steelblue")
;;(set-face-attribute 'company-tooltip-selection nil
;;		    :foreground "black" :background "steelblue")
;;(set-face-attribute 'company-preview-common nil
;;		    :background nil :foreground "lightgrey" :underline t)
;;(set-face-attribute 'company-scrollbar-fg nil
;;		    :background "orange")
;;(set-face-attribute 'company-scrollbar-bg nil
;;		    :background "gray40")

;;; company-quickhelp
(setq company-quickhelp-color-foreground "white")
(setq company-quickhelp-color-background "dark slate gray")
(setq company-quickhelp-max-lines 5)
(company-quickhelp-mode)

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

;;; Bridge projectile and project together so packages that depend on project
;;; like eglot work
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

  ;; アクティベート
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

;;; pangu-spacing.el
(require 'pangu-spacing)
(setq pangu-spacing-real-insert-separtor t)
;; 特定のモードで使う
;;(add-hook 'text-mode-hook 'pangu-spacing-mode)
;; すべてのメジャーモードで使う
;;(global-pangu-spacing-mode 1)

;;; which-key.el
(require 'which-key)

;;; Change mode-line color in view-mode
(use-package viewer)
(setq viewer-modeline-color-view "orange")
(viewer-change-modeline-color-setup)

;;; modeline font scaling
;;;   拡大・縮小と2パターン欲しい
;;;   画面の解像度を判別して拡大・縮小の動きを変える
;;;   
;;(defun modeline-scale ()
;;    (interactive)
;;    (custom-set-faces '(mode-line ((t (:height 0.5))))))

;;;;; end of init.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(flymake-error-bitmap nil)
 '(flymake-note-bitmap nil)
 '(flymake-warning-bitmap nil)
 '(frame-background-mode 'dark)
 '(package-selected-packages
   '(viewer python-mode hide-mode-line which-key deadgrep pangu-spacing mozc-im amx flycheck-pycheckers flymake-ruby flycheck-posframe package-lint-flymake package-lint flymake-diagnostic-at-point posframe counsel ivy company-quickhelp company-quickhelp-terminal exec-path-from-shell smart-jump projectile eglot espy mozc use-package markdown-mode bind-key hydra markdown-preview-mode slime helm-c-yasnippet robe w3m yasnippet flycheck ruby-block helm-ag recentf-ext company magit helm zenburn-theme total-lines solarized-theme smart-mode-line molokai-theme madhat2r-theme gruvbox-theme dakrone-theme calmer-forest-theme))
 '(which-key-mode t))
