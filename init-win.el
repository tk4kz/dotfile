;;;;; init.el

;;; package
(require 'package)
;; MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; 日本語と文字コード指定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Windows IME
(tr-ime-standard-install)
(setq default-input-method "W32-IME")
(setq-default w32-ime-mode-line-state-indicator "[--]")
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
(w32-ime-initialize)

;; IME 制御（yes/no などの入力の時に IME を off にする）
(wrap-function-to-control-ime 'universal-argument t nil)
(wrap-function-to-control-ime 'read-string nil nil)
(wrap-function-to-control-ime 'read-char nil nil)
(wrap-function-to-control-ime 'read-from-minibuffer nil nil)
(wrap-function-to-control-ime 'y-or-n-p nil nil)
(wrap-function-to-control-ime 'yes-or-no-p nil nil)
(wrap-function-to-control-ime 'map-y-or-n-p nil nil)

;; font
;;
;; 1234567890
;; abcdefghij
;; あいうえお
;;
(add-to-list 'default-frame-alist '(font . "Meiryoke_Console 12"))
;;(add-to-list 'default-frame-alist '(font . "Cica-14"))

;; IME の未確定文字列のフォント設定
(set-frame-font "Meiryoke_Console 12" nil t)
(modify-all-frames-parameters '((ime-font . "Meiryoke_Console 12")))

;; 起動時の Welcome 画面無し
(setq inhibit-splash-screen t)

;; scratch buffer のメッセージ非表示
(setq initial-scratch-message "")

;; 一時マークモードの自動有効化
(setq-default transient-mark-mode t)

;; C-x C-u が何もしないように変更する (undo の typo 時誤動作防止)
(global-unset-key "\C-x\C-u")

;; 括弧の対応をハイライト.
;;(show-paren-mode 1) 

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
;;(add-to-list 'ignored-local-variables 'syntax) 

;;; Solarized theme
(load-theme 'solarized-dark t)

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

;;; keyboard macro
;; commented pry in Ruby script
(fset 'comment-pry
   "\C-[xreplace-regexp\C-mbinding.pry\C-m#binding.pry\C-m")

;; reload init.el
(fset 'loadinitel
   [?\M-x ?l ?o ?a ?d ?- ?f ?i ?l ?e return ?/ ?h ?o ?m ?e ?/ ?t ?a ?k ?e ?/ ?. ?e ?m ?a ?c ?s ?. ?d ?/ ?i ?n ?i ?t ?. ?e ?l return])

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

;;; amx
(use-package amx)

;;; which-key.el
(require 'which-key)

;;; Change mode-line color in view-mode
(use-package viewer)
(setq viewer-modeline-color-view "orange")
(viewer-change-modeline-color-setup)

;;;;; end of init.el


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(solarized-theme tr-ime)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
