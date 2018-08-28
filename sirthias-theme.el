;;; sirthias-theme.el --- Emacs 24 theme with a dark background.

;; Copyright (C) 2014 , Alexander Ivanov <4lex1v@gmail.com>

;; Author: Alexander Ivanov <4lex1v@gmail.com>
;; Version: 0.5.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

(deftheme sirthias "Sirthias color theme for Emacs")

(defun colour-join (r g b)
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun colour-blend (c1 c2 alpha)
  (apply #'colour-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          c1 c2)))  

(defcustom sirthias-easy-mode t
  "Enable easy colour mode, simplifies the colouring by removing the highliting for some symbols") 

(defcustom sirthias-cold-mode nil
  "Use warm or cold colours by default")

(defun alt (regular easy-mode)
  (if sirthias-easy-mode easy-mode regular))

(defun blend (colour rate)
  (colour-blend
   (color-values colour)
   (color-values fg1)
   rate))

(defun blend2 (c1 c2 rate)
  (colour-blend
   (color-values c1)
   (color-values c2)
   rate))

(let* ((class '((class color) (min-colors 89)))
      (fg1     (blend2 "#eee8d5" "#e6d3b4" 0.00))
      (fg2     (blend2 "#d9d3c2" "#e3ceab" 0.00))
      (fg3     (blend2 "#c4bfaf" "#c7b597" 0.00))
      (fg4     (blend2 "#afab9d" "#a69982" 0.00))
      
      (bg1     (blend "#002b36" 0.98))
      (bg2     (blend "#183944" 0.98))
      (bg3     (blend "#2b4852" 0.98))
      (bg4     (blend "#3e5861" 0.98))

      (keyword (blend "#e93532" 0.77))
      (str     (blend "#859901" 0.65))
      (comment (blend "#93a1a1" 0.75))
      (warning (blend "#cb4b16" 0.7))
      (link    (blend "#efef8f" 0.7))
      
      (var     (alt "#277082" fg1))
      (const   (alt "#efef8f" fg1))
      (type    (alt "#efef8f" fg1))
      (func    (alt "#277082" fg1))
      (builtin (alt "#8cd0d3" fg1)))
            
  (custom-theme-set-faces
   'sirthias

   ;; General
   `(default                             ((,class (:foreground ,fg1 :background ,bg1)))) 
   `(region                              ((,class (:background ,bg3)))) 
   `(cursor                              ((,class (:background ,fg1)))) 
   `(fringe                              ((,class (:background ,bg1))))
   `(hl-line                             ((,class (:background ,bg2))))
   `(link                                ((,class (:foreground ,builtin :underline t :weight bold))))
   `(link-visited                        ((,class (:foreground ,builtin :underline t :weight normal))))
   `(header-line                         ((,class (:background ,bg3))))
   `(icompletep-determined               ((,class (:foreground ,builtin))))
   `(slime-repl-inputed-output-face      ((,class (:foreground ,type))))
   `(trailing-whitespace                 ((,class (:foreground ,bg1 :background ,warning))))
   `(info-quoted-name                    ((,class (:foreground ,builtin))))
   `(info-string                         ((,class (:foreground ,str))))
   `(ffap                                ((,class (:foreground ,fg4))))
   `(lazy-highlight                      ((,class (:foreground ,fg2 :background ,bg3))))

   ;; States
   `(success                             ((,class (:foreground ,str))))
   `(warning                             ((,class (:foreground ,const))))
   `(error                               ((,class (:foreground ,keyword))))

   ;; Mode line
   `(mode-line                           ((,class (:foreground ,bg1 :background ,fg1 :box nil))))
   `(mode-line-buffer-id-inactive        ((,class (:foreground ,bg1 :background ,comment :box nil))))
   `(mode-line-buffer-id                 ((,class (:foreground ,bg1 :background ,fg1 :weight bold))))
   `(mode-line-inactive                  ((,class (:foreground ,bg1 :background ,comment :box nil))))
   `(mode-line-highlight                 ((,class (:foreground ,bg1 :background ,bg1))))
   `(mode-line-emphasis                  ((,class (:foreground ,bg1 :background ,fg1 :box nil))))
   
   ;; Font Lock
   `(font-lock-keyword-face              ((,class (:foreground ,keyword))))
   `(font-lock-comment-face              ((,class (:foreground ,comment))))
   `(font-lock-builtin-face              ((,class (:foreground ,keyword))))
   `(font-lock-constant-face             ((,class (:foreground ,const))))
   `(font-lock-doc-face                  ((,class (:foreground ,comment))))
   `(font-lock-doc-string-face           ((,class (:foreground ,comment))))
   `(font-lock-function-name-face        ((,class (:foreground ,builtin))))
   `(font-lock-type-face                 ((,class (:foreground ,const))))
   `(font-lock-preprocessor-face         ((,class (:foreground ,keyword))))
   `(font-lock-negation-char-face        ((,class (:foreground ,const))))
   `(font-lock-string-face               ((,class (:foreground ,str))))
   `(font-lock-variable-name-face        ((,class (:foreground ,builtin))))
   `(font-lock-warning-face              ((,class (:foreground ,keyword :underline t))))

   ;; Other UI general faces
   `(minibuffer-prompt                   ((,class (:foreground ,keyword :bold t))))
   `(linum                               ((,class (:foreground ,fg1 :background ,bg2))))
   `(show-paren-match-face               ((,class (:foreground ,bg1 :background ,fg4))))

   ;; ISearch
   `(isearch                             ((,class (:foreground ,const :background ,bg3 :underline t))))
   `(isearch-fail                        ((,class (:foreground ,warning :background ,bg3 :bold t))))
   
   ;; Latex
   `(font-latex-bold-face                ((,class (:bold   t))))
   `(font-latex-italic-face              ((,class (:italic t))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   
   ;; Org-mode
   `(org-level-1                         ((,class (:bold t :foreground ,fg1 :height 1.1 :underline t))))
   `(org-level-2                         ((,class (:bold t :foreground ,fg2 :underline t))))
   `(org-level-3                         ((,class (:bold t :foreground ,fg2 :underline t))))
   `(org-level-4                         ((,class (:bold t :foreground ,fg2 :underline t))))
   `(org-checkbox                        ((,class (:bold t :foreground ,keyword))))
   `(org-checkbox-statistics-todo        ((,class (:bold t :foreground ,keyword))))
   `(org-checkbox-statistics-done        ((,class (:bold t :foreground ,str))))
   `(org-scheduled                       ((,class (:foreground ,fg1))))
   `(org-scheduled-today                 ((,class (:inherite 'org-scheduled))))
   `(org-upcoming-deadline               ((,class (:underline t))))
   `(org-code                            ((,class (:foreground ,fg2))))
   `(org-hide                            ((,class (:foreground ,fg4))))
   `(org-date                            ((,class (:underline t :foreground ,builtin) )))
   `(org-footnote                        ((,class (:underline t :foreground ,fg4))))
   `(org-link                            ((,class (:underline t :foreground ,link))))
   `(org-special-keyword                 ((,class (:foreground ,func))))
   `(org-verbatim                        ((,class (:foreground ,builtin :bold t))))
   `(org-block                           ((,class (:foreground ,fg3))))
   `(org-quote                           ((,class (:inherit org-block :slant italic))))
   `(org-verse                           ((,class (:inherit org-block :slant italic))))
   `(org-warning                         ((,class (:underline t :foreground ,warning))))
   `(org-ellipsis                        ((,class (:foreground ,builtin))))
   `(org-document-info-keyword           ((,class (:foreground ,func))))
   `(org-sexp-date                       ((,class (:foreground ,fg4))))
   `(org-tag                             ((,class (:foreground ,keyword))))
   `(org-todo                            ((,class (:foreground ,keyword))))
   `(org-done                            ((,class (:foreground ,str))))

   ;; Org-agenda
   `(org-agenda-done                     ((,class (:strike-through t :italic t :foreground ,comment))))
   `(org-agenda-structure                ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
   `(org-agenda-date                     ((,class (:foreground ,var))))
   `(org-agenda-date-weekend             ((,class (:inherit 'org-agenda-date))))
   `(org-agenda-date-today               ((,class (:weight bold :foreground ,const :height 1.2))))
   `(org-agenda-dimmed-todo-face         ((,class (:foreground ,comment))))

   ;; Helm
   `(helm-header                         ((,class (:foreground ,bg1 :background ,fg1 :bold t))))
   `(helm-source-header                  ((,class (:foreground ,bg1 :background ,fg4 :underline nil :bold t))))
   `(helm-match                          ((,class (:foreground ,keyword :underline t))))
   `(helm-visible-mark                   ((,class (:foreground ,warning :background ,bg2))))
   `(helm-selection                      ((,class (:background ,bg3)))) ;; Used to highlight the line in helm buffers
   `(helm-selection-line                 ((,class (:background ,bg1)))) ;; Used in helm-current-buffer
   `(helm-candidate-number               ((,class (:foreground ,bg1 :background ,fg1))))
   `(helm-separator                      ((,class (:foreground ,type))))
   `(helm-time-zone-current              ((,class (:foreground ,builtin))))
   `(helm-time-zone-home                 ((,class (:foreground ,type))))
   `(helm-buffer-not-saved               ((,class (:foreground ,type))))
   `(helm-buffer-process                 ((,class (:foreground ,builtin))))
   `(helm-buffer-saved-out               ((,class (:foreground ,fg1))))
   `(helm-buffer-size                    ((,class (:foreground ,fg1))))
   `(helm-ff-directory                   ((,class (:foreground ,str :weight bold))))
   `(helm-ff-file                        ((,class (:foreground ,fg1 :weight normal))))
   `(helm-ff-executable                  ((,class (:foreground ,fg1 :weight normal))))
   `(helm-ff-invalid-symlink             ((,class (:foreground ,fg1 :weight bold))))
   `(helm-ff-symlink                     ((,class (:foreground ,keyword :weight bold))))
   `(helm-ff-prefix                      ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
   `(helm-grep-cmd-line                  ((,class (:foreground ,fg1))))
   `(helm-grep-file                      ((,class (:foreground ,fg1))))
   `(helm-grep-finish                    ((,class (:foreground ,fg2))))
   `(helm-grep-lineno                    ((,class (:foreground ,fg1))))
   `(helm-grep-match                     ((,class (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running                   ((,class (:foreground ,func))))
   `(helm-moccur-buffer                  ((,class (:foreground ,func))))

   ;; Evil
   `(evil-ex-substitute-matches          ((,class (:foreground ,keyword :underline t :strike-through t))))
   `(evil-ex-substitute-replacement      ((,class (:foreground ,str :underline t))))
   
   ;; Macrostep
   `(macrostep-expansion-highlight-face  ((,class (:background ,bg2))))
   
   ;; Hideshow
   `(hs-face                             ((,class (:foreground ,comment :background ,bg2))))

   ;; Magit
   `(magit-process-ok                    ((,class (:foreground ,fg1))))
   `(magit-item-highlight                ((,class (:background ,bg3))))
   `(magit-section-heading               ((,class (:foreground ,keyword :weight bold))))
   `(magit-hunk-heading                  ((,class (:background ,bg3))))
   `(magit-section-highlight             ((,class (:background ,bg2))))
   `(magit-hunk-heading-highlight        ((,class (:background ,bg3))))
   `(magit-diff-context-highlight        ((,class (:background ,bg3 :foreground ,fg3))))
   `(magit-diffstat-added                ((,class (:foreground ,type))))
   `(magit-diffstat-removed              ((,class (:foreground ,var))))
   `(magit-process-ok                    ((,class (:foreground ,func :weight bold))))
   `(magit-process-ng                    ((,class (:foreground ,warning :weight bold))))
   `(magit-branch                        ((,class (:foreground ,const :weight bold))))
   `(magit-log-author                    ((,class (:foreground ,fg3))))
   `(magit-hash                          ((,class (:foreground ,fg2))))
   `(magit-diff-file-header              ((,class (:foreground ,fg2 :background ,bg3))))
   `(magit-diffstat-added                ((,class (:foreground ,str))))
   `(magit-diffstat-removed              ((,class (:foreground ,keyword))))

   ;; Eshell
   `(eshell-prompt                       ((,class (:foreground ,keyword))))
   
   ;; Company
   `(company-echo-common                 ((,class (:foreground ,bg1 :background ,fg1))))
   `(company-tooltip                     ((,class (:foreground ,bg1 :background ,fg1))))
   `(company-tooltip-selection           ((,class (:foreground ,bg1 :background ,fg1))))
   `(company-tooltip-common              ((,class (:foreground ,bg1 :background ,fg1))))
   `(company-tooltip-common-selection    ((,class (:background ,fg1 :underline t))))
   `(company-scrollbar-bg                ((,class (:background ,fg1))))
   `(company-scrollbar-fg                ((,class (:background ,fg1))))
   `(company-template-field              ((,class (:inherit region))))
   `(company-preview-search              ((,class (:inherit match))))
   
   ;; Elfeed
   `(elfeed-search-date-face             ((,class (:foreground ,const))))
   `(elfeed-search-feed-face             ((,class (:foreground ,const))))
   `(elfeed-search-tag-face              ((,class (:foreground ,keyword))))
   `(elfeed-search-title-face            ((,class (:foreground ,comment))))
   `(elfeed-search-unread-title-face     ((,class (:foreground ,fg1))))
   
   ;; Web-Mode
   `(web-mode-keyword-face               ((,class (:foreground ,keyword))))
   `(web-mode-string-face                ((,class (:foreground ,str))))
   `(web-mode-html-attr-name-face        ((,class (:foreground ,const))))
   `(web-mode-html-attr-value-face       ((,class (:foreground ,str))))
   `(web-mode-html-tag-face              ((,class (:foreground ,builtin)))))
   `(web-mode-builtin-face               ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face               ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face              ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-doctype-face               ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face         ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-type-face                  ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-warning-face               ((,class (:inherit ,font-lock-warning-face)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'sirthias)

;; Local Variables:
;; no-byte-compile: t
;; eval: (rainbow-mode)
;; End:

;;; sirthias-theme.el ends here
