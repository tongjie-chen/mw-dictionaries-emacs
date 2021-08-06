;; This file is not usage to get all words meaning
;;; mw-collegiate.el --- Merriam-Webster Collegiate -*- lexical-binding: t; -*-
;;
;; Author: 
;; URL: 
;; Created: Fri Apr 23 14:20:02 2021
;; Keywords: wp, matching
;; License: GPL v3
;; Package-Requires: ((emacs "25") (request "0.3.0") (dash "2.16.0"))
;; Version: 1.0.1

;;; Commentary:

;; Collegiate look up through www.dictionaryapi.com - Merriam-Webster online dictionary
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For more than 150 years, in print and now online, Merriam-Webster has been America's leading and most-trusted provider of language information.
;; Each month, Merriam-Webster web sites offer guidance to more than 40 million visitors. In print, publications include Merriam-Webster's Collegiate Dictionary (among the best-selling books in American history) and newly published dictionaries for English-language collegiate.
;; All Merriam-Webster products and services are backed by the largest team of professional dictionary editors and writers in America, and one of the largest in the world.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'request)
(require 'thingatpt)
(require 'xml)
(require 'org)
(require 'dash)

(defgroup mw-collegiate nil
  "Merriam-Webster Collegiate"
  :prefix "mw-collegiate-"
  :group 'applications)

(defvar mw-collegiate-mode-map (make-sparse-keymap)
  "Keymap for minor mode variable `mw-collegiate-mode'.")

(defvar mw-collegiate-buffer-name "* Merriam-Webster Collegiate *"
  "Default buffer name for Merriam-Webster Collegiate.")


(define-minor-mode mw-collegiate-mode
  "Merriam-Webster collegiate minor mode
\\{mw-collegiate-mode-map}"
  :group 'mw-collegiate
  :lighter " Merriam-Webster"
  :init-value nil
  :keymap mw-collegiate-mode-map)

(define-key mw-collegiate-mode-map [remap org-open-at-point] #'mw-collegiate-lookup-at-point)
(define-key mw-collegiate-mode-map (kbd "q") #'mw-collegiate--quit)
(define-key mw-collegiate-mode-map (kbd "w") #'mw-collegiate--save-to-builder)


(defcustom mw-collegiate-builder-file "~/org/vocab-builder/mw-collegiate.org"
  "Default buffer name for Merriam-Webster Collegiate."
  :type 'string)

(defcustom mw-collegiate--api-key
  "b2cedd24-e83d-49e0-ab31-3f4b26cd5bff"
  "Merriam-Webster API access key."
  :type 'string)

(defcustom mw-collegiate--base-url
  "http://www.dictionaryapi.com/api/v1/references/collegiate/xml/"
  "Merriam-Webster API base URL."
  :type 'string)

(defun mw-collegiate--save-to-builder ()
    "DOCSTRING"
  (interactive)
  (append-to-file nil nil mw-collegiate-builder-file)
    )

(defun mw-collegiate--get-xml-node (root path)
  "From parsed xml ROOT retrieves a node for given PATH.

Usage: `(mw-collegiate--get-xml-node html-root '(html head title))`"
  (let ((current-node (xml-get-children root (car path))))
    (if (< 1 (length path))
        (mw-collegiate--get-xml-node (car current-node) (cdr path))
      current-node)))

(defun mw-collegiate--italicize (prop)
  "Check for element PROP containing <it> tag, retrieves content, resulting string is placed between '/' and '/'."
  (let ((its (mw-collegiate--get-xml-node prop '(it))))
    (mapconcat
     (lambda (e)
       (if (member e its)
           (concat "​/" (-> e last car string-trim) "/​")
         (when (stringp e ) e)))
     prop "")))

(defun mw-collegiate--snd-subs (article)
  "Second level of ARTICLE. If this contains usage, do it as mapconcat."
  (let ((usages (mw-collegiate--get-xml-node article '(un))))
    (mapconcat
     (lambda (usage)
       (let ((desc (mw-collegiate--italicize usage))
             (third-lvl (mw-collegiate--fourth-lvl usage)))
	 ;; (debug)
         (string-join (list "*** " desc "\n" third-lvl) "")))
     usages
     "\n")))  
  ;; (let* ((whole-sub (-> article
  ;;                       (mw-collegiate--get-xml-node '(un))))
  ;;        (sub-str (mw-collegiate--italicize whole-sub)))
  ;;   (concat "   - " sub-str)))

(defun mw-collegiate--other-tag (article tag-type)
  "Parse ARTICLE for different TAG-TYPE."
  (let ((content (-> article
                     (mw-collegiate--get-xml-node `(,tag-type))
                     car
                     mw-collegiate--italicize))
        (title (cond
                ((eq tag-type 'phrase) "Phrases")
                ((eq tag-type 'vi) "Example sentences")
                ((eq tag-type 'near) "Near antonyms")
                ((eq tag-type 'ant) "Antonyms")
                ;; ((eq tag-type 'et) "Etymology")	;This is the same level as def.
                (t "Other"))))
    (when (and content (< 0 (length content)))
      (string-join (list "\n*** " title ":\n    " (replace-regexp-in-string ";" "\n   " content t t)) ""))))

(defun mw-collegiate--fourth-lvl (article)
  "Third level of ARTICLE."
  (let ((phrases (mw-collegiate--other-tag article 'phrase))
        (vis (mw-collegiate--other-tag article 'vi))
        (nears (mw-collegiate--other-tag article 'near))
        (ants (mw-collegiate--other-tag article 'ant))
        (et (mw-collegiate--other-tag article 'et)))    
    (string-join (list phrases vis nears ants et) "")))

(defun mw-collegiate--snd-level (entry)
  "Second level of ENTRY."
  ;; (setq testa (mw-collegiate--get-xml-node entry '(shortdef)))
  (let ((articles (mw-collegiate--get-xml-node entry '(def))))
    ;; Should iterate over dt as well.
    ;; (debug)
    (mapconcat
     (lambda (article)
       ;; (debug)
       (mw-collegiate--third-level article '(dt))
       )
     articles
     "\n")))

(defun mw-collegiate--third-level (entry entry-key)
  "Second level of ENTRY."

  (let ((articles (mw-collegiate--get-xml-node entry entry-key)))
    
    (mapconcat
     (lambda (article)
       ;; (debug)
       
       (let ((desc
	      (if (stringp (car (cdr (cdr article))))
		  (replace-regexp-in-string ":" "" (car (cdr (cdr article))))
		""
	      ))
             (snd-subs (mw-collegiate--snd-subs article))
             (third-lvl (mw-collegiate--fourth-lvl article)))
	 ;; (debug)
         (string-join (list "** " desc "\n" snd-subs third-lvl) "")))
     articles
     "\n")))


(defun mw-collegiate--get-title (entry)
  "Title for ENTRY."
  (cdr (car (car (cdr entry))))
  )

(defun mw-collegiate--get-type (entry)
  "Type of the ENTRY is at <fl> tag."
  (-> (mw-collegiate--get-xml-node entry '(fl))
      car (seq-drop 2) car))

(defun mw-collegiate--parse (xml-data)
  "Parse xml returned by Merriam-Webster dictionary API.

Take XML-DATA, Returns multi-line text in ‘org-mode’ format."
  (let* ((entry-list (assq 'entry_list xml-data))
         (entries (xml-get-children entry-list 'entry)))
    (mapconcat
     (lambda (entry)
       (let ((fst-level (concat "* " (mw-collegiate--get-title entry)
                                " ~" (mw-collegiate--get-type entry) "~\n"))
             (snd-level (mw-collegiate--snd-level entry)))
         (string-join (list fst-level snd-level) "")))
     entries "\n")))

(defun mw-collegiate--create-buffer (word data)
  "Build mw-collegiate buffer for WORD and the relevant DATA from Merriam-Webster API."
  (let ((dict-str (mw-collegiate--parse data)))
    (if (< (length dict-str) 1)
	(browse-url (concat "https://www.merriam-webster.com/dictionary/" word))
        ;; (message (concat "Sadly, Merriam-Webster doesn't seem to have anything for " word))
      (let ((temp-buf (get-buffer-create mw-collegiate-buffer-name)))
        ;; (print temp-buf)
        (when (not (bound-and-true-p mw-collegiate-mode))
          (switch-to-buffer-other-window temp-buf))
        (set-buffer temp-buf)
        (with-current-buffer temp-buf
          (read-only-mode -1)
          (setf (buffer-string) "")
          (setf org-hide-emphasis-markers t)
          (funcall 'org-mode)
          (funcall 'mw-collegiate-mode)
          (insert (decode-coding-string dict-str 'dos))
          (goto-char (point-min))
          (read-only-mode))))))

(defun mw-collegiate-get-original-word (beginning end)
  "Get a word to look for from the user.
`BEGINNING' and `END' correspond to the selected text (if selected).
If presented, the selected text will be used.
Otherwise, user must provide additional information."
  (if (use-region-p)
      (buffer-substring-no-properties beginning end)
    (read-string "Word to look up: ")))

(defun mw-collegiate-is-at-the-beginning-of-word (word-point)
  "Predicate to check whether `WORD-POINT' points to the beginning of the word."
  (save-excursion
    ;; If we are at the beginning of a word
    ;; this will take us to the beginning of the previous word.
    ;; Otherwise, this will take us to the beginning of the current word.
    (backward-word)
    ;; This will take us to the end of the previous word or to the end
    ;; of the current word depending on whether we were at the beginning
    ;; of a word.
    (forward-word)
    ;; Compare our original position with wherever we're now to
    ;; separate those two cases
    (< (point) word-point)))

;;;###autoload
(defun mw-collegiate-lookup-dwim ()
  "Look up a collegiate definition on demand using Merriam-Webster online dictionary.
If a region is selected use mw-collegiate-lookup-word
if a thing at point is not empty use mw-collegiate-lookup-word-at-point
otherwise as for word using mw-collegiate-lookup-word"
  (interactive)
  (let (beg end)
    (if (use-region-p)
        (progn
          (setq beg (region-beginning)
                end (region-end))
          (mw-collegiate-lookup beg end))
      (if (thing-at-point 'word)
          (mw-collegiate-lookup-at-point (point))
        (mw-collegiate-lookup)))))

;;;###autoload
(defun mw-collegiate-lookup-at-point (word-point)
  "Look up a collegiate definition for word at point using Merriam-Webster online dictionary."
  (interactive (list (point)))
  (save-mark-and-excursion
    (unless (mw-collegiate-is-at-the-beginning-of-word word-point)
      (backward-word))
    (set-mark (point))
    (forward-word)
    (activate-mark)
    (mw-collegiate-lookup (region-beginning) (region-end))))

;;;###autoload
(defun mw-collegiate-lookup (&optional beginning end)
  "Look up a collegiate definition for word using Merriam-Webster online dictionary.
`BEGINNING' and `END' correspond to the selected text with a word to look up.
If there is no selection provided, additional input will be required."
  (interactive
   ;; it is a simple interactive function instead of interactive "r"
   ;; because it doesn't produce an error in a buffer without a mark
   (if (use-region-p) (list (region-beginning) (region-end))
     (list nil nil)))
  (let* ((word (mw-collegiate-get-original-word beginning end))
         (url (concat (symbol-value 'mw-collegiate--base-url)
                      word "?key="
                      (symbol-value 'mw-collegiate--api-key))))
    (request url
             :parser (lambda () (xml-parse-region (point-min) (point-max)))
             :success (cl-function
                       (lambda (&key data word &allow-other-keys)
                         (mw-collegiate--create-buffer word data))))))

;;;###autoload
(defun mw-collegiate-lookup-string (word)
  "Look up a collegiate definition for word using Merriam-Webster online dictionary.
`BEGINNING' and `END' correspond to the selected text with a word to look up.
If there is no selection provided, additional input will be required."'
  (interactive)
  (let* ((url (concat (symbol-value 'mw-collegiate--base-url)
                      word "?key="
                      (symbol-value 'mw-collegiate--api-key)))
	 (word word))
    (request url
             :parser (lambda () (xml-parse-region (point-min) (point-max)))
             :success (cl-function
                       (lambda (&key data word &allow-other-keys)
                         (mw-collegiate--create-buffer word data))))))

(defun mw-collegiate--quit ()
  "Kill Merriam-Webster Collegiate buffer."
  (interactive)
  (when-let* ((buffer (get-buffer mw-collegiate-buffer-name)))
    (quit-window)
    (kill-buffer buffer)))

(provide 'mw-collegiate)

;;; mw-collegiate.el ends here
