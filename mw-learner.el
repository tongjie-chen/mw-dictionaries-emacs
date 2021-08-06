;; This file is not usage to get all words meaning
;;; mw-learner.el --- Merriam-Webster Learner -*- lexical-binding: t; -*-
;;
;; Author: Tongjie Chen
;; URL: 
;; Created: Fri Apr 23 14:20:02 2021
;; Keywords: wp, matching
;; License: GPL v3
;; Package-Requires: ((emacs "25") (request "0.3.0") (dash "2.16.0"))
;; Version: 1.0.1

;;; Commentary:

;; Learner look up through www.dictionaryapi.com - Merriam-Webster online dictionary
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For more than 150 years, in print and now online, Merriam-Webster has been America's leading and most-trusted provider of language information.
;; Each month, Merriam-Webster web sites offer guidance to more than 40 million visitors. In print, publications include Merriam-Webster's Collegiate Dictionary (among the best-selling books in American history) and newly published dictionaries for English-language learners.
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

(defgroup mw-learner nil
  "Merriam-Webster Learner"
  :prefix "mw-learner-"
  :group 'applications)

(defvar mw-learner-mode-map (make-sparse-keymap)
  "Keymap for minor mode variable `mw-learner-mode'.")

(defvar mw-learner-buffer-name "* Merriam-Webster Learner *"
  "Default buffer name for Merriam-Webster Learner.")

(define-minor-mode mw-learner-mode
  "Merriam-Webster learner minor mode
\\{mw-learner-mode-map}"
  :group 'mw-learner
  :lighter " Merriam-Webster"
  :init-value nil
  :keymap mw-learner-mode-map)

(define-key mw-learner-mode-map [remap org-open-at-point] #'mw-learner-lookup-at-point)
(define-key mw-learner-mode-map (kbd "q") #'mw-learner--quit)
(define-key mw-learner-mode-map (kbd "w") #'mw-learner--save-to-builder)


(defcustom mw-learner-builder-file "~/org/vocab-builder/mw-learner.org"
  "Default buffer name for Merriam-Webster Learner."
  :type 'string)


(defcustom mw-learner--api-key
  "53eb0727-c4bd-4bce-abc3-f9092c7a2792"
  "Merriam-Webster API access key."
  :type 'string)

(defcustom mw-learner--base-url
  "http://www.dictionaryapi.com/api/v1/references/learners/xml/"
  "Merriam-Webster API base URL."
  :type 'string)

(defun mw-learner--save-to-builder ()
    "DOCSTRING"
  (interactive)
  (append-to-file nil nil mw-learner-builder-file)
  )

(defun mw-learner--get-xml-node (root path)
  "From parsed xml ROOT retrieves a node for given PATH.

Usage: `(mw-learner--get-xml-node html-root '(html head title))`"
  (let ((current-node (xml-get-children root (car path))))
    (if (< 1 (length path))
        (mw-learner--get-xml-node (car current-node) (cdr path))
      current-node)))

(defun mw-learner--italicize (prop)
  "Check for element PROP containing <it> tag, retrieves content, resulting string is placed between '/' and '/'."
  (let ((its (mw-learner--get-xml-node prop '(it))))
    (mapconcat
     (lambda (e)
       (if (member e its)
           (concat "​/" (-> e last car string-trim) "/​")
         (when (stringp e ) e)))
     prop "")))

(defun mw-learner--snd-subs (article)
  "Second level of ARTICLE. If this contains usage, do it as mapconcat."
  (let ((usages (mw-learner--get-xml-node article '(un))))
    (mapconcat
     (lambda (usage)
       (let ((desc (mw-learner--italicize usage))
             (third-lvl (mw-learner--fourth-lvl usage)))
	 ;; (debug)
         (string-join (list "*** " desc "\n" third-lvl) "")))
     usages
     "\n")))  
  ;; (let* ((whole-sub (-> article
  ;;                       (mw-learner--get-xml-node '(un))))
  ;;        (sub-str (mw-learner--italicize whole-sub)))
  ;;   (concat "   - " sub-str)))

(defun mw-learner--other-tag (article tag-type)
  "Parse ARTICLE for different TAG-TYPE."
  (let ((content (-> article
                     (mw-learner--get-xml-node `(,tag-type))
                     car
                     mw-learner--italicize))
        (title (cond
                ((eq tag-type 'phrase) "Phrases")
                ((eq tag-type 'vi) "Example sentences")
                ((eq tag-type 'near) "Near antonyms")
                ((eq tag-type 'ant) "Antonyms")
                ((eq tag-type 'et) "Etymology")	;This is the same level as def.
                (t "Other"))))
    (when (and content (< 0 (length content)))
      (string-join (list "   " (replace-regexp-in-string ";" "\n   " content t t)) ""))))      
      ;; (string-join (list "\n*** " title ":\n    " (replace-regexp-in-string ";" "\n   " content t t)) ""))))

(defun mw-learner--fourth-lvl (article)
  "Third level of ARTICLE."
  (let ((phrases (mw-learner--other-tag article 'phrase))
        (vis (mw-learner--other-tag article 'vi))
        (nears (mw-learner--other-tag article 'near))
        (ants (mw-learner--other-tag article 'ant))
        (et (mw-learner--other-tag article 'et)))    
    (string-join (list phrases vis nears ants et) "")))

(defun mw-learner--snd-level (entry entry-key)
  "Second level of ENTRY."
  ;; (setq testa (mw-learner--get-xml-node entry '(shortdef)))
  (let ((articles (mw-learner--get-xml-node entry entry-key)))
    ;; Should iterate over dt as well.
    ;; (debug)
    (mapconcat
     (lambda (article)
       ;; (debug)
       (mw-learner--third-level article '(dt))
       )
     articles
     "\n")))

(defun mw-learner--third-level (entry entry-key)
  "Second level of ENTRY."

  (let ((articles (mw-learner--get-xml-node entry entry-key)))
    
    (mapconcat
     (lambda (article)
       ;; (debug)
       
       (let ((desc
	      (if (stringp (car (cdr (cdr article))))
		  (replace-regexp-in-string ":" "" (car (cdr (cdr article))))
		""
	      ))
             (snd-subs (mw-learner--snd-subs article))
             (third-lvl (mw-learner--fourth-lvl article)))
	 ;; (debug)
         (string-join (list "** " desc "\n" snd-subs third-lvl) "")))
     articles
     "\n")))

(defun mw-learner--get-title (entry)
  "Title for ENTRY."
  (cdr (car (car (cdr entry))))
  )

(defun mw-learner--get-type (entry)
  "Type of the ENTRY is at <fl> tag."
  (-> (mw-learner--get-xml-node entry '(fl))
      car (seq-drop 2) car))

(defun mw-learner--parse (xml-data)
  "Parse xml returned by Merriam-Webster dictionary API.

Take XML-DATA, Returns multi-line text in ‘org-mode’ format."
  (let* ((entry-list (assq 'entry_list xml-data))
         (entries (xml-get-children entry-list 'entry)))
    (mapconcat
     (lambda (entry)
       (let ((fst-level (concat "* " (mw-learner--get-title entry)
                                " ~" (mw-learner--get-type entry) "~\n"))
             (snd-level (mw-learner--snd-level entry '(def)))
	     ;; (etymology (concat "** Etymology\n" (mw-learner--italicize (mw-learner--get-xml-node entry '(et)))))
	     )
         (string-join (list fst-level snd-level) "")))
     entries "\n")))

(defun mw-learner--create-buffer (word data)
  "Build mw-learner buffer for WORD and the relevant DATA from Merriam-Webster API."
  (let ((dict-str (mw-learner--parse data)))
    (if (< (length dict-str) 1)
        (message (concat "Sadly, Merriam-Webster doesn't seem to have anything for " word))
      (let ((temp-buf (get-buffer-create mw-learner-buffer-name)))
        ;; (print temp-buf)
        (when (not (bound-and-true-p mw-learner-mode))
          (switch-to-buffer-other-window temp-buf))
        (set-buffer temp-buf)
        (with-current-buffer temp-buf
          (read-only-mode -1)
          (setf (buffer-string) "")
          (setf org-hide-emphasis-markers t)
          (funcall 'org-mode)
          (funcall 'mw-learner-mode)
          (insert (decode-coding-string dict-str 'dos))
          (goto-char (point-min))
          (read-only-mode))))))

(defun mw-learner-get-original-word (beginning end)
  "Get a word to look for from the user.
`BEGINNING' and `END' correspond to the selected text (if selected).
If presented, the selected text will be used.
Otherwise, user must provide additional information."
  (if (use-region-p)
      (buffer-substring-no-properties beginning end)
    (read-string "Word to look up: ")))

(defun mw-learner-is-at-the-beginning-of-word (word-point)
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
(defun mw-learner-lookup-dwim ()
  "Look up a learner definition on demand using Merriam-Webster online dictionary.
If a region is selected use mw-learner-lookup-word
if a thing at point is not empty use mw-learner-lookup-word-at-point
otherwise as for word using mw-learner-lookup-word"
  (interactive)
  (let (beg end)
    (if (use-region-p)
        (progn
          (setq beg (region-beginning)
                end (region-end))
          (mw-learner-lookup beg end))
      (if (thing-at-point 'word)
          (mw-learner-lookup-at-point (point))
        (mw-learner-lookup)))))

;;;###autoload
(defun mw-learner-lookup-at-point (word-point)
  "Look up a learner definition for word at point using Merriam-Webster online dictionary."
  (interactive (list (point)))
  (save-mark-and-excursion
    (unless (mw-learner-is-at-the-beginning-of-word word-point)
      (backward-word))
    (set-mark (point))
    (forward-word)
    (activate-mark)
    (mw-learner-lookup (region-beginning) (region-end))))

;;;###autoload
(defun mw-learner-lookup (&optional beginning end)
  "Look up a learner definition for word using Merriam-Webster online dictionary.
`BEGINNING' and `END' correspond to the selected text with a word to look up.
If there is no selection provided, additional input will be required."
  (interactive
   ;; it is a simple interactive function instead of interactive "r"
   ;; because it doesn't produce an error in a buffer without a mark
   (if (use-region-p) (list (region-beginning) (region-end))
     (list nil nil)))
  (let* ((word (mw-learner-get-original-word beginning end))
         (url (concat (symbol-value 'mw-learner--base-url)
                      word "?key="
                      (symbol-value 'mw-learner--api-key))))
    (request url
             :parser (lambda () (xml-parse-region (point-min) (point-max)))
             :success (cl-function
                       (lambda (&key data word &allow-other-keys)
                         (mw-learner--create-buffer word data))))))

;;;###autoload
(defun mw-learner-lookup-string (word)
  "Look up a learner definition for word using Merriam-Webster online dictionary.
`BEGINNING' and `END' correspond to the selected text with a word to look up.
If there is no selection provided, additional input will be required."'
  (interactive)
  (let* ((url (concat (symbol-value 'mw-learner--base-url)
                      word "?key="
                      (symbol-value 'mw-learner--api-key)))
	 (word word))
    (request url
             :parser (lambda () (xml-parse-region (point-min) (point-max)))
             :success (cl-function
                       (lambda (&key data word &allow-other-keys)
                         (mw-learner--create-buffer word data))))))

(defun mw-learner--quit ()
  "Kill Merriam-Webster Learner buffer."
  (interactive)
  (when-let* ((buffer (get-buffer mw-learner-buffer-name)))
    (quit-window)
    (kill-buffer buffer)))

(provide 'mw-learner)

;;; mw-learner.el ends here
