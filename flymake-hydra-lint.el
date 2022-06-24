;;; flymake-hydra-lint.el --- Flymake backend for linting hydras -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/flymake-hydra-lint
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flymake backend for linting hydras

;;; Code:


(eval-when-compile
  (require 'cl-lib))

(require 'flymake)

(declare-function flymake-diag-region "flymake")
(declare-function flymake-make-diagnostic "flymake")

(defun flymake-hydra-lint-re-search-backward-inner (regexp &optional
                                                           bound count)
  "This function is helper for `flymake-hydra-lint-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun flymake-hydra-lint-re-search-forward-inner (regexp &optional bound count)
  "This function is helper for `flymake-hydra-lint-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun flymake-hydra-lint-re-search-forward (regexp &optional
                                                    bound noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'flymake-hydra-lint-re-search-backward-inner)
               ((> count 0) #'flymake-hydra-lint-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

(defun flymake-hydra-lint-re-search-backward (regexp &optional
                                                     bound noerror count)
  "Search backward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (flymake-hydra-lint-re-search-forward regexp bound noerror
                                        (if count
                                            (- count) -1)))

(defun flymake-hydra-lint-get-hydra-dubs (heads)
  "Return hydra HEADS with dublicated keys."
  (let ((dups)
        (head)
        (items))
    (while (setq head (pop heads))
      (let ((key (car head)))
        (when (member key items)
          (push head dups))
        (push key items)))
    dups))

(defun flymake-hydra-lint-find-dublicate-keys ()
  "Check current buffer for dublicate keys in hydras and return list of links."
  (save-excursion
    (goto-char (point-max))
    (let ((problems))
      (while (flymake-hydra-lint-re-search-backward
              "\\_<\\(defhydra\\|pretty-hydra-define\\)\\_>"
              nil t 1)
        (when-let* ((sexp (progn (ignore-errors
                                   (backward-up-list 1))
                                 (sexp-at-point)))
                    (bounds (when (listp sexp)
                              (bounds-of-thing-at-point 'sexp)))
                    (type (and (car sexp)
                               (memq (car sexp) '(defhydra pretty-hydra-define))
                               (car sexp)))
                    (symb (and (when (symbolp (nth 1 sexp))
                                 (nth 1 sexp))))
                    (heads (seq-drop sexp 3)))
          (when (stringp (car heads))
            (setq heads (seq-drop heads 1)))
          (when (eq type 'pretty-hydra-define)
            (setq heads (apply 'append (seq-filter 'listp (car heads)))))
          (let ((dubs))
            (setq heads (flymake-hydra-lint-get-hydra-dubs
                         (seq-filter 'listp heads)))
            (setq dubs (mapcar
                        (lambda (head)
                          (save-excursion
                            (let ((re (concat "\\_<\\("
                                              (regexp-quote (car head))
                                              "\\)\\_>")))
                              (print re)
                              (when (re-search-forward re (cdr bounds) t 1)
                                (let ((col (current-column))
                                      (line (line-number-at-pos)))
                                  (setq col (if (>= col (length (car head)))
                                                (- col (length (car head)))
                                              col))
                                  `(,line ,col error "Hydra key dublicate"))))))
                        heads))
            (print dubs)
            (setq problems (nconc problems dubs)))))
      problems)))

(defun flymake-hydra-lint-keys (report-fn &rest _args)
  "A Flymake backend for checking dublicate keys in hydra.
Use `flymake-hydra-lint' to add this to `flymake-diagnostic-functions'.
Calls REPORT-FN directly."
  (let ((collection (flymake-hydra-lint-find-dublicate-keys)))
    (cl-loop for (line col type message) in
             collection
             for (beg . end) = (flymake-diag-region (current-buffer) line col)
             collect
             (flymake-make-diagnostic
              (current-buffer)
              beg end
              (if (eq type 'warning) :warning :error)
              message)
             into diags
             finally (funcall report-fn diags)))
  (funcall report-fn nil))

;;;###autoload
(defun flymake-hydra-lint ()
  "Add hydra linters to `flymake-diagnostic-functions' and run flymake."
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'flymake-hydra-lint-keys nil t)
  (flymake-mode))

(provide 'flymake-hydra-lint)
;;; flymake-hydra-lint.el ends here