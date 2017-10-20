;;; scws.el --- SCWS binding of Emacs Lisp           -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Keywords: Chinese

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'scws-module)

(defvar scws-dict "/usr/local/etc/dict.utf8.xdb" "XDB 词典.")
(defvar scws-rule "/usr/local/etc/rules.utf8.ini" "rules.ini 规则集.")

(defvar scws--object nil)

;;;###autoload
(defun scws (string)
  "对 STRING 进行分词操作，返回一个 String 列表."
  (unless scws--object
    (unless (setq scws--object (scws-module-new scws-dict scws-rule))
      (error "scws-module-new failed")))
  (scws-module-send-text scws--object string))

;;;###autoload
(defun scws-word-at-point ()
  "Return Chinese word at point."
  (interactive)                         ; Mainly for testing
  (let* ((string (thing-at-point 'word))
         (bounds (bounds-of-thing-at-point 'word))
         (offset (and bounds (- (point) (car bounds))))
         (length 0))
    (when string
      (catch 'found
        (dolist (word (scws string))
          (setq length (+ length (length word)))
          (when (> length offset)
            (when (called-interactively-p 'any)
              (message "%s" word))
            (throw 'found word)))))))

(provide 'scws)
;;; scws.el ends here
