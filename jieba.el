;;; jieba.el --- jieba binding of Emacs Lisp           -*- lexical-binding: t; -*-

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

(require 'jieba-module)

(defvar jieba-dict "/usr/local/etc/dict.utf8.xdb" "XDB 词典.")
(defvar jieba-rule "/usr/local/etc/rules.utf8.ini" "rules.ini 规则集.")

(defvar jieba--object nil)

;;;###autoload
(defun jieba (string)
  "对 STRING 进行分词操作，返回一个 String 列表."
  (unless jieba--object
    (unless (setq jieba--object (jieba-module-new jieba-dict jieba-rule))
      (error "jieba-module-new failed")))
  (jieba-module-send-text jieba--object string))

;;;###autoload
(defun jieba-word-at-point ()
  "Return Chinese word at point."
  (interactive)                         ; Mainly for testing
  (let* ((string (thing-at-point 'word))
         (bounds (bounds-of-thing-at-point 'word))
         (offset (and bounds (- (point) (car bounds))))
         (length 0))
    (when string
      (catch 'found
        (dolist (word (jieba string))
          (setq length (+ length (length word)))
          (when (> length offset)
            (when (called-interactively-p 'any)
              (message "%s" word))
            (throw 'found word)))))))

(provide 'jieba)
;;; jieba.el ends here
