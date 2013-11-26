;; buffer-buttons.el : Save/load buttons with buffers, code-friendly

;; buffer-buttons.el
;; Copyright (C) 2014  Ryan Pavlik
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defvar *buffer-buttons-definitions* (make-hash-table))

(defmacro define-buffer-button (args &rest body)
  (destructuring-bind (name arg-name &key (label "Button") (prefix " ") (help-text name) suffix)
      args
    (let ((function-name (intern (concat "buffer-button-fn-" (symbol-name name)))))
      `(progn
         (defun ,function-name (,arg-name) ,@body)
         (puthash ',name
                  `((name . ,',name)
                    (function . ,',function-name)
                    (label . ,',label)
                    (help-text . ,',help-text)
                    (prefix . ,',prefix)
                    (suffix . ,',suffix))
                  *buffer-buttons-definitions*)))))

(defun buffer-button-string (button-spec)
  (concat (cdr (assoc 'prefix button-spec))
          "#button:" (symbol-name (cdr (assoc 'name button-spec)))
          (cdr (assoc 'suffix button-spec))))

(defun buffer-button-modified (overlay after-p beg end &optional len)
  (when after-p
    (save-excursion
      (delete-region (overlay-start overlay)
                     (overlay-end overlay))
      (delete-overlay overlay))))

(defun make-buffer-button-on-region (button-spec beg end)
  (make-button beg end 'action (cdr (assoc 'function button-spec))
               'follow-link t
               'display (cdr (assoc 'label button-spec))
               'evaporate t
               'modification-hooks '(buffer-button-modified)
               'help-echo (cdr (assoc 'help-text button-spec))
               'field 'buffer-button))

(defun insert-buffer-button (name)
  (interactive
   (list
    (completing-read "Buffer Button name to insert: "
                     (let (list)
                       (maphash (lambda (k v) (push (symbol-name k) list))
                                *buffer-buttons-definitions*)
                       list)
                     nil t)))
  (let ((button-spec (gethash (intern name) *buffer-buttons-definitions*)))
    (if (not button-spec)
        (message "I don't know about buffer buttons called \"%s\"" name)
      (let ((beg (point)))
        (insert-string (buffer-button-string button-spec))
        (make-buffer-button-on-region button-spec beg (point))))))

(defun buffer-instance-button-in-buffer (name button-spec)
  (let ((tag (buffer-button-string button-spec)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward tag nil t)
        (make-buffer-button-on-region button-spec
                                      (- (point) (length tag))
                                      (point))))))

(defun buffer-button-find-file-hook ()
  (maphash 'buffer-instance-button-in-buffer
           *buffer-buttons-definitions*))

(add-hook 'find-file-hook 'buffer-button-find-file-hook)

 ;; Example Definitions

(define-buffer-button (elisp-eval b
                                  :label "<- Eval"
                                  :prefix ";;")
  (save-excursion
    (goto-char (button-start b))
    (eval-last-sexp nil)))

(define-buffer-button (slime-eval b
                                  :label "<- Eval"
                                  :prefix ";;")
  (save-excursion
    (goto-char (button-start b))
    (slime-eval-last-expression)))

(define-buffer-button (slime-connect b
                                     :label "Slime Connect"
                                     :prefix ";;")
  (slime-connect "127.0.0.1" 4005))
