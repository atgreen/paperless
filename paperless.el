; paperless.el -- Copyright (c) 2017 Anthony Green

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(require 'f)
(require 's)
(require 'cl-lib)

;;;###autoload
(defun paperless ()
  "File directory contents."
  (interactive)
  (setq paperless/table-contents
	(mapcar
	 (lambda (i)
	   (list i (vector "" (file-name-nondirectory i) "")))
	 (directory-files *paperless-capture-dir* t ".*pdf")))
  (pop-to-buffer (concat "*Paperless* - " *paperless-capture-dir*))
  ;; Recursively build the list of destination directories, but don't
  ;; include hidden directories.
  (setq paperless/directory-list
	(cl-remove-if
	 (lambda (s)
	   (s-contains? "/." s))
	 (f-directories *paperless-root-dir* nil t)))
  (paperless-mode) 
  (tabulated-list-print t))

(defun paperless-display ()
  (interactive)
  (save-selected-window
    (let ((filename (tabulated-list-get-id)))
      (switch-to-buffer-other-window "*Paperless Preview*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-file filename)
      (doc-view-mode)))
  (mapcar
   (lambda (i)
     (setf (elt (cadr i) 0) ""))
   paperless/table-contents)
  (setf (elt (cadr (assoc (tabulated-list-get-id) paperless/table-contents)) 0) "*")
  (tabulated-list-print t))

(defun paperless-file ()
  (interactive)
  (let ((new-dir (ido-completing-read "File destination: " (paperless/dirtree)))
	(vctr (cadr (assoc (tabulated-list-get-id) paperless/table-contents))))
    (setf (elt vctr 2) new-dir))
  (tabulated-list-print t))

(defun paperless-rename ()
  (interactive)
  (let ((new-name (completing-read "New name: " nil))
	(vctr (cadr (assoc (tabulated-list-get-id) paperless/table-contents))))
    (setf (elt vctr 1) (if (file-name-extension new-name)
			   new-name
			 (concat new-name ".pdf"))))
  (tabulated-list-print t))

(defun paperless-execute ()
  (interactive)
  (let ((delete-list
	 (mapcar
	  (lambda (i)
	    (let ((vctr (cadr i)))
	      (if (= (length (elt vctr 2)) 0)
		  nil
		(progn
		  (rename-file (car i) (concat *paperless-root* "/" (elt vctr 2) "/" (elt vctr 1)))
		  (car i)))))
	  paperless/table-contents)))
    (mapcar
     (lambda (i)
       (if (not (null i))
	   (setq paperless/table-contents (assq-delete-all i paperless/table-contents))))
     delete-list)
    (tabulated-list-print t)))

(defun paperless/table-entries ()
  "Make the entry table for the list."
  paperless/table-contents)

(define-derived-mode paperless-mode tabulated-list-mode "Paperless Filing"
  "Major mode for filing a list of PDF documents."
  (setq tabulated-list-format [(" " 1 nil)("Document" 30 nil)("Destination" 20 nil)])
  (setq tabulated-list-entries 'paperless/table-entries)
  (setq tabulated-list-padding 2)
  (setq paperless-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "SPC") 'paperless-display)
	  (define-key map "f" 'paperless-file)
	  (define-key map "r" 'paperless-rename)
	  (define-key map "x" 'paperless-execute)
	  map))
  (tabulated-list-init-header))

(defun paperless/dirtree ()
  paperless/directory-list)

(provide 'paperless)
