;;; paperless.el --- A major mode for sorting and filing PDF documents.

;; Copyright (c) 2017 Anthony Green

;; Author: Anthony Green <green@moxielogic.com>
;; URL: http://github.com/atgreen/paperless
;; Version: 1.1
;; Keywords: pdf, convenience
;; Package-Requires: ((emacs "24.4") (f "0.19.0") (s "1.10.0") (cl-lib "0.6.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

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

;;; Code:

(require 'f)
(require 's)
(require 'cl-lib)
(require 'doc-view)

(defgroup paperless nil
  "A group for paperless customtizations."
  :group 'applications
  :prefix "paperless-"
  )

(defcustom paperless-capture-directory nil
  "The directory in which paperless will look for PDF documents to file."
  :type '(directory)
  :group 'paperless)

(defcustom paperless-root-directory nil
  "The root of a directory hierarchy in which to file documents."
  :type '(directory)
  :group 'paperless)

(defvar paperless--table-contents)
(defvar paperless--directory-list)

;;;###autoload
(defun paperless ()
  "File directory contents."
  (interactive)
  (if (null paperless-capture-directory)
      (error "Set paperless-capture-directory with M-x customize-variable"))
  (if (null paperless-root-directory)
      (error "Set paperless-root-directory with M-x customize-variable"))
  (setq paperless--table-contents
	(mapcar
	 (lambda (i)
	   (list i (vector "" (file-name-nondirectory i) "")))
	 (directory-files paperless-capture-directory t ".*pdf")))
  (pop-to-buffer (concat "*Paperless* - " paperless-capture-directory))
  ;; Recursively build the list of destination directories, but don't
  ;; include hidden directories.
  (setq paperless--directory-list
	(cl-remove-if
	 (lambda (s)
	   (s-contains? "/." s))
	 (f-directories paperless-root-directory nil t)))
  (paperless-mode)
  (tabulated-list-print t))

(defun paperless-display ()
  "Open a preview display for the current document."
  (interactive)
  (save-selected-window
    (let ((filename (tabulated-list-get-id)))
      (switch-to-buffer-other-window "*Paperless Preview*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-file-contents filename)
      (if (fboundp 'pdf-view-mode)
	  (pdf-view-mode)
	(doc-view-mode))))
  (mapc
   (lambda (i)
     (setf (elt (cadr i) 0) ""))
   paperless--table-contents)
  (setf (elt (cadr (assoc (tabulated-list-get-id) paperless--table-contents)) 0) "*")
  (tabulated-list-print t))

(defun paperless-file ()
  "Select the directory in which to file the current document."
  (interactive)
  (let ((new-dir (ido-completing-read "File destination: " (paperless--dirtree)))
	(vctr (cadr (assoc (tabulated-list-get-id) paperless--table-contents))))
    (setf (elt vctr 2) new-dir))
  (tabulated-list-print t))

(defun paperless-rename ()
  "Rename the current document."
  (interactive)
  (let ((new-name (completing-read "New name: " nil))
	(vctr (cadr (assoc (tabulated-list-get-id) paperless--table-contents))))
    (setf (elt vctr 1) (if (file-name-extension new-name)
			   new-name
			 (concat new-name ".pdf"))))
  (tabulated-list-print t))

(defun paperless-execute ()
  "Batch execute all pending document processing."
  (interactive)
  (let ((delete-list
	 (mapcar
	  (lambda (i)
	    (let ((vctr (cadr i)))
	      (if (= (length (elt vctr 2)) 0)
		  nil
		(progn
		  (rename-file (car i) (concat (elt vctr 2) "/" (elt vctr 1)))
		  (car i)))))
	  paperless--table-contents)))
    (mapc
     (lambda (i)
       (if (not (null i))
	   (setq paperless--table-contents (assq-delete-all i paperless--table-contents))))
     delete-list)
    (tabulated-list-print t)))

(defun paperless--table-entries ()
  "Make the entry table for the list."
  paperless--table-contents)

;; Wrappers around doc-view commands...
(defun paperless-doc-view-enlarge (factor)
  "Enlarge the document by FACTOR."
  (interactive (list doc-view-shrink-factor))
  (save-selected-window
    (switch-to-buffer-other-window "*Paperless Preview*")
    (if (fboundp 'pdf-view-enlarge)
	(pdf-view-enlarge factor)
      (doc-view-enlarge factor))))

(defun paperless-doc-view-shrink (factor)
  "Shrink the document."
  (interactive (list doc-view-shrink-factor))
  (save-selected-window
    (switch-to-buffer-other-window "*Paperless Preview*")
    (if (fboundp 'pdf-view-shrink)
	(pdf-view-shrink factor)
      (doc-view-shrink factor))))

(defun paperless-doc-view-scale-reset ()
  "Reset the document size/zoom level to the initial one."
  (interactive)
  (save-selected-window
    (switch-to-buffer-other-window "*Paperless Preview*")
    (if (fboundp 'pdf-view-scale-reset)
	(pdf-view-scale-reset)
      (doc-view-scale-reset))))

(define-derived-mode paperless-mode tabulated-list-mode "Paperless Filing"
  "Major mode for filing a list of PDF documents."
  (setq tabulated-list-format [(" " 1 nil)("Document" 30 nil)("Destination" 20 nil)])
  (setq tabulated-list-entries 'paperless--table-entries)
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(setq paperless-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "SPC") 'paperless-display)
	(define-key map "f" 'paperless-file)
	(define-key map "r" 'paperless-rename)
	(define-key map "x" 'paperless-execute)
	
	;; Zoom in/out.
	(define-key map "+" 'paperless-doc-view-enlarge)
	(define-key map "=" 'paperless-doc-view-enlarge)
	(define-key map "-" 'paperless-doc-view-shrink)
	(define-key map "0" 'paperless-doc-view-scale-reset)

	map))

(defun paperless--dirtree ()
  "Return the cached list of target directories."
  paperless--directory-list)

(provide 'paperless)

;;; paperless.el ends here
