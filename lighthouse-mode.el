;; Copyright (C) 2011 by Felix Geller
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(require 'url)
(require 'cl)
(require 'widget)
(eval-when-compile
  (require 'wid-edit))

;;;;;;;;;;;;;;;;;;;
;; Customization ;;
;;;;;;;;;;;;;;;;;;;

(defgroup lighthouse nil
  "Lighthouse customization group."
  :group 'application)

(defcustom lighthouse-account ""
  "Lighthouse account. (e.g. foo: foo.lighthouseapp.com"
  :group 'lighthouse
  :type 'string)

(defcustom lighthouse-api-token ""
  "Your API token for REST requests."
  :group 'lighthouse
  :type 'string)

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(defun xml-get-content (node)
  (caddr node))

(defun xml-first-child-named (parentnode name)
  (car (xml-get-children parentnode name)))

(defun assocv (sym alist)
  (cdr (assoc sym alist)))

(defun insert-new-line ()
  (insert "\n"))

(defun kill-buffer-if-exists (buffer)
  (if (get-buffer buffer)
      (kill-buffer buffer)))

;; taken from notmuch-hello.el
(defun trim-whitespace (search)
  "Trim whitespace."
  (if (string-match "^[[:space:]]*\\(.*[^[:space:]]\\)[[:space:]]*$" search)
      (match-string 1 search)
    search))

;;;;;;;;;;;;;;;;;;;;;
;; Lighthouse mode ;;
;;;;;;;;;;;;;;;;;;;;;

(defvar lighthouse-projects nil)
(defvar lighthouse-ticket-list-markers (make-hash-table))

;;;###autoload
(defun lighthouse-mode ()
  "Major mode for managing Lighthouse projects."
  (interactive)
  (let ((projects-buffer-name "*Lighthouse[Projects]*"))
    (kill-buffer-if-exists projects-buffer-name)
    (switch-to-buffer projects-buffer-name)
    (kill-all-local-variables)
    (setq major-mode 'lighthouse-mode
          mode-name "Lighthouse")
    (setq show-trailing-whitespace nil)
    (run-hooks 'lighthouse-mode-hook)
    (lighthouse-load-projects)
    (lighthouse-populate-projects-buffer)))

(defun lighthouse-populate-projects-buffer ()
  (dolist (project-info lighthouse-projects)
    (let* ((project-name (assocv 'name project-info))
           (beginning-point (point))
           (button-callback `(lambda (&rest ignore) (lighthouse-goto-project-buffer ',project-info))))
      (insert-text-button project-name
                          'action button-callback
                          'help-echo "RET: open project buffer")
      (insert "\n"))))


(defun lighthouse-goto-project-buffer (project-info)
  (let ((project-buffer-name (format "*Lighthouse[%s]*" (assocv 'name project-info))))
    (kill-buffer-if-exists project-buffer-name)
    (switch-to-buffer project-buffer-name)
    (setq show-trailing-whitespace nil)
    (lighthouse-populate-project-buffer project-info)))


(defun lighthouse-populate-project-buffer (project-info)
  (let* ((project-id (assocv 'id project-info)))

    (widget-insert "Search: ")
    (widget-create 'editable-field
                   :size 42
                   :action `(lambda (widget &rest ignore)
                              (lighthouse-project-search-tickets
                               ',project-info
                               (trim-whitespace (widget-value widget)))))

    (widget-insert "\n")
    (puthash project-id (point-marker) lighthouse-ticket-list-markers)

    (widget-setup)))

(defun lighthouse-project-search-tickets (project-info query)
  (let* ((project-id (assocv 'id project-info))
         (first-page-path  (format "/projects/%s/tickets.xml?&q=%s" project-id query))
         (tickets (lighthouse-get-request first-page-path)))
    (goto-char (gethash project-id lighthouse-ticket-list-markers))
    (dolist (ticket (xml-get-children tickets 'ticket))
      (lighthouse-project-insert-ticket ticket))))

(defun lighthouse-project-insert-ticket (ticket)
  (let* ((ticket-title (xml-get-content (xml-first-child-named ticket 'title)))
         (ticket-number (xml-get-content (xml-first-child-named ticket 'number))))
    (widget-insert (format "#%s %s\n" ticket-number ticket-title))))


(defun lighthouse-load-projects ()
  (setq lighthouse-projects nil)
  (let* ((xml-projects (lighthouse-get-request "/projects.xml")))
    (dolist (xml-project (xml-get-children xml-projects 'project))
      (let ((project-name (xml-get-content (xml-first-child-named xml-project 'name)))
            (project-id (xml-get-content (xml-first-child-named xml-project 'id))))
        (push `((id . ,project-id) (name . ,project-name)) lighthouse-projects)))))


(defun lighthouse-get-request (path)
  (message "Looking for path %s" path)
  (let* ((url-request-method "GET")
         (url-request-extra-headers `(("X-LighthouseToken" . ,lighthouse-api-token)))
         (lighthouse-url (format "http://%s.lighthouseapp.com" lighthouse-account))
         (lighthouse-projects-url (format "%s%s" lighthouse-url path))
         (response-buffer (url-retrieve-synchronously lighthouse-projects-url)))
    (with-current-buffer response-buffer
      (goto-char (point-min))
      (re-search-forward "<?xml version=")
      (beginning-of-line)
      (car (xml-parse-region (point) (point-max))))))


(provide 'lighthouse-mode)
