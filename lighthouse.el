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

;;;;;;;;;;;;;;;;;
;; XML helpers ;;
;;;;;;;;;;;;;;;;;

(defun xml-get-content (node)
  (caddr node))
(defun xml-first-child-named (parentnode name)
  (car (xml-get-children parentnode name)))


;;;;;;;;;;;;;;;;;;;;;
;; Lighthouse mode ;;
;;;;;;;;;;;;;;;;;;;;;

(defvar lighthouse-projects nil)


;;;###autoload
(defun lighthouse-mode ()
  "Major mode for managing Lighthouse projects."
  (interactive)
  (switch-to-buffer "*Lighthouse*")
  (kill-all-local-variables)
  (setq major-mode 'lighthouse-mode
        mode-name "Lighthouse")
  (run-hooks 'lighthouse-mode-hook)
  (lighthouse-load-projects)
  (lighthouse-create-main-buffer))


(defun lighthouse-create-main-buffer ()
  (insert "Projects\n\n")
  (dolist (project lighthouse-projects)
    (insert (format "%s\n" (cdr (assoc 'name project))))))


(defun lighthouse-load-projects ()
  (setq lighthouse-projects nil)
  (let* ((xml-projects (lighthouse-get-request "/projects.xml")))
    (dolist (xml-project (xml-get-children xml-projects 'project))
      (let ((project-name (xml-get-content (xml-first-child-named xml-project 'name)))
            (project-id (xml-get-content (xml-first-child-named xml-project 'id))))
        (push `((id . ,project-id) (name . ,project-name)) lighthouse-projects)))))


(defun lighthouse-get-request (path)
  (let* ((url-request-method "GET")
         (url-request-extra-headers `(("X-LighthouseToken" . ,lighthouse-api-token)))
         (lighthouse-url (format "http://%s.lighthouseapp.com" lighthouse-account))
         (lighthouse-projects-url (format "%s%s" lighthouse-url "/projects.xml"))
         (response-buffer (url-retrieve-synchronously lighthouse-projects-url)))
    (with-current-buffer response-buffer
      (re-search-backward "<?xml version=")
      (beginning-of-line)
      (car (xml-parse-region (point) (point-max))))))


(provide 'lighthouse-mode)
