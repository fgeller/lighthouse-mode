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
  "Lighthouse account. (e.g. foo: foo.lighthouseapp."
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
  (car (xml-get-children proj name)))

(defun lighthouse-display-project-names ()
  (interactive)
  (let* ((url-request-method "GET")
         (url-request-extra-headers `(("X-LighthouseToken" . ,lighthouse-api-token)))
         (lighthouse-url (format "http://%s.lighthouseapp.com" lighthouse-account))
         (lighthouse-projects-url (format "%s/projects.xml" lighthouse-url)))
    (url-retrieve
     lighthouse-projects-url
     (lambda (status)
       (re-search-forward "<?xml version")
       (beginning-of-line)
       (let ((projects (car (xml-parse-region (point) (point-max))))
             (names ()))
         (dolist (proj (xml-get-children projects 'project))
           (push (xml-get-content (xml-first-child-named proj 'name)) names))
         (message "Projects: %s" names))))))

(provide 'lighthouse-mode)
