;;; rest --- Useful functions for REST API calls -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Coque Couto

;; Author: Coque Couto <coque.couto at gmail.com>
;; Version: 0.3
;; Package-Requires: ((emacs "24.4")
;;                    (json)
;;                    (json-mode)
;;                    (map)
;;                    (request))
;; Keywords: comm lisp tools
;; URL: https://github.com/coquec/emacs-rest

;; rest is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; rest is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; rest.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; rest contains some simple functions to write elisp code that interacts with
;; REST API endpoints.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'json-mode)
(require 'map)
(require 'request)


;; Main functions.

;;;###autoload
(cl-defun rest-call (&rest r
                           &key
                           endpoint
                           dry-run
                           (accept "*/*")
                           auth-header
                           content-type
                           (success #'rest-raw-to-buffer)
                           (error #'rest-show-error)
                           &allow-other-keys)
  "Call a REST API endpoint using `request'.

When the DRY-RUN parameter is nil, make a REST API call to the ENDPOINT
URL, including ACCEPT, AUTH-HEADER, and CONTENT-TYPE as headers.

The callback functions SUCCESS and ERROR, along with the rest of allowed
parameters, are described in the `request' documentation.

Return a list of the parameters used to call it, except DRY-RUN.  These
return values can be used to make parallel calls with
`rest-multiple-calls'."
  ;; There can be a headers parameter in r.  We merge it with the headers
  ;; passed to us as parameters.
  (let ((merged-headers (rest--replace-values
                         (map-elt r :headers)
                         `(("accept" . ,accept)
                           ("Content-Type" . ,content-type)
                           (,(car auth-header) . ,(cdr auth-header))))))
    ;; Parameters' default values are not present in r.  We add them here.
    (setq r (rest--replace-values r `((:accept . ,accept)
                                      (:success . ,success)
                                      (:error . ,error))))
    (let ((params-to-request
           `(,endpoint . ,(rest--remove-values
                           (rest--replace-value r :headers merged-headers)
                           :endpoint))))
      (unless dry-run
        (apply #'request params-to-request))
      (rest--remove-values r :dry-run))))

;;;###autoload
(cl-defun rest-multiple-calls (&key
                               parameters
                               dry-run
                               (success #'rest-multiple-raw-to-buffer)
                               (error #'rest-multiple-show-error)
                               type
                               accept
                               auth-header
                               content-type)
  "Call several REST API endpoints in parallel using `rest-call'.

When the DRY-RUN parameter is nil, make multiple parallel REST API calls
using `rest-call' with the parameters stored in each element of the
PARAMETERS plist, or the default parameters of `rest-call' for the
missing ones.  Gather the results of individual calls in a list with
elements being cons (SYMBOL-STATUS . DATA).  Pass this list as parameter
to SUCCESS if all the calls are successful, or to ERROR if any of them
fails.  Entries in this list do not follow the same order as elements in
the PARAMETERS plist.

For each element in the PARAMETERS plist, :endpoint is required, and
:dry-run, :sync, :success, :error, and :complete are ignored.

Any of the parameters TYPE, ACCEPT, AUTH-HEADER, and CONTENT-TYPE are
used for entries in the PARAMETERS list that don't specify them.

Both SUCCESS and ERROR must accept the results list as parameter.  See
`rest-multiple-raw-to-buffer' as an example.

The DRY-RUN is passed to `rest-call', so no API calls are made if this
parameter is non-nil.

Return a list with the lists of parameters used to call `rest-call',
except :success, :error, and :complete."
  (let ((counter (length parameters))
        (error-p nil)
        (api-results nil)
        (result nil))
    (dolist (entry parameters)
      (push
       (rest--remove-values
        (apply #'rest-call
               :dry-run dry-run
               :sync nil
               :success nil
               :error nil
               :complete (cl-function
                          (lambda (&key
                                   data
                                   symbol-status
                                   &allow-other-keys)
                            (push `(,symbol-status . ,data) api-results)
                            (setq error-p (or error-p
                                              (not (eq symbol-status
                                                       'success))))
                            (when (= 0 (cl-decf counter))
                              (if error-p
                                  (funcall error :results api-results)
                                (funcall success :results api-results)))))
               (rest--replace-values
                (rest--remove-values entry :sync :success :error)
                `((:type . ,type)
                  (:accept . ,accept)
                  (:auth-header . ,auth-header)
                  (:content-type . ,content-type))))
        :sync :success :error :complete)
       result))
    result))


;; Support functions useful for the users of the package.

(cl-defun rest-raw-to-buffer (&key
                              data
                              (buffer-name "*raw-results*")
                              (read-only-p t)
                              &allow-other-keys)
  "Delete the contents of the buffer BUFFER-NAME, paste DATA into it, and
leave it unformatted."
  (with-current-buffer (get-buffer-create buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (insert data)
    (read-only-mode read-only-p)
    (display-buffer (current-buffer))))

(cl-defun rest-json-to-buffer (&key
                               data
                               (buffer-name "*json-results*")
                               (read-only-p t)
                               &allow-other-keys)
  "Delete the contents of the buffer BUFFER-NAME, paste DATA into it, and
format it as JSON."
  (with-current-buffer (get-buffer-create buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (insert data)
    (json-mode-beautify (point-min) (point-max))
    (json-mode)
    (read-only-mode read-only-p)
    (display-buffer (current-buffer))))

(cl-defun rest-multiple-results-to-buffer (&key
                                           results
                                           format-buffer
                                           buffer-name-pattern
                                           (read-only-p t)
                                           &allow-other-keys)
  "Call FORMAT-BUFFER for all the entries in RESULTS.

The BUFFER-NAME-PATTERN is a string which is used to format each buffer
name.  It is used along with a counter for each of the entries in
RESULTS."
  (let ((count 1))
    (dolist (elem results)
      (funcall format-buffer
               :data (cdr elem)
               :read-only-p read-only-p
               :buffer-name (format buffer-name-pattern count))
      (cl-incf count))))

(cl-defun rest-multiple-raw-to-buffer (&key
                                       results
                                       (buffer-name-pattern
                                        "*raw-results-%03d*")
                                       (read-only-p t)
                                       &allow-other-keys)
  "Call `rest-raw-to-buffer' for all the entries in RESULTS, using
`rest-multiple-resuts-to-buffer'."
  (rest-multiple-results-to-buffer :results results
                                   :format-buffer #'rest-raw-to-buffer
                                   :buffer-name-pattern buffer-name-pattern
                                   :read-only-p read-only-p))

(cl-defun rest-multiple-json-to-buffer (&key
                                        results
                                        (buffer-name-pattern
                                         "*json-results-%03d*")
                                        (read-only-p t)
                                        &allow-other-keys)
  "Call `rest-json-to-buffer' for all the entries in RESULTS, using
`rest-multiple-resuts-to-buffer'."
  (rest-multiple-results-to-buffer :results results
                                   :format-buffer #'rest-json-to-buffer
                                   :buffer-name-pattern buffer-name-pattern
                                   :read-only-p read-only-p))

(cl-defun rest-multiple-show-error (results &allow-other-keys)
  "Display an error message when a multiple call fails."
  ;; TODO: provide useful information about the errors.
  (message "Some of the multiple calls failed."))

(cl-defun rest-show-error (&key error-thrown &allow-other-keys)
  "Write a message with the ERROR-THROWN."
  (message "Error: %S" error-thrown))

;;;###autoload
(cl-defun rest-auth-header (&key api-key (type nil))
  "Generate a header to be used with `rest-call' for authenticated calls.

API-KEY is a string with the API key or the JWT token.  Return nil it is
nil.

TYPE may be \\='jwt for JWT authentication, which will produce an
\\='Authentication: Bearer API-KEY\\=' header.

Any other TYPE will produce an \\='apikey: API-KEY\\=' header."
  (when api-key
    (cond ((eq type 'jwt)
           `("Authorization" . ,(concat "Bearer " api-key)))
          (t
           `("apikey" . ,api-key)))))


;; Useful internal functions.

(defun rest--replace-value (plist key value)
  "Replace the KEY and VALUE in PLIST if VALUE is not nil.

PLIST is not modified."
  (if value
      (map-insert plist key value)
    plist))

(defun rest--replace-values (plist list-of-cons)
  "Replace each pair of key and value in LIST-OF-CONS in PLIST.

Does not replace the pairs with nil values.

PLIST is not modified."
  (if list-of-cons
      (let ((cons (car list-of-cons)))
        (rest--replace-values
         (rest--replace-value plist (car cons) (cdr cons))
         (cdr list-of-cons)))
    plist))

(defun rest--remove-values (plist &rest keys)
  "Remove all the KEYS and its values from PLIST.

PLIST is not modified."
  (let ((result (map-copy plist)))
    (dolist (key keys)
      (setq result (map-delete result key)))
    result))

(provide 'rest)

;;; rest ends here
