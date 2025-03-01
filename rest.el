;;; rest.el --- Useful functions for REST API calls. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Coque Couto

;; Author: Coque Couto <coque.couto at gmail.com>
;; Version: 0.2
;; Package-Requires ((emacs "24.4") request json json-mode)
;; Keywords: comm lisp tools
;; URL: https://github.com/coquec/emacs-rest

;; rest.el is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; rest.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; rest.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; rest.el contains some simple functions to write elisp code that interacts
;; with REST API endpoints.

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'json)
(require 'json-mode)

(cl-defun rest-call (&key
                     type
                     entrypoint
                     (dry-run nil)
                     (sync nil)
                     (timeout nil)
                     (accept "application/vnd.api+json")
                     (auth-header nil)
                     (data nil)
                     (content-type nil)
                     (success #'rest-raw-to-buffer)
                     (error #'rest-show-error)
                     (complete nil))
  "When the DRY-RUN parameter is nil, make a REST API call of type TYPE to
the ENTRYPOINT URL, including ACCEPT, AUTH-HEADER, and CONTENT-TYPE as
headers.  SUCCESS is called with the resulting data.

The other parameters work as described in the `request' documentation.

Return a list of the parameters used to call it, except DRY-RUN, SYNC,
SUCCESS, ERROR, and COMPLETE.  Used along a non-nil DRY-RUN, these
return values can be used to make parallel calls with
`rest-multiple-calls'."
  (unless dry-run
    (request
      entrypoint
      :sync sync
      :timeout timeout
      :type type
      :headers
      (append `(("accept" . ,accept))
              (and auth-header
                   (list auth-header))
              (and content-type
                   `(("Content-Type" . ,content-type))))
      :data data
      :success success
      :error error
      :complete complete))
  (list :type type
        :entrypoint entrypoint
        :timeout timeout
        :accept accept
        :auth auth-header
        :data data
(cl-defun rest-api-multiple-calls (&key
                                   parameters
                                   results
                                   success
                                   error
                                   (type nil)
                                   (accept nil)
                                   (auth-header nil)
                                   (data nil)
                                   (content-type nil))
  "Make parallel REST API calls using `rest-api-call' with the parameters
stored in each of the entries of the PARAMETERS plist.  Leave the
results of individual calls in the RESULTS list, with entries being cons
with (SYMBOL-STATUS . DATA).  Calls SUCCESS if all the calls are
successful, or ERROR if any of them fails.

Entries in the RESULTS list do not follow the same order as elements in
the PARAMETERS plist.

For each entry in the PARAMETERS plist, :entrypoint is required, and
:sync, :success and :error are ignored.

Any of the parameters TYPE, ACCEPT, AUTH-HEADER, DATA, and CONTENT-TYPE
are used for entries in the PARAMETER list that don't specify them.

Both SUCCESS and ERROR must expect the RESULTS list as parameter."
  (let ((counter (length parameters))
        (error-p nil))
    (dolist (entry parameters)
      (apply #'rest-api-call
             (list
              :type (or type (plist-get entry :type))
              :entrypoint (plist-get entry :entrypoint)
              :accept (or accept (plist-get entry :accept))
              :auth-header (or auth-header (plist-get entry :auth-header))
              :data (or data (plist-get entry :data))
              :content-type (or content-type
                                (plist-get entry :content-type))
              :success nil
              :error nil
              :complete
              (cl-function
               (lambda (&key
                        data
                        symbol-status
                        &allow-other-keys)
                 (push `(,symbol-status . ,data) results)
                 (setq error-p (or error-p
                                   (not (eq symbol-status 'success))))
                 (setq counter (1- counter))
                 (when (= 0 counter)
                   (if error-p
                       (apply error results)
                     (apply success results))))))))))

(cl-defun rest-raw-to-buffer (&key
                              data
                              (buffer-name "*raw-results*")
                              (read-only-p t)
                              &allow-other-keys)
  "Deletes the contents of the buffer BUFFER-NAME, paste DATA into
it, and leaves it unformatted."
  (with-current-buffer (get-buffer-create buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (insert data)
    (read-only-mode read-only-p)
    (pop-to-buffer (current-buffer))))

(cl-defun rest-json-to-buffer (&key
                               data
                               (buffer-name "*json-results*")
                               (read-only-p t)
                               &allow-other-keys)
  "Deletes the contents of the buffer BUFFER-NAME, paste DATA into
it, and formats it as JSON."
  (with-current-buffer (get-buffer-create buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (insert data)
    (json-mode-beautify (point-min) (point-max))
    (json-mode)
    (read-only-mode read-only-p)
    (pop-to-buffer (current-buffer))))

(cl-defun rest-show-error (&key
                           error-thrown
                           &allow-other-keys)
  "Writes a message with the ERROR-THROWN."
  (message "Error: %S" error-thrown))

(cl-defun rest-auth-header (&key
                            api-key
                            (type nil))
  "Generates a header to be used with `rest-api-call' for
authenticated calls.

API-KEY is a string with the API key or the JWT token.

TYPE may be \\='jwt for JWT authentication, which will produce an
\\='Authentication: Bearer API-KEY\\=' header.

Any other TYPE will produce an \\='apikey: API-KEY\\=' header."
  (cond ((eq type 'jwt)
         `("Authorization" . ,(concat "Bearer " api-key)))
        (t
         `("apikey" . ,api-key))))

;;; rest.el ends here
