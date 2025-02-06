;;; rest.el --- Useful functions for REST API calls. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Coque Couto

;; Author: Coque Couto <coque.couto at gmail.com>
;; Version: 0.1
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

(cl-defun rest-api-call (type
                         entrypoint
                         &key
                         (sync nil)
                         (accept "application/vnd.api+json")
                         (auth-header nil)
                         (data nil)
                         (content-type nil)
                         (success #'rest-raw-to-buffer)
                         (error #'rest-show-error)
                         &allow-other-keys)
  "Make a REST API call of type TYPE to the ENTRYPOINT URL,
including ACCEPT, AUTH-HEADER, and CONTENT-TYPE as headers.
Calls SUCCESS with the resulting data.  The rest of parameters
work as described in the `request' documentation."
  (request
    entrypoint
    :sync sync
    :timeout (when sync 3)
    :type type
    :headers
    (append `(("accept" . ,accept))
            (and auth-header
                 (list auth-header))
            (and content-type
                 `(("Content-Type" . ,content-type))))
    :data data
    :success success
    :error error))

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

KEY is a string with the API key or the JWT token.

TYPE may be \\='jwt for JWT authentication, which will produce an
\\='Authentication: Bearer KEY\\=' header.

Any other TYPE will produce an \\='apikey: KEY\\=' header."
  (cond ((eq type 'jwt)
         `("Authorization" . ,(concat "Bearer " api-key)))
        (t
         `("apikey" . ,api-key))))

;;; rest.el ends here
