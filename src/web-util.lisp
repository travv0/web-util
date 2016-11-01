(in-package :web-util)

(defvar *conn* nil)
(defvar *sessions* (make-hash-table :test 'equal))

(defparameter *session-id-cookie-name* "sessionid")

(defun heroku-getenv (target)
  #+ccl (getenv target)
  #+sbcl (sb-posix:getenv target))

(defmacro with-db (conn &body body)
  `(let ((db-url (quri:uri (heroku-getenv "DATABASE_URL"))))
     (with-connection (,conn :postgres
                             :host (format nil "~a" (quri:uri-host db-url))
                             :username (first (split-sequence:split-sequence
                                               #\: (quri:uri-userinfo db-url)))
                             :password (second (split-sequence:split-sequence
                                                #\: (quri:uri-userinfo db-url)))
                             :database-name (format nil "~a"
                                                    (string-left-trim
                                                     '(#\/) (quri:uri-path db-url))))
       ,@body)))

(defmacro execute-query-loop (row query params &body body)
  `(let* ((q (prepare *conn* ,query))
          ,(if params
               `(result (execute q ,@params))
               `(result (execute q))))
     (loop for ,row = (fetch result)
        while ,row do
          ,@body)))

(defmacro execute-query-one (row query params &body body)
  `(let* ((q (prepare *conn* ,query))
          ,(if params
               `(result (execute q ,@params))
               `(result (execute q)))
          (,row (fetch result)))
     ,@body))

(defmacro execute-query-modify (query params)
  `(let* ((q (prepare *conn* ,query))
          ,(if params
               `(result (execute q ,@params))
               `(result (execute q))))))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
          (loop for key being the hash-keys of object
             using (hash-value value)
             collect (list key value))))

(defun get-session-var (session-var)
  (let ((session (gethash (cookie-in *session-id-cookie-name*) *sessions*)))
    (if session
        (gethash session-var session))))

(defun empty-string-if-nil (value)
  (if (not value)
      ""
      value))

(defun zero-if-nil (value)
  (if (not value)
      0
      value))

(defun is-null (x)
  (equal x :null))

(defun html-escape (text)
  (regex-replace-all
   "<"
   (regex-replace-all
    ">"
    (regex-replace-all "&" text "&amp;")
    "&gt;")
   "&lt;"))

(defun join-string-list (list &optional (separator " "))
  (with-output-to-string (s)
    (loop :for a :on list :do
       (write-string (car a) s)
       (unless (null (cdr a)) (write-string separator s)))))
