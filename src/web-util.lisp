(in-package :web-util)

(defvar *conn* nil)
(defvar *sessions* (make-hash-table :test 'equal))

(defparameter *session-id-cookie-name* "sessionid")

(defun heroku-getenv (target)
  #+ccl (getenv target)
  #+sbcl (sb-posix:getenv target))

(defun heroku-setenv (var val)
  #+ccl (ccl:setenv var val)
  #+sbcl (sb-posix:putenv (format nil "~a=~a" var val)))

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
               `(result (execute q))))
     (declare (ignore result))))

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

(defun nil-if-empty-string (value)
  (if (equal "" value)
      nil
      value))

(defun zero-if-nil (value)
  (if (not value)
      0
      value))

(defun nil-if-null (value)
  (not (null-p value)))

(defun empty-string-if-null (value)
  (if (null-p value)
      ""
      value))

(defun null-p (x)
  (equal x :null))

(defun is-null (x)
  (log-message* "NOTE" "is-null is deprecated, please use null-p instead.")
  (null-p x))

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
    (loop :for a :on (remove "" list :test #'equal) :do
       (write-string (car a) s)
       (unless (null (cdr a)) (write-string separator s)))))

(defmacro link (text url &key new-tab tooltip)
  `(with-html
     (:a :href ,url
         :target ,(when new-tab
                    "_blank")
         :title ,tooltip
         ,text)))

(defmacro row (&body body)
  `(with-html
     (:div :class "row"
           ,@body)))

(defmacro col (size &body body)
  `(with-html
     (:div :class (format nil "col-xs-~d" ,size)
           ,@body)))

(defmacro col-xs (size &body body)
  `(with-html
     (:div :class (format nil "col-xs-~d" ,size)
           ,@body)))

(defmacro col-sm (size &body body)
  `(with-html
     (:div :class (format nil "col-sm-~d" ,size)
           ,@body)))

(defmacro col-md (size &body body)
  `(with-html
     (:div :class (format nil "col-md-~d" ,size)
           ,@body)))

(defmacro col-lg (size &body body)
  `(with-html
     (:div :class (format nil "col-lg-~d" ,size)
           ,@body)))

(defmacro desktop-only (&body body)
  `(with-html
     (:span :class "hidden-xs"
            ,@body)))

(defmacro mobile-only (&body body)
  `(with-html
     (:span :class "visible-xs-inline"
            ,@body)))

(defun universal-to-unix (time)
  (timestamp-to-unix (universal-to-timestamp time)))
