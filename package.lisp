(in-package :cl-user)

(defpackage :web-util
  (:use :cl
        :hunchentoot
        :cl-ppcre
        :cl-dbi)
  (:export :*conn*
           :*sessions*
           :*session-id-cookie-name*
           :with-db
           :execute-query-loop
           :execute-query-one
           :execute-query-modify
           :random-elt
           :print-object
           :get-session-var
           :empty-string-if-nil
           :zero-if-nil
           :is-null
           :html-escape
           :join-string-list))
