(in-package :cl-user)

(defpackage :web-util
  (:use :cl
        :hunchentoot
        :local-time
        :spinneret
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
           :nil-if-empty-string
           :zero-if-nil
           :is-null
           :null-p
           :html-escape
           :join-string-list
           :link
           :row
           :col
           :col-xs
           :col-sm
           :col-md
           :col-lg
           :desktop-only
           :mobile-only
           :universal-to-unix))
