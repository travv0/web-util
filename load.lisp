(in-package :cl-user)

(ql:quickload "cl-dbi")
(ql:quickload "hunchentoot")
(ql:quickload "cl-ppcre")

(require "asdf")
(asdf:load-system :web-util)
