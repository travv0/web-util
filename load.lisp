(in-package :cl-user)

(ql:quickload "cl-dbi")
(ql:quickload "hunchentoot")
(ql:quickload "cl-ppcre")
(ql:quickload "quri")
(ql:quickload "spinneret")
(ql:quickload "local-time")

(require "asdf")
(asdf:load-system :web-util)
