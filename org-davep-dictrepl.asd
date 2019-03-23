;;; org-davep-dictrepl --- REPL-a-like dict client for Common Lisp.
;;
;; org-davep-dictrepl.asd --- asdf package defintion file.
;; Copyright 2003,2004 by Dave Pearson <davep@davep.org>
;; $Revision: 1.4 $
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2003, 2004.
;;
;; Dave Pearson grants you the rights to distribute and use this software as
;; governed by the terms of the Lisp Lesser GNU Public License
;; <URL:http://opensource.franz.com/preamble.html>, known as the LLGPL.

;;; Commentary:
;;
;; org-davep-dictrepl provides a simple test of org-davep-dict.. To use this
;; code fire up your CL implementation of choice, load org-davep-dictrepl
;; and then evaluate (dict:repl). Type "help" and follow your nose.
;;
;; You can always find the latest version of this code at:
;;
;;   <URL:https://github.com/davep/org-davep-dictrepl>

(defpackage #:org-davep-dictrepl-system
  (:use #:common-lisp #:asdf))

(in-package :org-davep-dictrepl-system)

(defsystem org-davep-dictrepl
  :name        "org-davep-dictrepl"
  :author      "Dave Pearson <davep@davep.org>"
  :maintainer  "Dave Pearson <davep@davep.org>"
  :licence     "LLGPL"
  :version     "2.2"
  :description "RFC2229 client REPL-a-like for Common Lisp."
  :long-description
  "org-davep-dictrepl provides a RFC 2229 client REPL-a-like for Common Lisp.
See <URL:http://www.dict.org/> for more details about dict servers and clients.

See <URL:http://www.davep.org/lisp/#org-davep-dictrepl> for the latest version of
this package."
  :depends-on  (:org-davep-dict)
  :components  ((:file "dictrepl")))

;;; org-davep-dictrepl.asd ends here.
