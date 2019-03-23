;;; org-davep-dictrepl --- REPL-a-like dict client for Common Lisp.
;;
;; dictrepl.lisp --- Main code for org-davep-dictrepl.
;; Copyright 2003,2004 by Dave Pearson <davep@davep.org>
;; $Revision: 1.3 $
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2003, 2004.
;;
;; Dave Pearson grants you the rights to distribute and use this software as
;; governed by the terms of the Lisp Lesser GNU Public License
;; <URL:http://opensource.franz.com/preamble.html>, known as the LLGPL.

;;; Commentary:
;;
;; The following code serves as a simple test of org-davep-dict. To use this
;; code fire up your CL implementation of choice, load org-davep-dictrepl
;; and then evaluate (dict:repl). Type "help" and follow your nose.
;;
;; You can always find the latest version of this code at:
;;
;;   <URL:https://github.com/davep/org-davep-dictrepl>

;;; Code:

;; Do everything in the org.davep.dict package.
(in-package :org.davep.dict)

;; Export the REPL function.
(export 'REPL)

(defun repl-read-line (client)
  "Read a line from the user."
  (format t "~&dict~A: " (if (connectedp client)
                             (format nil "[~A:~A]" (host client) (port client))
                           ""))
  (finish-output)
  (with-safe-reading
   (read-string-as-list (read-line *standard-input* nil "quit"))))

(defun repl-list (data &key (formatter (lambda (item) (format t "~&~A~%" item))))
  "Output DATA.

Optionally format the output with the function given to the FORMATTER keyword."
  (declare (type function formatter))
  (mapc formatter (data data)))

(defun repl-info-list (data)
  "Show the value of DATA as a name/description formatted list."
  (repl-list data
             :formatter (lambda (info)
                          (format t "~&~A~10T - ~A~%" (name info) (description info)))))

(defun repl-define (client args)
  "Do a define command on CLIENT with ARGS."
  (if args
      (let ((defs (if (= (length args) 1)
                      (define client (format nil "~A" (first args)))
                    (define client
                      (format nil "~A" (first args))
                      :database (format nil "~A" (second args))))))
        (if defs
            (mapc (lambda (def)
                    (format t "~&~%~A~%~%" (name def))
                    (mapc (lambda (line)
                            (format t "~&~A~%" line))
                          (definition def)))
                  defs)
          (format t "~&No definitions found~%")))
    (format t "~&Please give a word to define~%")))

(defun repl-match (client args)
  "Do a match comand on CLIENT with ARGS."
  (if args
      (let ((matches (cond ((= (length args) 1)
                            (match client (format nil "~A" (first args))))
                           ((= (length args) 2)
                            (match client (format nil "~A" (first args))
                                   :database (format nil "~A" (second args))))
                           (t
                            (match client (format nil "~A" (first args))
                                   :database (format nil "~A" (second args))
                                   :strategy (format nil "~A" (third args)))))))
        (if (data matches)
            (repl-info-list matches)
          (format t "~&No matches found~%")))
    (format t "~&Please give a word to match~%")))

(defun repl-info (client args)
  "Show information about a database."
  (if args
      (let ((info (info client (format nil "~A" (car args)))))
        (if (data info)
            (repl-list info)
          (if (= (code info) 550)
              (format t "~&Invalid database, use \"dbs\" for list.~%")
            (format t "~&~A~%" (response info)))))
    (format t "~&Please provide the name of a database.")))

(defmacro with-reconnect (client &body body)
  "Force a reconnect to CLIENT after BODY has been evaluated."
  `(prog1
       (progn
         ,@body)
     (when (connectedp ,client)
       (disconnect ,client)
       (connect ,client))))

(defun repl ()
  "Provide a simple dict REPL.

Commands are:

open                            : Open a connection to the dictionary server.
connect                         : Alias for \"open\".
close                           : Close the connection to the dictionary
                                  server.
disconnect                      : Alias for \"close\".
host                            : Display the current host.
host <name>                     : Set the host to <name>.
port                            : Display the current port.
port <name>                     : Set the port to <port>.
define <word> [<dictionary>]    : Display definitions for <word>. Optionally
                                  only display the definition from <dictionary>.
match <word>                    : Display matches for <word>. Optionally only
      [<database> [<strategy>]]   display matches from <dictionary>.
                                  Optionally match using <strategy>.
databases                       : Display list of available databases.
dbs                             : Alias for \"databases\".
strategies                      : Display list of matching strategies.
strats                          : Alias for \"strategies\".
info <database>                 : Display information about <database>.
sinfo                           : Display server information.
shelp                           : Display server help.
help                            : Display help.
quit                            : Quit the dict REPL.
exit                            : Alias for \"quit\".
"
  (clear-input)
  (let ((d (make-dict-client)))
    (unwind-protect
        (loop for command = (repl-read-line d)
              until (eq 'quit
                        (handler-case
                         (case (intern (symbol-name (car command)) :keyword)
                           ((:open :connect)
                            (unless (connectedp d)
                              (connect d))
                            (when (connectedp d)
                              (format t "~&Connected~%~A~%Capabilities:~{ ~A~}~%Message-ID: ~A~%"
                                      (server-details d)
                                      (capabilities d)
                                      (message-id d))))
                           ((:close :disconnect)
                            (when (connectedp d)
                              (disconnect d)))
                           (:host
                            (if (cdr command)
                                (with-reconnect d (setf (host d) (format nil "~A" (cadr command))))
                              (format t "~&Host is ~A~%" (host d))))
                           (:port
                            (if (integerp (cadr command))
                                (with-reconnect d (setf (port d) (cadr command)))
                              (format t "~&Port is ~A~%" (port d))))
                           ((:def :define)
                            (repl-define d (cdr command)))
                           (:match
                            (repl-match d (cdr command)))
                           ((:databases :dbs)
                            (repl-info-list (databases d)))
                           ((:strategies :strats)
                            (repl-info-list (strategies d)))
                           (:info
                            (repl-info d (cdr command)))
                           (:sinfo
                            (repl-list (server-info d)))
                           (:shelp
                            (repl-list (server-help d)))
                           (:help
                            (let ((doc (documentation 'repl 'function)))
                              (format t "~%~A~%" (if doc doc "Sorry, your CL implementation didn't provide the documentation."))))
                           ((:quit :exit)
                            'quit)
                           (otherwise
                            (format t "~&Unknown command~&")))
                         (error (e) (format t "~&Error: ~A~%" e)))))
      (when (connectedp d)
        (disconnect d)))))

;;; dictrepl.lisp ends here.
