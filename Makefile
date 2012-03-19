SOURCES=org-davep-dictrepl.lisp
include ../lisp.cf

asdf:
	rm -rf asdf-package/*
	sbcl --load build-asdf
