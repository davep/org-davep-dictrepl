(require :asdf-install)
(require :asdf-packaging-tools)
(asdf:oos 'asdf-packaging-tools:release-op :org-davep-dictrepl :directory "asdf-package/" :force t)
