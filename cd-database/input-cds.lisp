(load "db.lisp")

(if (probe-file "cds.db") (load-db "cds.db"))
(add-cds)
(save-db "cds.db")
