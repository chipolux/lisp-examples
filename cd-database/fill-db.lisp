(load "db.lisp")

(if (probe-file "cds.db") (load-db "cds.db"))

(add-record (make-cd "Badongle My Badingles" "Beemper" 8 t))
(add-record (make-cd "Fleembops Trouble In Gripple" "Beemper" 5 nil))
(add-record (make-cd "Karpikle Grimbos" "MKP" 6 t))
(add-record (make-cd "Farface" "Beemper" 9 t))
(add-record (make-cd "Ooooooooohhhhhhh" "MKP" 10 nil))
(add-record (make-cd "Zimpop Mt Ulcrease" "Beemper" 2 t))

(save-db "cds.db")
