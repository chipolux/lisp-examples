;;; Recursive local function using `label`
(defun collect-leaves (tree)
  (let ((leaves ()))
    (labels ((walk (tree)
               (cond
                 ((null tree))                     ; do nothing if the tree is nil
                 ((atom tree) (push tree leaves))  ; if we're on a leaf, add it to leaves
                 (t (walk (car tree))              ; otherwise run walk on the car (most likely a leaf)
                    (walk (cdr tree))))))          ; then run walk on the cdr (most likely following the tree)
      (walk tree))
    (nreverse leaves)))  ; push is like insert, so the last item will be on the front
