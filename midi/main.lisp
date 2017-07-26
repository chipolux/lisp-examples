(load (compile-file "midi.lisp"))


(defun describe-midifile (path)
  (let ((midifile (midi:read-midi-file "bach_bourree.mid")))
    (format t "File: ~a~%" path)
    (format t "Format: ~a~%" (midi:midifile-format midifile))
    (format t "Tracks: ~a~%" (length (midi:midifile-tracks midifile)))))


(describe-midifile "bach_bourree.mid")
