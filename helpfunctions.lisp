(defun slurp-stream (stream)
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defun print-hashmap (hashmap)
  (loop for key being the hash-keys of hashmap
     using (hash-value value)
     do (format t "K:~S - V:~S~%" key value)))
