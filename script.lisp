(defun create-statistics-table ()
  (let ((table (make-hash-table :test 'equal)))
    ;; This initializes the hashtable with predefined keys.
    (mapcar (lambda (x) 
	      (setf (gethash x table) 0))
	    ;; TODO: This is sort of ugly. There is a better way to generate the fields, but this will do for now.
	    '(;; The ratings per month
	      "released-01" "critical-score-01" "audience-score-01"
	      "released-02" "critical-score-02" "audience-score-02"
	      "released-03" "critical-score-03" "audience-score-03"
	      "released-04" "critical-score-04" "audience-score-04"
	      "released-05" "critical-score-05" "audience-score-05"
	      "released-06" "critical-score-06" "audience-score-06"
	      "released-07" "critical-score-07" "audience-score-07"
	      "released-08" "critical-score-08" "audience-score-08"
	      "released-09" "critical-score-09" "audience-score-09"
	      "released-10" "critical-score-10" "audience-score-10"
	      "released-11" "critical-score-11" "audience-score-11"
	      "released-12" "critical-score-12" "audience-score-12"
	      ;; The Ratings per MPAA rating
	      "released-G"       "critical-score-G"       "audience-score-G"
	      "released-PG"      "critical-score-PG"      "audience-score-PG"
	      "released-PG-13"   "critical-score-PG-13"   "audience-score-PG-13"
	      "released-Unrated" "critical-score-Unrated" "audience-score-Unrated"
	      "released-R"       "critical-score-R"       "audience-score-R"
	      "released-NC-17"   "critical-score-NC-17"   "audience-score-NC-17"
	      ;; The average ratings
	      "released-unsorted" "critical-score-unsorted" "audience-score-unsorted"))
    table))

(defun print-data (hashmap keylist format-string output-stream)
  "Prints out specified statistical data from a given hashmap, in a format specified, to the stream specified. This includes files and http sockets and what not
Please note that this function depends on the hashmap structure defined in the function create-statistics-table"
  (mapcar (lambda (arg)
	    (let ((rel (gethash (concatenate 'string "released-" arg) hashmap)))
	      (if (> rel 0) (format output-stream format-string arg rel
				    (float (/ (gethash (concatenate 'string "critical-score-" arg) hashmap) rel))
				    (float (/ (gethash (concatenate 'string "audience-score-" arg) hashmap) rel))))))
	  keylist))

(defun print-data-formatted (statistics-table)
  "A small convenience function that prints the data we get by calling print-data with the appropriate variables."
  (print-data statistics-table '("unsorted") "Number of ~A films: ~A Average critical rating: ~A Average audience rating: ~A~%" 'T)
  (print-data statistics-table '("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" ) "  Movies in ~Ath month: ~A Average critical rating: ~A Average audience rating: ~A~%" 'T)
  (print-data statistics-table '("G" "PG" "PG-13" "Unrated" "R" "NC-17") "Movies rated ~A: ~A Average critical rating: ~A Average audience rating: ~A~%" 'T))

(defun generate-basic-statistics (movie-corpus)
  (let ((table (create-statistics-table)))
    (loop for key being the hash-keys of movie-corpus
       do
	 (insert-movie-data (gethash key movie-corpus) table))
    table))

(defun scripted-run ()
  "A simple convenience function for running the program as a small standalone script that does some basic stats."
  (start)
  (print-data-formatted (generate-basic-statistics *movie-data*))
  (exit))
