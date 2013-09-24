;; Because I cannot into ASDF nor QuickProject
(ql:quickload "yason")
(ql:quickload "drakma")


(defparameter *api-key*
  "jh67gk6sjhajmt6gt5b2ujvq")
(defparameter *api-call-movies-in-theaters*
  "http://api.rottentomatoes.com/api/public/v1.0/lists/movies/in_theaters?page_limit=50&country=us&")

(defun create-movie-data-table ()
  (make-hash-table :test 'equal))
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

;; TODO: The end goal is to not have these globs, but to pass them around from function to function. Therefore, all functions must be written so that they accept a hashmap that looks just like *statistics-table*/*movie-data* so I can pass it in. The glob stays though, because it makes new functionality easier to test.
(defparameter *statistics-table*
  (make-hash-table :test 'equal))
(defparameter *movie-data*
  (make-hash-table :test 'equal))

(defun reset-stats ()
  (setf *statistics-table* (create-statistics-table))
  (setf *movie-data* (create-movie-data-table))
  'T)


(defun insert-movie-if-unknown (moviehash)
  (let* ((movieID (gethash "id" moviehash)))
    (unless (gethash movieID *movie-data*)
      (setf (gethash movieID *movie-data*) moviehash))))

(defun insert-movie-data (moviehash statistics-table)
  (let ((critical-score (gethash "critics_score" (gethash "ratings" moviehash)))
	(audience-score (gethash "audience_score" (gethash "ratings" moviehash))))
    ;;(runtime (gethash "runtime" moviehash)) ;← Currently commented out, since we do not use this information for anything.
    (flet ((string-to-month (string)
	     (subseq string 5 7))
	   (stat-inc (string)
	     (incf (gethash (concatenate 'String "released-" string) statistics-table) 1)
	     (incf (gethash (concatenate 'String "critical-score-" string) statistics-table) critical-score)
	     (incf (gethash (concatenate 'String "audience-score-" string) statistics-table) audience-score)))
      (stat-inc "unsorted")
      (stat-inc (string-to-month (gethash "theater" (gethash "release_dates" moviehash))))
      (stat-inc (gethash "mpaa_rating" moviehash)))))

(defun scrape (api-call)
  (let ((uri (concatenate 'string api-call "apikey=" *api-key*)))
    (drakma:http-request uri)))

(defun scrape-theaters (api-call)
  (let ((dl (yason:parse (scrape api-call))))
    (mapcar #'(lambda (x) ;;; Currying *statistics-table* into the function as part of the refactoring effort to remove globs
		(insert-movie-data x *statistics-table*)
		(insert-movie-if-unknown x))
	    (gethash "movies" dl)) 
    (let ((next (gethash "next" (gethash "links" dl))))
      (if next (scrape-theaters (concatenate 'string next "&")))
      *statistics-table*)))

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

(defun start ()
  (reset-stats)
  (scrape-theaters *api-call-movies-in-theaters*)
  (print-data-formatted *statistics-table*)
  'DONE)

(defun scripted-run ()
  "A simple convenience function for running the program as a small standalone script."
  (start)
  (exit))

;; TODO: Make it clear that we're saving the statistics over the movies, not the overall movies.
;; TODO: Use the insertion function to recreate the *statistics-table* hashmap
;; TODO: Nuke the *statistics-hashmap*, but append to the movie-hashmap, and THEN recreate.
(defun save-statistics-to-file (filename statistics-hashmap)
  (with-open-file (fstream filename :direction :output)
    (yason:encode statistics-hashmap (yason:make-json-output-stream fstream :indent 'T))))

(defun load-statistics-from-file (filename)
  (with-open-file (fstream filename :direction :input)
    (yason:parse fstream)))

;; This code is for testing porpoises. Don't ruin everything. ;)
(defparameter *movie-testing*
  (with-open-file (fstream "sample.json" :direction :input)
    (yason:parse fstream)))

;(defun probability
