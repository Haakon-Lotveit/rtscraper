;; Because I cannot into ASDF nor QuickProject
(ql:quickload "yason")
(ql:quickload "drakma")

;; globs and functions that manipulatethem
(defparameter *api-key*
  "jh67gk6sjhajmt6gt5b2ujvq")
(defparameter *api-call-movies-in-theaters*
  "http://api.rottentomatoes.com/api/public/v1.0/lists/movies/in_theaters?page_limit=50&country=us&")

(defun create-movie-data-table ()
  (make-hash-table :test 'equal))

;; TODO: The end goal is to not have these globs, but to pass them around from function to function. Therefore, all functions must be written so that they accept a hashmap that looks just like *movie-data* so I can pass it in. The glob stays though, because it makes new functionality easier to test.
(defparameter *movie-data*
  (make-hash-table :test 'equal))

(defun reset-stats ()
  (setf *movie-data* (create-movie-data-table))
  'T)

;; Functions that deals with data and their manipulation and representation
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
    (mapcar #'(lambda (x)
		(insert-movie-if-unknown x))
	    (gethash "movies" dl)) 
    (let ((next (gethash "next" (gethash "links" dl))))
      (if next (scrape-theaters (concatenate 'string next "&")))
      *movie-data*)))

;; Some basic facilities to run this program as a standalone thing and not running it through a REPL
(defun start ()
  "Downloads the movies currently in theaters according to Rotten Tomatoes"
  (reset-stats)
  (scrape-theaters *api-call-movies-in-theaters*)
  'DONE)


(defun append-statistics-to-file (filename movie-corpus)
  (error "Appending the corpus ~A to file ~A is a great idea, but is unimplemented"
	 movie-corpus
	 filename))

(defun join-corpus (corpus-1 corpus-2)
  (error "Joining two corpuses (~A and ~A) together is a great idea, but is unimplemented"
	 corpus-1 corpus-2))


(defun save-statistics-to-file (filename statistics-hashmap)
  (with-open-file (fstream filename :direction :output)
    (yason:encode statistics-hashmap (yason:make-json-output-stream fstream :indent 'T))))

(defun load-statistics-from-file (filename)
  (with-open-file (fstream filename :direction :input)
    (yason:parse fstream)))

(defun save-stats (filename)
  (save-statistics-to-file filename *movie-data*))

(defun load-stats (filename)
  (error "Loading statistics from ~A is a great idea, but is unimplemented" filename))

;; This code is for testing porpoises. Don't ruin everything. ;)
(defparameter *movie-testing*
  (with-open-file (fstream "sample.json" :direction :input)
    (yason:parse fstream)))

					; This entire section is for calculating probabilities.
					; From the definition of different types of probabilities to functions that generate the tests (or filter functions)
					; to feed these probability functions.
					; All that's really needed here is a CLi bit, and I'm golden.
					; I've decided to not do anything about reading from globs for now,
					; the rationale being that there is no reason to fear multiple sources of reading from them,
					; as long as there is only one source writing to them.

; probability functions
(defun probability (f)
  "Iterates over the movies and calculates the propability of testfun returning a non-nil value."
  (let ((num-movies 0)
	(num-successful 0))
    (loop for key being the hash-keys of *movie-data*
       do
	 (incf num-movies)
	 (if (funcall f (gethash key *movie-data*))
	     (incf num-successful)))
    (float (/ num-successful num-movies))))

(defun joint-probability (a b)
  (let ((num-movies 0)
	(num-successful 0))
    (loop for key being the hash-keys of *movie-data*
       do
	 (incf num-movies)
	 (if (and (funcall a (gethash key *movie-data*))
		  (funcall b (gethash key *movie-data*)))
	     (incf num-successful)))
    (float (/ num-successful num-movies))))
  
(defun conditional-probability (a b) 
  (if (= b 0) 0) ;; If b = 0, then we return zero. We SHOULD have returned NaN, but I don't know how to do that.
  (/ (joint-probability a b)
     (probability b)))

					; listing functions, lists out all movies that fits a filter.
(defun list-movies (f)
  (loop for key being the hash-keys of *movie-data*
     do
       (let ((movie (gethash key *movie-data*)))
	 (if (funcall f movie)
	     (format 'T "~A (~A) Rated:~A (id:~A)~%"
		        (gethash "title" movie)
			(gethash "year" movie)
			(gethash "mpaa_rating" movie)
			(gethash "id" movie)))))
  'DONE)

;; General helpful functions
(defun nested-hash-lookup (strings hash)
  (let ((nexthash (gethash (car strings) hash))
	(nextkey (cdr strings)))
    (if nextkey
	(nested-hash-lookup nextkey nexthash)
	nexthash)))

;; Generator functions that generates filter functions for the probability functions.
(defun critic-rating (&key (min 0) (max 100))
  (lambda (movie)
    (and
     (>= (gethash "critics_score" (gethash "ratings" movie)) min)
     (<= (gethash "critics_score" (gethash "ratings" movie)) max))))

(defun audience-rating (&key (min 0) (max 100))
  (lambda (movie)
    (and
     (>= (gethash "audience_score" (gethash "ratings" movie)) min)
     (<= (gethash "audience_score" (gethash "ratings" movie)) max))))

(defun mpaa-rating (rating)
  (lambda (movie)
    (string-equal (gethash "mpaa_rating" movie) rating)))

(defun released-month (month)
  (lambda (movie)
    (string-equal
     month
     (subseq (gethash "theater" (gethash "release_dates" movie))
	     5 7)))) ;;; The fifth to seventh element in the string represent the month of the ISO-date
  
(defun runtime (&key (min 0) (max 0))
  (lambda (movie)
    (let ((runtime (gethash "runtime" movie)))
      (cond ((not runtime)  'NIL)
	    ((equal "" runtime) 'NIL)
	    ('ELSE (and
		    (if (> max 0)
			(<= runtime max)
			'T)
		    (> runtime min)))))))

