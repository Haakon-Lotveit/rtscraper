;; Because I cannot into ASDF nor QuickProject
;; TODO: Load these a different way, and if they're not found, try to load them via QuickLisp instead
(ql:quickload "yason")
(ql:quickload "drakma")

;; globs and functions that manipulatethem
(defparameter *api-key*
  "jh67gk6sjhajmt6gt5b2ujvq")
(defparameter *api-call-get-all-movies-in-theaters*
  "http://api.rottentomatoes.com/api/public/v1.0/lists/movies/in_theaters?page_limit=50&country=us&")

(defun create-movie-data-table ()
  (make-hash-table :test 'equal))

;; TODO: The end goal is to not have these globs, but to pass them around from function to function. Therefore, all functions must be written so that they accept a hashmap that looks just like *movie-data* so I can pass it in. The glob stays though, because it makes new functionality easier to test.
(defparameter *movie-data*
  (make-hash-table :test 'equal))

(defun reset-stats ()
  (setf *movie-data* (create-movie-data-table))
  'T)

;; Hashmap convenience functions
(defun get-all-members (hashmap disallowed-keys)
  "returns all the members of a given hashmap, excepting the ones in the list of disallowed-keys"
  (let ((members ()))
    (loop for key being the hash-keys of hashmap do
	 (unless (member key disallowed-keys :test 'string-equal)
	   (setf members (cons (gethash key hashmap) members))))
    members))

(defun get-all-movies ()
  (get-all-members *movie-data* '("timestamp")))

;; Functions that deals with data and their manipulation and representation
(defun insert-movie-if-unknown-into (moviehash corpus)
  (let* ((movieID (gethash "id" moviehash)))
    (unless (gethash movieID corpus)
      (setf (gethash movieID corpus) moviehash))))

(defun insert-movie-if-unknown (moviehash)
  (insert-movie-if-unknown-into moviehash *movie-data*))

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

(defun download ()
  "Downloads the movies currently in theaters according to Rotten Tomatoes, and sets the current movie-corpus to this."
  (reset-stats)
  (scrape-theaters *api-call-get-all-movies-in-theaters*)
  (setf (gethash "timestamp" *movie-data*) (get-universal-time))
  'DONE)

(defun join-corpus (corpus-1 corpus-2)
  "Returns a new corpus, made from the corpuses given, without overwriting them."
  (let ((new-corpus (make-hash-table :test 'equal))
	(oldest   (if (< (gethash "timestamp" corpus-1) (gethash "timestamp" corpus-2))
		      corpus-1
		      corpus-2))
	(youngest (if (< (gethash "timestamp" corpus-1) (gethash "timestamp" corpus-2))
		      corpus-2
		      corpus-1)))
    (mapcar (lambda (movie) (insert-movie-if-unknown-into movie new-corpus))
	    (get-all-members youngest '("timestamp")))
    (mapcar (lambda (movie) (insert-movie-if-unknown-into movie new-corpus))
	    (get-all-members oldest '("timestamp")))
    (setf (gethash "timestamp" new-corpus) (gethash "timestamp" youngest))
    new-corpus))
    
(defun save-statistics-to-file (filename statistics-hashmap)
  (with-open-file (fstream filename :direction :output)
    (yason:encode statistics-hashmap (yason:make-json-output-stream fstream :indent 'T))))

(defun load-statistics-from-file (filename)
  (with-open-file (fstream filename :direction :input)
    (yason:parse fstream)))

(defun append-statistics-to-file (filename movie-corpus)
  (let* ((filecorpus (load-statistics-from-file filename))
	 (new-corpus (join-corpus filecorpus movie-corpus)))
    (save-statistics-to-file filename new-corpus)))

(defun save-stats (filename)
  (save-statistics-to-file filename *movie-data*))

(defun load-stats (filename)
  (setf *movie-data* (load-statistics-from-file filename)))

(defun append-stats (filename)
  (append-statistics-to-file filename *movie-data*))

					; This entire section is for calculating probabilities.
					; From the definition of different types of probabilities to functions that generate the tests (or filter functions)
					; to feed these probability functions.
					; All that's really needed here is a CLi bit, and I'm golden.
					; I've decided to not do anything about reading from globs for now,
					; the rationale being that there is no reason to fear multiple sources of reading from them,
					; as long as there is only one source writing to them.

					; probability functions
(defun probability (f)
  "Iterates over the movies and calculates the propability of f returning a non-nil value."
  (let ((num-movies 0)
	(num-successful 0))
    (mapcar (lambda (movie)
	      (incf num-movies)
	      (if (funcall f movie)
		  (incf num-successful)))
	    (get-all-movies))
    (float (/ num-successful num-movies))))

(defun joint-probability (a b)
  (let ((num-movies 0)
	(num-successful 0))
    (mapcar (lambda (movie)
	      (incf num-movies)
	      (if (and (funcall a movie)
		       (funcall b movie))
		  (incf num-successful)))
	    (get-all-movies))
    (float (/ num-successful num-movies))))
  
(defun conditional-probability (a b) 
  (let ((prob-b (probability b)))
    (if (= 0 prob-b) 
	0
	(/ (joint-probability a b)
	   prob-b))))

					; data representation functions, for listing out, counting, etc.
(defun list-movies (f)
  (mapcar (lambda (movie)
	    (if (funcall f movie)
		(format 'T "~A (~A) Rated:~A (id:~A)~%"
		        (gethash "title" movie)
			(gethash "year" movie)
			(gethash "mpaa_rating" movie)
			(gethash "id" movie))))
	  (get-all-movies))
  'DONE)

(defun count-movies (f)
  (let ((num 0))
    (mapcar (lambda (movie)
	      (if (funcall f movie) (incf num)))
	    (get-all-movies))
    num))

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

