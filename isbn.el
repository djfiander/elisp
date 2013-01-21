(defun isbn-digit (c)           ;private internal function
  "Return the value of the ISBN digit c (in 0..10), or nil if c is invalid."
  (cond
   ((and (>= c ?0) (<= c ?9)) (- c ?0))
   ((or (= c ?X) (= c ?x)) 10)))

(fset 'issn-digit 'isbn-digit)          ;ISSN uses subset of ISBN digits


;; Declarative version of calc-isbn10-checksum:
;;
;; (defun calc-isbn-checksum (isbn)
;;   (let ((sum 0) (k 0) (c 0) (pos 1) (result nil))
;;     (while (< k (length isbn))
;;       (setq c (isbn-digit (aref isbn k)))
;;       (setq k (1+ k))
;;       (cond
;;        ((and c (= pos 10))
;;         (setq result (mod sum 11) c))
;;        ((and c (< pos 10))
;;         (setq sum (+ sum (* c pos)) pos (1+ pos)))
;;        ))
;;     result))

(defun calc-isbn10-checksum (isbn)
  "Given an ISBN as a string, calculate the checksum.
The formula used is
    (sum(k=1:9) digit(k) * k) mod 11
This function does not support hyphenated ISBNs."
  (let ((weights '(10 9 8 7 6 5 4 3 2)))
    (- 11 (mod (apply '+ (mapcar* '* (mapcar 'isbn-digit (substring isbn 0 -1))
				  weights))
	       11))))

(defun calc-isbn13-checksum (isbn)
  "Given an ISBN13 as a string, calculate the checksum.
The formula used is
    x13 = (10 - (x1 + 3x2 + x3 + 3x4 + x5 + 3x6 +
                 x7 + 3x8 + x9 + 3x10 + x11 + 3x12) mod 10) mod 10
This function does not support hyphenated ISBNs."
  (let ((weights '(1 3 1 3 1 3 1 3 1 3 1 3)))
    (mod (- 10 (mod (apply '+ (mapcar* '* (mapcar 'isbn-digit
						  (substring isbn 0 -1))
				       weights))
		    10))
	 10)))

(defun isbn-numberp (isbn)
  "Check ISBN to see if it is a valid ISBN.
Use the standard ISBN checksum function:
        (sum(k=1:9) digit(k) * k) mod 11 == digit(10)
where X as a digit has value 10."
  (= (isbn-digit (elt isbn (- (length isbn) 1)))
     (cond
      ((= (length isbn) 10)
       (calc-isbn10-checksum isbn))
      ((= (length isbn) 13)
       (calc-isbn13-checksum isbn))
      (t (error "Not a valid ISBN")))))

(defun read-isbn (&optional default)
  (let ((isbn (apply 'concat
		     (split-string (read-from-minibuffer "ISBN: ") "-" t))))
    (cond
     ((isbn-numberp isbn) isbn)
     ((= (length isbn) 0) nil)
     (t (error "Not a valid ISBN")))))

(defun insert-isbn-loop ()
  (interactive)
  (let ((done nil))
    (while (not done)
      (condition-case nil
	  (let ((isbn (read-isbn)))
	    (if isbn
		(insert isbn "\n")
	      (setq done t)))
	(error
	 (message "Not a valid ISBN")
	 (sit-for 1))))))


