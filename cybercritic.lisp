;; cybercritic.lisp -  The artwork description generation system.
;; Copyright (C) 2009  Rob Myers rob@robmyers.org
;;
;; This file is part of cybercritic-microblogger.
;;
;; cybercritic-microblogger is free software; you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License as published 
;; by the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; cybercritic-microblogger is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License

(in-package #:cybercritic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maybe (fun &key (probability 0.5) (default nil))
  "Call fun with aqrgs if random(0..1) is less than probability."
  (if (< (random 1.0) probability)
      (funcall fun)
    default))

(defun choose-randomly (choices)
  "Choose one of the parameters randomly."
  (nth (random (list-length choices)) 
       choices))

(defun choose-randomly-deep (choices)
  "Choose one item from a list of lists."
  (choose-randomly (choose-randomly choices)))

(defun plus-or-minus-one ()
  "Choose either one or minus one."
  (choose-randomly '(1.0 -1.0)))

(defun random-range (from to)
  "Choose a random number from from to to."
  (+ (random (- to from))
     from))

(defmethod choose-one-of ((possibilities list))
  "Choose one or none of the option"
  (nth (random (length possibilities)) possibilities))

;;FIXME Force unique choices
(defmethod choose-n-of ((n integer) (choice-list list))
  "Choose n different entries from choice-list."
  (assert (<= n (length choice-list)))
  (let ((choices choice-list)
        (chosen '()))
    (dotimes (i n)
      (let ((choice (choose-one-of choices)))
        (setf chosen (cons choice chosen))
        (setf choices (remove choice choices))))
    chosen))

(defun concatenate-string (&rest strings)
  "Concatenate a list of strings with an optional given prefix, separator and suffix."
  (let ((all (car strings)))
    (dolist (s (cdr strings))
      (when (not (equal s ""))
	(setf all (concatenate 'string all
			       (if (equal all "")
				   ""
				 " ") 
			       s))))
    all))

(defun make-stretchy-vector (element-type)
  "Make an empty stretchy vector of element-type."
  (make-array 0
	      :element-type element-type
	      :fill-pointer 0
	      :adjustable t))

(defun make-char-stretchy-vector ()
  "Make an empty stretchy character vector."
  (make-stretchy-vector 'character))

(defun make-string-stretchy-vector ()
  "Make an empty stretchy string vector."
  (make-stretchy-vector 'string))

(defun vector-empty-p (vec)
  "Is the vector empty?"
  (= (length vec) 0))

(defmethod tokenize-string ((source string) (separators string))
  "Tokenize string to produce words separated by runs of separators."
  (let ((words (make-string-stretchy-vector))
	(chars (make-char-stretchy-vector)))
    (loop for char across source  
	  do (if (find char separators :test #'char-equal)
		 (when (not (vector-empty-p chars))
		   (vector-push-extend chars words)
		   (setf chars (make-char-stretchy-vector)))
	       (vector-push-extend char chars)))
    ;; If a word is at the end of the string without a terminator, add it
    (when (not (vector-empty-p chars))
      (vector-push-extend chars words))
    words))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter monochromes '("black" "grey" "white"))
(defparameter hues '("red" "orange" "yellow" "green" "blue" "purple"))
(defparameter colours '("magenta" "cyan" "brown" "pink" "turquoise" "mauve"))
(defparameter metals '("gold" "silver" "bronze" "platinum" "copper" 
		       "rust-coloured"))
(defparameter fabrics '("khaki" "cotton-coloured" "denim blue" 
			"suede-coloured"))
(defparameter naturals '("sky blue" "leaf green" "sea green" "sunset red"))
(defparameter artificials '("neon blue" "sunset yellow" "shocking pink" 
			    "non-repro blue" "blue-screen blue"))
(defparameter palettes (list monochromes hues colours metals fabrics naturals 
			     artificials))
;;(defparameter tone '("pale" "" "rich" "bright" "" "dark"))

(defun colour ()
  "Generate a colour description."
  ;;(concatenate-string (choose-randomly tone)
		      (choose-randomly-deep palettes)) ;;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Texture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun texture ()
  "Choose a texture."
  (choose-randomly '("halftoned" "crosshatched" "scumbled" "glazed" "sketchy" 
		     "smooth")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shape
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter geometric '("circle" "triangle" "square" "pentagon" "hexagon" 
			  "octagon"))
(defparameter form '("organic shape" "spiky shape" "irregular shape"))
(defparameter abstract-shapes (list geometric form))
(defparameter abstract-shape-treatment '("" "" "outlined"))
(defparameter building '("house" "skyscraper"))
(defparameter transport '("car" "aeroplane" "ship"))
(defparameter animal '("bird" "cat" "dog" "horse"))
(defparameter generic-shapes (list building transport animal))
(defparameter generic-shape-treatments '("" "" "" "silhouetted" "outlined" 
					 "abstracted"))

(defun shape ()
  "Generate a shape."
  (cond 
   ((> (random 1.0) 0.5)
    (choose-randomly-deep abstract-shapes))
   (t
    (choose-randomly-deep generic-shapes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aesthetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *properties* (make-hash-table :test 'equal)
  "The properties of the aesthetic that the critic currently adheres to.")

(defun list-properties ()
  "Gather the property names without their values."
  (loop for key being each hash-key of *properties*
       collect key))

(defun property-opinions ()
  "Sort the properties into likes and dislikes."
  (let ((likes '())
	(dislikes '()))
    (maphash #'(lambda (key val)
		 (if (>= val 0.0)
		     (push key likes)
		     (push key dislikes))) 
     *properties*)
    (values likes dislikes)))

(defun describe-property-opinions ()
  "Describe the current likes and dislikes."
  ;;FIXME - Replace the final comma with an and or ampersand.
  (multiple-value-bind (likes dislikes) (property-opinions)
    (format nil "I like~{ ~A~^,~}. I dislike~{ ~A~^,~}." likes dislikes)))

(defun properties-count ()
  "Get the current size of *properties*."
  (hash-table-count *properties*))

(defun new-property ()
  "Choose a new property."
  (funcall (choose-one-of (list #'shape #'colour #'texture))))

(defun new-properties (count)
  "Choose n properties."
  (loop for i below count
       collect (new-property)))

(defun set-property (prop)
  "Set valenced property."
    (setf (gethash prop *properties*)
	  (plus-or-minus-one)))

(defun set-properties (props)
  "Set valenced properties."
  (dolist (prop props)
    (set-property prop)))

(defparameter +min-properties+ 1)

(defparameter +max-properties+ 12)

(defun initialize-properties ()
  "Generate an initial set of properties."
  (setf *properties* (make-hash-table :test 'equal))
  (set-properties (new-properties (random-range +min-properties+
						+max-properties+))))

(defparameter +max-properties-to-delete+ 2)
(defparameter +max-properties-to-mutate+ 2)
(defparameter +max-properties-to-add+ 2)

(defun delete-properties ()
  "Delete 0+ properties, don't reduce properties below +min-properties+."
  ;;FIXME - Set the correct number here rather than checking with when
  (dolist (prop (choose-n-of (random +max-properties-to-mutate+)
			     (list-properties)))
    (when (> (properties-count) +min-properties+) 
	       (remhash prop *properties*))))

(defun mutate-properties ()
  "Mutate zero or more properties."
  (dolist (prop (choose-n-of (random +max-properties-to-mutate+)
			     (list-properties)))
    (setf (gethash prop *properties*)
	  (- (gethash prop *properties*))))) 

(defun add-properties ()
  "Add zero or more properties."
  (loop with remaining = (min (max +min-properties+ 
				   (random +max-properties-to-add+))
			      (- +max-properties+ (properties-count)))
	while (> remaining 0)
	do (let ((prop (new-property)))
	     (when (not (gethash prop *properties*))
	       (set-property prop)
	       (decf remaining)))))

;; How to handle ambiguities? yellow vs sunset yellow?
;; Sort keys by length
;; Find longest first
;; Remove from string
;; But what if that creates bogus sequences? Break into substrings?
;; Just always hypenate multi-word sequences?

(defun score-artwork (artwork)
  "Process the string description of the artwork to generate a score."
  (let ((clean-artwork (string-downcase artwork)))
    (loop for prop being each hash-key of *properties*
       if (search prop clean-artwork)
       sum (gethash prop *properties* 0))))

(defun evaluate-artwork (artwork)
  "Set range to max of +/-1, probably less."
  (/ (score-artwork artwork)
     (properties-count)))

(defun describe-artwork-evaluation (score)
  "Turn the -1.0..1.0 score into a verbal description."
  (cond
    ((< score -0.6) "terrible")
    ((< score -0.1) "bad")
    ((< score 0.1) "ok")
    ((< score 0.6) "good")
    (t "excellent")))

(defun critique-artwork (description identifier)
  "Verbally critique the artwork."
  (format nil 
	  "~a ~a is ~a." 
	  (choose-one-of '("I think that" "In my opinion," "I would say that"
			   "Aesthetically speaking," ))
	  identifier
	  (describe-artwork-evaluation (evaluate-artwork description))))