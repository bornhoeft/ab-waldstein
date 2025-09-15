;;;---------------------------------------------------------
;;; AB88 - Waldstein
;;; Copyright © 2018 Achim Bornhoeft
;;;
;;; Lisp functions
;;;---------------------------------------------------------

(defun dx-x (ls &optional (st 0))
  "Constructs a list of numbers from <start> with the consecutives intervals of <ls>.
  <ls> can also be a list of lists of intervals. Used convert rhythms to entry points."
  (labels ((dx-x-fun (ls &optional (st 0))
             (let ((r st))     
               (loop for i in ls
                 for n = (+ r i)
                 collect n into reslis
                 do (setf r n)
                 finally (return (cons st reslis))))))
    (if (numberp (first ls))
      (dx-x-fun ls st)
      (loop for i in ls collect (dx-x-fun i st)))))
;; (dx-x '(2 2 1 2 2 2 1) 60) ;; midis
;; (dx-x '(1 1 2 3 5 8 13) 0) ;; time entry points, fibonacci
;; (dx-x '(0.5 0.5 0.25 0.25 0.25 0.3)) => (0 0.5 1.0 1.25 1.5 1.75 2.05)
;; (dx-x '(0.5 0.5 0.25 0.25 0.25 0.3) -4) => (0 0.5 1.0 1.25 1.5 1.75 2.05)

(defun group-lst (inlst sublst)
  "group list into sublists"
(let ((pl (dx-x sublst)))
  (loop for i in pl
    for j in (cdr pl)
    collect (subseq inlst i j))))
;; (group-lst '(1 2 3 4 5 6 7 8 9) '(2 3 4))

(defun g+ (val1 val2)
    "Version of +. Both arguments can be numbers or lists"
(let* ((va1 (if (numberp val1) (list val1) val1))
       (va2 (if (numberp val2) (list val2) val2))
       (v1 (if (numberp val1) (loop repeat (length va2) collect val1) va1))
       (v2 (if (numberp val2) (loop repeat (length va1) collect val2) va2)))
       (loop for i in v1
         for j in v2
         collect (+ i j) into reslis
         finally (return (if (equal (length reslis) 1)
                           (first reslis)
                           reslis)))))
;; (g+ 4 2) => 6
;; (g+ 2 4) => 6
;; (g+ 4 '(1 2 3)) => (5 6 7)
;; (g+ '(1 2 3) 4) => (5 6 7)
;; (g+ '(1 2 3) '(2 3 4)) => (3 5 7)
;; (g+ '(1 2 3) '(2 3)) => (3 5)

(defun g- (val1 val2)
    "Version of -. Both arguments can be numbers or lists"
(let* ((va1 (if (numberp val1) (list val1) val1))
       (va2 (if (numberp val2) (list val2) val2))
       (v1 (if (numberp val1) (loop repeat (length va2) collect val1) va1))
       (v2 (if (numberp val2) (loop repeat (length va1) collect val2) va2)))
       (loop for i in v1
         for j in v2
         collect (- i j) into reslis
         finally (return (if (equal (length reslis) 1)
                           (first reslis)
                           reslis)))))
;; (g- 4 2) => 2
;; (g- 2 4) => -2
;; (g- 4 '(1 2 3)) => (3 2 1)
;; (g- '(1 2 3) 4) => (-3 -2 -1)
;; (g- '(1 2 3) '(2 3 4)) => (-1 -1 -1)
;; (g- '(1 2 3) '(2 3)) => (-1 -1)

(defun g* (val1 val2)
    "Version of *. Both arguments can be numbers or lists"
(let* ((va1 (if (numberp val1) (list val1) val1))
       (va2 (if (numberp val2) (list val2) val2))
       (v1 (if (numberp val1) (loop repeat (length va2) collect val1) va1))
       (v2 (if (numberp val2) (loop repeat (length va1) collect val2) va2)))
       (loop for i in v1
         for j in v2
         collect (* i j) into reslis
         finally (return (if (equal (length reslis) 1)
                           (first reslis)
                           reslis)))))
;; (g* 4 2) => 8
;; (g* 2 4) => 8
;; (g* 4 '(1 2 3)) => (4 8 12)
;; (g* '(1 2 3) 4) => (4 8 12)
;; (g* '(1 2 3) '(2 3 4)) => (2 6 12)
;; (g* '(1 2 3) '(2 3)) => (2 6)

(defun g/ (val1 val2)
    "Version of /. Both arguments can be numbers or lists"
(let* ((va1 (if (numberp val1) (list val1) val1))
       (va2 (if (numberp val2) (list val2) val2))
       (v1 (if (numberp val1) (loop repeat (length va2) collect val1) va1))
       (v2 (if (numberp val2) (loop repeat (length va1) collect val2) va2)))
       (loop for i in v1
         for j in v2
         collect (/ i j) into reslis
         finally (return (if (equal (length reslis) 1)
                           (first reslis)
                           reslis)))))
;; (g/ 4 2) => 2
;; (g/ 2 4) => 1/2
;; (g/ 4.0 '(1 2 3)) => (4.0 2.0 1.3333334)
;; (g/ '(1 2 3) 4) => (1/4 1/2 3/4)
;; (g/ '(1 2 3) '(2 3 4)) => (1/2 2/3 3/4)
;; (g/ '(1 2 3) '(2 3)) => (1/2 2/3)

(defmacro bao (arith val1 val2)
    "Basic Arithmetic Operationa (BAO). Both arguments can be numbers or lists."
`(let* ((va1 (if (numberp ,val1) (list ,val1) ,val1))
       (va2 (if (numberp ,val2) (list ,val2) ,val2))
       (v1 (if (numberp ,val1) (loop repeat (length va2) collect ,val1) va1))
       (v2 (if (numberp ,val2) (loop repeat (length va1) collect ,val2) va2)))
       (loop for i in v1
         for j in v2
         collect (,arith i j) into reslis
         finally (return (if (equal (length reslis) 1)
                           (first reslis)
                           reslis)))))
;; (bao / '(1 2 3) '(2 3 4))

;;; with seed
(defun +-1 ()
  ""
       (let ((r (random 2)))
                 (if (= r 0) -1 1)))
;; (loop repeat 10 collect (+-1)) => (1 -1 1 -1 1 -1 -1 -1 -1 1)

(defun quant (num steps)
"Quantize a number to a defined step value (number or list of numbers)."
  (labels ((index-no (val lis)
    "Returns the list index of key in row."
  (if (not (member val lis))
    (format t "Value is not in list!")
    (loop for i in lis
       for y from 0 do
       (when (= i val) (return y))))))
  (let* ((stp (if (numberp steps) (list steps) steps))
	 (round-lis (loop for i in stp collect (* (round (/ num i)) i)))
	 (diff-lis (loop for i in round-lis collect (abs (- i num))))
	 (smallest (apply #'min diff-lis)))
    (nth (index-no smallest diff-lis) round-lis))))

;; (quant 0.13 '(0.25 0.33))
;; (loop for i in (gen-transition 1/8 7/4 20 0.85) collect (quant i '(1/8 1/12)))
;; (quantize (float-to-ratio (gen-transition 1/8 7/4 20 1) :ratio 1) '(1 2 4) :type :ratio)

(defun round-even (val)
  (let ((rv (round val))
        (dec (nth-value 1 (floor val))))
    (if (and (oddp rv) (< rv val))
      (+ val (- 1 dec))
      (if (and (oddp rv) (> rv val))
      (- val dec)
        (if (oddp rv)
          (* 1.0 (+ val 1))
          rv)))))

;; (round-even 1.4) => 2
;; (round-even 0.9) => 0
;; (round-even 1) => 2
;; (round-even 0.8) => 0
;; (round-even 11.7) => 12
;; (round-even 5.5) => 6
;; (round-even 4.99) => 4

(defun cycle (lis rep &key (abs nil))
  ""
  (loop repeat rep 
    for i from 0 
    if abs
    collect  (nth (mod i (length lis)) lis)
    else
    append lis
    end))

;; (cycle '(1 2 3 4) 3)
;; (cycle '(1 2 3 4) 11 :abs t)

(defun half-list (lis &key (type))
  (let* ((lenlis (length lis))
         (listest (oddp lenlis))
         (half (round (* 0.5 lenlis)))
         (half-elt-right (nth half lis))
         (half-elt-left (nth (- half 1) lis)))
    (if listest
      half-elt-right
      (case type
        (left half-elt-left)
        (right half-elt-right)
        (otherwise (list half-elt-left half-elt-right))))))

;; (half-list '(3 5 4 6 8))
;; (half-list '(3 5 4 6 8 1))
;; (half-list '(3 5 4 6 8 1) :type 'left)
;; (half-list '(3 5 4 6 8 1) :type 'right)
;; (half-list '(3 5 4 6 8) :type 'right)

(defun sample-env (env samples &key type)
  "Taking any number of samples from an envelope of x/y-values.
  Envelope can be a list of xy-value pairs or a list containing 
  a list of x-values and a list of y-values. Output is the same
  format as the input by default. It can be declared with the
  type keyword :xy = 1 list, :x-y = 2 lists."
  (labels ((split-xy (lst)
             "Split a list of x/y-values into a list of x-values 
              and a list of y-values."
             (loop for i from 0 to (- (length lst) 1) by 2
               for j from 1 by 2
               collect (nth i lst) into xlis
               collect (nth j lst) into ylis
               finally (return (list xlis ylis))))
           (resample-x (lst samples)
             "Resample a list of x-values to any number of 
              equidistant x-values in the same range."
             (let* ((start (first lst))
                    (end (first (reverse lst)))
                    (dist (- end start))
                    (stp (/ dist (- samples 1))))
               (loop repeat samples
                 for i from start by stp collect (* 1.0 i))))
           (map-range (a1 a2 b1 b2 s)
             "Calculates the value s according to a range between 
              a1 and a2 into a new range between b1 and b2."
             (* 1.0 (+ b1
                       (/ (* (- s a1)
                             (- b2 b1))
                          (- a2 a1))))))
    (let* ((type? (numberp (first env)))
           ;; if type? T then its a xy-list otherwise 2 lists.
           (xy (if type? (split-xy env)))
           (xlst (if type? (first xy) (first env)))
           (ylst (if type? (second xy) (second env)))
           (rs-x (resample-x xlst samples)))
      (loop for i in rs-x
        for j =  (loop 
          for x in xlst
          for xc in (cdr xlst)
          for y in ylst
          for yc in (cdr ylst)
          for r1 = (map-range x xc 0 1 i)
          for r2 = (map-range 0 1 y yc r1)
          when (>= xc i x) do (return r2))
        collect i into results
        collect j into results
        collect j into y-lis
        finally (case type
                  (:xy (return results))
                  (:x-y (return (list rs-x y-lis)))
                  (:y (return y-lis))
                  (otherwise
                        (if type? 
                           (return results) 
                           (return (list rs-x y-lis)))))))))

;; (sample-env '(0 1 1 3 2 2 3 4) 5) 
;; => (0.0 1.0 0.75 2.5 1.5 2.5 2.25 2.5 3.0 4.0)
;; (sample-env '((0 1 2 3) (1 3 2 4)) 5) 
;; => ((0.0 0.75 1.5 2.25 3.0) (1.0 2.5 2.5 2.5 4.0))

;; (sample-env '(0 1 1 3 2 2 3 4) 5 :type :x-y)
;; => ((0.0 0.75 1.5 2.25 3.0) (1.0 2.5 2.5 2.5 4.0))
;; (sample-env '((0 1 2 3) (1 3 2 4)) 5 :type :xy)
;; =>(0.0 1.0 0.75 2.5 1.5 2.5 2.25 2.5 3.0 4.0)
;; (sample-env '((0 1 2 3) (1 3 2 4)) 5 :type :y)
;; => (1.0 2.5 2.5 2.5 4.0)

(defun quant (num steps)
"Quantize a number to a defined step value (number or list of numbers)."
  (labels ((index-no (val lis)
    "Returns the list index of key in row."
  (if (not (member val lis))
    (format t "Value is not in list!")
    (loop for i in lis
       for y from 0 do
       (when (= i val) (return y))))))
  (let* ((stp (if (numberp steps) (list steps) steps))
	 (round-lis (loop for i in stp collect (* (round (/ num i)) i)))
	 (diff-lis (loop for i in round-lis collect (abs (- i num))))
	 (smallest (apply #'min diff-lis)))
    (nth (index-no smallest diff-lis) round-lis))))

;; (loop for i in (gen-transition 1/8 7/4 20 0.85) collect (quant i '(1/8 1/12)))

(defun average (numbers)
  "Numerical average (mean) of a list of numbers."
  (* 1.0 (/ (reduce #'+ numbers) (length numbers))))


(defun scaling-sum (new-sum lis)
  (let* ((sum (reduce #'+ lis))
    (sum-factor (/ new-sum sum)))
    (loop for i in lis
      collect (* sum-factor i))))

;; (scaling-sum 30 '(1 2 3 4 5)) => (2 4 6 8 10)
;; (reduce #'+ (scaling-sum 30 '(1 2 3 4 5))) => 30

(defun half-list (lis &key (type))
  (let* ((lenlis (length lis))
         (listest (oddp lenlis))
         (half (round (* 0.5 lenlis)))
         (half-elt-right (nth half lis))
         (half-elt-left (nth (- half 1) lis)))
    (if listest
      half-elt-right
      (case type
        (left half-elt-left)
        (right half-elt-right)
        (otherwise (list half-elt-left half-elt-right))))))

;; (half-list '(3 5 4 6 8))
;; (half-list '(3 5 4 6 8 1))
;; (half-list '(3 5 4 6 8 1) :type 'left)
;; (half-list '(3 5 4 6 8 1) :type 'right)
;; (half-list '(3 5 4 6 8) :type 'right)
;;; WRONG:
;; (half-list '(0 1 2 3 4 5 6 7 8 9 10))