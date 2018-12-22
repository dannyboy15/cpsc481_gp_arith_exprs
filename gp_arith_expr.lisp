;; GP Arithmetic Expressions
;; Authors:
;; - Daniel Bravo - [bravod@csu.fullerton.edu]
;; - Barry Ibarra - [barryjr01@csu.fullerton.edu]
;;
;; Description
;; This program attempts to simulate the fitness of expressions
;; over the course of various generations. See the general
;; framework below.
;;
;; GP Framework
;; (progn)
;;   (do until current pool filled ;; init population
;;       create new expr
;;       add expr to current pool )
;;   (do until exceed terminal generation count
;;       bump generation count
;;       (for each expr in current pool
;;            calc fitness for expr )
;;       save a copy of most fit expr for this generation
;;       (do until no more exprs in current pool
;;           select 2 exprs as parents
;;           remove parents from current pool
;;           select crossover point/node in each parent
;;           make crossed kids
;;           expose each kid to mutation
;;           add kids to next pool )
;;       current pool = next pool ))

; load support functions
; (load "support_fcns")

; Seed the random function
(setf *random-state* (make-random-state t))

; Global variables
(setq MAX_GENERATIONS 50)
(setq POPULATION_SIZE 50)
(setq RANGE_MIN -9)
(setq RANGE_MAX 9)
(setq OPERANDS_VARS '(x y z))
(setq OPERATORS '(+ - *))
(setq MAX_NUM_ARGS 4)
(setq SAMPLES '((0 -2 1 -16)
                (-4 -5 -3 58)
                (9 8 -6 72)
                (9 -7 5 113)
                (-8 7 3 150)
                (5 4 -5 20)
                (6 -4 6 41)
                (-5 3 -7 -24)
                (-6 -5 9 -18)
                (1 0 2 2)))
; (setq SAMPLES '((0 -2 1 -16)))


; Source: https://stackoverflow.com/questions/13937520/pythons-range-analog-in-common-lisp
(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))


(defun run_expr (rvars rexpr)
  "Evaluate expr using vars."
  (setq x (nth 0 rvars))
  (setq y (nth 1 rvars))
  (setq z (nth 2 rvars))

  (eval rexpr))


; Source: https://stackoverflow.com/a/3243264
(defun avg (the-list)
  (let ((count 0) (sum 0))
    (dolist (n the-list)
      (incf count)
      (incf sum n))
    (/ sum (float count))))


(defun population_init ()
  (setq pool  ())

  (setq current_pop 0)
  (loop
    (setq current_pop (+ current_pop 1))
    (push (create_new_expr) pool)
    (when (>= current_pop POPULATION_SIZE) (return current_pop)))

  pool)


(defun create_new_expr ()
  ; pick operation
  (setq n (random (length OPERATORS)))
  (setq op (nth n OPERATORS))

  ; pick number of args
  (setq num_args (random MAX_NUM_ARGS))

  ; create list of operands
  (setq operands (append OPERANDS_VARS (append OPERANDS_VARS (range (1+ RANGE_MAX) :min RANGE_MIN))))

  ; pick a random value for each arg and add it to expression
  (setq expr ())
  (setq counter 0)
  (loop
    (setq counter (1+ counter))
    (setq m (random (length operands)))
    (push (nth m operands) expr)
    (when (>= counter num_args) (return counter)))

  ; add the operator
  (push op expr)

  expr)


(defun calculate_fitness (expr)
  (setq fitness 0)

  (loop for s in SAMPLES
    ; get the arguements and expected result from sample
    do (setq vars (reverse (cdr (reverse s))))
    do (setq expected (car (reverse s)))
    ; (print vars)
    ; (print expected)

    ; get the delta
    do (setq res (run_expr vars expr))
    ; (print res)
    do (setq delta (- expected res))
    ; (print delta)

    ; accumulate delta
    do (setq fitness (+ fitness (abs delta))))

  (list fitness expr))

(defun find_best_fit_expr (exprs_list)
  ; (reduce 'max exprs_list))
  (setq mx -100000)
  (setq mxe NIL)

  (loop for e in exprs_list
    do (if (> (car e) mx)
         (progn
           (setq mx (car e))
           (setq mxe e))))
  mxe)

(defun get_gen_stats (gen exprs_list)
  "Returns a list as (gen min max avg)"
  (setq scores_only ())
  (loop for e in exprs_list
    do (push (car e) scores_only))
  (setq stats ())
  (push (avg scores_only) stats)
  (push (reduce 'max scores_only) stats)
  (push (reduce 'min scores_only) stats)
  (push gen stats)

  stats)


(defun make_crossed_kids (p1 p2 c1 c2)
  (setq ck1 (list (car p1)))
  (setq ck2 (list (car p2)))

  ; (print p1)
  (format t "crossing: ~a~%" (list p1 p2))

  (if (or (> 2 (length p1)) (> 2 (length p1)))
    (progn
      (setq ck1 p1)
      (setq ck1 p1))
    (progn
      (loop
        for i from 1 to (1- c1)
        do (setq ck1 (append ck1 (list (nth i p1)))))
      (loop
        for i from c2 to (1- (length p2))
        do (setq ck1 (append ck1 (list (nth i p2)))))

      (loop
        for i from 1 to (1- c2)
        do (setq ck2 (append ck2 (list (nth i p2)))))
      (loop
        for i from c1 to (1- (length p1))
        do (setq ck2 (append ck2 (list (nth i p1)))))))


  ; ; make crossed kid 1
  ; (if (> 2 (length p1))
  ;   (progn
  ;     (loop
  ;       for i from 1 to (1- c1)
  ;       do (setq ck1 (append ck1 (list (nth i p1)))))
  ;     (loop
  ;       for i from c2 to (1- (length p2))
  ;       do (setq ck1 (append ck1 (list (nth i p2))))))
  ;   (setq ck1 p1))
  ;
  ; ; make crossed kid 2
  ; (if (> 2 (length p1))
  ;   (progn
  ;     (loop
  ;       for i from 1 to (1- c2)
  ;       do (setq ck2 (append ck2 (list (nth i p2)))))
  ;     (loop
  ;       for i from c1 to (1- (length p1))
  ;       do (setq ck2 (append ck2 (list (nth i p1))))))
  ;   (setq ck1 p1))

  (setq cks (list ck1 ck2))
  (format t "crossed: ~a~%" cks)
  cks)


(defun make_mutated_kids (kids)
  (setq r (random 100))
  (setq new_ks ())
  ; (print kids)
  (format t "before mutate: ~a~%" kids)
  (format t "r ~d~%" r)
  (loop for k in kids
    do (if (or (= 20 r) (= 40 r) (= 60 r) (= 80 r) (= 100 r))
        (if (< r 50)
          (progn
            ; pick operation
            (setq n (random (length OPERATORS)))
            (setq op (nth n OPERATORS))

            (format t "inside even k: ~a" k)
            (pop k)
            (setq new_k (append (list op) k))
            (push new_k new_ks))
          (progn
            (setq n (/ r 20))
            (setq operands (append OPERANDS_VARS (append OPERANDS_VARS (range (1+ RANGE_MAX) :min RANGE_MIN))))
            (setq m (random (length operands)))
            (setq new_k ())
            (if (< n (length k))
              (progn
                (format t "k ~d n~d~%" (length k) n)
                (loop
                  for i from 0 to (1- (length k))
                  do (if (eq i n)
                       (setq new_k (append new_k (list (nth m operands))))
                       (setq new_k (append new_k (list (nth i k))))))
                (push new_k new_ks))
              (progn
                (format t "k is~a~%" k)
                (setq new_k k)
                (push new_k new_ks)))))

        (push k new_ks)))
  (format t "mutated: ~a~%" new_ks)
  new_ks)


(defun next_pool_gen (pool gen)
  (setq new_pool ())

  ; (when pool
  ;   (format t "length of pool: ~d" (length pool))
  ;   (setq parent1 (pop pool))
  ;   (setq parent2 (pop pool))
  ;   (setq cross1 (1+ (random (1- (length parent1)))))
  ;   (setq cross2 (1+ (random (1- (length parent2)))))
  ;   ; (print cross1)
  ;   ; (print cross2)
  ;
  ;   (setq ckids (make_crossed_kids parent1 parent2 cross1 cross2))
  ;   (setq mkids (make_mutated_kids ckids))
  ;
  ;   (loop for k in mkids
  ;     do (push k new_pool)))

  (setq l (length pool))
  (dotimes (i (/ l 2))
    (format t "length of pool: ~d~%" (length pool))
    (format t "current pool: ~a~%" pool)
    (setq parent1 (pop pool))
    (setq parent2 (pop pool))
    (setq cross1 (1+ (random (1- (length parent1)))))
    (setq cross2 (1+ (random (1- (length parent2)))))
    ; (print cross1)
    ; (print cross2)

    (setq ckids (make_crossed_kids parent1 parent2 cross1 cross2))
    (setq mkids (make_mutated_kids ckids))

    (loop for k in mkids
      do (push k new_pool)))

  (format t "new pool: ~a~%" new_pool)
  new_pool)


(defun write_to_file (file data)
  (with-open-file (stream file :direction :output)
    (loop for l in data
        do (format stream "~{~A~#[~:;, ~]~}~%" l))))

; Main program
(progn
  (setq current_gen 0)
  (setq best_fit_exprs ())
  (setq gen_stats ())

  ; Initialize the poplution
  (setq pool (population_init))
  ; (print pool)

  ; Run through generations
  (loop
    (setq current_gen (+ current_gen 1))
    (setq fitness_list ())
    (format t "Current generation: ~d~%" current_gen)
    (print pool)

    ; calculate the fitness of each expression
    (loop for e in pool
      do (format t "working with: ~a~%" e)
      do (push (calculate_fitness e) fitness_list))
      ; do (print fitness_list))

    ; get the generation statistics
    (push (get_gen_stats current_gen fitness_list) gen_stats)

    ; find the best expression and add it to our list
    (push (find_best_fit_expr fitness_list) best_fit_exprs)

    ; get the new generation
    (setq pool (next_pool_gen pool current_gen))
    ; (print pool)

    (when (>= current_gen MAX_GENERATIONS) (return current_gen)))

  ; (print gen_stats) ; save to file
  (write_to_file "data.csv" gen_stats)
  (print best_fit_exprs))

; (print (population_init))
; (setq e (create_new_expr))
; (print e)
; (print (calculate_fitness e))
; (print (find_best_fit_expr '(9 -7 5 113)))

; (setq l (list (create_new_expr) (create_new_expr)))
; (print l)
; (setq lf (list (list (car l) (calculate_fitness (car l))) (list (car (cdr l)) (calculate_fitness (car (cdr l))))))
; (print lf)
; (print (find_best_fit_expr lf))
; (print (get_gen_stats 1 lf))

; (print (make_crossed_kids '(1 2 3 4 5 6 7 8 9 0) '(a b c d e f g h i j k l) 2 6))

; (print (make_mutated_kids (list '(+ 1 2 3 4) '(- 0 9 8 7 7 6))))

; (print (next_pool_gen '((+ 1 2 3 4 5) (- 0 9 8 7 6)) 1))




; Print out the best expr for each generation along with its score
; Print best, worst, avg

; Create our Tree

; Crossover

; Check fitness


;; ----------------------------------------------------------- deep-copy ----
; (defun deepcopy ( rn )
;;  (cond
;;    ((not (listp rn)) rn)
;;   (T (let ((lx (deepcopy(car rn)))
;;             (rx (deepcopy (cdr rn))))
;;            (cons lx rx)))))
