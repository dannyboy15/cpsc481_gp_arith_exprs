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


; Source: https://stackoverflow.com/questions/13937520/pythons-range-analog-in-common-lisp
(defun range (max &key (min 0) (step 1))
  "Create a list of numbers starting from min to max changing by step."
  (loop for n from min below max by step
    collect n))


(defun run_expr (rvars rexpr)
  "Evaluate expr using vars."
  (setq x (nth 0 rvars))
  (setq y (nth 1 rvars))
  (setq z (nth 2 rvars))

  (eval rexpr))


; Source: https://stackoverflow.com/a/3243264
(defun avg (lst)
  "Compute the average of a list."
  (let ((count 0) (sum 0))
    (dolist (n lst)
      (incf count)
      (incf sum n))
    (/ sum (float count))))


(defun population_init ()
  "Initialize a population and return it."
  (setq pool  ())

  (setq current_pop 0)
  (loop
    (setq current_pop (+ current_pop 1))
    (push (create_new_expr) pool)
    (when (>= current_pop POPULATION_SIZE) (return current_pop)))

  pool)


(defun create_new_expr ()
  "Create a new expression."
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
  "Calculate the fitness of an expression."
  (setq fitness 0)

  (loop for s in SAMPLES
    ; get the arguements and expected result from sample
    do (setq vars (reverse (cdr (reverse s))))
    do (setq expected (car (reverse s)))

    ; get the delta
    do (setq res (run_expr vars expr))
    do (setq delta (- expected res))

    ; accumulate delta
    do (setq fitness (+ fitness (abs delta))))

  ; return the fitness and the expression
  (list fitness expr))

(defun find_best_fit_expr (exprs_list)
  "Find the best fist expression from a list."
  ; set initial max to a really small number
  (setq mn 100000)
  (setq mne NIL)

  ; find the expr with lowest score
  (loop for e in exprs_list
    do (if (< (car e) mn)
         (progn
           (setq mn (car e))
           (setq mne e))))
  mne)


(defun get_gen_stats (gen exprs_list)
  "Return a list as (gen min max avg)."
  (setq scores_only ())

  ; get a list of the scores only
  (loop for e in exprs_list
    do (push (car e) scores_only))

  ; add the stats to a list
  (setq stats ())
  (push (avg scores_only) stats)
  (push (reduce 'max scores_only) stats)
  (push (reduce 'min scores_only) stats)
  (push gen stats)

  stats)


(defun make_crossed_kids (p1 p2 c1 c2)
  "Return a list of kids that result from crossing parents."
  (setq ck1 (list (car p1)))
  (setq ck2 (list (car p2)))

  (if (or (> 2 (length p1)) (> 2 (length p1)))
    ; if the length of the expr is < 2 just return the expr
    (progn
      (setq ck1 p1)
      (setq ck1 p1))
    ; otherwise cross the parents at the specified points
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

  (setq cks (list ck1 ck2))
  cks)


(defun make_mutated_kids (kids)
  "Return kids that may have been mutated."
  (setq r (random 100))
  (setq new_ks ())

  (loop for k in kids
    ; if the number 1 of 5 numbers mutate (~5%)
    do (if (or (= 20 r) (= 40 r) (= 60 r) (= 80 r) (= 100 r))
        (if (< r 50)
          ; if random number is less that 50 change the operator
          (progn
            ; pick operation
            (setq n (random (length OPERATORS)))
            (setq op (nth n OPERATORS))

            (pop k)
            (setq new_k (append (list op) k))
            (push new_k new_ks))
          ; otherwise change a random operand
          (progn
            (setq n (/ r 20))
            (setq operands (append OPERANDS_VARS (append OPERANDS_VARS (range (1+ RANGE_MAX) :min RANGE_MIN))))
            (setq m (random (length operands)))
            (setq new_k ())
            (if (< n (length k))
              (progn
                (loop
                  for i from 0 to (1- (length k))
                  do (if (eq i n)
                       (setq new_k (append new_k (list (nth m operands))))
                       (setq new_k (append new_k (list (nth i k))))))
                (push new_k new_ks))
              (progn
                (setq new_k k)
                (push new_k new_ks)))))

        (push k new_ks)))
  new_ks)


(defun next_pool_gen (pool gen)
  "Create another generation pool."
  (setq new_pool ())

  (setq l (length pool))
  (dotimes (i (/ l 2))
    (setq parent1 (pop pool))
    (setq parent2 (pop pool))
    (setq cross1 (1+ (random (1- (length parent1)))))
    (setq cross2 (1+ (random (1- (length parent2)))))

    (setq ckids (make_crossed_kids parent1 parent2 cross1 cross2))
    (setq mkids (make_mutated_kids ckids))

    (loop for k in mkids
      do (push k new_pool)))
  new_pool)


(defun write_to_file (file data)
  "Write the contents of a list of lists to a file."
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

  ; Run through generations
  (loop
    (setq current_gen (+ current_gen 1))
    (setq fitness_list ())

    ; calculate the fitness of each expression
    (loop for e in pool
      do (push (calculate_fitness e) fitness_list))

    ; get the generation statistics
    (push (get_gen_stats current_gen fitness_list) gen_stats)

    ; find the best expression and add it to our list
    (push (find_best_fit_expr fitness_list) best_fit_exprs)

    ; get the new generation
    (setq pool (next_pool_gen pool current_gen))

    (when (>= current_gen MAX_GENERATIONS) (return current_gen)))

  (write_to_file "data.csv" gen_stats)
  (print best_fit_exprs))
