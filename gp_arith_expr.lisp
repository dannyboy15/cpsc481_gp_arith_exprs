; GP Framework
; (progn)
;   (do until current pool filled ;; init population
;       create new expr
;       add expr to current pool )
;   (do until exceed terminal generation count
;       bump generation count
;       (for each expr in current pool
;            calc fitness for expr )
;       save a copy of most fit expr for this generation
;       (do until no more exprs in current pool
;           select 2 exprs as parents
;           remove parents from current pool
;           select crossover point/node in each parent
;           make crossed kids
;           expose each kid to mutation
;           add kids to next pool )
;       current pool = next pool ))

; load support functions
; (load "support_fcns")

; Seed the random function
(setf *random-state* (make-random-state t))

; Global variables
(setq MAX_GENERATIONS 5)
(setq POPULATION_SIZE 5)
(setq RANGE_MIN -9)
(setq RANGE_MAX 9)
(setq OPERANDS_VARS '(x y z))
(setq OPERATORS '(+ - *))
(setq MAX_NUM_ARGS 4)
; (setq SAMPLES '((0 -2 1 -16)
;                 (-4 -5 -3 58)
;                 (9 8 -6 72)
;                 (9 -7 5 113)
;                 (-8 7 3 150)
;                 (5 4 -5 20)
;                 (6 -4 6 41)
;                 (-5 3 -7 -24)
;                 (-6 -5 9 -18)
;                 (1 0 2 2)))
(setq SAMPLES '((0 -2 1 -16)))


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

  fitness)

(defun find_best_fit_expr (exprs_list)
  (reduce 'max exprs_list))

(defun get_gen_stats (gen expr_list)
  (setq rand_list ())
  (push 'stats rand_list)
  (push gen rand_list))


(defun next_pool_gen (pool gen)
  (setq new_pool ())
  (loop for e in pool
    do (push gen new_pool))

  new_pool)

; Main program
; (progn
;   (setq current_gen 0)
;   (setq best_fit_exprs ())
;   (setq gen_stats ())
;
;   ; Initialize the poplution
;   (setq pool (population_init))
;   (print pool)
;
;   ; Run through generations
;   (loop
;     (setq current_gen (+ current_gen 1))
;     (setq fitness_list ())
;
;     ; calculate the fitness of each expression
;     (loop for e in pool
;       do (push (calculate_fitness e) fitness_list)
;       do (print fitness_list))
;
;     ; get the generation statistics
;     (push (get_gen_stats current_gen fitness_list) best_fit_exprs)
;
;     ; find the best expression and add it to our list
;     (push (find_best_fit_expr fitness_list) gen_stats)
;
;     ; get the new generation
;     (setq pool (next_pool_gen pool current_gen))
;     (print pool)
;
;     (when (>= current_gen MAX_GENERATIONS) (return current_gen)))
;
;   (print gen_stats) ; save to file
;   (print best_fit_exprs))

; (print (population_init))
; (setq e (create_new_expr))
; (print e)
; (print (calculate_fitness e))
; (print (find_best_fit_expr '(9 -7 5 113)))

(setq l (list (create_new_expr) (create_new_expr)))
(print l)
(setq lf (list (calculate_fitness (car l)) (calculate_fitness (car (cdr l)))))
(print lf)
(print (find_best_fit_expr lf))




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
