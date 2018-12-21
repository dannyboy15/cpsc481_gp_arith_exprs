; load support functions
; (load "support_fcns")


; Global variables
(setq MAX_GENERATIONS 5)
(setq POPULATION_SIZE 5)
(setq INT_RANGE '(-9 9))
(setq OPERANDS_VARS '(x y z))
(setq OPERATORS '(+ - *))

(defun population_init ()
  (setq pool  ())

  (setq current_pop 0)
  (loop
    (setq current_pop (+ current_pop 1))
    (push (create_new_expr) pool)
    (when (>= current_pop POPULATION_SIZE) (return current_pop)))

  pool)

(defun create_new_expr ()
  'hello)

(defun calculate_fitness (expr)
  'fit)

(defun find_best_fit_expr (exps_list)
  'best_expr)

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
(progn
  (setq current_gen 0)
  (setq best_fit_exprs ())
  (setq gen_stats ())

  ; Initialize the poplution
  (setq pool (population_init))
  (print pool)

  ; Run through generations
  (loop
    (setq current_gen (+ current_gen 1))
    (setq fitness_list ())

    ; calculate the fitness of each expression
    (loop for e in pool
      do (push (calculate_fitness e) fitness_list)
      do (print fitness_list))

    ; get the generation statistics
    (push (get_gen_stats current_gen fitness_list) best_fit_exprs)

    ; find the best expression and add it to our list
    (push (find_best_fit_expr fitness_list) gen_stats)

    ; get the new generation
    (setq pool (next_pool_gen pool current_gen))
    (print pool)

    (when (>= current_gen MAX_GENERATIONS) (return current_gen)))

  (print gen_stats) ; save to file
  (print best_fit_exprs))



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
