 ;;-------------- support functions tests --------------------------------------
(load "support_fcns")

;; ----------------- cell-count ------------------------------------------------
(cell-count '(a b c))
;;3
(cell-count '(a (b) c))
;;4
(cell-count '(a))
;;1

;; ------------------------------------------------------- tree-nth-cell ----
(defun tree-nth-cell (rnth rtree)
  "Return the DFS N-th cell in the given tree: 1-based."
  (let ((size (cell-count rtree)))
    ;;(print (list :dbga rnth size (car rtree)))
    (cond
     ((not (integerp rnth)) nil)
     ((not (listp rtree)) nil) ;; Not a tree?
     ((null rtree) nil) ;; No tree elts?
     ((= 1 rnth) rtree) ;; 1st elt of list is the tree, itself.
     ((>= 0 rnth) nil) ;; Nth 0 or negative?
     ((> rnth size) nil) ;; N-th beyond tree's end?
     (t ;; Elt is in car subtree or cdr "subtree".
      (setq rnth (1- rnth)) ;;Account: Elt isn't the current (car+cdr's) node.
      (let ((size1 (cell-count (car rtree))))
        ;;(print (list :dbgb rnth size1 (car rtree)))
        (cond
         ((>= 0 size1) (tree-nth-cell ;; No car subtree.
                        rnth
                        (cdr rtree))) ;; Find elt in the cdr subtree.
         ((<= rnth size1) (tree-nth-cell ;;  Elt is in car subtree.
rnth
                           (car rtree))) ;; Find elt in the car subtree.
         (t (tree-nth-cell ;; Elt is in cdr subtree.
             (- rnth size1) ;; Account for skipping car subtree.
             (cdr rtree))))))))) ;; Skip car subtree.


;; ---------------------------------------------------- random-tree-cell ----
(defun random-tree-cell (rtree)
  "Return random cell in the tree, but not the whole tree."
  (let* ((size (cell-count rtree))
         (rx (1+ (random (1- size)))) ;; Avoid 1st cell (the whole tree).
         (nth (1+ rx)) ;; Incr cuz our fcn is 1-based, not 0-based.
         (spot (tree-nth-cell nth rtree)))
    ;; (print (list :dbg size nth spot))
    spot))

;; ------------------------------------------------------------ make-kid ----
(defun make-kid (rmom rtgt rnew)
  "Return kid: copy of mom with tgt cell replaced by given new cell, or nil."
  (if (not (and rmom rtgt rnew
                (listp rmom)
                (listp rtgt)
                (listp rnew)))
      rmom
    (if (eq rmom rtgt)
        rnew
      (cons (make-kid (car rmom) rtgt rnew)
            (make-kid (cdr rmom) rtgt rnew)))))

;; ------------------------------------------------------- test-make-kid ----
(defun test-make-kid (rtree)
  "Test make-kid with random tgt cell and fixed replacement list."
  (let* ((tgt (random-tree-cell rtree))
         (newop '(/ 2 3)) ;; New has op.
         (newnop '(8 9)) ;; New has no op.
         (ops '(+ - * /)))
    (print (list :dbg :tgt tgt))
    (make-kid rtree
              tgt
              (if (member (car tgt) ops) ;; Tgt also has an op?
                  newop
                newnop))))


;; -------------------------------------------------- get-front-upto-nth ----
(defun get-front-upto-nth ( rn rlist )
  "Return list head from 0-th thru N-th elt.  Assumes elt-n is unique."
  (let ((elt-n (nth rn rlist)))
    (reverse (member elt-n (reverse rlist)))))


;; ---------------------------------------------------------- get-score ----
(defun get-score (rcritter)
  "Get score for critter.  Dummy fcn: just return its length."
  (length rcritter))


;; ---------------------------------------------------------- score-pop ----
(defun score-pop ( rpop ) ;; Pop is a population.
  "Create Pop-Scored pairs (Score Critter) given Pop list of critters."
  (mapcar #'(lambda (critter)
              (let ((score (get-score critter)))
                (list score critter)))
          rpop))

;; ------------------------------------------------ safe-sort-scored-pop ----
(defun safe-sort-scored-pop ( rscored-pop )
  "Return a sorted list of scored-critter elts.  Don't change given list.
   NB, your Lisp's built-in sort fcn may damage the incoming list."
  (let ((sacrifice-list (copy-list rscored-pop)))
    (sort sacrifice-list
          #'(lambda (scored-critter-1 scored-critter-2)
              (< (car scored-critter-1) (car scored-critter-2))))))

-- get-pop-from-scored ----
(defun get-pop-from-scored (rscored-pop)
  "Return just the Pop of critters from the Scored Pop."
  ;;Alt: (mapcar #'(lambda (elt) (nth 1 elt)) rscored-pop)
  (mapcar #'cadr rscored-pop))


