;; -------------- support functions tests --------------------------------------
(load "support_fcns")

;; ----------------- cell-count ------------------------------------------------
(cell-count '(a b c))
;;3
(cell-count '(a (b) c))
;;4
(cell-count '(a))
;;1

;; --------------------------- tree-nth ----------------------------------------
(tree-nth 1.3 '(a))
;; nil
(tree-nth 1 nil)
;; nil
(tree-nth 1 'a)
;; nil
(tree-nth 1 '(a))
;; a
(tree-nth 1 '(a b c))
;; a
(tree-nth 1 '((a) b c))
;; (a)
(tree-nth 2 '(a b c))
;; b
(tree-nth 2 '(a (b) c))
;; (b)
(tree-nth 3 '(a b c))
;; c
(tree-nth 4 '(a b c))
;; nil
(tree-nth 3 '(a (b) c))
;; b
(tree-nth 3 '((a f) b c))
;; f
(tree-nth 4 '((a f) b c))
;; b
(tree-nth 5 '((a f) b c))
;; c
(tree-nth 6 '((a f) b c))
;; nil
(tree-nth 6 '((a f) b c))
;; nil
(tree-nth 6 '((a f) ((b)) c))
;; b
(tree-nth 7 '((a f) ((b)) c))
;; c


;; ------------------------------------------------------- tree-nth-cell ----
(tree-nth-cell 1 nil)
;; nil
(tree-nth-cell 1 '(a b c))
;; (a b c)
(tree-nth-cell 2 '(a b c))
;; (b c)
(tree-nth-cell 3 '(a b c))
;; (c)
(tree-nth-cell 1 '((a b) c))
;; ((a b) c)
(tree-nth-cell 2 '((a b) c))
;; (a b)
(tree-nth-cell 3 '((a b) c))
;; (b)
(tree-nth-cell 4 '((a b) c))
;; (c)
(tree-nth-cell 5 '((a b) c))
;; nil
(tree-nth-cell 2 '((a f) ((b)) c))
;; (a f)
(tree-nth-cell 3 '((a f) ((b)) c))
;; (f)
(tree-nth-cell 4 '((a f) ((b)) c))
;; (((b)) c)
(tree-nth-cell 5 '((a f) ((b)) c))
;; ((b))
(tree-nth-cell 6 '((a f) ((b)) c))
;; (b)
(tree-nth-cell 7 '((a f) ((b)) c))
;; (c)
(tree-nth-cell 8 '((a f) ((b)) c))
;; nil


;; ---------------------------------------------------- random-tree-cell ----
(random-tree-cell '(+ (* 5 a b) (* c (- d 6))))
;; ((* c (- d 6))) ;; No op
;; (- d 6) ;; Has op
;; ((* 5 a b) (* c (- d 6))) ;; No op
;; (* 5 a b) ;; Has op
;; (+ (* 5 a b) (* c (- d 6))) ;; Has op
;; (* c (- d 6)) ;; Has op


;; ------------------------------------------------------------ make-kid ----


;; ------------------------------------------------------- test-make-kid ----
(cell-count '(+ (* 5 a b) (* c (- d 6))))
;; 13
(test-make-kid '(+ (* 5 a b) (* c (- d 6))))
(:dbg :tgt (c (- d 6)))
;; (+ (* 5 a b) (* 8 9))
(:dbg :tgt ((- d 6)))
;; (+ (* 5 a b) (* c 8 9))
(:dbg :tgt ((- d 6)))
;; (+ (* 5 a b) (* c 8 9))
(:dbg :tgt (d 6))
;; (+ (* 5 a b) (* c (- 8 9)))
(:dbg :tgt (5 a b))
;; (+ (* 8 9) (* c (- d 6)))
(:dbg :tgt (a b))
;; (+ (* 5 8 9) (* c (- d 6)))
(:dbg :tgt ((* 5 a b) (* c (- d 6))))
;; (+ 8 9)
(:dbg :tgt ((- d 6)))
;; (+ (* 5 a b) (* c 8 9))
(:dbg :tgt (6))
;; (+ (* 5 a b) (* c (- d 8 9)))


;; -------------------------------------------------- get-front-upto-nth ----
(setq my-list '((1 a) (2 b) (3 c) (4 d) (5 e) (6 f) (7 g)))
(get-front-from-nth 4 my-list)
;; ((1 a) (2 b) (3 c) (4 d) (5 e))
(get-front-from-nth 2 my-list)
;; ((1 a) (2 b) (3 c))


;; ---------------------------------------------------------- get-score ----
(get-score '(+ 3 4))
;; 3


;; ---------------------------------------------------------- score-pop ----
(setq my-pop '((a b c)
;; (a)
;; (e f g)
;;                (a d)))
;; ((a b c) (a) (e f g) (a d))
(setq my-pop-scored (score-pop my-pop))
;; ((3 (a b c)) (1 (a)) (3 (e f g)) (2 (a d)))

;; ------------------------------------------------ safe-sort-scored-pop ----
;; my-pop-scored
;; ((3 (a b c)) (1 (a)) (3 (e f g)) (2 (a d)))
(safe-sort-scored-pop my-pop-scored)
;; ((1 (a)) (2 (a d)) (3 (a b c)) (3 (e f g)))
;; my-pop-scored
;; ((3 (a b c)) (1 (a)) (3 (e f g)) (2 (a d)))

;; ------------------------------------------------- get-pop-from-scored ----
;; my-pop-scored
;; ((3 (a b c)) (1 (a)) (3 (e f g)) (2 (a d)))
(get-pop-from-scored my-pop-scored)
;; ((a b c) (a) (e f g) (a d))
