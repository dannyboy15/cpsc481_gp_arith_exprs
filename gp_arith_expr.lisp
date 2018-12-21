;; ----------------------------------------------------------- deep-copy ----
; (defun deepcopy ( rn )
;;  (cond
;;    ((not (listp rn)) rn)
;;   (T (let ((lx (deepcopy(car rn)))
;;             (rx (deepcopy (cdr rn))))
;;            (cons lx rx)))))
