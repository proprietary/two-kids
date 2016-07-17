;; "I have two kids, and I'll tell you that one of them at least is a girl. What would you wager that you can guess whether my other child is a boy or a girl?"

(require 'cl-lib)

(defun average (x y)
  (/ (+ (float x)
        (float y))
     2))

(defun make-n-kids (n)
  (if (zerop n) '()
    (cons (elt '(♂ ♀)
               (random 2))
          (make-n-kids (1- n)))))

(defun make-at-least-one-girl (count)
  (let ((kids (make-n-kids 2)))
    (if (member '♀ kids) (list kids count)
      (make-at-least-one-girl (1+ count)))))

(defun prob-male (n-tries)
  (let* ((lst (cl-loop for x from 0 to n-tries
                       collect (make-at-least-one-girl 0)))
         (non-one-girl (apply #'+ (mapcar #'cadr lst)))
         (cnt (length lst))
         (cnt-males (length (cl-remove-if-not (lambda (x)
                                                (member '♂ x))
                                              lst))))
    (/ (+ (float cnt-males)
          (float non-one-girl))
       (float cnt))))

(prob-male 100000) ;; => 0.334...
