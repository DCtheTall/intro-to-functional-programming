;; Arithmetic functions


;; Sum numbers from n to 0
(defun sum-range (n)
  (if (eq 0 n) 0 (+ n (sum-range (- n 1)))))


;; n factoral
(defun fact (n)
  (if (<= n 1) 1 (* n (fact (- n 1)))))


;; Sum fn(n) for i from n to 0
(defun sum-apply (fn n)
  (if (eq 0 n)
      (funcall fn 0)
      (+ (funcall fn n) (sum-apply fn (- n 1)))))


;; Sum fn(i) for i from n to 0 in steps of s
(defun sum-apply-step (fn n s)
  (if (eq 0 n)
      (funcall fn 0)
      (+ (funcall fn n)
         (sum-apply-range fn (max 0 (- n s)) s))))


;; Fibonacci
(defun fib (n)
  (labels ((f0 (n) (if (= n 0) 0 (f1 0 1 n)))
           (f1 (a b n)
             (if (= n 1)
                 b
                 (f1 b (+ a b) (- n 1)))))
          (f0 n)))

