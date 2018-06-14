;; List functions


;; Test if a list (lst) starts with sublist (sublst)
(defun list-starts (sublst lst)
  (cond ((null sublst) t)
        ((null lst) nil)
        ((eq (car lst) (car sublst))
          (list-starts (cdr sublst) (cdr lst)))
        (t nil)))


;; Test if a list (lst) contains a sublst (sublst)
(defun list-contains (sublst lst)
  (cond ((list-starts sublst lst) t) ;; checks if sublst is nil here and returns true
        ((null lst) nil) ;; if sublst is not nil and lst is, return nil
        (t (list-contains sublst (cdr lst)))))


;; Return how many times a list (lst) contains a sublist (sublst)
(defun num-list-contains (sublst lst)
  (cond ((null lst) 0)
        ((list-starts sublst lst)
          (+ 1 (num-list-contains sublst (cdr lst))))
        (t (num-list-contains sublst (cdr lst))))))


;; Remove a sublist (sublst) from a list(lst) if it starts with it
(defun list-start-remove (sublst lst)
  (if (and (list-starts sublst lst) (not (null sublst)))
      (list-start-remove (cdr sublst) (cdr lst))
      lst))


;; Remove a sublst (sublst) from a list (lst) on its first occurence
(defun list-remove (sublst lst)
  (if (list-starts sublst lst)
      (list-start-remove sublst lst)
      (cons (car lst) (list-remove sublst (cdr lst)))))


;; Concat two lists
(defun concat (left right)
  (if (null left)
      right
      (cons (car left) (concat (cdr left) right))))


;; Insert new list into a list after first ocurrence of a sublist
(defun list-insert (newlst sublst lst)
  (if (list-starts sublst lst)
      (concat sublst
             (concat newlst
                    (list-start-remove sublst lst)))
      (cons (car lst)
            (list-insert newlst sublst (cdr lst)))))


;; Replace a sublist in a list with a new one
(defun list-replace (newlst sublst lst)
  (if (list-starts sublst lst)
      (concat newlst
             (list-start-remove sublst lst))
      (cons (car lst)
            (list-replace newlst sublst (cdr lst)))))


;; Merge a pair of ordered lists into a single ordered list
(defun list-merge (left right)
  (cond ((null left) right)
        ((null right) left)
        ((< (car left) (car right))
          (cons (car left)
                (list-merge (cdr left) right)))
        (t (cons (car right)
                 (list-merge left (cdr right))))))


;; Merges a list of ordered lists
(defun list-merge-flatten (lst)
  (if (null lst)
      lst
      (list-merge (car lst)
                  (list-merge-flatten (cdr lst)))))


;; Map
(defun list-map (fn lst)
  (if (null lst)
      lst
      (cons (funcall fn (car lst))
            (list-map fn (cdr lst)))))


;; Filter
(defun list-filter (fn lst)
  (if (null lst)
      lst
      (if (funcall fn (car lst))
          (cons (car lst) (list-filter fn (cdr lst)))
          (list-filter fn (cdr lst)))))


;; Reduce
(defun list-reduce (fn lst val)
  (if (null lst)
      val
      (list-reduce fn (cdr lst) (funcall fn val (car lst)))))


;; List length
(defun list-length (lst)
  (labels ((l (n lst)
                (if (null lst)
                    n
                    (l (+ n 1) (cdr lst)))))
          (l 0 lst)))

