;; Basic time functions which use lists
;; of the format (hour minute second)
;; where each element is an integer


;; Return number of seconds since the start of the day from the time
(defun to-secs (time)
  (let ((hour (car time))
        (minute (car (cdr time)))
        (sec (car (cdr (cdr time)))))
       (+ sec (* 60 (+ minute (* 60 hour))))))


;; Return the time from seconds since the start of the day
(defun from-secs (secs)
  (cons (floor (/ secs 3600))
        (cons (floor (/ (mod secs 3600) 60))
              (cons (mod secs 60) nil))))


;; Tick the input time 1 second
(defun tick (time)
  (from-secs (+ 1 (to-secs time))))


;; The following functions are for list
;; representations of transactions for
;; an item (string) and the time the
;; transaction was made
;; (item (hour minute second))


;; Get time transaction was made
(defun get-time (item) (car (cdr item)))


;; Determine if the time item1 was made is before item2
(defun item-time-less (item1 item2)
  (< (to-secs (get-time item1))
     (to-secs (get-time item2))))


;; Insert an item into a list of items of ascending order
(defun item-insert (item lst)
  (cond ((null lst)
         (cons item nil))
        ((item-time-less item (car lst))
         (cons item lst))
        (t (cons (car lst)
                 (item-insert item (cdr lst))))))


;; Sort items by time in ascending order
(defun item-insert-sort (lst)
  (if (null lst) lst
      (item-insert (car lst)
                   (item-insert-sort (cdr lst)))))


