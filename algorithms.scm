(define input
  (map (lambda (x) (string->number x))
       (file->lines "/Users/tomobrien/Dropbox/AlgoClass/IntegerArray.txt")))

(define merge
  (lambda (list-left list-right precedes?)
    (cond ((null? list-left) list-right)
          ((null? list-right) list-left)
          ((precedes? (car list-left) (car list-right))
           (cons (car list-left)
                 (merge (cdr list-left) list-right precedes?)))
          ((precedes? (car list-right) (car list-left))
           (cons (car list-right)
                 (merge list-left (cdr list-right) precedes?))))))

(define merge-iter
  (lambda (left right merged count)
    (cond ((null? left) (cons (foldl cons merged right) count))
          ((null? right) (cons (foldl cons merged left) count))
          ((<= (car left) (car right))
           (merge-iter (cdr left) right (cons (car left) merged) count))
          (else
           (merge-iter left (cdr right) (cons (car right) merged)
                       (+ count (length left)))))))

(define merge-and-count
  (lambda (left right init)
    (let ((result (merge-iter left right '() init)))
      (cons (reverse (car result)) (cdr result)))))

(define count-inversions
  (lambda (list count)
    (cond ((or (null? list) (null? (cdr list))) (cons list count))
          (else
           (define half (floor (/ (length list) 2)))
           (let ((B (count-inversions (take list half) count))
                 (C (count-inversions (drop list half) count)))
             (merge-and-count (merge-sort (car B)) (merge-sort (car C))
                              (+ (cdr B) (cdr C))))))))

(define merge-sort
  (lambda (list precedes?)
    (cond ((or (null? list) (null? (cdr list))) list)
          (else
           (let ((half (floor (/ (length list) 2))))
             (merge (merge-sort (take list half) precedes?)
                    (merge-sort (drop list half) precedes?)
                    precedes?))))))

(define leq-x?
  (lambda (point1 point2)
    (<= (car point1) (car point2))))

(define leq-y?
  (lambda (point1 point2)
    (<= (cadr point1) (cadr point2))))

(define sum-squares
  (lambda (x y)
    (+ (* x x) (* y y))))

(define squared-distance
  (lambda (point1 point2)
    (sum-squares (- (car point1) (car point2))
                 (- (cadr point1) (cadr point2)))))

;; (squared-distance '(3 4) '(3 4))
;; (squared-distance '(3 4) '(0 0))
;; (squared-distance '(3 4) '(3 1))

;; (define closest-pair
;;   (lambda (points)
;;     (if (or (null? points) (null? (cdr points)))
;;         points
;;         (let ((Q-x (merge-sort (take points (/ (length points) 2)) leq-x?))
;;               (Q-y (merge-sort (take points (/ (length points) 2)) leq-y?))
;;               (R-x (merge-sort (drop points (/ (length points) 2)) leq-x?))
;;               (R-y (merge-sort (deop points (/ (length points) 2)) leq-y?)))
;;           ))))

(define test-point-set (list '(1 6) '(2 5) '(4 4) '(3 7) '(0 1) '(9 6)))
(merge-sort test-point-set leq-x?)
(merge-sort test-point-set leq-y?)
(take test-point-set 3)
(drop test-point-set 3)
(merge-sort (take test-point-set 3) leq-x?)
(merge-sort (take test-point-set 3) leq-y?)
(merge-sort (drop test-point-set 3) leq-x?)
(merge-sort (drop test-point-set 3) leq-y?)

(define form-halves-naive
  (lambda (points)
    (list (merge-sort (take points (/ (length points) 2)) leq-x?)
          (merge-sort (take points (/ (length points) 2)) leq-y?)
          (merge-sort (drop points (/ (length points) 2)) leq-x?)
          (merge-sort (drop points (/ (length points) 2)) leq-y?))))

test-point-set
(merge-sort test-point-set leq-x?)
(take test-point-set 3)

(contains? (take test-point-set 3)
           (cadr (merge-sort test-point-set leq-x?)))

(filter (lambda (P) (contains? (drop test-point-set 3) P))
        (merge-sort test-point-set leq-y?))

(form-halves-naive test-point-set)

(define form-lists
  (lambda (point-set)
    ))

(define contains?
  (lambda (list a)
    (cond ((null? list) #f)
          ((eq? (car list) a) #t)
          (else (contains? (cdr list) a)))))

(define closest-pair
  (lambda (point-set)
    (define cp
      (lambda (sorted-x sorted-y)
        (let )))))


;; (contains? '(1 2 3 4) 4)
;; (contains? '(1 2 3 4) 5)

(define swap
  (lambda (vec i j)
    (let ((temp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j temp))))

(define partition-iter
  (lambda (A pivot-ref i j max)
    (cond ((= j max) (swap A pivot-ref (- i 1)) (- i 1))
          ((<= (vector-ref A j) (vector-ref A pivot-ref))
           (swap A i j)
           (partition-iter A pivot-ref (+ i 1) (+ j 1) max))
          (else
           (partition-iter A pivot-ref i (+ j 1) max)))))

(define partition
  (lambda (A l r)
    (partition-iter A l (+ l 1) (+ l 1) r)))

(define qs
  (lambda (A l r)
    (cond ((<= (- r l) 1) 0)
          (else
           (let ((pivot (partition A l r)))
             (+ (- (- r l) 1)
                (qs A l pivot)               
                (qs A (+ pivot 1) r)))))))

(define qs-last
  (lambda (A l r)
    (cond ((<= (- r l) 1) 0)
          (else
           (swap A l (- r 1))
           (let ((pivot (partition A l r)))
             (+ (- (- r l) 1)
                (qs-last A l pivot)               
                (qs-last A (+ pivot 1) r)))))))

(define qs-median
  (lambda (A l r)
    (cond ((<= (- r l) 1) 0)
          (else
           (swap A l
                 (caadr (merge-sort (median-of-three A l r) leq-y?)))
           (let ((pivot (partition A l r)))
             (+ (- (- r l) 1)
                (qs-median A l pivot)               
                (qs-median A (+ pivot 1) r)))))))

(caadr (merge-sort (median-of-three '#(3 8 2 5 1 4 7 6) 0 8) leq-y?))

(define test-vector-1 (vector 3 8 2 5 1 4 7 6))
(define test-vector-2 (vector 1 2 3 4 5))
(define test-vector-3 (vector 5 4 3 2 1))

(qs-last test-vector-1 0 8)
(qs-last test-vector-2 0 5)
(qs-median test-vector-1 0 8) ;; 13
(qs-median test-vector-2 0 5)
(qs-median test-vector-3 0 5) ;; Expect 6

(define median
  (lambda (l r)
    (+ l (ceiling (/ (- r l) 2)))))

(define input-2
  (map (lambda (x) (string->number x))
       (file->lines "/Users/tomobrien/Dropbox/AlgoClass/QuickSort.txt")))

(define input-vec
  (list->vector (map (lambda (x) (string->number x))
                     (file->lines "/Users/tomobrien/Dropbox/AlgoClass/QuickSort.txt"))))

(qs input-vec 0 10000) ;; 162085
(qs-last input-vec 0 10000) ;; 164123
(qs-median input-vec 0 10000) ;; 138382

(define median-of-three
  (lambda (A l r)
    (list (list l (vector-ref A l))
          (list (- (median l r) 1) (vector-ref A (- (median l r) 1)))
          (list (- r 1) (vector-ref A (- r 1))))))

;; (median-of-three '#(3 5 9 6)
;; a <= b <= c
;; a <= c <= b
;; 

(median-of-three '#(3 5 6 9 10) 0 5)
(caadr (merge-sort (median-of-three '#(3 5 2 9 10) 0 5) leq-y?))
(median 0 4)
