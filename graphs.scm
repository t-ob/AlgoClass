(define graph-test
  '((1 (2 3))
    (2 (1 3))
    (3 (1 2))))

(map car graph-test)

(define vertex-test
  '(1 (2 3)))

(cadr vertex-test)

(define get-edges
  (lambda (vertex)
    (map (lambda (node)
           (list (car vertex) node)) (cadr vertex))))

(get-edges vertex-test)

(map get-edges graph-test)

(define make-edge
  (lambda (vertex-1 vertex-2)
    (list vertex-1 vertex-2)))

(define graph-test
  '((1 (2 3 4))
    (2 (1 3 4))
    (3 (1 2 4 5))
    (4 (1 2 3))
    (5 (3 6 7 8))
    (6 (5 7 8))
    (7 (5 6 8))
    (8 (5 6 7))))

(define count-vertices
  (lambda (graph)
    (length graph)))

(define count-edges
  (lambda (graph)
    (/ (foldl + 0 (map length (map cadr graph))) 2)))

(define nth
  (lambda (list n)
    (if (= n 0)
        (car list)
        (nth (cdr list) (- n 1)))))

(count-edges graph-test)

(define merge-edge
  (lambda (graph edge)
    ))

(define edges
  (lambda (graph result)
    ))

(define collapse-test-1
  '((a (b c d))
    (b (a c d d))
    (c (a b d))
    (d (a b b c))))

(define collapse-test-2
  '((a ((b d) (b d) c))
    ((b d) (a a c c))
    (c (a (b d) (b d)))))

(count-edges collapse-test-2)

(define remove-self-loop
  )

(define de-loop
  '((b d) (a a (b d) c c)))

(filter (lambda (vertex) (eq? '(b d) vertex)) (cadr de-loop))
(cadr de-loop)
(car de-loop)

(caddr (cadr de-loop))
(eq? '(b d) (caddr (cadr de-loop)))

(map identity (cadr de-loop))
(filter (lambda (x) (not (even? x))) '(1 2 3 4 5))

(filter (lambda (head) (not (equal? (car de-loop) head))) (cadr de-loop))

(define remove-self-loops
  (lambda (adj-node)
    (list (car adj-node)
          (filter
           (lambda (head)
             (not (equal? (car adj-node) head))) (cadr adj-node)))))

(define make-adj-node
  (lambda (tail . heads)
    (list tail heads)))

(make-adj-node 1 2 3 4 4 5)
graph-test
(random 10)

(cadr (nth graph-test (random (length graph-test))))

(define random-edge
  (lambda (graph)
    (let ((adj-node (nth graph (random (length graph)))))
      (list (car adj-node)
            (nth (cadr adj-node)
                 (random (length (cadr adj-node))))))))

(random-edge graph-test)

(define get-index
  (lambda (graph vertex)
    (if (equal? (caar graph) vertex)
        0
        (+ 1 (get-index (cdr graph) vertex)))))

(define new-vertex
  (lambda (vertex edge)
    (if (or (equal? vertex (car edge)) (equal? vertex (cadr edge)))
        (flatten edge)
        vertex)))

(define new-adj-node
  (lambda (adj-node edge)
    (list (new-vertex (car adj-node) edge)
          (map (lambda (vertex) (new-vertex vertex edge)) (cadr adj-node)))))

(define remove-n
  (lambda (list n)
    (if (= n 0)
        (cdr list)
        (cons (car list) (remove-n (cdr list) (- n 1))))))

(define merge-tails
  (lambda (adj-node-1 adj-node-2)
    (list (car adj-node-1)
          (append (cadr adj-node-1) (cadr adj-node-2)))))

graph-test
(define test-edge (random-edge graph-test))
test-edge
(nth graph-test 6)
(marge-tails)
(map (lambda (adj-node) (new-adj-node adj-node test-edge)) graph-test)
(remove-self-loops (merge-tails '((7 8) (5 6 (7 8))) '((7 8) (5 6 (7 8)))))



(define collapse-along-edge
  (lambda (graph edge)
    (let ((i (get-index graph (car edge)))
          (j (get-index graph (cadr edge)))
          (temp (map
                 (lambda (adj-node)
                   (new-adj-node adj-node edge)) graph)))
      (remove-n
       (map
        (lambda (adj-node)
          (if (or (= i (get-index temp (car adj-node)))
                  (= j (get-index temp (car adj-node))))
              (remove-self-loops (merge-tails (nth temp i) (nth temp j)))
              adj-node)) temp) i))))

(random-edge (collapse-along-edge graph-test '(7 4)))
(remove-n (collapse-along-edge graph-test '(7 4)) 6)
(collapse-along-edge (collapse-along-edge graph-test '(7 4)) '((7 4) 8))

(define karger
  (lambda (graph)
    (if (= 2 (length graph))
        graph
        (karger (collapse-along-edge graph (random-edge graph))))))

(collapse-along-edge graph-test (random-edge graph-test))

(karger graph-test)
(length (karger graph-test))
(define karger-test (karger graph-test))
karger-test
(cadar karger-test)
(length (cadar (karger graph-test)))
(flatten '(6 (7 4)))
(collapse-along-edge
 (collapse-along-edge graph-test '(7 4)) '((7 4) 6))

(define graph-test-2
  '((1 (2 3))
    (2 (1 3 4))
    (3 (1 2 4))
    (4 (2 3))))

(collapse-along-edge (collapse-along-edge graph-test-2 '(1 3)) '((1 3) 2))
(length (cadar (karger graph-test-2)))

(define graph-test-3
  '((1 (2 3 4))
    (2 (1 3 4))
    (3 (1 2 4))
    (4 (1 2 3))))

(length (cadar (karger graph-test-3)))
(define karger-count
  (lambda (graph)
    (length (cadar (karger graph)))))

(define least-trials
  (lambda (f arg count max result)
    (let ((temp (f arg)))
      (cond ((> count max) result)
            ((< temp result) (least-trials f arg (+ count 1) max temp))
            (else (least-trials f arg (+ count 1) max result))))))

(define karger-trial
  (lambda (graph)
    (let ((N (* (length graph) (length graph) (log (length graph)))))
      (least-trials karger-count graph 1 N N))))

(karger-trial graph-test)
(karger-trial graph-test-2)
(karger-trial graph-test-3)

(define input
  (file->lines "/Users/tomobrien/Dropbox/AlgoClass/graph.txt"))

input

(filter
 (lambda (adj-node)
   (equal? adj-node (nth (collapse-along-edge graph-test '(7 4)) 3)))
 (collapse-along-edge graph-test '(7 4)))
(map
 (lambda (adj-node)
   (cond ((= 3 (get-index graph-test adj-node))
          (marge-tails (nth graph-test 3)
                       (nth graph-test 6)))
         ((= 6 (get-index graph-test adj-node))
          '())
         (else adj-node))) (collapse-along-edge graph-test '(7 4)))

(define temp (collapse-along-edge graph-test '(7 4)))
(map
 (lambda (adj-node)
   (if (or (= 6 (get-index temp (car adj-node)))
           (= 3 (get-index temp (car adj-node))))
       (merge-tails (nth temp 6) (nth temp 3))
       adj-node)) temp)

(get-index )
(merge-tails (nth (collapse-along-edge graph-test '(7 4)) 3)
             (nth (collapse-along-edge graph-test '(7 4)) 6))


(values '(1 2 3 4 5))

(define big-graph
  '((1 (19 15 36 23 18 39))
    (2 (36 23 4 18 26 9))
    (3 (35 6 16 11))
    (4 (23 2 18 24))
    (5 (14 8 29 21))
    (6 (34 35 3 16))
    (7 (30 33 38 28))
    (8 (12 14 5 29 31))
    (9 (39 13 20 10 17 2))
    (10 (9 20 12 14 29))
    (11 (3 16 30 33 26))
    (12 (20 10 14 8))
    (13 (24 39 9 20))
    (14 (10 12 8 5))
    (15 (26 19 1 36))
    (16 (6 3 11 30 17 35 32))
    (17 (38 28 32 40 9 16))
    (18 (2 4 24 39 1))
    (19 (27 26 15 1))
    (20 (13 9 10 12))
    (21 (5 29 25 37))
    (22 (32 40 34 35))
    (23 (1 36 2 4))
    (24 (4 18 39 13))
    (25 (29 21 37 31))
    (26 (31 27 19 15 11 2))
    (27 (37 31 26 19 29))
    (28 (7 38 17 32))
    (29 (8 5 21 25 10 27))
    (30 (16 11 33 7 37))
    (31 (25 37 27 26 8))
    (32 (28 17 40 22 16))
    (33 (11 30 7 38))
    (34 (40 22 35 6))
    (35 (22 34 6 3 16))
    (36 (15 1 23 2))
    (37 (21 25 31 27 30))
    (38 (33 7 28 17 40))
    (39 (18 24 13 9 1))
    (40 (17 32 22 34 38))))

(karger-trial big-graph)
(karger-count big-graph)
