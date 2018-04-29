(
  ;; input grid specification
  (grid
    (
       (x0 -25.0)
       (x1  25.0)
       (nx 100)
    ))

  ;; alternative input grid specification, lighter syntax
  (grid2
   (x0 -25.0)
   (x1 25.0)
   (nx 100))

  ;; output file
  ;; (output foo.sexp)
  (output foo.sexp)
  (polynomial (1.0 1.1 1.2 1.3 1.4))
)
