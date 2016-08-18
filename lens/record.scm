(define-module
  (lens record)
  #:export (record-lens))

(define (record-lens rdt field-name)
  "Return a function which changes a record's field in a pure way."
  (lambda (inst transformer)
    (apply (record-constructor rdt)
      (map
        (lambda (field)
          (let ((property ((record-accessor rdt field) inst)))
            (if (equal? field field-name)
              (transformer property)
              property)))
        (record-type-fields rdt)))))
