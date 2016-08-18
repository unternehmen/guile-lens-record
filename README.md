# What is guile-lens-record?
A "lens" library for nondestructively modifying fields in Guile 2.0 records.

It is inspired by the lens implementations that already exist for languages like Haskell and Python.

## Usage
The `record-lens` function takes a record RDT and a field name.  It returns a lens for modifying that field on any instance of the record type.

    (use-modules (srfi srfi-26))

    (define-syntax ->
      (syntax-rules ()
        ((_ a (f xs ...) rest ...) (-> (f a xs ...) rest ...))
        ((_ a) a)))

    (define <mouse> (make-record-type "mouse" '(name age)))

    (define mouse (record-constructor <mouse>))

    (define mouse-born (cut mouse <> 0))

    (define change-name
      (let ((lens (record-lens <mouse> 'name)))
        (lambda (inst name)
          (lens inst (const name)))))

    (define eat-birthday-cake
      (let ((lens (record-lens <mouse> 'age)))
        (cut lens <> 1+)))

    (display
      (->
        (mouse-born "Mickey Mouse")
        (eat-birthday-cake)
        (eat-birthday-cake)
        (change-name "Fred Mouse")
        (eat-birthday-cake)
        (change-name "Minnie Mouse")
        (eat-birthday-cake)))
    (newline)

## License
guile-lens-record is licensed under the CC0 1.0 Universal license, a copy of which should have come with this package. Basically, that means that I (to the extent possible under law) waive all copyright and related or neighboring rights to the work, make no warranty about the work, and disclaim liability for all uses of the work.
