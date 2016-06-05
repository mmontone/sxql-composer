SXQL-COMPOSER
=============

[![Quicklisp](http://quickdocs.org/badge/sxql-composer.svg)](http://quickdocs.org/sxql-composer)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

Build and compose SXQL queries dynamically.

Motivation
----------

It is not possible to dynamically build and compose SXQL queries because it uses macros for query building. For example, something like:

```lisp
(defun build-query (fields &key order-by)
    (select fields
      (where (:= 'user.id 1))
      (when order-by
        (order-by order-by))))
```
is not possible.

SXQL-COMPOSER implements an equivalent set of functions in order to be able to either build new queries dynamically, or modify and composer already existent queries.

With SXQL-COMPOSER:

```lisp
(defun build-query (fields &key order-by)
   (let ((q (select fields
               (where (:= 'user.id 1)))))
      (when order-by (order-by= q order-by))
      q))
```

Also, it is not possible to modify already existent queries. That is useful for filtering:

```lisp
(defun all-users ()
   (select :* (from :users)))

(defun married-users ()
   (and-where (all-users)
       '(:= status "married")))
```

CL-ARROWS library is potentially useful for building the queries:

```lisp
(defun married-users ()
   (-> (all-users)
      (and-where '(:= status "married"))))
```