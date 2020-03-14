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

## Functions

### and-where

```lisp
(statement expression)
```

Add an AND clause to the WHERE clause of SQL STATEMENT



### fields+

```lisp
(statement &rest fields)
```

Add FIELDS to the SELECT SQL STATEMENT



### fields=

```lisp
(statement fields)
```

Set the FIELDS of the SELECT SQL STATEMENT



### from+

```lisp
(statement &rest tables)
```

Add TABLES to the FROM clause of the SQL STATEMENT



### from=

```lisp
(statement &rest tables)
```

Set the FROM clause of the SQL STATEMENT



### group-by+

```lisp
(statement &rest expressions)
```

Add expressions to the GROUP BY  clause of the SQL STATEMENT





### group-by=

```lisp
(statement &rest expressions)
```

Set the GROUP BY clause of the SQL statement



### limit=

```lisp
(statement count1 &optional count2)
```

Set the LIMIT clause of the SQL STATEMENT





### offset=

```lisp
(statement offset)
```

Set the OFFSET clause of the SQL STATEMENT



### or-where

```lisp
(statement expression)
```

Add an OR clause to the WHERE clause of SQL STATEMENT



### order-by+

```lisp
(statement &rest expressions)
```

Add EXPRESSIONS ot the ORDER BY clause of the SQL STATEMENT



### order-by=

```lisp
(statement &rest expressions)
```

Set the ORDER BY clause of the SQL STATEMENT



### returning=

```lisp
(statement expression)
```

Set the RETURNING clause of SQL STATEMENT



### where=

```lisp
(statement expression)
```

Set SQL WHERE clause of the SQL STATEMENT
