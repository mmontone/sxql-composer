(in-package :sxql.composer)

(defun expand-op (object)
  (if (and (listp object)
           (keywordp (car object)))
      (apply #'sxql:make-op (car object) (mapcar #'expand-op (cdr object)))
      object))

(defun expand-expression (expressions)
  (cond
    ((not (listp expressions)) expressions)
    ((and (symbolp (car expressions))
          (not (keywordp (car expressions))))
     expressions)
    (t (mapcar #'expand-op expressions))))

(defun set-child (statement child)
  (let ((slot-name (type-of child)))
    (setf (slot-value statement slot-name)
          (list child)))
  statement)

(defun where* (statement expression &optional (action :set))
  (declare (type (member :set :and :or) action))
  (let ((exp (if (and (listp expression)
                      (keywordp (car expression)))
                 (expand-op expression)
                 expression)))
    (if (null (slot-value statement 'sxql::where-clause))
        (set-child statement
                   (sxql:make-clause :where exp))
        ;; else
        (ecase action
          (:set
           (set-child statement
                      (sxql:make-clause :where exp)))
          (:and
           (set-child statement
                      (sxql:make-clause :where
                                        (sxql:make-op :and
                                                      (sxql.clause::where-clause-expression
                                                       (first (slot-value statement 'sxql::where-clause)))
                                                      exp))))
          (:or
           (set-child statement
                      (sxql:make-clause :where
                                        (sxql:make-op :or
                                                      (sxql.clause::where-clause-expression
                                                       (first (slot-value statement 'sxql::where-clause)))
                                                      exp))))))))

(defun and-where (statement expression)
  "Add an AND clause to the WHERE clause of SQL STATEMENT"
  (where* statement expression :and))

(defun or-where (statement expression)
  "Add an OR clause to the WHERE clause of SQL STATEMENT"
  (where* statement expression :or))

(defun where= (statement expression)
  "Set SQL WHERE clause of the SQL STATEMENT"
  (where* statement expression :set))

(defun from* (statement table-or-tables &optional (action :set))
  (declare (type (member :set :and) action))
  (let ((tables (if (listp table-or-tables)
                    table-or-tables
                    (list table-or-tables))))
    (if (null (slot-value statement 'sxql::from-clause))
        (set-child statement
                   (apply #'sxql:make-clause :from tables))
        ;; else
        (ecase action
          (:set
           (set-child statement
                      (apply #'sxql:make-clause :from tables)))
          (:and
           (set-child statement
                      (apply #'sxql:make-clause :from
                             (append
                              (sxql.clause::sql-list-elements
                               (sxql.clause::from-clause-statement
                                (first (slot-value statement 'sxql::from-clause))))
                              tables))))))))

(defun from+ (statement &rest tables)
  "Add TABLES to the FROM clause of the SQL STATEMENT"
  (from* statement tables :and))

(defun from= (statement &rest tables)
  "Set the FROM clause of the SQL STATEMENT"
  (from* statement tables :set))

(defun fields (statement fields &optional (action :set))
  (declare (type (member :set :append) action))
  (let ((clause (apply #'sxql:make-clause :fields
                       (mapcar #'expand-op fields))))
    (if (null (slot-value statement 'sxql::fields-clause))
        (set-child statement clause)
        ;; else
        (ecase action
          (:set
           (set-child statement clause))
          (:append
           (set-child statement
                      (apply #'sxql:make-clause :fields
                             (append
                              (sxql.clause::sql-list-elements
                               (sxql.clause::fields-clause-statement
                                (first (slot-value statement 'sxql::fields-clause))))
                              (mapcar #'expand-op fields)))))))))

(defun fields+ (statement &rest fields)
  "Add FIELDS to the SELECT SQL STATEMENT"
  (fields statement fields :append))

(defun fields= (statement fields)
  "Set the FIELDS of the SELECT SQL STATEMENT"
  (fields statement fields :set))

(defun limit= (statement count1 &optional count2)
  "Set the LIMIT clause of the SQL STATEMENT"
  (let ((clause (sxql:limit count1 count2)))
    (set-child statement clause)))

(defun offset= (statement offset)
  "Set the OFFSET clause of the SQL STATEMENT"
  (set-child statement (sxql:make-clause :offset offset)))

(defun returning= (statement expression)
  "Set the RETURNING clause of SQL STATEMENT"
  (set-child statement
             (sxql:make-clause :returning
                               (if (and (listp expression)
                                        (keywordp (car expression)))
                                   (expand-op expression)
                                   expression))))

(defun order-by* (statement expressions &optional (action :set))
  (declare (type (member :set :append) action))
  (let ((clause (apply #'sxql:make-clause :order-by
                       (expand-expression expressions))))
    (if (null (slot-value statement 'sxql::fields-clause))
        (set-child statement clause)
        ;; else
        (ecase action
          (:set
           (set-child statement clause))
          (:append
           (set-child statement
                      (apply #'sxql:make-clause :order-by
                             (append
                              (sxql.clause::order-by-clause-expressions
                               (first (slot-value statement 'sxql::order-by-clause)))
                              (expand-expression expressions)))))))))

(defun order-by+ (statement &rest expressions)
  "Add EXPRESSIONS ot the ORDER BY clause of the SQL STATEMENT"
  (order-by* statement expressions :append))

(defun order-by= (statement &rest expressions)
  "Set the ORDER BY clause of the SQL STATEMENT"
  (order-by* statement expressions :set))

(defun group-by* (statement expressions &optional (action :set))
  (declare (type (member :set :append) action))
  (let ((clause (apply #'sxql:make-clause :group-by
                       (expand-expression expressions))))
    (if (null (slot-value statement 'sxql::fields-clause))
        (set-child statement clause)
        ;; else
        (ecase action
          (:set
           (set-child statement clause))
          (:append
           (set-child statement
                      (apply #'sxql:make-clause :group-by
                             (append
                              (sxql.clause::group-by-clause-expressions
                               (first (slot-value statement 'sxql::group-by-clause)))
                              (expand-expression expressions)))))))))

(defun group-by+ (statement &rest expressions)
  "Add expressions to the GROUP BY  clause of the SQL STATEMENT"
  (group-by* statement expressions :append))

(defun group-by= (statement &rest expressions)
  "Set the GROUP BY clause of the SQL statement"
  (group-by* statement expressions :set))
