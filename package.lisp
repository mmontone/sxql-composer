(defpackage :sxql-composer
  (:use :cl)
  (:export :and-where
           :or-where
           :where=
           :from=
           :from+
           :fields=
           :fields+
           :limit=
           :offset=
           :order-by=
           :group-by=
           :order-by+
           :group-by+
           :returning=
           :having=)
  (:documentation "Dynamic SXQL query composition"))
