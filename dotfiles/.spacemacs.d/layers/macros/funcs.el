
(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))
