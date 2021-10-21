(defun print-standard-library ()
  (print-yaml *standard-library*))

(defun generate-standard-library (out-file)
  (generate-yaml-file *standard-library* out-file))

(defun generate-translate-yaml ()
  (format t (format-packages *michelson*))
  (format t (group->string *morley-deps*)))

(defun main ()
  (generate-yaml-file *standard-library*   "library/StandardLibrary/stack.yaml")
  (generate-yaml-file *frontend*           "library/Frontend/stack.yaml")
  (generate-yaml-file *sexp*               "library/Sexp/stack.yaml")
  (generate-yaml-file *Context*            "library/Context/stack.yaml")
  (generate-yaml-file *core*               "library/Core/stack.yaml")
  (generate-yaml-file *translate*          "library/Translate/stack.yaml")
  (generate-yaml-file *Michelson*          "library/Backends/Michelson/stack.yaml")
  (generate-yaml-file *llvm*               "library/Backends/llvm/stack.yaml")
  (generate-yaml-file *plonk*              "library/Backends/Plonk/stack.yaml")
  (generate-yaml-file *easy-pipeline*      "library/EasyPipeline/stack.yaml")
  (generate-yaml-file *berlin-pipeline*     "library/BerlinPipeline/stack.yaml")
  (generate-yaml-file *pipeline*           "library/Pipeline/stack.yaml")
  (generate-yaml-file *witch*              "library/Witch/stack.yaml")
  (generate-yaml-file *http*               "library/HTTP/stack.yaml")
  (generate-yaml-file *juvix*              "stack.yaml"))
