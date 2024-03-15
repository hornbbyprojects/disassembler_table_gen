;;;; disassembler_table_gen.asd

(asdf:defsystem #:disassembler_table_gen
  :description "Describe disassembler_table_gen here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (iterate cl-interpol trivia)
  :components ((:file "package")
               (:file "disassembler_table_gen")))
