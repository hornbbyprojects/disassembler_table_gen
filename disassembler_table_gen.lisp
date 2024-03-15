;;;; disassembler_table_gen.lisp

(in-package #:disassembler_table_gen)

(named-readtables:in-readtable :interpol-syntax)

(defparameter *out-stream* *standard-output*)

(defstruct context
  (bits-through-byte 0 :type integer)
  (literal-count 0 :type integer))

(defun print-count-mask (count)
  (format *out-stream* "0b")
  (i:iterate
    (i:repeat count)
    (format *out-stream* "1")))

(defun increment_current_byte ()
  (format *out-stream* #?"current_byte = read_byte(file);\n")
  (format *out-stream* #?"if (feof(file)) { ret -> failed = 1; return; }\n"))


(defun get-bits (context count target)
  (when (= (context-bits-through-byte context) 8)
    (setf (context-bits-through-byte context) 0)
    (increment_current_byte))
  (let ((shift-amount (- 8 (+ count (context-bits-through-byte context)))))
    (format *out-stream* #?"${target} = ((current_byte >> ${shift-amount}) &")
    (print-count-mask count)
    (format *out-stream* ");")
    (incf (context-bits-through-byte context) count)
    (if (> (context-bits-through-byte context) 8)
        (error "Overflowed byte!"))))

(defun parse-jump-destination ()
  (increment_current_byte)
  (format *out-stream* #?"ret -> jump_destination = current_byte;")
  (format *out-stream* #?"ret -> has_jump_destination = 1;"))

(defun parse-bits (context name count)
  (get-bits context count #?"ret -> ${name}"))

(defun parse-flag (context flag_name)
  (parse-bits context flag_name 1))

(defun parse-literal (context literal)
  (let ((bits (length literal))
        (literal-name #?"literal_${(incf (context-literal-count context))}"))
    (get-bits context bits #?"u8 ${literal-name}");
    (format *out-stream* #?"if (0b${literal} != ${literal-name}) {ret -> failed = 1; return;}")))

(defun parse-data ()
  (format *out-stream* #?"read_data(file, ret);")
  (format *out-stream* #?"if (feof(file)) { ret -> failed = 1; return; }"))

(defun parse-displacement ()
  (format *out-stream* #?"read_displacement(file, ret);")
  (format *out-stream* #?"if (feof(file)) { ret -> failed = 1; return; }"))

(defun override-value (field value)
  (format *out-stream* #?"ret -> ${field} = ${value};"))



(defun parse-form (context form)
  (cond
    ((equal form 'displacement) (parse-displacement))
    ((equal form 'data) (parse-data))
    ((equal form 'mod) (parse-bits context "mod" 2))
    ((equal form 'reg) (parse-bits context "reg" 3))
    ((equal form 'r_m) (parse-bits context "r_m" 3))
    ((equal form 'd) (parse-flag context "flag_d"))
    ((equal form 'w) (parse-flag context "flag_w"))
    ((equal form 's) (parse-flag context "flag_s"))
    ((equal form 'jump-destination) (parse-jump-destination))
    ((listp form)
     (trivia:let-match (((list 'override field value) form))
                       (override-value field value)))
    ((stringp form) (parse-literal context form))
    (t (error #?"Unrecognised form ${form}")))
  (format *out-stream* #?"\n"))


(defparameter *all-operations* nil)
(defparameter *current-remember-count* 0)
(defparameter *operation-counts* nil)
(defun remember-operation (operation)
  (if (equal (car *all-operations*) operation)
      (incf *current-remember-count*)
      (progn
        (when *all-operations*
          (setf (gethash (car *all-operations*) *operation-counts*) *current-remember-count*))
        (setf *current-remember-count* 0)
        (push operation *all-operations*))))

(defun get-parse-method-name (op count)
  #?"parse_${op}_${count}")

(defun get-instruction-type-enum (op)
  #?"INSTRUCTION_TYPE_${(string-upcase op)}")

(defmacro define-operation (opname dsl)
  `(let ((context (make-context)))
     (remember-operation ,opname)
     (format *out-stream* #?"void ${(get-parse-method-name ,opname *current-remember-count*)}(struct InstructionData *ret, FILE *file) {\n")
     (format *out-stream* #?"u8 current_byte = read_byte(file);\n")
     (format *out-stream* #?"if (feof(file)) { fprintf(stderr, \"EOF AT START OF instruction\"); ; exit(-2); }\n")
     (format *out-stream* #?"ret -> instruction_type = ${(get-instruction-type-enum ,opname)};\n")
     (i:iterate
       (i:for form in (quote ,dsl))
       (parse-form context form))
     (format *out-stream* #?"}\n\n")))

(defun print-operation-enum ()
  (format *out-stream* #?"enum InstructionType {\n")
  (i:iterate
    (i:for op in *all-operations*)
    (format *out-stream* #?"${(get-instruction-type-enum op)},\n"))
  (format *out-stream* #?"};\n"))

(defun print-parse-methods-array ()
  (format *out-stream* #?"void (*parse_methods[]) (struct InstructionData *, FILE *) = {\n")
  (i:iterate
    (i:for (op count) in-hashtable *operation-counts*)
    (i:iterate
      (i:for i from 0 to count)
      (format *out-stream* #?"${(get-parse-method-name op i)},\n")))
  (format *out-stream* #?"};\n"))



(defmacro define-binary-operations (name tricode)
  `(progn
     (define-operation ,name ("00" ,tricode "0" d w mod reg r_m displacement))
     (define-operation ,name ("100000" s w mod ,tricode r_m displacement data))
     (define-operation ,name ("00" ,tricode "10" w data (override "reg" 0)))))

(defun print-table ()
  (let ((*all-operations* nil)
        (*operation-counts* (make-hash-table :test 'equal)))
    (with-open-file (*out-stream* "~/performance/table_disassembler/parse_methods.inl" :direction :output :if-exists :supersede)
      (define-operation "mov" ("100010" d w mod reg r_m displacement))
      (define-operation "mov" ("1100011" w mod "000" r_m displacement data))
      (define-operation "mov" ("1011" w reg data))
      (define-operation "mov" ("101000" d (override "flag_d" "!ret -> flag_d") w (override "mod" 0) (override "reg" 0) (override "r_m" "0b110") displacement))
      (define-binary-operations "add" "000")
      (define-binary-operations "sub" "101")
      (define-binary-operations "cmp" "111")
      (define-operation "JE" ("01110100" jump-destination))
      (define-operation "JL" ("01111100" jump-destination))
      (define-operation "JLE" ("01111110" jump-destination))
      (define-operation "JB" ("01110010" jump-destination))
      (define-operation "JBE" ("01110110" jump-destination))
      (define-operation "JP" ("01111010" jump-destination))
      (define-operation "JO" ("01110000" jump-destination))
      (define-operation "JS" ("01111000" jump-destination))
      (define-operation "JNE" ("01110101" jump-destination))
      (define-operation "JNL" ("01111101" jump-destination))
      (define-operation "JG" ("01111111" jump-destination))
      (define-operation "JAE" ("01110011" jump-destination))
      (define-operation "JA" ("01110111" jump-destination))
      (define-operation "JNP" ("01111011" jump-destination))
      (define-operation "JNO" ("01110001" jump-destination))
      (define-operation "JNS" ("01111001" jump-destination))
      (define-operation "LOOP" ("11100010" jump-destination))
      (define-operation "LOOPE" ("11100001" jump-destination))
      (define-operation "LOOPNE" ("11100000" jump-destination))
      (define-operation "JCXZ" ("11100011" jump-destination))
      (setf (gethash (car *all-operations*) *operation-counts*) *current-remember-count*)
    (print-parse-methods-array))
    (with-open-file (*out-stream* "~/performance/table_disassembler/operation_enum.inl" :direction :output :if-exists :supersede)
      (print-operation-enum))))
