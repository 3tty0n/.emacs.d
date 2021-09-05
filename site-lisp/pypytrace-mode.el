(provide 'pypytrace-mode)
(eval-when-compile
  (require 'generic-x)
  (require 'hi-lock)
  (require 'compile)
  (require 'cus-edit))

(defun set-truncate-lines ()
  (setq truncate-lines t))

(defun pypytrace-beginning-of-defun ()
  (search-backward "{")
  (beginning-of-line))

(defun pypytrace-end-of-defun ()
  (search-forward "}")
  (end-of-line))


;; to generate the list of keywords:
;; from rpython.jit.metainterp import resoperation
;; print ' '.join(sorted('"%s"' % op.lower() for op in resoperation.opname.values() if not op.startswith('GUARD')))



(define-generic-mode
  'pypytrace-mode                   ;; name of the mode to create
  nil
  '("jump" "finish" "label" "vec_guard_true" "vec_guard_false" "int_add" "int_sub" "int_mul" "uint_mul_high" "int_and" "int_or" "int_xor" "int_rshift" "int_lshift" "uint_rshift" "int_signext" "float_add" "float_sub" "float_mul" "float_truediv" "float_neg" "float_abs" "cast_float_to_int" "cast_int_to_float" "cast_float_to_singlefloat" "cast_singlefloat_to_float" "convert_float_bytes_to_longlong" "convert_longlong_bytes_to_float" "vec_int_add" "vec_int_sub" "vec_int_mul" "vec_int_and" "vec_int_or" "vec_int_xor" "vec_float_add" "vec_float_sub" "vec_float_mul" "vec_float_truediv" "vec_float_neg" "vec_float_abs" "vec_float_eq" "vec_float_ne" "vec_float_xor" "vec_int_is_true" "vec_int_ne" "vec_int_eq" "vec_int_signext" "vec_cast_float_to_singlefloat" "vec_cast_singlefloat_to_float" "vec_cast_float_to_int" "vec_cast_int_to_float" "vec_i" "vec_f" "vec_unpack_i" "vec_unpack_f" "vec_pack_i" "vec_pack_f" "vec_expand_i" "vec_expand_f" "int_lt" "int_le" "int_eq" "int_ne" "int_gt" "int_ge" "uint_lt" "uint_le" "uint_gt" "uint_ge" "float_lt" "float_le" "float_eq" "float_ne" "float_gt" "float_ge" "int_is_zero" "int_is_true" "int_neg" "int_invert" "int_force_ge_zero" "same_as_i" "same_as_f" "same_as_r" "cast_ptr_to_int" "cast_int_to_ptr" "ptr_eq" "ptr_ne" "instance_ptr_eq" "instance_ptr_ne" "nursery_ptr_increment" "arraylen_gc" "strlen" "strgetitem" "getarrayitem_gc_pure_r" "getarrayitem_gc_pure_f" "getarrayitem_gc_pure_i" "unicodelen" "unicodegetitem" "load_from_gc_table" "load_effective_address" "gc_load_r" "gc_load_f" "gc_load_i" "gc_load_indexed_r" "gc_load_indexed_f" "gc_load_indexed_i" "getarrayitem_gc_r" "getarrayitem_gc_f" "getarrayitem_gc_i" "getarrayitem_raw_f" "getarrayitem_raw_i" "raw_load_f" "raw_load_i" "vec_load_f" "vec_load_i" "getinteriorfield_gc_r" "getinteriorfield_gc_f" "getinteriorfield_gc_i" "getfield_gc_r" "getfield_gc_f" "getfield_gc_i" "getfield_raw_r" "getfield_raw_f" "getfield_raw_i" "new" "new_with_vtable" "new_array" "new_array_clear" "newstr" "newunicode" "force_token" "virtual_ref" "strhash" "unicodehash" "gc_store" "gc_store_indexed" "increment_debug_counter" "setarrayitem_gc" "setarrayitem_raw" "raw_store" "vec_store" "setinteriorfield_gc" "setinteriorfield_raw" "setfield_gc" "zero_array" "setfield_raw" "strsetitem" "unicodesetitem" "cond_call_gc_wb" "cond_call_gc_wb_array" "enter_portal_frame" "leave_portal_frame" "jit_debug" "escape_r" "escape_f" "escape_i" "escape_n" "force_spill" "virtual_ref_finish" "copystrcontent" "copyunicodecontent" "quasiimmut_field" "assert_not_none" "record_exact_class" "keepalive" "save_exception" "save_exc_class" "restore_exception" "call_r" "call_f" "call_i" "call_n" "cond_call" "cond_call_value_r" "cond_call_value_i" "call_assembler_r" "call_assembler_f" "call_assembler_i" "call_assembler_n" "call_may_force_r" "call_may_force_f" "call_may_force_i" "call_may_force_n" "call_loopinvariant_r" "call_loopinvariant_f" "call_loopinvariant_i" "call_loopinvariant_n" "call_release_gil_f" "call_release_gil_i" "call_release_gil_n" "call_pure_r" "call_pure_f" "call_pure_i" "call_pure_n" "check_memory_error" "call_malloc_nursery" "call_malloc_nursery_varsize" "call_malloc_nursery_varsize_frame" "int_add_ovf" "int_sub_ovf" "int_mul_ovf") ;; keywords
  '( ;; additional regexps
    ("^# Loop.*" . 'hi-blue)
    ("\\[.*\\]" . 'font-lock-comment-face) ;; comment out argument lists
    ("guard_[a-z_]*" . 'widget-button-pressed)
    ("\\(ptr\\|p\\)[0-9][0-9]*" . 'font-lock-variable-name-face)
    ("i[0-9][0-9]*" . 'custom-button-pressed-unraised)
    ("\\(descr=<.*FieldDescr \\)\\([^ ]*\\.\\)\\([^ ]*\\)\\( .*>\\)"
     (1 'font-lock-comment-face)
     (2 'font-lock-variable-name-face)
     (3 'escape-glyph)
     (4 'font-lock-comment-face))
    ("<.*FieldDescr \\([^ ]*\\)" (1 'font-lock-variable-name-face))
    ;; comment out debug_merge_point, but then highlight specific part of it
    ("^debug_merge_point.*" . font-lock-comment-face)
    ("^\\(debug_merge_point\\).*code object\\(.*\\). file \\('.*'\\). \\(line .*\\)> \\(.*\\)"
     (1 'compilation-warning t)
     (2 'escape-glyph t)
     (3 'font-lock-string-face t)
     (4 'escape-glyph t)
     (5 'custom-variable-tag t)))
  '("\\.trace$")
  '(set-truncate-lines
    (lambda ()
      (set (make-local-variable 'beginning-of-defun-function)
           'pypytrace-beginning-of-defun)
      (set (make-local-variable 'end-of-defun-function) 'pypytrace-end-of-defun))
    )
  "A mode for pypy traces files")

;; debug helpers
;; (switch-to-buffer-other-window "strslice2.trace")
;; (pypytrace-mode)
