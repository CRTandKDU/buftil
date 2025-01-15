;;; stacks.el -- Implementing stacks in text buffers
;;  Inspired by [[https://nicholas.carlini.com/writing/2025/regex-chess.html]]. Here, stacks buffers are in org-mode.

;; Generic stack operations
(defun buftil-push (str &optional stack)
  (let ((stack-regexp
	 (format "* %s\n" (or stack "STACK"))))
    (with-current-buffer (get-buffer "test")
      (goto-char (point-min))
      (if (search-forward-regexp stack-regexp
				 nil
				 t)
	  (replace-match
	   (concat stack-regexp (format "%s\n" str)))
	)
      )
    )
  )

(defun buftil-pop (&optional stack)
  (let ((stack-regexp
	 (format "* %s\n" (or stack "STACK"))))
    (with-current-buffer (get-buffer "test")
      (goto-char (point-min))
      (if (search-forward-regexp
	   (concat stack-regexp
		   "\\([-[:word:]]+\\)\n")
	   nil
	   t)
	  (let ((str (match-string 1)))
	    (replace-match stack-regexp)
	    str)
	nil)
      )
    )
  )

(defun buftil--up-push (str &optional stack)
  (let ((rx (format "* %s\n" (or stack "END"))))
    (goto-char (point-min))
    (if (search-forward-regexp rx nil t)
	(replace-match (format "%s%s" str rx) t))))

(defun buftil--up-update (line-string val)
  (goto-line (string-to-number line-string))
  (kill-whole-line)
  (insert (format "%s\n" val)))
  

;; System variables operations
(defun buftil--get-variable (var &optional buf)
  (with-current-buffer (get-buffer (or buf "test"))
    (goto-char (point-min))
    (if (search-forward-regexp
	 (concat (format "* %s " var) "\\([[:word:]]+\\)\n") nil t)
	(match-string 1)
      nil)))

(defun buftil--set-variable (var mark &optional buf)
  (with-current-buffer (get-buffer (or buf "test"))
    (goto-char (point-min))
    (if (search-forward-regexp
	 (concat (format "* %s " var) "\\([[:word:]]+\\)\n") nil t)
	(replace-match (format "* %s %s\n" var mark)))
    )
  )

;; Aliases for Word Register operations
(defun buftil-get-wordreg (&optional buf) (buftil--get-variable "WORDREG" buf))
(defun buftil-set-wordreg (mark &optional buf) (buftil--set-variable "WORDREG" mark buf))

;; Conditional jump operations
(defun buftil-jump-relative-if-zero-wordreg ()
  ;; This is called from the dictionary buffer
  (save-excursion
    (goto-line (string-to-number (buftil-get-wordreg)))
    (let ((delta (if (= 0 (string-to-number (buftil-pop)))
		     (string-to-number (thing-at-point 'line t))
		   1)))
      ;; (debug (+ delta (line-number-at-pos)))
      (buftil-set-wordreg (+ delta (line-number-at-pos))))
    )
  )

(defun buftil-jump-relative-not-zero-wordreg ()
  ;; This is called from the dictionary buffer
  (save-excursion
    (goto-line (string-to-number (buftil-get-wordreg)))
    (let ((delta (if (= 0 (string-to-number (buftil-pop)))
		     1
		   (string-to-number (thing-at-point 'line t)))))
      (buftil-set-wordreg (+ delta (line-number-at-pos))))
    )
  )

;; Dictionary operations
(defun buftil-create-header (&optional buf)
  (with-current-buffer (get-buffer-create (or buf "dict"))
    ;; This is called by a primitive code in dictionary: keep calling line
    (let ((keep-data (line-number-at-pos)))
      (buftil--up-push (format "** %s secondary\nCOLON\n" (buftil--pop-instr)))
      (goto-line keep-data)
      )
    ))

(defun buftil-create-footer () (buftil-create-body "SEMI\n\n"))

(defun buftil-create-body (instr &optional buf)
  (with-current-buffer (get-buffer-create (or buf "dict"))
    ;; This is called by a primitive code in dictionary: keep calling line
    (let ((keep-data (line-number-at-pos)))
      (buftil--up-push instr)
      (goto-line keep-data)
      )
    ))

(defun buftil-create-body-from-code (&optional buf)
  ;; (let ((word (buftil--pop-instr))) (create-body word)))
  (let ((keep-data (line-number-at-pos))
	(lineno (string-to-number (buftil-get-wordreg)))
	)
    (goto-line lineno)
    (buftil-create-body
     (format "%s\n" (car (split-string-and-unquote (thing-at-point 'line t)))) buf)
    (buftil-set-wordreg (1+ lineno))
    (goto-line keep-data)))


(defun buftil--create-body-from-stack (s &optional buf)
  (let ((scalar (buftil-pop s))) (buftil-create-body (format "%s\n" scalar) buf)))

(defun buftil-create-body-from-stack () (buftil--create-body-from-stack "STACK"))
(defun buftil-create-body-from-call  () (buftil--create-body-from-stack "CALL"))

(defun buftil-update-body (there here)
  ;; This is called from the dictionary buffer
  (save-excursion
    (let ((delta (- here (string-to-number there))))
      (buftil--up-update there delta)
      )))
  
(defun buftil-create-line-number (&optional stack buf)
  (let ((lineno nil))
    (with-current-buffer (get-buffer-create (or buf "dict"))
    ;; This is called by a primitive code in dictionary: keep calling line
    (let ((keep-data (line-number-at-pos)))
      (goto-char (point-min))
      (if (search-forward-regexp (format "* %s\n" (or stack "END")) nil t)
	  (setq lineno (1- (line-number-at-pos))))
      (goto-line keep-data)
      )
    lineno
    )
    ))

;; Interpreters: inner
(defun buftil--next ()
  (let ((cur (string-to-number (buftil-get-wordreg))))
    (buftil-set-wordreg (1+ cur))
    (with-current-buffer (get-buffer-create "dict")
      (goto-line cur)
      ;;
      ;; (read-string (format "%s : %s (Press RETURN)"
      ;; 			   cur
      ;; 			   (string-trim-right (thing-at-point 'line t))))
      ;;
      (buftil--find-execute (string-trim-right (thing-at-point 'line t)))
      )
    )
  )

(defun buftil--execute (word wtype wmode)
  (cond
   ;; Primitive in underlying language, emacs-lisp
   ((string= wtype "primitive")
    (while (not (string= "\n" (thing-at-point 'line t)))
      (let ((instr (string-trim-right (thing-at-point 'line t))))
	;; (with-current-buffer (get-buffer-create "*EXECTRACE*")
	;;   (insert (format "> %s\n" instr))
	;;   )
	(eval (car (read-from-string instr)))
	)
      (forward-line)
      ))
   ;; Secondary: consecutive words, one per line
   ((string= wtype "secondary")
    (buftil-push (buftil-get-wordreg) "CALL")
    (buftil-set-wordreg (line-number-at-pos)))
   ))

(defun buftil--find-execute (word)
  (with-current-buffer (get-buffer-create "dict")
    (goto-char (point-min))
    (if (search-forward-regexp
	 (format "^** %s \\([[:word:]]+\\)\\( [[:word:]]+\\)?\n" word) nil t)
	(let ((wtype (match-string 1))
	      (wmode (match-string 2))
	      (mode (buftil--get-variable "MODE")))
	  (set-text-properties 0 (length wtype) nil wtype)
	  (set-text-properties 0 (length wmode) nil wmode)
	  ;;
	  (cond
	   ((string= "EXECUTION" mode)
	    (buftil--execute word wtype wmode))
	   ;;
	   ((string= "COMPILE" mode)
	    (if (string= " immediate" wmode)
		(buftil--execute word wtype wmode)
	      (buftil-create-body (format "%s\n" word))))
	   ;;
	   (t nil)
	   )
	  wtype)
      ;; Not a word
      (let ((res nil)
	    (mode (buftil--get-variable "MODE")))
	(cond
	   ((string= "EXECUTION" mode)
	    (buftil-push word))
	   ;;
	   ((string= "COMPILE" mode)
	    (buftil-create-body (format "%s\n" word)))
	   ;;
	   (t nil)
	   )
	res))
    )
  )

;; Interpreters: outer
(defvar buftil-stack-instruction nil "TIL Instruction stack")

(defun buftil--pop-instr ()
  (let ((res (car buftil-stack-instruction)))
    (setq buftil-stack-instruction (cdr buftil-stack-instruction))
    res))

(defun buftil--outer () 
  (let ((instr (buftil--pop-instr)))
    (while instr
      (buftil-set-wordreg "OUTER")
      (buftil--find-execute instr)
      (while (not (string= "OUTER" (buftil-get-wordreg)))
	(buftil--next)
	)
      (setq instr (buftil--pop-instr)))
    )
  )

(defun buftil-repl ()
  (interactive)
  (let ((inline (read-string "Enter FORTH line: ")))
    (buftil--set-variable "MODE" "EXECUTION")
    (setq buftil-stack-instruction (split-string inline))
    (buftil--outer)))

