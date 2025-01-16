;;; buftil.el -- REPL-like client for buftil.
;;  Inspired by [[https://nicholas.carlini.com/writing/2025/regex-chess.html]]. Here, stacks buffers are in org-mode.
(require 'buftil-stacks)

(defun buftil-init ()
  (with-current-buffer (get-buffer-create "dict")
    (erase-buffer)
    (insert-file-contents "core-dict"))
  (with-current-buffer (get-buffer-create "test")
    (erase-buffer)
    (insert-file-contents "core-ws"))
  )

(defun buftil ()
  "Starts a primitive FORTH/TIL REPL with dictionary `dict',
and workspace `test'."
  (interactive)
  (buftil-init)
  ;; Set up window/buffer configuration
  (switch-to-buffer "test")
  (split-window-right)
  (display-buffer "dict")
  (with-current-buffer "dict"
    (org-mode)
    (org-cycle-global 2)
    )
  ;; Exit with C-g
  (while t (buftil-repl))
  )

(provide 'buftil)

   
