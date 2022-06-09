;; Funzioni custom per caricare gli excerpt 

;;; Excerpts

;; This section implements inline viewing of entry excerpts in the
;; search buffer.  Ideally we would insert text directly into the
;; buffer and use text-properties to distinguish inline content from
;; entry rows, because that would allow users to select and navigate
;; the inline text.  But because of the way `elfeed-search--offset'
;; works, inserting entry content between entry rows would cause
;; havoc.  So unless and until that functionality is changed (which
;; Chris is open to considering), we use overlays, which don't
;; interfere with the buffer's text.  However, this means that users
;; can't select text in the inline content, a minor inconvenience.

;; However, overlays present a certain problem of their own: if they
;; are longer than the window's text area, it can be impossible to
;; scroll to the end of the overlay, depending on the overlay's size.
;; And when that happens, the user can keep doing `forward-line', and
;; the line number changes, but the cursor does not appear to move,
;; and the buffer does not appear to scroll.  I'm not sure if it's a
;; bug in Emacs or just an idiosyncrasy, and I haven't found a way to
;; handle it well.  When it happens, the user will have to view the
;; entry in its own buffer to see all of it.

(defmacro elfeed-search-at-entry (entry &rest body)
  "If ENTRY is in search buffer, eval BODY with point at it."
  (declare (indent defun))
  `(let ((n (cl-position ,entry elfeed-search-entries)))
     (when n
       (elfeed-goto-line (+ elfeed-search--offset n))
       ,@body)))

(defun elfeed-search-excerpt-toggle (&optional all)
  "Toggle display of excerpts for current or selected entries.
If ALL is non-nil (interactively, with prefix), toggle all
excerpts: if any are present, hide all; otherwise, show as many
as can fit in the window."
  (interactive "P")
  (cl-labels ((present-p
               () (cl-loop for ov in (overlays-in (point-min) (point-max))
                           when (overlay-get ov :elfeed-excerpt)
                           return t))
              (lines-remaining
               () (- (save-excursion
                       (move-to-window-line -1)
                       (line-number-at-pos))
                     (line-number-at-pos)))
              (num-lines (s) (length (split-string s "\n" t)))
              (unread-at-point-p
               () (member 'unread (elfeed-entry-tags (elfeed-search-selected t))))
              (show-maybe
               (entry) (elfeed-search-at-entry entry
                         (when (<= (num-lines (elfeed-search-entry-excerpt entry))
                                   (lines-remaining))
                           (elfeed-search-excerpt-show))))
              (show-all-entries
               ()
               ;; Move to first unread entry.
               (cl-loop until (unread-at-point-p)
                        while (not (eobp))
                        do (forward-line 1))
               ;; Scroll line to top of window.
               (recenter-top-bottom 0)
               ;; Show first entry unconditionally.
               (elfeed-search-excerpt-show)
               ;; Show other entries as they fit.
               (forward-line 1)
               (setf (mark) (point-max))
               (cl-loop while (< (point) (mark))
                        while (show-maybe (elfeed-search-selected t))
                        do (forward-line 1))
               ;; HACK: If point is inside an overlay (sort of), move back to entry
               ;; to avoid weird behavior.  NOTE: This doesn't actually work, because
               ;; for some weird reason, the overlays at point are nil until the
               ;; command finishes.  Then, at the same point, the overlays are there.
               (let ((ovs (overlays-at (point))))
                 (when ovs
                   (goto-char (1- (overlay-start (car ovs))))
                   (beginning-of-line)))))
    (if all
        (if (present-p)
            (elfeed-search-excerpt-hide-all)
          (show-all-entries))
      (dolist (entry (elfeed-search-selected))
        (elfeed-search-at-entry entry
          (or (elfeed-search-excerpt-hide)
              (elfeed-search-excerpt-show)))))))

(defun elfeed-search-excerpt-hide-all ()
  "Hide all inline overlays."
  (cl-loop for ov in (overlays-in (point-min) (point-max))
           when (overlay-get ov :elfeed-excerpt)
           do (delete-overlay ov)))

;; Remove all overlays when search buffer is updated.
(add-hook 'elfeed-search-update-hook #'elfeed-search-excerpt-hide-all)

(defun elfeed-search-excerpt-hide ()
  "Hide excerpt for entry at point.
Return t when excerpt was found and hidden."
  (interactive)
  (cl-loop with pos = (1+ (line-end-position))
           for ov in (overlays-in pos pos)
           when (overlay-get ov :elfeed-excerpt)
           do (delete-overlay ov)
           and return t))

(defun elfeed-search-excerpt-show (&optional max-lines)
  "Show excerpt for entry at point and mark it read.
If MAX-LINES, limit excerpt to that many lines."
  (interactive)
  (let* ((entry (elfeed-search-selected 'ignore-region))
         (string (elfeed-search-entry-excerpt entry max-lines)))
    (save-excursion
      (goto-char (1+ (point-at-eol)))
      (elfeed-search-excerpt-insert entry string))
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)))

(defun elfeed-search-excerpt-insert (entry string)
  "Insert overlay at point showing STRING for ENTRY."
  (declare (indent defun))
  (let* ((ov (make-overlay (point) (point))))
    (overlay-put ov :elfeed-excerpt entry)
    (overlay-put ov 'after-string string)))

(defun elfeed-search-entry-excerpt (entry &optional max-lines)
  "Return excerpt string for ENTRY.
If MAX-LINES, limit excerpt to that many lines."
  (let* ((ref (elfeed-entry-content entry))
         (content (elfeed-deref ref))
         limit-reached-p
         (string (with-temp-buffer
                   (elfeed-insert-html
                    (concat "<blockquote>" content "</blockquote>"))
                   (when max-lines
                     (let ((limit (save-excursion
                                    (goto-char (point-min))
                                    (forward-line max-lines)
                                    (point-at-eol))))
                       (when (> (point-max) limit)
                         (setf limit-reached-p t)
                         (narrow-to-region (point-min) limit))))
                   (buffer-string))))
    (when limit-reached-p
      (setf string (concat string "...")))
    (propertize (concat string "\n")
                'face '(:inherit (variable-pitch default)))))
(provide 'elfeed-arxiv-excerpts)
