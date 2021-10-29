;;; -*- lexical-binding: t -*-

(require 'hydra)
(require 'go-translate)
(require 'org-capture)

(defvar gts-kill-ring-only-translation-render-source-text nil
  "")


(defun org-englearn-trim-string (string)
  (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" string))

(defclass gts-kill-ring-only-translation-render (gts-render) ())

(cl-defmethod gts-out ((_ gts-kill-ring-only-translation-render) task)
  (deactivate-mark)
  (with-slots (result ecode) task
    (unless ecode
      (kill-new (org-englearn-trim-string (replace-regexp-in-string (regexp-quote gts-kill-ring-only-translation-render-source-text) "" result))))
    (setq gts-kill-ring-only-translation-render-source-text nil)
    (message "Translation has saved to kill ring.")))

(defvar org-englearn-gts-translator
  (gts-translator
       :picker (gts-prompt-picker)
       :engines (list (gts-google-engine))
       :render (gts-kill-ring-only-translation-render))
  "")

(defun org-englearn-fill-heading ()
  (interactive)
  (if (region-active-p)
      (atomic-change-group
        (let* ((sentence-begin (region-beginning))
               (sentence-end (region-end))
               last-mark
               (first-loop t))
          (goto-char sentence-begin)
          (while (progn
                   (when (not (eq (point) last-mark))
                     (set-mark (setq last-mark (point)))
                     (forward-sexp)
                     (let* ((phrase (buffer-substring (region-beginning) (region-end)))
                            (element (pcase (read-key)
                                       (`?s (pcase (read-key)
                                              (`?t "=sth.=")
                                              (`?p "=sp.=")
                                              (`?b "=sb.=")
                                              (x (error "Unknown element of s%s!" x))))
                                       (`?n "=n.=")
                                       (`?v "=v.=")
                                       (`?p "=prep.=")
                                       (`? phrase)
                                       (`?. "=...=")
                                       (`?  "")
                                       (x (error "Unknown element of %s!" x))))
                            (insertion (concat (if (or (string-empty-p element) first-loop)
                                                   "" " ")
                                               element)))
                       (save-excursion
                         (org-back-to-heading)
                         (end-of-line)
                         (insert insertion)
                         (let ((inc (length insertion)))
                         (cl-incf sentence-begin inc)
                         (cl-incf sentence-end inc)
                         (cl-incf last-mark inc))))
                     (ignore-errors
                       (forward-sexp)
                       (backward-sexp))
                     (setq first-loop nil)
                     (and (< (point) sentence-end)))))
          (deactivate-mark)
          (goto-char sentence-end)
          (insert "_")
          (goto-char sentence-begin)
          (insert "_")))
    (error "Please select a region first !")))

(defun org-englearn-capture (&optional beg end)
  (interactive)
  (let* ((beg (or beg (if (region-active-p) (region-beginning) (mark))))
         (end (or end (if (region-active-p) (region-end) (point))))
         (cap (buffer-substring-no-properties beg end)))
    (if (string-match-p (regexp-quote ".") cap)
        (org-capture-string cap "e")
      (let* ((beg (save-excursion
                    (unless (> (point) (mark)) (exchange-point-and-mark))
                    (backward-sentence)
                    (point)))
             (end (save-excursion
                    (unless (< (point) (mark)) (exchange-point-and-mark))
                    (forward-sentence)
                    (point)))
             (sentence (buffer-substring-no-properties beg end)))
        (deactivate-mark)
        (org-capture-string sentence "e")
        (search-forward cap)
        (set-mark (- (point) (length cap)))
        (setq gts-kill-ring-only-translation-render-source-text sentence)
        (gts-translate org-englearn-gts-translator sentence "en" "zh")
        (activate-mark)
        (org-englearn-fill-heading)
        (end-of-buffer)
        (indent-for-tab-command)
        (yank)))))

(provide 'org-englearn)
