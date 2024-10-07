;;; org-englearn.el --- English learning workflow with Org -*- lexical-binding: t -*-

;; Commentary:
;; This package provides an efficient English learning workflow, combining with org-mode, org-capture, org-roam.

(require 'cl-lib)
(require 'hydra)
(require 'go-translate)
(require 'org)
(require 'org-capture)
(require 'org-roam)

(defcustom org-englearn-file-meanings (expand-file-name "org-roam/english-learning/meanings.org" org-directory)
  "Path to the file for English meanings.")

(defcustom org-englearn-file-inbox (expand-file-name "org-capture/english.org" org-directory)
  "Path to the file as a inbox of English vocabulary and difficult sentences.")

(defcustom org-englearn-file-words (expand-file-name "org-roam/english-learning/words.org" org-directory)
  "Path to the file for English words or phrases.")

(defcustom org-englearn-translation-engines (make-instance 'gt-youdao-dict-engine)
  "The translation engines used by `go-translate'.")

(defun org-englearn-trim-string (string)
  (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" string))

(defun org-englearn-fill-heading ()
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
                     (let* ((phrase (buffer-substring-no-properties (region-beginning) (region-end)))
                            (element (cl-ecase (car (read-multiple-choice
                                                     "What's the form of this word in heading?"
                                                     `((?s "n.(sth./sb./sp.)")
                                                       (?n "n.(noun)")
                                                       (?v "v.(verb)")
                                                       (?p "prep.")
                                                       (?k ,phrase)
                                                       (?. "...")
                                                       (?  "(empty)"))))
                                       (?s (cl-ecase (car (read-multiple-choice "Some?"
                                                                                '((?t "sth.")
                                                                                  (?p "sp.")
                                                                                  (?b "sb.")
                                                                                  (?o "one's")
                                                                                  (?s "oneself"))))
                                             (?t "=sth.=")
                                             (?p "=sp.=")
                                             (?b "=sb.=")
                                             (?o "=one's=")
                                             (?s "=oneself=")))
                                       (?n "=n.=")
                                       (?v "=v.=")
                                       (?p "=prep.=")
                                       (?k phrase)
                                       (?. "=...=")
                                       (?  "")))
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
    (error "No mark set!")))

(defun org-englearn-move-capture-timestamp ()
  (org-back-to-heading)
  (forward-line)
  (let ((beg (point))
        timestamp)
    (end-of-line)
    (setq timestamp (buffer-substring-no-properties beg (point)))
    (delete-region beg (point))
    (delete-char 1)
    (goto-char (point-max))
    (forward-line -1)
    (end-of-line)
    (insert " " timestamp)))

(defun org-englearn-remove-redundant-delimiters-in-string (string)
  (replace-regexp-in-string "[[:space:]\n]+" " " string))

(defun org-englearn-translate-to-kill-ring (text)
  (gt-start
   (make-instance
    'gt-translator
    :taker (make-instance
            'gt-taker
            :text (org-englearn-remove-redundant-delimiters-in-string text)
            :langs '(en zh))
    :engines org-englearn-translation-engines
    :render (make-instance 'gt-kill-ring-render))))

;;;###autoload
(defun org-englearn-org-roam-node-insert ()
  (interactive)
  (setq-local org-roam-capture-templates `(("m" "Meaning"
                                            entry "* ${title}" :if-new (file ,org-englearn-file-meanings) :unnarrowed t :immediate-finish t)))
  (add-hook 'org-capture-after-finalize-hook #'org-englearn-capture-heading-by-id-hook -5 t)
  (unwind-protect (org-roam-node-insert)
    (remove-hook 'org-capture-after-finalize-hook #'org-englearn-capture-heading-by-id-hook t)
    (kill-local-variable 'org-roam-capture-templates)))

(defun org-englearn-capture-process-buffer (text context)
  (search-forward text)
  (set-mark (- (point) (length text)))
  (org-englearn-translate-to-kill-ring context)
  (activate-mark)
  (org-englearn-fill-heading)
  (org-englearn-move-capture-timestamp)
  (goto-char (point-max))
  (forward-line -2)
  (end-of-line)
  (insert " \\\\")
  (forward-line)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command)
  (yank)
  (org-back-to-heading)
  (forward-line 1)
  (indent-for-tab-command)
  (insert "- ")
  (let ((beg (point)))
    (forward-line)
    (beginning-of-line)
    (delete-char 1)
    (insert "-")
    (org-indent-item)
    (goto-char beg)
    (org-englearn-org-roam-node-insert)
    (beginning-of-line)
    (re-search-forward "^[[:space:]]*- ")
    (pcase (completing-read "What's its part?" '("adj." "adv." "n." "v." "vt." "vi." "prep.")
                            nil nil nil nil nil)
      (`"adv." (insert "adv. ")
       (end-of-line)
       (insert "地"))
      (`"adj." (insert "adj. ")
       (end-of-line)
       (insert "的"))
      (x (insert x " ")))))

;;;###autoload
(defun org-englearn-capture-process-region (&optional beg end)
  (interactive)
  (let* ((beg (or beg (if (region-active-p) (region-beginning) (mark))))
         (end (or end (if (region-active-p) (region-end) (point))))
         (_ (goto-char beg))
         (cap (buffer-substring-no-properties beg end))
         (beg (save-excursion
                (unless (> (point) (mark)) (exchange-point-and-mark))
                (backward-sentence)
                (point)))
         (end (save-excursion
                (unless (< (point) (mark)) (exchange-point-and-mark))
                (forward-sentence)
                (point)))
         (sentence (buffer-substring-no-properties beg end)))
    (org-englearn-capture-process-buffer cap sentence)))

(defun org-englearn-capture-heading-by-id-hook ()
  (when-let* ((roam-capture (org-roam-capture-p))
              (roam-list (plist-get org-capture-plist :org-roam))
              (ins-link (eq (plist-get roam-list :finalize) 'insert-link)))
    (with-current-buffer (org-capture-get :buffer)
      (goto-char (point-min))
      (re-search-forward (concat "^* " (plist-get roam-list :link-description)))
      (org-roam-capture--put :id (org-id-get-create)))))

(cl-pushnew
 `("e" "English Vocabulary" entry (file ,org-englearn-file-inbox) "* %?\n%U\n\n  %i\n  %a" :kill-buffer t)
 org-capture-templates :test #'string= :key #'car)

;;;###autoload
(defun org-englearn-capture (&optional beg end)
  (interactive)
  (let* ((beg (or beg (if (region-active-p) (region-beginning) (mark))))
         (end (or end (if (region-active-p) (region-end) (point))))
         (cap (org-englearn-remove-redundant-delimiters-in-string (buffer-substring-no-properties beg end))))
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
             (sentence (org-englearn-remove-redundant-delimiters-in-string (buffer-substring-no-properties beg end))))
        (deactivate-mark)
        (org-capture-string sentence "e")
        (org-englearn-capture-process-buffer cap sentence)))))

;;;###autoload
(defun org-englearn-process-inbox ()
  (interactive)
  (find-file org-englearn-file-inbox)
  (org-map-entries
   (lambda ()
     (setq org-map-continue-from (org-element-property
                                  :begin
                                  (org-element-at-point)))
     (org-narrow-to-element)
     (org-fold-show-subtree)
     (cl-ecase (car (read-multiple-choice "Which category does it belong to?"
                                          '((?w "words")
                                            (?c "complex sentence"))))
       (?w (org-cut-subtree)
           (with-current-buffer (find-file-noselect org-englearn-file-words) 
             (goto-char (point-max))
             (yank)
             (org-englearn-process-new-heading))))
     (widen))))

(defun org-englearn-process-new-heading ()
  (let ((words (org-map-entries (lambda () (nth 4 (org-heading-components)))))
        (word (nth 4 (org-heading-components)))
        item-title
        item)
    (when (-contains-p words word)
      (org-back-to-heading)
      (re-search-forward "^[[:space:]]*- ")
      (save-excursion (let ((beg (point)))
                        (end-of-line)
                        (setq item-title (org-englearn-trim-string (buffer-substring-no-properties beg (point))))))
      (backward-char 2)
      (org-mark-element)
      (setq item (buffer-substring-no-properties (mark) (point)))
      (deactivate-mark)
      (org-cut-subtree)
      (condition-case nil
          (progn (re-search-backward (concat (regexp-quote (concat "* " word)) "[[:space:]]*$"))
                 (save-restriction
                   (org-narrow-to-subtree)
                   (condition-case nil
                       (progn                              ; try
                         (re-search-forward (concat "^[[:space:]]*- " (regexp-quote item-title)))
                         (widen)
                         (org-end-of-item)
                         (let ((insert-point (point)))
                           (insert item)
                           (goto-char insert-point)
                           (forward-line)
                           (beginning-of-line)
                           (delete-region insert-point (point))))
                     (error                                ; catch
                      (re-search-forward "^[[:space:]]*- ")
                      (widen)
                      (org-end-of-item-list)
                      (insert item)))))
        (error
         (yank)
         (org-id-get-create))))))

(provide 'org-englearn)
