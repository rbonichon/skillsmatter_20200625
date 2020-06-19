;; Of course we need org ;-)
(require 'org)
;; org-ref to export citations
(require 'org-ref)
;; avoid exporting Beamer notes
(setq org-export-exclude-tags '("noexport" "B_note" "B_noteNH"))
;; inform the user of what we'are doing
(message
 (format "Exporting %s to HTML (excluding tags %s)"
         (current-buffer)
         org-export-exclude-tags))
;; Export !
(org-html-export-to-html)
