;;; cmdragons-play-mode --- Major mode for cmdragons play files

;;; Commentary:

;; Used for editing CMDragons plays

;;; Code:

;; define keyword categories

(setq cmdragons-play-keywords '("PLAY" "GAMESTATE" "PRECOND" "INVARIANT" "NUMROBOTS" "FLAGS" "ROLE" "ABORT"))
(setq cmdragons-play-constants '("game_on"
				 "game_off"
				 "our_kickoff"
				 "their_kickoff"
				 "our_penalty"
				 "their_penalty"
				 "offense"
				 "defense"
				 "loose_ball"
				 "contended"
				 "their_side"
				 "our_side"
				 "midfield"
				 "their_defense_area"
				 "our_defense_area"
				 "our_corner"
				 "their_corner"
				 "opp_our_side"
				 "their_set_play"
				 "winning"
				 "losing"
				 "winning_big"
				 "losing_big"
				 "near_end"
				 "completed"
				 "no_goalie"
				 "static_roles"
				 "loop"
				 "no_flip_y"))

;; generate regexp for keywords

;; (setq cmdragons-play-types-regexp "\\(\\(@\\)[[:ascii:]]\\|[[:digit:]]+\\.?[[:digit:]]*\\(o\\|d\\|f\\)\\)")
(setq cmdragons-play-at-sign-regexp "\\(@\\)[[:ascii:]]+?\\b")
(setq cmdragons-play-coordinate-symbols "[[:digit:]]+\\.?[[:digit:]]*\\(o\\|d\\|f\\)")
(setq cmdragons-play-keywords-regexp (regexp-opt cmdragons-play-keywords 'words))
(setq cmdragons-play-constants-regexp (regexp-opt cmdragons-play-constants 'words))
(setq cmdragons-play-comment-regexp "#.*")

(setq cmdragons-play-font-lock-keywords
      `(
	(,cmdragons-play-at-sign-regexp (1 font-lock-type-face))
	(,cmdragons-play-coordinate-symbols (1 font-lock-type-face))
	(,cmdragons-play-keywords-regexp . font-lock-keyword-face)
	(,cmdragons-play-constants-regexp . font-lock-constant-face)
	(,cmdragons-play-comment-regexp . font-lock-comment-face)))

(defvar cmdragons-play-indent-offset 4
  "*Indentation offset for `cmdragons-play-mode'.")

(defvar cmdragons-play-mode-hook '())

(define-derived-mode cmdragons-play-mode prog-mode "cmdragons-play-mode"
  "Major mode for editing CMDragons play files"
  (setq font-lock-defaults '((cmdragons-play-font-lock-keywords)))
  (setq comment-start "#")
  (setq comment-end "")
  (setq tab-width cmdragons-play-indent-offset)
  (setq indent-tabs-mode nil))

(provide 'cmdragons-play-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; cmdragons-play-mode.el ends here

