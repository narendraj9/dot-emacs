;;; cricbuzz.el --- A simple wrapper around cricbuzz api  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: cricket

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Fetch cricket scores of recent matches.

;;; Code:


(defun match->string (match)
  "Render a single MATCH.
Example match:
\(match ((id . 16713)
        (type . ODI)
        (srs . ICC Champions Trophy, 2017)
        (mchDesc . BAN vs IND)
        (mnum . 2nd Semi-Final (A2 v B1))
        (inngCnt . 2)
       (state ((mchState . Result) (status . Ind won by 9 wkts)) 	)
       (manofthematch ((NoOfPlayers . 1))
                      (mom ((Name . Rohit Sharma)))
                      )
       (Tm ((id . 6) (Name . Ban) (sName . BAN) (flag . 1)))
       (Tm ((id . 2) (Name . Ind) (sName . IND) (flag . 1)))

       (Tme ((Dt . Jun 15 2017) (stTme . 09:30) (enddt . Jun 15
  2017))))"
  (let* ((description (xml-get-attribute match 'mchDesc))
         (series (xml-get-attribute match 'srs))
         (state (car (xml-get-children match 'state)))
         (status (xml-get-attribute state 'status)))
    (format "%-10s | %-40s | %-20s  " description series status)))


(defun cricbuzz-callback (status)
  (search-forward "\n\n")
  (let* ((match-data (car (xml-parse-region (point) (point-max))))
         (matches (xml-get-children match-data 'match))
         (score-info (mapconcat #'match->string matches "\n")))
    (message score-info)))

(defun show-all-scores ()
  "Show all scores in echo area."
  (interactive)
  (let ((url-request-method "GET")
        (url (url-encode-url "http://synd.cricbuzz.com/j2me/1.0/livematches.xml")))
    (url-retrieve url #'cricbuzz-callback)))

(provide 'cricbuzz)
;;; cricbuzz.el ends here
