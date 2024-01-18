;;; norwegian-holidays.el --- Norwegian holidays for the calendar  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2024 Simen Heggestøyl

;; Author: Simen Heggestøyl <simenheg@runbox.com>
;; Keywords: calendar
;; Version: 0.1

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

;; Provides Norwegian holidays for the calendar package.

;; When installed with `package.el', the holidays will automatically
;; be put into `holiday-other-holidays'.

;; Else, to have them appear in the calendar, add something like the
;; following to your initialization file:

;; (dolist (h holiday-norwegian-holidays)
;;   (add-to-list 'holiday-other-holidays h))

;;; Code:

;;;###autoload
(defvar holiday-norwegian-holidays
  '((holiday-fixed 1 1 "Første nyttårsdag")
    (holiday-fixed 5 1 "Arbeidernes dag")
    (holiday-fixed 5 17 "Grunnlovsdag")
    (holiday-fixed 12 25 "Første juledag")
    (holiday-fixed 12 26 "Andre juledag")
    (holiday-easter-etc -7 "Palmesøndag")
    (holiday-easter-etc -3 "Skjærtorsdag")
    (holiday-easter-etc -2 "Langfredag")
    (holiday-easter-etc 0 "Første påskedag")
    (holiday-easter-etc 1 "Andre påskedag")
    (holiday-easter-etc 39 "Kristi himmelfartsdag")
    (holiday-easter-etc 49 "Første pinsedag")
    (holiday-easter-etc 50 "Andre pinsedag"))
  "Norwegian holidays.")

;;;###autoload
(dolist (h holiday-norwegian-holidays)
  (add-to-list 'holiday-other-holidays h))

(provide 'norwegian-holidays)
;;; norwegian-holidays.el ends here
