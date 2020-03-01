;;; mylife-plotter.el --- Helper functions for plotting data  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: multimedia

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

;; 

;;; Code:

(defcustom mylife-gnu-binary
  "gnuplot"
  "Name of the gnuplot binary. It should be \"wgnuplot\" on Windows"
  :group 'my-life
  :type 'string)

(defun mylife-gnu-plot-region (beg end)
  "Plot the columns in region BEG to END."
  (ignore))

(provide 'mylife-plotter)
;;; mylife-plotter.el ends here
