;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Author				: Aaron Siegler
;; Version				: 0.1
;; Last Modified		: 2011-03-03

;; 0.1   - initial version
;; TODO 
;; * Minor mode?
;; * Maximum # of buffers kept

(eval-when-compile
  (require 'cl))

(defvar *undo-close-last* nil
  "the last file that was closed")

(defun undo-close-undo ()
  "uncloses a buffer"
  (interactive)
  (when *undo-close-last*
	(find-file (pop *undo-close-last*))))

(defun undo-close-set-closed ()
  "okay"
  (let ((file (buffer-file-name (current-buffer))))
	(when file
	  (push file *undo-close-last*))))

(add-hook 'kill-buffer-hook 'undo-close-set-closed)

