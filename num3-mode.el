;;; num3-mode.el --- highlight groups of digits in long numbers  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2020 Free Software Foundation, Inc.

;; Author: Felix Lee <felix8a@gmail.com>, Michal Nazarewicz <mina86@mina86.com>
;; Maintainer: Michal Nazarewicz <mina86@mina86.com>
;; Keywords: faces, minor-mode
;; Version: 1.5

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Num3 is a minor mode that makes long numbers and other numeric data
;; more readable by highlighting groups of digits in a string when
;; font-lock is on.
;;
;; For example, an integer ‘28318530’ would be split into 3-digit-long
;; groups as ‘(28)(318)(530)’ and then each group highlighted using
;; alternating face.  This improves readability of the number by clearly
;; denoting millions, thousands and ones.  Besides integers and real
;; numbers, the mode also supports dates in ‘20151021T0728’ format.
;;
;; The mode supports:
;; - decimal numbers, e.g. 1234567 or 6.2831853,
;; - hexadecimal integers, e.g. 0x1921FB5, #x1921FB5 or unprefixed 1921FB5,
;; - octal integers, e.g. 0o1444176 or #o1444176,
;; - binary integers, e.g. 0b1100100100 or #b1100100100,
;; - hexadecimal floating point numbers, e.g. 0x1.921FB54p+2,
;; - timestamps, e.g. 20151021T0728
;;
;; Decimal and octal numbers are split into 3-digit-long groups (the
;; length is customisable); hexadecimal and binary numbers are split
;; into 4-digit-long groups; timestamps are split based on the part of
;; the date or time.

;;; Usage:

;;     M-x num3-mode           toggle for current buffer.
;;     M-x global-num3-mode    toggle for all buffers.
;;
;; Or add the following to your ~/.emacs file:
;;     (load "path/to/num3")
;;     (global-num3-mode)

;;; Code:

(defgroup num3 nil
  "Num3 is a minor mode that makes long numbers more readable by
highlighting groups of digits in a number."
  :group 'text)

(defcustom num3-group-size 3
  "Number of digits to group in decimal numbers."
  :type 'integer)

(defcustom num3-threshold 5
  "Number must be at least that long to start highlighting."
  :type 'integer)

(defface num3-face-odd
  '((t))
  "Face to add for odd groups of digits."
  :group 'num3)

(defface num3-face-even
  '((t :underline t :weight bold :background "#eeeeee"))
  "Face to add for even groups of digits.
The default face uses redundant signaling, because this is in
addition to any other font-lock highlighting."
  :group 'num3)

;;; Implementation:

;;;###autoload
(define-minor-mode num3-mode
  "Toggle num3 minor mode in the current buffer.
Num3 minor mode makes long numbers and timestamps more readable
by highlighting groups of digits when font-lock mode is enabled.

If a number is longer than `num3-threshold', the mode will split
it into a group of `num3-group-size' (if number is decimal) or
four (if number is hexadecimal or binary) digits and highlight
alternating groups using `num3-face-odd' and ‘num3-face-even’
faces.

Hexadecimal integers are recognised by \"0x\" or \"#x\" prefix
and binary numbers by \"0b\" or \"#b\" prefix.  There is no
special handling for octal numbers – starting with \"0o\" or
\"#o\" – and instead they are handled like decimal numbers.

Decimal and hexadecimal floating-point numbers are recognised as
well.  Their fractional part is grouped from the beginning rather
then the end.  For instance \"12345.12345\" will be split into
groups as follows: \"12|345.123|45\".  Hexadecimal floating-point
numbers must start with \"0x\" prefix and include the exponent,
e.g. \"0x1d.2e5p3\" (which equals 29 + 741/4096 * 2⁵)

Timestamps are recognised if they match basic ISO 8061 form (for
example \"20220805T1258\") and can include seconds, fractions of
seconds and timezone offset."
  :lighter    " num3"
  (if num3-mode
      (unless (assoc 'num3--matcher font-lock-keywords)
        (font-lock-add-keywords nil '(num3--matcher) 'append))
    (font-lock-remove-keywords nil '(num3--matcher)))
  (if (fboundp 'font-lock-flush) (font-lock-flush)
    (when font-lock-mode (with-no-warnings (font-lock-fontify-buffer)))))

;;;###autoload
(define-globalized-minor-mode global-num3-mode num3-mode num3-mode)

(defun global-num3-mode-cmhh nil
  "Only enable num3-mode if variable `font-lock-mode' is enabled."
  (if font-lock-mode
      (add-to-list 'global-num3-mode-buffers
                   (current-buffer))
    (setq global-num3-mode-buffers
          (delete (current-buffer) global-num3-mode-buffers))))

(defconst num3--number-re
  ;; Hexadecimal and binary are both using the first capture group because we
  ;; group them both in four-digit groups.  There’s no explicit support for
  ;; octal numbers because we just use logic for a decimal number, i.e. the same
  ;; grouping.
  (rx
   (or
    ;; ISO 8601 basic form, e.g. ‘20210203T0405’.  Note that this needs to be
    ;; placed before other cases because we want it to match before the regex
    ;; engine matches plain decimal numbers.
    (submatch-n
     5                                             ; 5 - date-time
     (= 8 num)
     ?T
     (submatch-n
      6                                            ; 6 - time
      (= 2 num)
      (? (= 2 num)
         (? (= 2 num)
            (? ?. (submatch-n 3 (+ num))))))       ; 3 - decimal fraction
     (? (in "-+") (submatch-n 7 (+ num))))         ; 7 - offset
    ;; ‘#x1234’
    (seq ?# (in "xX") (submatch-n 1 (+ hex)))      ; 1 - hexadecimal integer
    ;; ‘0x1234’ or ‘0x1234.5678p±9’.  Hexadecimal floating point numbers
    ;; require exponent to be present.  ‘0x1234.5678’ are two separate
    ;; numbers with a dot between them.
    (seq ?0 (in "xX") (submatch-n 1 (* hex))       ; 1 - hexadecimal integer
         (? ?. (submatch-n 4 (+ hex))              ; 4 - hexadecimal fraction
            (in "pP") (? (in "-+"))
            (submatch-n 2 (+ num))))               ; 2 - decimal int (power)
    ;; ‘0b1010’ or ‘#b1010’.  Binary numbers use the same group as hexadecimal
    ;; numbers as they also use grouping of four when highlighted.  Note that
    ;; this group must be before we match unprefixed hexadecimal numbers.
    (seq (in "0#") (in "bB")
         (submatch-n 1 (+ (in "01"))))             ; 1 - binary integer
    ;; ‘1234abcd’, i.e. hexadecimal number w/o prefix.  There are a few things
    ;; we want to watch out for:
    ;; - the match must not be part of a word since we don’t want to match leet
    ;;   speak or parts of usernames,
    ;; - the match must contain at least one decimal digits since we don’t want
    ;;   to match words which happen to match [a-f]*,
    ;; - the match must contain at least on of non-decimal hexadecimal digits or
    ;;   else it’s just a decimal integer and
    ;; - the match must not start with ‘0b’ and contain only ones and zeros
    ;;   since that’s binary number.  Note that we actually do nothing to
    ;;   prevent binary numbers from matching.  Instead we rely upon those cases
    ;;   being caught by previous cases.
    (submatch-n 1                                  ; 1 - hexadecimal integer
                word-boundary
                (or (seq (+ num) (in "a-fA-F"))
                    (seq (+ (in "a-fA-F")) num))
                (* hex)
                word-boundary)
    ;; ‘1234’
    (submatch-n 2 (+ num))                         ; 2 - decimal integer
    (seq ?. (submatch-n 3 (+ num))))))             ; 3 - decimal fraction

(defun num3--matcher (lim)
  "Function used as a font-lock-keywoard handler used in `num3-mode'.
Performs fontification of numbers from point to LIM."
  (save-excursion
    (while (re-search-forward num3--number-re lim t)
      (num3--int  (match-beginning 1) (match-end 1) 4)
      (num3--frac (match-beginning 4) (match-end 4) 4)
      (num3--int  (match-beginning 2) (match-end 2) num3-group-size)
      (num3--frac (match-beginning 3) (match-end 3) num3-group-size)

      ;; date-time
      (when-let ((lo (match-beginning 5)))
        (num3--put t      lo     (+ lo  4))               ; year
        (num3--put nil (+ lo  4) (+ lo  6))               ; month
        (num3--put t   (+ lo  6) (+ lo  8))               ; day
        (let ((lo (match-beginning 6)) (hi (match-end 6)))
          (num3--frac lo (min hi (+ 6 lo)) 2 0 t))        ; hhmmss
        ;; hhmm of the timezone offset, but check length in case it’s too long
        ;; to be an offset and then treat it like a number.
        (when-let ((lo (match-beginning 7)) (hi (match-end 7)))
          (if (<= (- hi lo) 4)
              (num3--frac lo hi 2 0 t)
            (num3--int lo hi num3-group-size))))))
  nil)

(defun num3--int (lo hi n)
  "Highlight groups of digits in a long number.
LO and HI arguments specify the range where the number is
located.  If the length of that region exceeds `num3-threshold',
the function will split it into groups of N digits and fontify
tham alternately using `num3-face-odd' and `num3-face-even'
faces.  Grouping is done from the end, eg. (12)(345)."
  (when (and lo (>= (- hi lo) num3-threshold))
    (let (even)
      (while (< lo hi)
        (num3--put even (max lo (- hi n)) hi)
        (setq hi (- hi n) even (not even))))))

(defun num3--frac (lo hi n &optional threshold even)
  "Highlight groups of digits in a long number.
LO and HI arguments specify the range where the number is
located.  If the length of that region exceeds THRESHOLD (or
`num3-threshold' if that argument is nil), the function will
split it into groups of N digits and fontify tham alternately
using `num3-face-odd' and `num3-face-even' faces.  Grouping is
done from the beginning, eg. (123)(45).  EVEN argument specify
whether the first group should be highlighted using even or odd
face."
  (when (and lo (>= (- hi lo) (or threshold num3-threshold)))
    (while (< lo hi)
      (num3--put even lo (min hi (+ lo n)))
      (setq lo (+ lo n) even (not even)))))

(defun num3--put (even lo hi)
  "Add font lock text property to highlight a single group of digit.
Use `num3-face-odd' if EVEN is nil and `num3-face-even' if EVEN is
non-nil.  The region the face is set to is from LO to HI."
  (font-lock-append-text-property lo hi 'face
                                  (if even 'num3-face-even 'num3-face-odd)))

(provide 'num3-mode)
;;; num3-mode.el ends here
