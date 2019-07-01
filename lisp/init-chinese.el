;; init-chinese.el --- Initialize chinese font configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Rice Wine

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;


;;; Commentary:
;;
;; Show Chinese characters correctly.
;;

;;; Code:

;; 能自动在系统中寻找能用的中文字体。

;; 找到中文字体很重要，因为在windows下，如果没有合适的字体，emacs会变得无法忍受的慢！

;; 代码还没看。

;; 参考资料：[[http://zhuoqiang.me/torture-emacs.html]]

(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil
    t))

(defvar font-list '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))

(require 'cl) ;; find-if is in common list package
(find-if #'qiang-font-existsp font-list)

(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)

  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl) ; for find if
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                            :size chinese-font-size)))

    ;; Set the default English font
    ;;
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (message "Set English Font to %s" en-font)
    (set-face-attribute 'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font))))
;; (when (display-graphic-p))
(qiang-set-font
'("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") ":pixelsize=18"
'("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))

(set-language-info
     "UTF-8"
     'coding-priority
     '(utf-8 gb18030 gbk gb2312 iso-2022-cn chinese-big5 chinese-iso-8bit iso-8859-1))

(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8)

(when (eq system-type 'windows-nt)
  (set-default 'process-coding-system-alist
               '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
	         ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos))))

(setq file-name-coding-system 'gb18030)

(defun rw-proced-coding-transform ()
  (with-current-buffer "*Proced*"
    ;; if there are "\324" "\302", then the data of buffer is raw bytes before any coding interpratation
    (when (eq system-type 'windows-nt)
      (decode-coding-region (point-min) (point-max) 'gbk)))
  )

;; Changes representation of time from Chinese to English.
;; Someone says it may bug Chinese Input Method in Linux. 
(when (eq system-type 'windows-nt)
  (setq system-time-locale "C"))

(provide 'init-chinese)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
