;; manipulate frames
(require 'cl)
(require 'cl-lib)

(defun rw-count-frames ()
  "count current frames, return a number"
  (let ((current-frame-list (frame-list)))
    (length current-frame-list)))

(defun rw-only-one-frame? ()
  "is there only one frame?"
  (let ((n (rw-count-frames)))
    (if (= n 1) t
      nil)))

(defun rw-get-monitor-name-of-frame (&optional frame)
  (let ((attributes
         (frame-monitor-attributes frame)))
    (cl-loop for attribute in attributes
             do (pcase attribute
                  (`(name . ,monitor-name)
                   (return monitor-name))
                  (_ nil))
             finally return nil)))

(defun rw-select-frame-in-other-monitor (&optional frame)
  "return the frame in other monitor comparing with frame, if no such frame, return nil"
  (let* ((frame (or frame (selected-frame)))
         (monitor1 (rw-get-monitor-name-of-frame frame)))
    (cl-loop for xframe = (next-frame frame) then (next-frame xframe)
             until (eql xframe frame)
             do
             (unless (string= (rw-get-monitor-name-of-frame xframe) monitor1)
               ;; xframe in different monitor with frame
               (return xframe))
             finally return nil)))

(defun rw-next-frame-in-same-monitor (&optional frame)
  "return the next frame in same monitor with frame, if no such frame, return frame itself"
  (let* ((frame (or frame (selected-frame)))
         (monitor1 (rw-get-monitor-name-of-frame frame)))
    (cl-loop for xframe = (next-frame frame) then (next-frame xframe)
             until (eql xframe frame)
             do
             (when (string= (rw-get-monitor-name-of-frame xframe) monitor1)
               ;; xframe in different monitor with frame
               (return xframe))
             finally return frame)))

(defun rw-previous-frame-in-same-monitor (&optional frame)
  "return the previous frame in same monitor with frame, if no such frame, return frame itself"
  (let* ((frame (or frame (selected-frame)))
         (monitor1 (rw-get-monitor-name-of-frame frame)))
    (cl-loop for xframe = (previous-frame frame) then (previous-frame xframe)
             until (eql xframe frame)
             do
             (when (string= (rw-get-monitor-name-of-frame xframe) monitor1)
               ;; xframe in different monitor with frame
               (return xframe))
             finally return frame)))

(provide 'rw-frame-lib)
