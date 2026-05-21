(in-package :cl-cc/runtime)

(defstruct rt-registered-buffer
  (id nil)
  (buffer nil)
  (pinned-p nil)
  (registered-at 0))

(defvar *rt-reg-bufs* (make-hash-table))

(defun rt-register-buffer (id buffer &key pin)
  (setf (gethash id *rt-reg-bufs*)
        (make-rt-registered-buffer :id id :buffer buffer :pinned-p pin
                                   :registered-at (get-internal-real-time)))
  t)

(defun rt-registered-buffer (id)
  (gethash id *rt-reg-bufs*))

(defun rt-unregister-buffer (id)
  (remhash id *rt-reg-bufs*)
  t)

(defun rt-sendfile (out in &optional offset count)
  "Portable sendfile stub: copy from stream IN to stream OUT."
  (when offset (file-position in offset))
  (let ((buf (make-array 8192 :element-type '(unsigned-byte 8)))
        (remaining count)
        (written 0))
    (loop while (or (null remaining) (plusp remaining))
          for want = (if remaining (min remaining (length buf)) (length buf))
          for n = (read-sequence buf in :end want)
          while (plusp n)
          do (write-sequence buf out :end n)
             (incf written n)
             (when remaining (decf remaining n)))
    (finish-output out)
    written))

(defun rt-splice (in out count)
  (rt-sendfile out in nil count))

(defun rt-copy-buffer (source-id target-id &key (start 0) end)
  (let* ((source (rt-registered-buffer-buffer (rt-registered-buffer source-id)))
         (target (rt-registered-buffer-buffer (rt-registered-buffer target-id)))
         (limit (or end (length source))))
    (replace target source :start2 start :end2 limit)
    (- limit start)))

(defun rt-zerocopy-init ()
  (clrhash *rt-reg-bufs*)
  t)
