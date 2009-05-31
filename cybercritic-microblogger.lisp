;; cybercritic-microblogger.lisp -  The main generate-and-blog code.
;; Copyright (C) 2009  Rob Myers rob@robmyers.org
;;
;; This file is part of cybercritic-microblogger.
;;
;; cybercritic-microblogger is free software; you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License as published 
;; by the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; cybercritic-microblogger is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(in-package #:cybercritic)

(defclass cybercritic-microblogger (microblog-bot:microblog-follower-bot)
  ())

(defmethod respond-to-mention ((bot cybercritic-microblogger) mention)
  "Respond to the mention by plugging the source."
  (cl-twit:update (format nil "@~a Hi! You can see my source code here - http://robmyers.org/git/?p=cybernetic-critic-microblogger.git" 
			  (cl-twit:user-screen-name 
			   (cl-twit:status-user mention)))))

(defmethod periodic-task ((bot cybercritic-microblogger))
  "Update the aesthetic and dent it."
  (update-aesthetic)
  (let ((description (describe-aesthetic)))
    ;; Handle description being longer than the microblogging limit of 140 chars
    (if (> (length description) 140)
	(setf description 
	      (format nil "~a..." (subseq description 0 137))))
    (twit:update description)))

(defmethod respond-to-message ((bot cybercritic-microblogger) mention)
  "Respond to the artwork by critiquing it."
  (cl-twit:update 
   (format nil 
	   (critique-artwork (cl-twit:status-text mention) 
			     (format nil
				     "http://identi.ca/notice/~a"
				     (cl-twit:status-id mention))))))

(defun run-cybercritic-bot (user password follow)
  (setf *random-state* (make-random-state t))
  (microblog-bot:set-microblog-service "http://identi.ca/api" "cybercritic")
  (initialize-aesthetic)
  (let ((bot (make-instance 'cybercritic-microblogger
			    :nickname user	    
			    :password password
			    :follow-id follow)))
    (microblog-bot:run-bot bot)))

(defun run ()
  "Configure and run the bot."
  (assert (>= (length sb-ext:*posix-argv*) 3))
  (run-cybercritic-bot (second sb-ext:*posix-argv*) 
		       (third sb-ext:*posix-argv*) 
		       (fourth sb-ext:*posix-argv*)))
