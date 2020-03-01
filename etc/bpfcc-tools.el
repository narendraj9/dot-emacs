;;; bpfcc-tools.el --- Helper functions for using or learning about eBPF tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar bfcc-tools '(argdist-bpfcc bashreadline-bpfcc biolatency-bpfcc biosnoop-bpfcc biotop-bpfcc bitesize-bpfcc bpflist-bpfcc btrfsdist-bpfcc btrfsslower-bpfcc cachestat-bpfcc cachetop-bpfcc capable-bpfcc cobjnew-bpfcc cpudist-bpfcc cpuunclaimed-bpfcc criticalstat-bpfcc dbslower-bpfcc dbstat-bpfcc dcsnoop-bpfcc dcstat-bpfcc deadlock_detector-bpfcc deadlock_detector.c-bpfcc execsnoop-bpfcc ext4dist-bpfcc ext4slower-bpfcc filelife-bpfcc fileslower-bpfcc filetop-bpfcc funccount-bpfcc funclatency-bpfcc funcslower-bpfcc gethostlatency-bpfcc hardirqs-bpfcc inject-bpfcc javacalls-bpfcc javaflow-bpfcc javagc-bpfcc javaobjnew-bpfcc javastat-bpfcc javathreads-bpfcc killsnoop-bpfcc llcstat-bpfcc mdflush-bpfcc memleak-bpfcc mountsnoop-bpfcc mysqld_qslower-bpfcc nfsdist-bpfcc nfsslower-bpfcc nodegc-bpfcc nodestat-bpfcc offcputime-bpfcc offwaketime-bpfcc oomkill-bpfcc opensnoop-bpfcc perlcalls-bpfcc perlflow-bpfcc perlstat-bpfcc phpcalls-bpfcc phpflow-bpfcc phpstat-bpfcc pidpersec-bpfcc profile-bpfcc pythoncalls-bpfcc pythonflow-bpfcc pythongc-bpfcc pythonstat-bpfcc reset-trace-bpfcc rubycalls-bpfcc rubyflow-bpfcc rubygc-bpfcc rubyobjnew-bpfcc rubystat-bpfcc runqlat-bpfcc runqlen-bpfcc runqslower-bpfcc shmsnoop-bpfcc slabratetop-bpfcc sofdsnoop-bpfcc softirqs-bpfcc solisten-bpfcc sslsniff-bpfcc stackcount-bpfcc statsnoop-bpfcc syncsnoop-bpfcc syscount-bpfcc tclcalls-bpfcc tclflow-bpfcc tclobjnew-bpfcc tclstat-bpfcc tcpaccept-bpfcc tcpconnect-bpfcc tcpconnlat-bpfcc tcpdrop-bpfcc tcplife-bpfcc tcpretrans-bpfcc tcpstates-bpfcc tcpsubnet-bpfcc tcptop-bpfcc tcptracer-bpfcc tplist-bpfcc trace-bpfcc ttysnoop-bpfcc vfscount-bpfcc vfsstat-bpfcc wakeuptime-bpfcc xfsdist-bpfcc xfsslower-bpfcc zfsdist-bpfcc zfsslower-bpfcc))

(defvar bpfcc-tools-with-descriptions
  (mapcar (lambda (tname)
            (propertize (symbol-name tname)
                        'display
                        (s-trim (shell-command-to-string (format "whatis %s" tname)))))
          bfcc-tools))


(defun bpfcc-tools-man-page ()
  "Open `man' page for a bpfcc tool."
  (interactive)
  (man (completing-read "man: " bpfcc-tools-with-descriptions)))


(provide 'bpfcc-tools)
;;; bpfcc-tools.el ends here
