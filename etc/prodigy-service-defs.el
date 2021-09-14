;;; prodigy-service-defs.el --- Services for prodigy.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience

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

;; Definitions of services that can be used by prodigy.el

;;; Code:

(require 'prodigy)
(require 's)

(prodigy-define-service
  :name "zookeeper@:2181"
  :command "bash"
  :args `("-c"
          ,(concat
            " docker network create kafka-net || true; "
            " docker stop zookeeper || true; "
            " docker rm zookeeper || true; "
            " docker run "
            " -e 'ZOOKEEPER_CLIENT_PORT=2181' "
            " --net kafka-net "
            " --name zookeeper "
            " --publish 2181:2181 "
            " confluentinc/cp-zookeeper "))
  :cwd "/tmp"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "kafka@:9092"
  :command "bash"
  :args `("-c"
          ,(concat
            " docker network create kafka-net || true; "
            " docker stop kafka || true; "
            " docker rm kafka || true; "
            " docker run --hostname localhost "
            " --net kafka-net "
            " --name kafka "
            " --publish 9092:9092 "
            " --publish 7203:7203 "
            " --env KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR=1 "
            " --env KAFKA_ZOOKEEPER_CONNECT=zookeeper:2181 "
            " --env KAFKA_ADVERTISED_LISTENERS=PLAINTEXT://127.0.0.1:9092 "
            " confluentinc/cp-kafka "))
  :cwd "/tmp"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)


(prodigy-define-service
  :name "echo_server@:9191"
  :command "bash"
  :args '("-c"
          " while [ true ]; do
			  echo 'HTTP/1.1 200 Ok\r\n' | nc -l -p 9191;
			  echo -e '\n';
		    done")
  :cwd "~"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "blog@localhost"
  :command "python"
  :args '("-m" "http.server" "8000")
  :cwd "~/code/blog/"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "OutlineVPN"
  :sudo t
  :command "outline-client"
  :args (list)
  :stop-signal 'int
  :kill-process-buffer-on-stop t
  :truncate-output t)

(prodigy-define-service
  :name "root@sshuttle-remote"
  :sudo t
  :command "sshuttle"
  :args (lambda () (list "-r" (read-string "Host: ") "--dns" "-v" "0/0"))
  :stop-signal 'int
  :kill-process-buffer-on-stop t
  :truncate-output t)

(prodigy-define-service
  :name "Host Stats"
  :command "bash"
  :args (lambda ()
          (let ((interval (read-number "Interval: ")))
            (list "-c"
                  (format "while [ true ] ; do
                               echo 'ƛ';
                               printf '\n%s\n' 'IO Stats';
                               iostat --human;
                               printf '\n%s\n' 'VM Stats';
                               vmstat -tSM;
                               printf '\n%s\n' 'Processor Stats';
                               mpstat -P ALL;
                           sleep %s;
                           done;"
                          "%60s"
                          "%60s"
                          "%60s"
                          interval))))
  :on-output (lambda (&rest service-opts)
               (prodigy-with-service-process-buffer
                   (plist-get service-opts :service)
                 (save-excursion
                   (goto-char (point-min))
                   (when (search-forward "ƛ" (point-max) t)
                     (delete-region (point-min)
                                    (point))))))
  :stop-signal 'int
  :kill-process-buffer-on-stop t
  :truncate-output t)


(prodigy-define-service
  :name "SSH SOCKS5 Proxy"
  :command "ssh"
  :stop-signal 'int
  :kill-process-buffer-on-stop t
  :args (lambda (&rest _service-opts)
          (list "-D"  (read-string "Port: ")
                "-fN" (read-string "Host: "))))


(provide 'prodigy-service-defs)
;;; prodigy-service-defs.el ends here
