(import urntils/bindings/luasocket socket)
(import urntils/http http)
(import lua/os os)
(import lua/io (popen))
(import extra/io (append-all!))

(define print-output (= (car arg) "-l"))
(define repo-path "~/Programs/urn")
(define listen-port 2378)


(defun os-exec! (command)
  (when-let* [(handle (popen command))
              (data (self handle :read "*a"))]
    (self handle :close)
    data))

(defun gen-loc! ()
  (log! "Calculating lines of code...")
  (cut-head-lines (os-exec! (.. "cloc " repo-path)) 5))

(defun gen-compile-times! ()
  (log! "Calculating compile times...")
  (cut-tail-lines (cut-head-lines (os-exec! (.. "make -C " repo-path " tacky/cli LUA=luajit LUA_FLAGS+=-t")) 2) 2))

(defun escape (str)
  (id (string/gsub str "\n" "<br />")))

(defun log! (str)
  (with (logstr (.. (os/date "%m-%d %X") ": " str))
    (when print-output
      (print! logstr))  
    (append-all! "log.txt" (.. logstr "\n"))))

(defun cut-head-lines (str n)
  (with (lines (string/split str "\n"))
    (concat (drop lines n) "\n")))

(defun cut-tail-lines (str n)
  (with (lines (string/split str "\n"))
    (concat (take lines (- (# lines) n)) "\n")))
    

(defun generate-page! ()
  (let* [(page '())]
    (push-cdr! page 
     "<html>
        <head>
          <style>
            body {
              background-color: #2b2b2b;
              color: #dddddd;
              display: flex;
              align-items: center;
              justify-content: center;
            }
            a {
              color: #f0f0f0;
            }
            h1 {
              text-align: center;
            }
            h3 {
              text-align: center;
            }
            .page {
              background-color: #333333;
              padding-left: 20px;
              padding-right: 20px;
            }
            .monospace {
              font-family: Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New, monospace;
              white-space: pre;
            }
          </style>
        </head>
        <body><div class=\"page\">")
    (push-cdr! page "<h1>Urn main repository statistics</h1>")
    (push-cdr! page "<h5>Powered by <a href=\"https://squiddev.github.io/urn/\">Urn</a> and <a href=\"https://github.com/CrazedProgrammer/urntils\">urntils</a><br />")
    (push-cdr! page (.. "Last updated: " (os/date "%a %b  %d %X %Y EST") "</h5>")) 
    (push-cdr! page "<h3>Lines of code</h3>")
    (push-cdr! page (.. "<span class=\"monospace\">" (escape (gen-loc!)) "</span>"))
    (push-cdr! page "<h3>Commit history</h3>")
    (push-cdr! page "<span class=\"monospace\">")
    (push-cdr! page (.. "Total amount of commits (master): " (string/trim (os-exec! (.. "git --git-dir " repo-path "/.git rev-list --count master")))))
    (push-cdr! page (.. "Amount of commits in the last 24 hours (master): " (string/trim (os-exec! (.. "git --git-dir " repo-path "/.git rev-list --count master --max-age=" (number->string (- (os/time) 86400)))))))
    (push-cdr! page (.. "Amount of commits in the past week (master): " (string/trim (os-exec! (.. "git --git-dir " repo-path "/.git rev-list --count master --max-age=" (number->string (- (os/time) 604800)))))))
    (push-cdr! page "</span>")
    (push-cdr! page "<h3>Compile times</h3>")
    (push-cdr! page (.. "<span class=\"monospace\">" (escape (gen-compile-times!)) "</span>"))
    (push-cdr! page "<h6>Made by CrazedProgrammer<br />")
    (push-cdr! page "<a href=\"https://github.com/CrazedProgrammer/urn-stats\">Source</a></h6>")
    (push-cdr! page "</div></body></html>")
    (concat page "\n")))


(log! "Generating page:")
(let* [(page (generate-page!))
       (server (socket/bind! "*" listen-port))]
  (log! (.. "Started server on " (concat (list (self server :getsockname)) " ")))
  (while true
    (let* [(client (self server :accept))]      
      (self client :settimeout 1)
      (with (request-path (http/get-request-path (car (list (self client :receive)))))
        (log! (.. "GET " request-path))
        (self client :send (http/generate-response page))
        (self client :close)))))

