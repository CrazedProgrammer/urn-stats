(import urntils/bindings/luasocket socket)
(import urntils/http http)
(import lua/os os)
(import lua/io (popen))
(import extra/io (append-all! read-all!))

(define print-output (= (car arg) "-l"))
(define repo-path "~/Programs/urn")
(define listen-port 2378)
(define log-path "log.txt")
(define stats-path "stats.txt")


(defun os-exec! (command)
  (when-let* [(handle (popen command))
              (data (self handle :read "*a"))]
    (self handle :close)
    data))

(defun gen-loc! ()
  (log! "Calculating lines of code...")
  (cut-head-lines (os-exec! (.. "cloc " repo-path)) 5))

(defun gen-data! ()
  (log! "Calculating compile times...")
  (let* [(last-commit (car (string/split (last (string/split (cut-tail-lines (read-all! stats-path) 1) "\n")) "%,")))
         (current-commit (string/trim (os-exec! (.. "git --git-dir " repo-path "/.git rev-list master | head -c 9"))))
         (compile-times (string/split (cut-tail-lines (cut-head-lines (os-exec! (.. "make -C \"" repo-path "\" bin/urn LUA=luajit LUA_FLAGS+=-t")) 2) 2) "\n"))]
    (log! (.. last-commit " " current-commit))
    (if (/= last-commit current-commit)
      (progn
        (log! "Calculating amount of tests and appending data to stats file...")
        (append-all! stats-path current-commit)
        (append-all! stats-path (.. "," (os/time)))
        (for-each line compile-times
          (append-all! stats-path (.. "," (string/trim (string/sub line 21)))))
        (append-all! stats-path (.. "," (string/trim (os-exec! (.. "make -C \"" repo-path "\" -k QUIET=1 LUA=luajit test | grep passed | awk '{s+=$1}END{print s}'")))))
        (append-all! stats-path "\n"))
      (log! "No new commit, skipping..."))
    (concat compile-times "\n")))

(defun escape (str)
  (id (string/gsub str "\n" "<br />")))

(defun log! (str)
  (with (logstr (.. (os/date "%m-%d %X") ": " str))
    (when print-output
      (print! logstr))
    (append-all! log-path (.. logstr "\n"))))

(defun cut-head-lines (str a)
  (with (lines (string/split str "\n"))
    (concat (drop lines a) "\n")))

(defun cut-tail-lines (str a)
  (with (lines (string/split str "\n"))
    (concat (take lines (- (n lines) a)) "\n")))


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
              display: block;
              width: 100%;
              font-family: Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New, monospace;
              white-space: pre;
            }
          </style>
          <script src=\"https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.js\"></script>
        </head>
        <body>
          <div class=\"page\">")
    (push-cdr! page "<h1>Urn main repository statistics</h1>")
    (push-cdr! page "<h5>Powered by <a href=\"https://squiddev.github.io/urn/\">Urn</a> and <a href=\"https://github.com/CrazedProgrammer/urntils\">urntils</a><br />")
    (push-cdr! page (.. "Last updated: " (os/date "%a %b  %d %X %Y EST") "</h5>")) 
    (push-cdr! page "<h3>Lines of code</h3>")
    (push-cdr! page (.. "<span class=\"monospace\">" (escape (gen-loc!)) "</span>"))
    (push-cdr! page "<h3>Commit history</h3>")
    (push-cdr! page "<span class=\"monospace\">")
    (push-cdr! page (.. "Total number of commits (master): " (string/trim (os-exec! (.. "git --git-dir " repo-path "/.git rev-list --count master")))))
    (push-cdr! page (.. "Number of commits in the last 24 hours (master): " (string/trim (os-exec! (.. "git --git-dir " repo-path "/.git rev-list --count master --max-age=" (number->string (- (os/time) 86400)))))))
    (push-cdr! page (.. "Number of commits in the past week (master): " (string/trim (os-exec! (.. "git --git-dir " repo-path "/.git rev-list --count master --max-age=" (number->string (- (os/time) 604800)))))))
    (push-cdr! page "</span>")
    (push-cdr! page "<h3>Compile times</h3>")
    (push-cdr! page (.. "<span class=\"monospace\">" (escape (gen-data!)) "</span>"))
    (push-cdr! page "<canvas id=\"myChart\" width=\"400\" height=\"400\"></canvas>")
    (push-cdr! page "<h6>Made by CrazedProgrammer<br />")
    (push-cdr! page "<a href=\"https://github.com/CrazedProgrammer/urn-stats\">Source Code</a></h6>")
    (push-cdr! page "  </div>
          <script>
var csvdata = ")
    (push-cdr! page (.. "\"" (id (string/gsub (or (read-all! stats-path) "") "\n" "|")) "\""))
    (push-cdr! page
               ";
                var csvlines = csvdata.split('|');
                csvlines.pop();
                var csvdata = [];
                for (i = 0; i < csvlines.length; i++)
                  csvdata[i] = csvlines[i].split(',');

                var csvcolumns = [];
                for (i = 0; i < csvdata[0].length; i++)
                {
                  csvcolumns[i] = [];
                  for (j = 0; j < csvdata.length; j++)
                      csvcolumns[i][j] = csvdata[j][i];
                }

                var ctx = document.getElementById(\"myChart\");
                var myChart = new Chart(ctx, {
                    type: 'line',
                    data: {
                        labels: csvcolumns[0],
                        datasets: [
                        {
                            label: 'emit-lua',
                            backgroundColor: '#A74800',
                            data: csvcolumns[5],
                            fill: true,
                            borderWidth: 1
                        },
                        {
                            label: 'optimise',
                            data: csvcolumns[4],
                            backgroundColor: '#944000',
                            borderWidth: 1,
                            fill: true
                        },
                        {
                            label: 'warning',
                            data: csvcolumns[3],
                            backgroundColor: '#7E3700',
                            borderWidth: 1,
                            fill: true
                        },
                        {
                            label: 'loading',
                            data: csvcolumns[2],
                            backgroundColor: '#5F2900',
                            borderWidth: 1,
                            fill: true
                        }
                        ]
                    },
                    options: {
                        tooltips: {
                                        mode: 'index',
                                        intersect: false
                                    },
                        scales: {
                            yAxes: [{
                                stacked: true,
                                ticks: {
                                    beginAtZero:true
                                }
                            }]
                        }
                    }
                });
                </script>
                </body></html>")
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

