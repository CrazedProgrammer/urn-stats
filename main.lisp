(import urntils/bindings/luasocket socket)
(import urntils/http http)


(defun generate-page! ()
  (let* [(page '())]
    (push-cdr! page 
     "<html>
        <head>
          <style>
            body {
              background-color: #333333;
              color: #dddddd;
            }
          </style>
        </head>
        <body>")
    (push-cdr! page "poop</body>")
    (concat page "\n")))

(let* [(server (socket/bind! "*" 2378))]
  (print! (.. "Starting server on " (concat (list (self server :getsockname)) " ")))
  (while true
    (let* [(client (self server :accept))]      
      (self client :settimeout 1)
      (with (request-path (http/get-request-path (car (list (self client :receive)))))
        (print! request-path)
        (self client :send (http/generate-response (generate-page!)))
        (self client :close)))))

