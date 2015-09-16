(require-extension fastcgi)

(fcgi-accept-loop
  9000
  0
  (lambda (in out err env)
    (out "Content-Type: text/html\r\n\r\n")
    (out "<h1>Hello</h1>")
  ))
