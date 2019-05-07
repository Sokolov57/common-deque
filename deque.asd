;;;; deque.asd

(asdf:defsystem #:deque
  :description "C++ std::deque clone."
  :author "Andrew aun.sokolov@gmail.com"
  :license  "Specify license here"
  :version "0.1.4"
  :serial t
  :depends-on (#:iterate #:alexandria)
  :components ((:file "package")
               (:file "circular-buffer")
               (:file "deque")))
