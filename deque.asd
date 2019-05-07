;;;; deque.asd

(asdf:defsystem #:deque
  :description "C++ std::deque clone."
  :author "Andrew aun.sokolov@gmail.com"
  :license  "Specify license here"
  :version "0.5"
  :serial t
  :depends-on (#:iterate)
  :components ((:file "package")
               (:file "circular-buffer")
               (:file "deque")))
