(defsystem "dexador-usocket"
  :depends-on ("dexador"
               (:feature (:not :dexador-no-ssl) "cl+ssl"))
  :components ((:file "src/backend/usocket")))
