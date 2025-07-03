FROM fukamachi/sbcl

WORKDIR /app

RUN set -x; \
  sbcl --eval '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)' --quit && \
  mkdir -p "$HOME/.config/common-lisp/source-registry.conf.d/" && \
  echo '(:tree "/app")' >> "$HOME/.config/common-lisp/source-registry.conf.d/ci.conf"

ENTRYPOINT ["sbcl", "--noinform", "--non-interactive"]
CMD ["--eval", "(ql:quickload :rove)", "--eval", "(rove:run :dexador-test)"]
