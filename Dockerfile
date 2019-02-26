FROM ocaml/opam2:alpine

RUN sudo apk update && \
    opam switch 4.07 && \
    eval `opam config env` && \
    opam update && \
    opam install depext && \
    opam upgrade

WORKDIR /whokey
COPY whokey.opam .
RUN sudo chown -R opam:nogroup . && \
    opam pin add -yn whokey . && \
    opam depext whokey && \
    opam install --deps-only whokey
COPY . .
RUN sudo chown -R opam:nogroup . && \
    opam config exec make

CMD "/bin/bash"
