####################
### build_shared ###
####################
FROM asemio/mountain-caravan:2.4.0 AS build_shared
WORKDIR /app
RUN sudo apk update \
  && sudo apk upgrade

#############
### build ###
#############
FROM build_shared AS build
COPY hl_yaml.opam .

ENV DUNE_PROFILE=release

RUN opam update \
  && OPAMYES=1 opam install . --deps-only -t

COPY src src
COPY bin bin
COPY dune-project dune-project

RUN sudo chown -R opam /app

RUN echo '=== Building ===' \
  && opam exec -- dune build bin/hlyaml.exe \
  && cp /app/_build/default/bin/hlyaml.exe . \
  && chmod 755 hlyaml.exe \
  && strip hlyaml.exe
