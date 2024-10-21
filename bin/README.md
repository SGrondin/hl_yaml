# README

This directory contains a simple CLI program that takes a path as first argument, processes the YAML file using the default options and outputs the resulting YAML to `stdout`.

Download it from the Releases page on Github.

### Build on MacOS

```sh
# From the root of the repo:
rm -f hlyaml.mac && dune clean && DUNE_PROFILE=release dune build bin/hlyaml.exe && cp _build/default/bin/hlyaml.exe hlyaml.mac && chmod 755 hlyaml.mac && strip hlyaml.mac
```

### Build on Linux

```sh
# From the root of the repo:
docker build . -t hlyaml:latest

HLYAML_CID="$(docker create hlyaml:latest)" \
&& rm -f hlyaml.linux \
&& docker cp "$HLYAML_CID":/app/hlyaml.exe hlyaml.linux \
&& docker rm "$HLYAML_CID"

# Trying it on Ubuntu 20.04
docker run -it --rm \
  -v "$(pwd):/app" \
  -w /app \
  ubuntu:20.04
  # Then run:
  ## apt-get update && apt-get install musl
  ## ./hlyaml.linux test/files/advanced.yml
```
