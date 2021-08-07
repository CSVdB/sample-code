#!/usr/bin/env bash

export DEVELOPMENT=True

stack install pact-web-server \
  --file-watch \
  --fast \
  --no-nix-pure \
  --ghc-options="-freverse-errors -j4 +RTS -A128M -n2m -RTS" \
  --exec='./scripts/restart-pact-web-server.sh'

