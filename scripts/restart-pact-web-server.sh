#!/usr/bin/env bash


killall pact-web-server || true

pact-web-server &
