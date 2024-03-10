FROM haskell:9.6.3-slim AS build

WORKDIR /app
COPY . .

RUN stack setup

RUN stack build

FROM build as run-tests
RUN stack test

ENTRYPOINT stack ghci
