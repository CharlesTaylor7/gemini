FROM haskell:8.10
EXPOSE 8080
WORKDIR /build-dir
RUN cabal update
COPY [".", "/build-dir"]
RUN cabal build
ENTRYPOINT ["cabal", "run", "gemini"]
