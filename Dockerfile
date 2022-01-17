FROM haskell:8.10
EXPOSE 8080
WORKDIR /build-dir
# RUN ["apt-get", "update"]
# RUN ["apt-get", "install", "libpcre++"]
# RUN ["apt-get", "install", "libpcre++-dev"]
RUN apt-get update && apt-get install -y libpcre++-dev
RUN cabal update
COPY [".", "/build-dir"]
RUN cabal build --only-dependencies -j4
RUN cabal build -j4
CMD ["cabal", "run", "gemini"]
