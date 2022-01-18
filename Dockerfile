# pull base
FROM fpco/stack-build:lts-18.21

# make a dir for the project
RUN mkdir -p gemini
WORKDIR /gemini

# copy files for build
COPY stack.yaml  .

# configure stack
RUN stack upgrade
RUN stack setup

# jsaddle-dom deps & commone haskell packages
RUN stack install lens aeson attoparsec base64-bytestring bytestring containers deepseq primitive process random jsaddle unordered-containers

# largest dependency
RUN stack install jsaddle-dom

# copy remaining files for build
COPY stack.yaml  .

# build remaining dependencies
RUN stack install --dependencies-only

# build the server
RUN stack install gemini:server

##########
# Runtime
##########

# pull same base
FROM fpco/stack-build:lts-18.21

# expose a port for local dev
EXPOSE 8080

RUN mkdir -p /app
WORKDIR /app

# copy binary from previous stage
COPY --from=0  ~/.local/bin/server  /app/server

# start the server
CMD ["/app/server"]
