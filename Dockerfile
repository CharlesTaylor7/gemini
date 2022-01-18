# pull base
FROM fpco/stack-build:lts-18.21

# stack global project
RUN mkdir -p ~/.stack/global-project

# copy stack.yaml into global dir
COPY stack.yaml  ~/.stack/global-project/stack.yaml

# configure stack
RUN stack upgrade && stack setup

# make a dir for the project
RUN mkdir -p gemini
WORKDIR /gemini


# build jsaddle-dom deps 
RUN stack install lens aeson attoparsec base64-bytestring bytestring containers deepseq primitive process random jsaddle unordered-containers

# build jsaddle-dom (this takes between 90-120 minutes)
RUN stack install jsaddle-dom

# copy remaining files for build
COPY .  .

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
