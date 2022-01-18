# pull base
FROM fpco/stack-build:lts-18.21 

# make a dir for the project
RUN mkdir -p gemini
WORKDIR /gemini

# copy files for build
COPY . .

# configure stack
RUN stack upgrade
RUN stack setup 

# large haskell packages, separate steps
RUN stack install lens
RUN stack install aeson attoparsec base64-bytestring bytestring containers deepseq primitive process random
RUN stack install jsaddle
RUN stack install jsaddle-dom
RUN stack install wai

# build remaining dependencies
RUN  stack install --dependencies-only 

# compile the server
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


COPY --from=0  ~/.local/bin/server  /app/server
# start the server 
CMD ["/app/server"]
