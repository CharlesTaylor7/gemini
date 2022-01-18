# pull base
FROM fpco/stack-build:lts-18.21

RUN mkdir build/
WORKDIR build/

# copy stack.yaml into global dir
COPY global.stack.yaml  /root/.stack/global-project/stack.yaml

# build jsaddle-dom deps 
RUN stack install lens aeson attoparsec base64-bytestring bytestring containers deepseq primitive process random jsaddle unordered-containers

# append allow-newer: true to the global config
RUN echo "allow-newer: true" >> /root/.stack/config.yaml

# build jsaddle-dom (this takes between 90-120 minutes)
RUN stack install jsaddle-dom

# build other large deps
RUN stack install relude finitary groups jsaddle-warp

# build remaining dependencies
COPY stack.yaml package.yaml gemini.cabal .
RUN stack install --dependencies-only

# build app code
COPY . .
RUN stack install gemini:server


##########
# Runtime
##########

# pull same base
FROM ubuntu:18.04

RUN mkdir -p /app
WORKDIR /app

# copy binary from previous stage
COPY --from=0 /root/.local/bin/server /app/server

# copy static assets into image
COPY public/ /app/public/

# test for styles
RUN echo /app/public/styles/index.css

# start the server
ENTRYPOINT ["/app/server"]
