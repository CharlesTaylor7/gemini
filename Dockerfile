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
RUN stack build gemini:server

# expose a port for local dev. heroku seems to find the port 
EXPOSE 8080

# start the server 
CMD ["stack", "run", "gemini:server" ]
