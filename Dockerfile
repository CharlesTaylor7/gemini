# pull base
FROM fpco/stack-build:lts-18.21 

# make a dir for the project
RUN mkdir -p gemini
WORKDIR /gemini

# copy files for build
COPY stack.yaml package.yaml gemini.cabal README.md app/ lib/ /gemini/

# configure stack
RUN stack upgrade
RUN stack setup 

# large haskell packages, separate steps
RUN stack install --resolver lts-18.21 lens
RUN stack install --resolver lts-18.21 jsaddle-dom
RUN stack install --resolver lts-18.21 aeson wai text mtl transformers parsec


# build remaining dependencies
RUN  stack install --resolver lts-18.21 --dependencies-only 

# compile the server
RUN stack build gemini:server

# expose a port for local dev. heroku seems to find the port 
EXPOSE 8080

# start the server 
CMD ["stack", "run", "gemini:server" ]
