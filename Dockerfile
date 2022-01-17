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

#
# runtime
#

# pull same base
FROM fpco/stack-build:lts-18.21 

# expose a port for local dev. heroku seems to find the port 
EXPOSE 8080

# make the file structure the server expects
RUN mkdir -p /app
WORKDIR /app

# Executable(s) from build stage (stage 0, therefore `--from=0`).
COPY --from=0 /gemini/.stack-work/install/x86_64-linux/lts-18.21/8.10.7/bin/server /app/server

# make user for the app (unsure if needed)
RUN useradd app
USER app

# start the server 
CMD ["/app/server" ]
