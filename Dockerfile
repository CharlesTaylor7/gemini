#
# Multi stage
#

# pull base
FROM fpco/stack-build:lts.18-21 

# configure stack
RUN stack upgrade
RUN stack setup 

# large haskell packages, separate step for better caching
RUN stack install --resolver lts.18-21 aeson servant-server wai text warp mtl transformers parsec

# make a dir for the project
RUN mkdir -p reversible-debugger
WORKDIR /reversible-debugger

# copy only the stack.yaml and the cabal file; build only dependencies
COPY stack.yaml reversible-debugger.cabal /reversible-debugger/
RUN stack setup && stack install --resolver lts.18-21 --dependencies-only 

# compile the haskell app
WORKDIR /reversible-debugger
RUN stack build reversible-debugger:server

#
# runtime
#

# pull same base
FROM fpco/stack-build:lts.18-21 

# expose a port for local dev. heroku seems to find the port 
EXPOSE 8080

# make the file structure the server expects
RUN mkdir -p /app
WORKDIR /app

# Executable(s) from build stage (stage 0, therefore `--from=0`).
COPY --from=0 /reversible-debugger/.stack-work/install/x86_64-linux/lts.18-21/8.0.2/bin/server /app/server

# make user for the app (unsure if needed)
RUN useradd app
USER app

# start the server 
CMD ["/app/server" ]
