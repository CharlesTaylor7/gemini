FROM haskell:8.10
EXPOSE 8080
WORKDIR /build-dir
RUN stack update
COPY [".", "/build-dir"]
RUN stack build
ENTRYPOINT ["stack", "run", "gemini"]
