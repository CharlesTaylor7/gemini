docker build --memory-swap -1 -t ninjapenguin/gemini-server:latest .
docker run -it -t ninjapenguin/gemini-server:latest --network=host
