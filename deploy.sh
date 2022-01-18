# On mac, the build of jsaddle-dom takes 90-120 minutes
# The build runs out of memory unless I remove the swap limit 
docker build --memory-swap -1 -t ninjapenguin/gemini-server .
docker push ninjapenguin/gemini-server

# Deploy to fly.io
fly deploy --detach

# Deploy to heroku 
heroku container:push web
heroku container:release web
