# On mac, the build of jsaddle-dom takes 90-120 minutes
# The build runs out of memory unless I remove the swap limit 
docker build --memory-swap -1 .

# Use heroku's docker image registry 
heroku container:push web

# Deploy
heroku container:release web
