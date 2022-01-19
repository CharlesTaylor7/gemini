if test -n "$(git status --porcelain)"; then
  echo "Can only deploy from a clean index; commit or remove uncommited changes"
  exit 1
fi

IMAGE_NAME="ninjapenguin/gemini-server"
COMMIT_HASH=$(git rev-parse HEAD)


# On mac, the build of jsaddle-dom takes 90-120 minutes
# The build runs out of memory unless I remove the swap limit 
echo "1. Build docker image"
docker build --memory-swap -1 -t $IMAGE_NAME  .

echo "2. Push docker image"
docker push $IMAGE_NAME:latest

echo "3. Deploy to Fly.io"
fly deploy
fly open
