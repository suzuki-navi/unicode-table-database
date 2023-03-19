set -Ceu

docker_image_name=sbtpersist

cd $(dirname $0)
docker build -t $docker_image_name .

