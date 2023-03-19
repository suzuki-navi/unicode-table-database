set -Ceu

docker_image_name=sbtpersist

# If the specified Docker image has not been built yet
if [ -z "$(docker images -q $docker_image_name)" ]; then
    bash $(dirname $0)/docker-build.sh
fi

# To inherit the user ID and group ID of the host inside Docker
user=$(whoami)
uid=$(id -u $user)
gid=$(id -g $user)

if [ -t 0 ] && [ -t 1 ]; then
    term_opt="-it"
else
    term_opt="-i"
fi

# directory for persistant sbt cache
mkdir -p .sbt-docker-cache/.sbt
mkdir -p .sbt-docker-cache/.cache

work_dirs="-v $(pwd):$(pwd) -v $(pwd)/.sbt-docker-cache/.sbt:$HOME/.sbt -v $(pwd)/.sbt-docker-cache/.cache:$HOME/.cache"

# HOST_UID, HOST_GID, HOST_USER are referenced in entrypoint.sh
run_options="$term_opt -e HOST_UID=$uid -e HOST_GID=$gid -e HOST_USER=$user -w $(pwd)"

docker run --rm $run_options $work_dirs $docker_image_name bash /usr/local/entrypoint.sh "$@"

