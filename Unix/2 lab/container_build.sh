for i in {1..50}; do
    docker run -d --mount type=volume,src=shared_volume,dst=/shared_volume my_image
done