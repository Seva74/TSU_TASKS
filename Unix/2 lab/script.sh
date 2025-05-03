#!/bin/bash
CONTAINER_ID=$(hostname)
SEQ=1
touch /shared_volume/lockfile
while true; do
    {
        flock -x 200
        N=1
        while [ -e "/shared_volume/$(printf '%03d' $N)" ]; do
            N=$((N + 1))
        done
        filename=$(printf '%03d' $N)
        echo "$CONTAINER_ID $SEQ" > "/shared_volume/$filename"
        SEQ=$((SEQ + 1))
    } 200> /shared_volume/lockfile
    sleep 1
    rm "/shared_volume/$filename"
done