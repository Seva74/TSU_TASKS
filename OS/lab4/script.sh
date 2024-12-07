#!/bin/bash

sudo insmod tsulab.ko
sudo dmesg | tail
echo "in the /proc/tsu file it is written:" 
cat /proc/tsu