#!/bin/bash

make
sudo insmod tsulab.ko
sudo dmesg | tail
echo "in the /proc/tsu file it is written:" 
cat /proc/tsu 
echo "that means correct output"
sudo rmmod tsulab
sudo dmesg | tail
cat /proc/tsu
rm tsulab.ko  tsulab.mod  tsulab.mod.c  tsulab.mod.o  tsulab.o modules.order Module.symvers