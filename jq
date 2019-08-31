#!/bin/bash
K=$1
N=$2
echo -e "$K\n$N" | jq -f golden.jq -rs
