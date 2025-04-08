#!/bin/bash

eval "$(conda shell.bash hook)"
conda activate base
python3 ./meta_run.py
conda deactivate