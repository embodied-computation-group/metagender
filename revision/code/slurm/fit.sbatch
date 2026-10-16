#!/usr/bin/env bash
#SBATCH --account=ecg_general
#SBATCH --partition=short
#SBATCH --cpus-per-task=64
#SBATCH --mem=120g
#SBATCH --time=12:00:00
#SBATCH --output=revision/logs/%x_%j.out
#SBATCH --error=revision/logs/%x_%j.err
#
# Resources only. Everything about the MODEL is an argument, passed straight through to
# 03_fit_unified.R, so this one file runs all four fits:
#
#   sbatch --account=ecg_general --job-name=fe_fixed revision/code/slurm/fit.sbatch \
#     --data revision/data/hmetad_trial.rds --out revision/models/fit_firstedition_fixed.rds \
#     --nsub 0 --iter 2000 --chains 4 --threads 16
#
#   ... --relative --out revision/models/fit_firstedition_relative.rds
#   ... --data revision/data/hmetad_trial_vmp.rds --out revision/models/fit_vmp_fixed.rds
#
# 4 chains x 16 threads = 64 cpus. Partition `short` caps walltime at 12 h; sibling fits
# of this size ran 6-40 min. Do NOT extrapolate runtime from early warmup: chains can sit
# at ~2 iterations/min through adaptation and then accelerate ~25x once it settles.
set -euo pipefail

cd "${SLURM_SUBMIT_DIR:-$PWD}"
source revision/code/slurm/genome_env.sh
mkdir -p revision/logs revision/models

echo "=== $(date -Is) job ${SLURM_JOB_NAME:-local} (${SLURM_JOB_ID:-none}) on $(hostname) ==="
echo "args: $*"
Rscript revision/code/03_fit_unified.R "$@"
echo "=== $(date -Is) done ==="
