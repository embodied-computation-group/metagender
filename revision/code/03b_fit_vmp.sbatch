#!/usr/bin/env bash
#SBATCH --job-name=vmp_replication
#SBATCH --partition=debug
#SBATCH --cpus-per-task=5
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --output=revision/logs/vmp_%j.out
#SBATCH --error=revision/logs/vmp_%j.err
#
# The VMP replication fit. NOT YET RUN.
#
#   Rscript revision/code/01b_prep_vmp.R           # first: build the trial file
#   sbatch  revision/code/03b_fit_vmp.sbatch
#   Rscript revision/code/05b_effects_vmp.R        # after it lands
#
# COST -- measured pilot, then extrapolated. Do not trust the extrapolation.
#   MEASURED : 30 subjects x 600 iter = 5m05s wall, 4 divergences at adapt_delta 0.99.
#   FULL     : 456 subjects (15x the random effects) x 2000 iter (3.3x the draws).
# A linear extrapolation gives ~4 h. Treat that as an order of magnitude, NOT a promise:
# per-iteration cost grows with the random-effect count, so the true figure will likely
# exceed it. The K=4 sibling fit (456 subjects, 9 dpars) took 1h11m; this has 15 dpars
# because K=7 gives twelve criteria instead of six, so 3-6 h is the honest band.
#
# DO NOT EXTRAPOLATE FROM EARLY WARMUP. On the sibling fit the slow chains sat at ~2
# iterations/min for the first 25 minutes -- implying 16 h -- and then accelerated 25x once
# adaptation settled, finishing in 1h11m. Early-warmup rates at adapt_delta 0.99 are
# meaningless. Wait for chains to cross into sampling before estimating anything.
#
# adapt_delta 0.99: the pilot threw 4 divergences at that level already. If they persist at
# scale, the answer is reparameterisation, not another nudge upward.
#
# MEMORY: the sibling K=4 fit peaked at 5.3 GB. Fifteen dpars rather than nine should stay
# well inside 64 G. If it OOMs, drop --chains to 2 rather than raising --mem.
#
# BACKEND rstan. The first-edition 03_fit_model.R uses cmdstanr; hmetad's custom family +
# stanvars have been exercised against rstan here and not against cmdstanr. Change it
# deliberately, with a side-by-side check, not by default.
set -euo pipefail

PROJ=/mnt/penguino/micah/metagender
cd "$PROJ"
export R_LIBS_USER=/mnt/penguino/micah/Rlib_meta_intero
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1

mkdir -p revision/logs revision/models

echo "=== $(date -Is) VMP replication fit starting on $(hostname) ==="
/usr/bin/time -v Rscript revision/code/03b_fit_vmp.R \
  --nsub 0 \
  --iter "${HMETAD_ITER:-2000}" \
  --chains 4 \
  --refresh 50 \
  --backend rstan \
  --adapt-delta 0.99 \
  --max-treedepth 12 \
  --out revision/models/fit_vmp.rds
rc=$?
echo "=== $(date -Is) exit=${rc} ==="
exit $rc
