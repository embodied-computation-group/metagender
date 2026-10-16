# Source before running any R for the metagender revision on GenomeDK.
#
#   source revision/code/slurm/genome_env.sh
#
# The ONLY thing borrowed from the unrelated gender_intero project is its R TOOLCHAIN:
# R 4.4.3, brms 2.23, cmdstanr 0.9 / CmdStan 2.39 and hmetad 0.2.0 (dev). No data, code,
# models or analysis decisions from that project are used here. This project's own files
# live in /faststorage/project/ecg_general/metagender/.
source /faststorage/project/ecg_general/gender_intero/slurm/genome_env.sh

# Stan does its own parallelism; let BLAS keep out of the way.
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
