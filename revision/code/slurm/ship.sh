#!/usr/bin/env bash
# Ship the code and the prepared data to GenomeDK. Code and derived .rds/.csv only --
# no models, no figures, no logs.
#
#   bash revision/code/slurm/ship.sh
set -euo pipefail

REMOTE=genome:/faststorage/project/ecg_general/metagender
LOCAL_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"

ssh genome 'mkdir -p /faststorage/project/ecg_general/metagender/revision/{code,data,models,logs}'
rsync -avh --delete "$LOCAL_ROOT/revision/code/" "$REMOTE/revision/code/"
rsync -avh --include='*.rds' --include='*.csv' --exclude='*' \
      "$LOCAL_ROOT/revision/data/" "$REMOTE/revision/data/"
