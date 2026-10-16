#!/usr/bin/env bash
# Pull finished fits (and their logs) back from GenomeDK.
#
#   bash revision/code/slurm/fetch_models.sh [<pattern>]
#
# Default pattern is every .rds. Models are gitignored; they are regenerated, not stored.
set -euo pipefail

REMOTE=genome:/faststorage/project/ecg_general/metagender
LOCAL_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
PATTERN="${1:-*.rds}"

mkdir -p "$LOCAL_ROOT/revision/models" "$LOCAL_ROOT/revision/logs"
rsync -avh --progress --include="$PATTERN" --exclude='*' \
      "$REMOTE/revision/models/" "$LOCAL_ROOT/revision/models/"
rsync -avh --include='*.out' --include='*.err' --exclude='*' \
      "$REMOTE/revision/logs/" "$LOCAL_ROOT/revision/logs/"

ls -lh "$LOCAL_ROOT/revision/models/"
