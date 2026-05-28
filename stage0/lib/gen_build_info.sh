#!/bin/sh
# Emits the stage0 [Build_info] module. [commit] is the HEAD commit
# hash, baked in at build time so JSON outputs can record which source
# snapshot produced them. Falls back to "unknown" when git or the .git
# directory is unavailable (for instance a build fed from `git archive`,
# which carries no repository metadata).
commit=$(git rev-parse HEAD 2>/dev/null || echo unknown)
printf 'let commit = "%s"\n' "$commit"
