#!/bin/sh
# Emits the stage0 [Build_info] module, identifying the source that
# produced this binary.
#
# [tree] is the authoritative field: the git tree hash of the working
# tree at build time, computed against a scratch index so the real one
# is never touched. Unlike a commit hash it names what was *built*, not
# what happened to be committed at the time -- the two differ whenever a
# change is built and tested before being committed, which is the normal
# order of work. Because the hash is content-addressed it is also stable
# in time: committing the tested change later makes [tree] equal that
# commit's tree, so the same untouched binary becomes attributable after
# the fact, with no rebuild and no re-stamping.
#
# That makes "is this binary's source committed?" a question to ask at
# query time rather than a bit frozen at build time:
#
#     test "$tree" = "$(git rev-parse HEAD^{tree})"   # matches the checkout
#     git log --all --format='%T %H' | grep "^$tree " # which commit has it
#
# [commit] is kept as a human-readable hint (HEAD at build time, i.e.
# usually the parent of the commit that ends up containing this source)
# and because downstream tooling already reads it.
#
# Both fall back to "unknown" when git or the .git directory is
# unavailable -- for instance a build fed from `git archive`, which
# carries no repository metadata.
commit=$(git rev-parse HEAD 2>/dev/null || echo unknown)

# Seed the scratch index from the real one so `add -A` reuses git's stat
# cache and only re-hashes files that actually changed; starting from an
# empty index would re-read the whole tree on every build. Note this can
# write loose objects for uncommitted content, which is harmless: they
# are unreferenced and collected by `git gc` like any other.
tree=unknown
git_dir=$(git rev-parse --absolute-git-dir 2>/dev/null)
if [ -n "$git_dir" ]; then
  idx=$(mktemp 2>/dev/null) || idx=
  if [ -n "$idx" ]; then
    cp -f "$git_dir/index" "$idx" 2>/dev/null
    # `add -A` with no pathspec covers the whole tree regardless of cwd,
    # and honours .gitignore, so _build and _opam stay out of the hash.
    tree=$(GIT_INDEX_FILE=$idx git add -A 2>/dev/null &&
           GIT_INDEX_FILE=$idx git write-tree 2>/dev/null) || tree=unknown
    rm -f "$idx"
  fi
fi
[ -n "$tree" ] || tree=unknown

printf 'let commit = "%s"\n' "$commit"
printf 'let tree = "%s"\n' "$tree"
