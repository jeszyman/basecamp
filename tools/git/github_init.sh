# Initialize github-linked repository                      :bashscript:
# :PROPERTIES:
# :header-args:bash: :tangle ./tools/git/github_init.sh :mkdirp yes
# :ID:       5bd05481-a646-4a4b-bb93-2c1a44c346e9
# :END:
# [[file:./tools/git/github_init.sh]]

#!/usr/bin/env bash
set -o errexit   # abort on nonzero exitstatus
set -o nounset   # abort on unbound variable
set -o pipefail  # don't hide errors within pipes

variables(){
    repo="${1}"
}

main(){
    variables $1
    git_init
}

git_init(){
    cd "${repo}"
    git init
    touch README.md
    git add README.md
    git commit -m "first commit"
    git branch -M master
    base=$(basename $repo)
    git remote add origin git@github.com:jeszyman/${base}.git
    git push -u origin master
    }

main "$@"
