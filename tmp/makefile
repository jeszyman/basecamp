.PHONY: all

all: git_setup

git_setup:
	git init
	curl 'https://raw.githubusercontent.com/jeszyman/basecamp/master/resources/git/jeszyman_gitignore' > .gitignore
	curl 'https://raw.githubusercontent.com/jeszyman/basecamp/master/tools/git/pre-commit' > .git/hooks/precommit_sizecheck
	chmod +x .git/hooks/precommit_sizecheck
