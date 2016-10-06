###
### Voice commands for dealing with git
###
###   See also item_operation.vch for per file operations.
###

include "string.vch";

include "projects.vch";


##
## Supporting voice routines
##

  # git = "Unix git" in vocabulary
<git> := ( git | grit );

Place(before, after) := $before $after {left_ Len($after) };

  # replace *1st* @ with point, leaving any later @s intact:
PlaceAt(text) := Place( Split($text@,@,0), Mid($text, Len(Split($text@,@,0)@)) );

Contains(string, substring) := EvalTemplate('%s.find(%s) != -1', $string, $substring);

  # equals $command{enter} unless $command has 1 or more @s, in which
  # case it's equivalent to PlaceAt($command):
Command(command) := If(Contains($command,@), PlaceAt($command), $command{enter});



##
## Help:
##

<git> help [<_anything>] = "git " Replace($2," ","") " --help"{enter};

# also man git-<cmd>



##
## Configuration:
##

# git config --global user.name "Mark Lillibridge"
# git config --global user.email mark.lillibridge@hpe.com
# git config --global user.email mdl@alum.mit.edu

  # <<<>>>:
# git config --global color.ui auto
# git config --global color.pager false

  # I don't use currently, but maybe on Windows:
# git config --global core.editor emacs


  # summarizes, merging all config files:
<git> show config = "git config --list"{enter};



##
## Creating repositories:
##

# mkdir myproject; cd myproject
# git init                         OR    git clone <source-path-spec> myproject
# git add .
# git commit



##
## Local changes (not operations across branches/tags)
##
##      These are for the current working area containing .; see
##   x_item.vcl for per file operations.
##

<git_op> := ( status
            | short status  = "status -s"     # left column is staged, right working

	    | describe

            | diff                            # compare working and staged
            | diff staged   = "diff --staged" # compare staged and committed

            | commit [all]  = "commit -a"     # stage all tracked first
	    | commit inline = 'commit -a -m "@"'
            | commit some   = commit          # commit only what already staged
                # combine previous commit and new changes into 1 new
                # commit; can change commit message as well; note lack of -a here
            | amend commit  = "commit --amend"

            | pull  # for tracking branches, which remember remote&remote branch
);

<bar_more> := (bar more = " | more");  # do we really need this here? <<<>>>
<git> <git_op> here [<bar_more>] = Command("git $2" $3);

# can use `git commit file...' to commit just the changes to those
# files; rest of staged stuff left unchanged.



##
## Ignoring files:
##

# create .gitignore file
# can check in or not

# examples: 
#
#   # comment
#   /in_current_directory
#   *.pyc
#   *.[oa]
#   *~
#   !but_keep_this_one.pyc
#   foo/

<git> show ignored files = 
    "git ls-files --other --ignored --exclude-standard"{enter};


##
## Viewing history:
##

<log_option> := ( last	   = "-1 HEAD" 
	        | stats	   = "--stat"
	        | graph	   = "--graph"
	        | details  = "-p"          # full diffs
	           # also available: short, full, fuller
	        | one line = "--pretty=oneline"
	        | summary  = "--summary"   # add info on files created/deleted
	        # for more options, see https://git-scm.com/book/en/v2/Git-Basics-Viewing-the-Commit-History
);

<git> log [<log_option>] here = "git log $2"{enter};

  # X --not Y is equivalent to Y..X: commits on X history not on Y history
  # this does *not* show changes on Y not present on X!
<git> log [<log_option>] changes [from <branch>] = 
    Command("git log $2 HEAD --not " When($3,$3,origin/master));
<git> short log changes [from <branch>] = 
    Command("git shortlog " When($2,$2,origin/master) "..HEAD");

<git> log [<log_option>] provided by <branch> = 
    Command("git log $2 $3 --not HEAD");

  # searching commit messages:
<git> log [<log_option>] grep    <_anything> = Command("git log $2 '--grep=" When($3,$3,@) "'");
  # searching code added or removed:
<git> log [<log_option>] where changed <_anything> = 
    Command("git log $2 '-S" When($3,$3,@) "'");


  # can be any commits here:
  # this compares *tips* of branches
<git> diff <branch> with <branch> = Command("git diff $2 $3");

  # compare HEAD and it's common ancestor with <branch>:
<git> diff changes [from <branch>] = Command("git diff " When($2,$2,origin/master) "...HEAD");


# equivalent of svn cat is git show $id:$file; e.g. git show master:./log.py

  # get version $id of $file; changes working -- not sticky:
# git checkout $id $file


  # create server on 1234; fires up firefox as well
<git> visualize = "git instaweb --httpd=webrick"{enter};



##
## Branches:
##

  # "*" marks the current branch in the output for local branches:
<git> show [(merged=--merged|unmerged=--no-merged|verbose=-vv|remote=-r|all=-av)] branches 
    = "git branch $2"{enter};

  # HEAD = current checked out branch
<git> ( checkout                  # switch to given branch

         # "git branch <branch>" creates but does *not* switch current branch!
      | branch="checkout -b"	  # create branch with given name based on HEAD
                                  # and switch to that branch

         # delete given branch; need -D if throwing away work:
      | delete branch="branch -d" 

      | merge="merge --no-ff --log"       # merge given branch into HEAD

      | show                      # actually works on any commit

      ) <branch> = Command("git $2 $3");

  # to create branch at given point; <id> could be remote/foo:
# git checkout -b <new_branch_name> <id>

# git reflog [<branch>] shows historic values of branch refs

#
# merging:
#

# git mergetool (after conflict) for GUI tool
# use git add/rm to mark files as resolved

# git merge --abort

  # pull = fetch then merge
<git> pull master = "git pull origin master"{enter};

# git cherry-pick <commit>


#
# rebasing:
#

# don't rebase published commits!

  # apply changes from common ancestor of HEAD and <branch> to HEAD
  # onto <branch>, yielding new HEAD/current branch location;
  # this produces similar ending snapshot to merge <branch>, but the
  # history is linear rather than a DAG
<git> rebase <branch> = PlaceAt("git rebase $2");

  # general case: takes changes from ancestor(B,C) to C, then replay
  # them on A to get new HEAD
# git rebase --onto A B C


# recipe to update a branch: ??? <<<>>>
#
#   (on branch)
#   git pull origin master ("git pull master")
#   (test, edit, commit as needed)
#   git checkout master
#   git merge <branch>



##
## Working with remotes:
##

<git> show remotes = "git remote -v"{enter};

# git clone ... adds remote repository as origin

# git remote add <shortname> <url>
# git remote rename <current_shortname> <new_shortname>  # local change only
# git remote rm <shortname>                              # local change only

  # adding a (local) tracking branch for a new remote branch:
# git checkout -b localname remote/remote_branch

  # change what remote branch a existing local branch tracks:
# git branch -u remote/remote_branch branch

  # delete a remote branch: (also git branch -dr <remote/branch>)
<git> delete remote <branch> = PlaceAt("git push origin --delete $2");
# git push origin --delete <branch>


<git> show remote [<remote>] = "git remote show " When($2,$2,"origin") {enter};

<git> fetch [<remote>] here = "git fetch " When($2,$2,origin) {enter};

  # often push origin master:
<git> push  [<remote> [<branch>]] here = 
    Command("git push " When($2,$2,"") " " When($3,$3,""));

  # push a local branch to remote, setting up tracking information
<git> push upstream [<remote>] <branch> = 
    Command("git push -u " When($2,$2,"origin") " " When($3,$3,""));
  # for all branches:
# git push --all -u


##
## Tagging:
##

<git> show tags = "git tag"{enter};

# git show <tag>

  # creating a new tag:
<git> make tag here = "git tag -a ";
# git tag -a <tag>  [<SHA1>]   # brings up commit window

# git push <remote> <tag>      # tags *not* pushed to remotes by default push
<git> push tags = "git push origin --tags{enter}";       # all tags at once

  # creating a new branch starting from a tag:
# git checkout -b <branch> <tag>
