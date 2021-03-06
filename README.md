# Seneschal

Picture it: you're sitting at your terminal, and you're confronted with a task
that you have to over and over. Maybe you're updating the blizzard of
repositories on your work laptop, or you're pushing up a bunch of docker images
to your production container repo, or you want to kick off a bunch of tests.

Being both a good software engineer, and familiar with the devops-y tools on
your Linux machine, you think "This is a job for shell scripting!"

```bash
for REPO in $REPOS
do
   pushd $REPO
   git pull
   popd
done
```

Great! Now you aren't doing a bunch of toil by hand. You get yourself a nice
latte as a reward for a job well done.

Bad news, however: On your way back from the espresso machine at your Sleek(tm)
Hip(tm) With-It(tm) silicon valley office, the dreaded
[PHB](https://en.wikipedia.org/wiki/Pointy-haired_Boss) corners you. Even
worse, he needs to Add Value and Demonstrate Synergistic Brilliance, which
means he needs to find something both annoying and pointless to sic you on.

PHB then asks:

> Why are you updating these git repos serially? Can't you do this in parallel and save a bunch of time?
>
> -- PHB

This is a disaster. Not only is it bothersome, what's even worse is, he's _right_ !

No problem. Don't panic. Bash is good at this. Really, it's a very simple change!


```bash
for REPO in $REPOS
do
    bash -c "cd $REPO && git pull" &
done

wait
```

There we go! It's now done in parallel. You get another latte.

Unfortunately, you are again accosted, but this time by a fellow engineer.

> So, the repo update script dumps a bunch of unreadable junk in the terminal, I didn't see that there was a problem with one of the repos, it wasn't getting updated and it cost me a lot of time.
>
> -- Engineer Coworker

Man, this is getting tiresome. Not to worry, however, there's a solution to this.

```bash
function updateRepo {
  REPO=$1
  (cd $REPO && git pull) > $TEMP/$REPO.out
}

TEMP=$(mktemp)
for REPO in $REPOS
do
    updateRepo $REPO &
done

wait

cat $TEMP/*.out
rm -rf $TEMP
```

You look over this and think:

> Well. This works. But man, it's kinda ugly, rather elaborate, and very
> baroque. It's probably not terribly easy to maintain. Isn't there a tool that
> just does this sorta thing?
>
> You, A World-Class Genius

So, you look around. Your DevOps friend grumbles something about "parallel
chiding you about citations at every install", and mentions that `xargs
--max-proc 0 --max-args 1` does something like what you want, without the
ceremony and self-aggrandizement.

```bash
function updateRepo {
  REPO=$1
  (cd $REPO && git pull) > $TEMP/$REPO.out
}

TEMP=$(mktemp)
echo $REPOS | xargs --max-proc 0 --max-args 1 updateRepo
cat $TEMP/*.out
rm -rf $TEMP
```

Then you discover, much to your distress, that xargs isn't aware of bash
functions. Not even when you `export -f` them, so they're available to
subprocesses. Even worse, xargs isn't terribly suited to executing a string of
shell commands, like `cd`ing into a directory and running `git pull`.

At times like this, you start to daydream about buying a Yurt in the middle of
nowhere, far away from pointy-haired bosses and troublesome coworkers. Alpaca
keeping is probably really relaxing, right?

Enter seneschal.

```bash
echo $REPOS | seneschal --prefix="cd {} && git pull"
```

The objective of seneschal is to be simple to use, both in scripts and directly
in your terminal. It runs your commands in parallel, captures their output, and
prints the commands out, in input order, once everything has finished.

It runs the same shell you're running, so it understands the functions and
aliases you've defined in your environment.

In short, it's built to let you get a lot of things done quickly, then get out
of your way.

## But how does it work?

There are two parts. Part one is the optional `--prefix` command, the other is
reading from stdin.

A simple way to use seneschal is to just execute a list of commands in parallel.

so:

```bash
COMMANDS="pwd
sleep 5 && echo boom
echo Howdy
whoami"

echo "$COMMANDS" | seneschal --debug
```

If you run the above command, the output is:

```bash
[merlin@DESKTOP-ISNBLMC seneschal]$ bash example.sh
21:43:38Z (00.001) command =
Raw command: /bin/bash -c "echo Howdy"
21:43:38Z (00.001) command =

21:43:38Z (00.001) command =
Raw command: /bin/bash -c "sleep 5 && echo boom"

21:43:38Z (00.001) command =
Raw command: /bin/bash -c pwd

<five seconds pass>
/home/merlin/git/seneschal

boom

Howdy

merlin


[merlin@DESKTOP-ISNBLMC seneschal]$
```

Each line is treated as a separate command. The commands are passed to your
shell (it reads the `$SHELL` environment variable, but can be overridden via
the `--shell` argument) in the `bash -c <yourCommand>` format. This is built up
into a list, executed in parallel, with the stdout and stderr returned in a
list, corresponding to the same input order.

This is why, despite the second command having a five second sleep, the output
is still in the same order as the commands.

There's another way to use this command. If you're familiar with `xargs`, this
will be familiar, and it's the way that was used in the first example.

Say we had a bunch of repos in our `~/git` repo that we wanted to update.

We can get the list via `ls -1`, but this gives us the list of things to
iterate over, it doesn't help us to build the set of commands, or kick them off
in parallel.

What we want is:

```
pushd $inputLine; git pull; popd;
```

Fortunately, `xargs` gives us a good idiom for this -- the common convention is
that xargs commands are appended to the end. If we need it elsewhere in the
argument list, we specify `{}` in the command string.

```
pushd {}; git pull; popd
```

(If we need to override the default `{}` value, we can, via the `--prefix`
flag)

Putting the pieces together, we get:

```bash
ls -1 | seneschal --prefix="pushd {}; git pull; popd;"
```

Internally, each line of stdin is substituted in for the `{}` value. Each
resulting command is then fed as an argument to `bash -c` (or whatever shell
you're running or scripting), executed in parallel, aggregated, and printed out
to stdout.

It's simple, but it should work the way you expect!

## How to get it

`git clone git@github.com:TheWizardTower/seneschal.git && pushd seneschal && stack install`

(I need to upload it to stackage)

## Acknowledgements

Boundless thanks to @istathar for support as we hammered out the idea, and for
the excellent [unbeliever](https://github.com/aesiniath/unbeliever) library,
and @etorreborre for help with refactoring the main function into something
significantly less indent-of-doom like.
