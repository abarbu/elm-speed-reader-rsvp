# Elm Speedreader

This is a speedreader written in Elm for those that love to read and want much
more control over what's going on. It loads epub files and records your progress
locally. There's no server, just `build/index.html`. You'll need to serve up the
rest of the files in `build` for this to run.

The reader tries to be smart about what it shows, what it groups, and which
timings it uses. As far as I know it's the most advanced reader around. At
least, it's the only one that I can consistently use to read entire books
without being constantly distracted by timing and grouping problems. Its
features include: adjusting time for different kinds of punctuation, grouping
logical units together (numbers, idioms, etc.), tries to give you extra time for
asides like braces, color codes certain kinds of punctuation, and much more. All
of its parameters are easy to adjust.

## Install

All the steps are in the makefile. You can just run

> sudo make install

This will install some global npm packages that are generally useful. On some
distros running `elm-github-install` fails because of permissions errors even
though it runs locally. In that case you'll want to run

> sudo chmod a+rw /usr/local/lib/node_modules/elm-github-install/scripts/dist-*/elm-install-*-linux-x86_64/lib/vendor/Gemfile.lock

## Run

To run execute the command below. It will update any changes you make to the css on the fly.

> make start

To rebuild execute the command below.

> make build

If you're going to make many edits running this will update the build on the fly.

> make watch
