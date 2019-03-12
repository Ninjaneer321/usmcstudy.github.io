# Building the USMC Study Guide

As with any software of large size, this project is build with strong opinions on organizational quality
and language preferences. Specifically, it is built with the following paradigms in mind:

- The target platform is the Browser, in which there is no server to communicate with (a single-page-app).
- The final build should be a single file, with small exceptions to fringe dependencies like Google fonts.
- We are working almost entirely in Browser-safe JavaScript - care must be taken to ensure maximum compatibility.
- As a dynamic application, we will be using the [React.js](https://reactjs.org/) framework to maintain the user-interface.
- Correctness and stability are of paramount concern - we will be using [PureScript](http://www.purescript.org/) as the
  source language, to compile to JavaScript, the target language.
- The [Google Material Design Specification](https://material.io/) is elegant, modern, intuitive, and schnazzy. We will be
  using the React.js implementation, [MaterialUI](https://material-ui.com/).
- The Node.js ecosystem is notoriously riddled with incompatibility between versions of fundamental build tools -
  we will be using the [Node Version Manager](https://github.com/creationix/nvm) to fix that.
- We assume you are building the software on a POSIX compliant operating system, like GNU/Linux or... Mac, kinda.
  If you're using Windows, you're on your own. Say it with me: "one of us, one of us"
  

## Prerequisites

There are a number of build tools we will first need to build and install to get your ecosystem up-to-speed. This
may be compensated with a [Docker](https://www.docker.com/) container in the future.


1. Install PureScript

We'll need a later version of the `v0.12.x` branch of the PureScript compiler - namely, we are using
version `v0.12.3` at the time this document was written. To build the compiler, we'll first need to get a
[Haskell](https://www.haskell.org/) build environment set up, and the easiest way to do that is with
[Stack](https://docs.haskellstack.org/en/stable/README/). Follow Stack's installation instructions to get up-to-speed.

Next, we'll need to clone, build, and install the PureScript compiler, version `v0.12.3`:

```bash
git clone https://github.com/purescript/purescript
cd purescript/
git checkout tags/v0.12.3 -b tags/v0.12.3
stack build
stack install
```

This will install the `purs` executable - the PureScript compiler:

```bash
which purs # to verify its presence in your $PATH
```

2. Install NVM

NVM is going to be necessary to maintain which version of Node.js, NPM (the node package manager), and the V8 runtime
we're using. Install it by [following its installation instructions](https://github.com/creationix/nvm#installation-and-update).

After it's installed, we'll be using the `v10.15.3` (Dubnium) version. Install it with the following commands:

```bash
nvm install v10.15.3
nvm alias default node
```

Now, whenever you actually _want_ to use the Node.js toolchain (i.e. `npm` et al), you'll have to run `nvm use default`.


3. Bower

PureScript uses [Bower](https://bower.io/) as its package manager and public repository, for the most part. We could use
a stricter dependency mapping with a customized package set, but that's a little too much work for this project.

To install the utility, invoke

```bash
npm install -g bower
```

4. lText

There's a fringe tool used in this project, [ltext](http://ltext.github.io/), made by yours truly. It helps with
concatenating multiple text files together - perfect for our goal regarding a single output build file.

To install it, you won't need to clone any code. Just run `stack install ltext` from a top-level directory
(like `~/Documents`), and stack will populate it in your `~/.local/bin` for global access to your `$PATH`.

Verify its installation with:

```bash
which ltext
```



## Building USMCStudy's Codebase

Okay, now that we have our toolchain set-up, we can get started building it.

`cd` into a development directory of your choosing (i.e. `~/Documents` or something) so a fresh clone of the repo won't
hurt other projects:

```bash
cd ~/Documents
git clone https://github.com/usmcstudy/usmcstudy.github.io
cd usmcstudy.github.io/
```

Make sure you've got Node up-and-ready:

```bash
nvm use default
```

Install the project's dependencies

```bash
bower install && npm install
```

Now you're ready to compile the project. Note that this process may chew up upwards of 3GB of RAM; make sure your system
can handle it, or it may get bogged down.

```bash
./build.sh
```

> If there were any errors thrown during the build, please [File a Bug](https://github.com/usmcstudy/usmcstudy.github.io/issues).

On completion, `index.html` should be rendered with a new version. If you'd like a more compact output, run
the build script with a `production` flag:


```bash
./build.sh production
```

This will minify the javascript, and use production dependencies.


## Contributing

Before filing pull requests, please verify that the software at least builds correctly from the PureScript phase - this
can be achieved with `pulp build`.
