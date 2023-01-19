# Git object explorer

Ever wondered what is going on inside your git repo? What is really at the heart of the git machine? What people are talking about when they mention commits, blobs, trees, branches, and more?

Then go somewhere else.

But if you are already somewhat familiar with the internals of the git object system this project provides a way to visualize your project. Git works on a linking system wherein the multiplicity of git objects refer to one another through a content addressing file system. Since the web is an excellent tool for exploring hyperlinked data it seemed natural to make a simple conversion tool to turn a git repo into a series of html pages that can be explored.

(Shoutout to the [picocss](https://github.com/picocss/pico) project which provides styles for this project.)

## Build and run instructions

This project is built in haskell, so you'll need to have both haskell and the cabal build tool installed. In addition it doesn't directly parse git object files, instead relying on the output of several git commands in order to gather data on the state of the repo. So you'll need to have git installed.

Once the haskell/cabal/git dependencies are met you should be able to grab the source

```
git clone https://github.com/seanlucrussell/object-explorer.git
```

and run it:

```
cabal run object-explorer -- $PATH_TO_GIT_REPO
```

or install it to your system with

```
cabal install
```

and then run

```
object-explorer $PATH_TO_GIT_REPO
```

Upon running the object explorer will create a directory `generated` in the current working directory. This `generated` directory contains all the html and styling needed to explore your git project. I recommend starting by opening the `overview.html` file in a browser and looking around.

Note that when you make changes (either by creating or commit or just by creating/swapping branches) the html will need to be regenerated if you want to explore the most up-to-date stuff.

## Quirks

I created this project in a real hurry; the bulk of the code was designed and written over the course of 24 hours. This was intended as an exploratory project so some design compromises have been made. I figured I'd at least document some of the weirder things here to save anyone (including my future self) who wants to deal with this project some time.

1. This is very low quality code. Read and modify at your own risk.
2. The generated directory is always going to be in your current working directory. No way to change that without modifying the source
3. This won't work on windows due to the usage of the `mkdir` command to create the generated html directory. This project could add some dependencies or do some smarter things to support windows but I couldn't be bothered.
4. It requires git on the system. I already mentioned this but the git executables are a dependency since this project runs git commands to gather information from the system.