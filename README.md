# Scaled

Scaled is a modern programmer's text editor, built atop Java 8 and JavaFX 8, mostly written in
Scala, and which is designed to be extensible "all the way down" like Emacs. Like Emacs, "all the
way down" doesn't actually go all the way down, but it goes down a lot further than most other
editors.

Scaled focuses on the text editing experience first, and IDE-like features second. This does not
mean that the IDE features suck (indeed, a goal of Scaled is to push the frontiers of "intelligent"
code editing), but rather that they are not "in your face" from a user experience standpoint. We
start from the pristine calm of a colorized window of code, and tastefully grow from there.

Scaled is designed to be extensible in any JVM language. This is technically possible already, but
will be substantially improved before we claim that anyone would actually want to do this. Our goal
is that a programmer using Scaled can comfortably extend the editor in their preferred JVM language
with no more cognitive dissonance than they already endure when using a third party library written
in Java. We may never reach perfection in that regard, but it will be a damned sight better than
extending the editor in elisp (nothing against lisp, use Clojure if that's your bag).

## Development

Presently, the only way to run Scaled is to build it yourself from source. This is not terribly
difficult, but there are some hoops through which you must jump due to the high velocity with which
Scaled is currently evolving.

If you're looking at this README on the filesystem, then you have checked out the `scaled/scaled`
project from Github and have a directory structure that contains three submodules:

  * `scaled/api` - the API against which all extension are written, and some standard extensions
  * `scaled/editor` - implementation details: the JavaFX internals, package management, etc.
  * `scaled/devel` - a meta-package that makes life easier when developing (see below)

This is merely the core of the editor. Most functionality lives in extension packages, which you
can read about below. Before we get into extensions, let's get the core editor built and running.
This requires first checking out two libraries on which Scaled depends, and building/installing
those locally. Before we do *that*, let's pick our build system poison.

NOTE: all of this has to be done with Java 8. If you're not yet regularly building things using
Java 8, you will need to do some jockeying to get Maven or SBT working therewith. Google is your
friend here.

### Building with Maven

Check out, build and install the following two libraries:

    cd wherever-you-keep-other-peoples-libraries
    git clone https://github.com/samskivert/pom-util.git
    cd pom-util
    mvn install

    cd wherever-you-keep-other-peoples-libraries
    git clone https://github.com/samskivert/reactual.git
    cd reactual
    mvn install

Now go back to your top-level `scaled` directory and invoke:

    mvn test -Pdevel

After a lot of downloading, compiling, buying, selling and processing, you will eventually be
greeted with a Scaled window that displays an empty scratch buffer.

### Building with SBT

Check out, build and install the following two libraries:

    cd wherever-you-keep-other-peoples-libraries
    git clone https://github.com/samskivert/pom-util.git
    cd pom-util
    sbt publishLocal

    cd wherever-you-keep-other-peoples-libraries
    git clone https://github.com/samskivert/reactual.git
    cd reactual
    sbt publishLocal

Now go back to your top-level `scaled` directory and invoke:

    sbt devel/run

After a lot of downloading, compiling, buying, selling and processing, you will eventually be
greeted with a Scaled window that displays an empty scratch buffer.

### On the devel submodule and classloaders

The `devel` submodule exists to make life easier when working on Scaled. This is why:

In normal operation, the Scaled core editor loads installed packages into a tree of custom class
loaders. A package can depend on other packages, or on plain old Maven or Ivy dependencies. This
allows packages to be loaded and unloaded on demand, but confuses the crap out of useful
development tools like JRebel. It also results in a massive proliferation of little projects, which
can be annoying to build.

When running Scaled from the `devel` package, the build system aggregates the editor and whatever
extension package dependencies you care about into a single project. This is all included in a
single "normal" classpath when running the editor. Note that Scaled _ignores_ the `Packages`
directory in this mode, so only the packages included in the development project are known to it.

This also allows one to take advantage of SBT's incremental recompilation while working on Scaled,
until such time as Scaled can fully host its own development environment.

## Scaled Extensions

Scaled extensions come in three main flavors:

  * services: programmatic services which provide functionality to other services and to modes
  * plugins: services can define plugin APIs so that one package can define a service and other
    packages can extend it
  * modes: major and minor editing modes (ala Emacs), which provide editing smarts specific to a
    particular programming language or activity

An example of all of these flavors working in harmony is the "project" package (which is a built-in
package, but could easily be maintained outside the Scaled core if there was a good reason to do
so). Project support comes in three parts:

  * `ProjectService` is a service that a major mode can inject to gain access to "project services"
     (e.g. enumerate all files in the project, rebuild the project)
  * `ProjectFinder` is a plugin used by the project service to allow other packages to provide code
    that identifies a project based on what it sees on the file system (a `pom.xml` file, a
    `build.sbt` file, etc.) and provide code for operating on projects of that kind
  * `ProjectMode` is a minor mode which is automatically activated for any mode which is tagged
    with `project`; the project minor mode adds key bindings for things like recompiling the
    project and annotating the appropriate buffers with warnings/errors

Most packages are simpler than the project package. They just export a major mode or two
(`scala-mode` and `java-mode` for example), or just export a plugin for another service
(`maven-project` for example)

Anyone can write a Scaled extension, but all currently known Scaled extensions live in the
[Github Scaled project](https://github.com/scaled).

Eventually, Scaled will automatically download and install extensions via a friendly in-editor user
interface. Right now, you have to download and build them manually.

To make interesting things happen, check out the following extensions:

    cd scaled
    git clone https://github.com/scaled/textmate-grammar.git
    git clone https://github.com/scaled/maven-project.git
    git clone https://github.com/scaled/java-mode.git
    git clone https://github.com/scaled/scala-mode.git
    git clone https://github.com/scaled/xml-mode.git

Check them out under the top-level `scaled` directory so that they can be automatically integrated
into the `devel` build. Once you have them checked out, your directory structure will look like:

  * `scaled/api` - same as before
  * `scaled/editor` - same as before
  * `scaled/devel` - same as before
  * `scaled/textmate-grammar` - a library for grokking TextMate grammars
  * `scaled/maven-project` - extends project-mode with some Maven project smarts
  * `scaled/java-mode` - a (primitive) major mode for editing .java, and .properties files
  * `scaled/scala-mode` - a (less primitive) major mode for editing .scala files
  * `scaled/xml-mode` - a (primitive) major mode for editing XML files

Now you can use the `bootstrap` Maven profile to automatically include these depends into the
build:

    mvn test -Pdevel -Pbootstrap

In SBT you have to edit `project/Build.scala` and uncomment the `profiles` line that contains
`"bootstrap"` and comment out the one that doesn't. Then run Scaled in SBT as before.

## Using Scaled

With the above extensions installed, you can run Scaled and type `C-x C-f` (that means hold down
control and press and release `x`, keep control held down and press and release `f`) and type
`pTAB` into the box that pops up and it will load the `pom.xml` file.

You should see colorized XML in the buffer (if not something's not working). Assuming you do see
colorized XML, then you'll also be in project mode which means that if you type `C-x C-f` again,
that will have been rerouted by the project minor mode to load any file in the entire project (you
can access the default `find-file` via `S-C-x S-C-f`).

If you ran from Maven, the cwd will have been the top-level `scaled` directory and you'll be
loading from the top-level project defined by the `scaled/pom.xml` in that directory, which means
you'll see all the Scaled source if you try to load another file and use tab completion. If you ran
from SBT, the cwd will have been the `scaled/devel` submodule directory and you'll be loading from
the project defined by `scaled/devel/pom.xml` which contains only two files.

In that case you can use `S-C-x S-C-f` (or `M-x find-file-default`) to load the `scaled/pom.xml`
file and trigger a switch to the parent project. Eventually `project-mode` and tab completion will
be enhanced to allow one to easily expand the scope of completion from a project to the project and
its dependencies, which will make working on a graph of interdependent projects more comfortable.
One will also be able to navigate based on type information, jumping to the definition of a type or
method, for project types that support such intelligence.

That's about the extent of the excitement for the moment. I'm using Scaled to work on Scaled with
reasonably productivity, so things are pretty robust for Scala editing, but your tolerance for
missing editor features may be lower than my own, so

At the moment Scaled's "UI" follows Emacs where that makes sense (pretty much all of the basic
editing key bindings). Extensions like `project-mode` introduce new interactions and I'm not making
an effort to model those the myriad hodge-podge Emacs IDE-like extensions that exist, I'm just
trying to come up with sensible bindings.

At any time, you can invoke `M-x describe-mode` (or `C-h m`) to see all of the key bindings and
config vars for the the active major and minor modes. You can cross-reference that with the
[Emacs reference card] to see basic editing commands organized more usefully than alphabetic order
by key binding description.

## License

Scaled is released under the New BSD License. The most recent version of the code is available at
https://github.com/scaled/scaled


[Emacs reference card]: http://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf
