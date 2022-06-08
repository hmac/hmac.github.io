---
title: The Ideal Package Manager
publish: true
---
# The Ideal Package Manager

These days, every language has its own package manager. Each has their strengths
and weaknesses, and I like to think that with each generation of language we get
a bit better at designing our package management systems. Nonetheless, none is
perfect. So here's an opinionated list of features that would make the ideal
package manager.

## The goal

A package manager is, at heart, a tool to connect code authors to code users.
The ideal package manager should seek to minimise the burden on both groups.
This feature set is intended to

- (almost) eliminate the possibility of a broken build
- (almost) eliminate security issues from third-party packages
- provide all the (language) tools for a productive development environment
- be welcoming to new users

# Setting the scene

Firstly, let's agree on what we're talking about. Package managers come in
different types, for different use cases, and many problems stem from conflating
one type with another. In this article we're only concerned with a so-called
Project Dependency Manager. This is a tool that will manage the dependencies of
a particular project, in a particular language, for the purposes of developing
that project. We are not concerned with:

- installing packages globally, to be used directly or in multiple projects (`go get`, `cabal install`)
- managing end-user software (`apt-get`, `yum`, `brew`)
- managing all dependencies, across languages, from top to bottom (`nix`, `guix`)

Our idealised package manager will function similarly to Rust's `cargo` or
Ruby's `bundler`. We assume that each dependency has a version, and in our
project we specify our dependencies by writing their name and the version we
want, and then our package manager will do all the work to ensure those
dependencies with the correct versions are used when we build our project.

# Foundations

These features are absolute requirements that are present in any modern package
manager. They should go without saying, but in the interest of completeness
we'll state them anyway.

## Semantic versioning

[SemVer](semver.org) is a format for describing the version of a software library. The idea is
that your version number is formatted as three numbers separated by dots:
`MAJOR.MINOR.PATCH`. For example, `1.5.2` has major version 1, minor version 5
and patch version 2. Quoting the spec:

>Given a version number `MAJOR.MINOR.PATCH`, increment the:
>
>  1. `MAJOR` version when you make incompatible API changes,
>  2. `MINOR` version when you add functionality in a backwards compatible manner, and
>  3. `PATCH` version when you make backwards compatible bug fixes.

Of the myriad possible outcomes of upgrading a dependency, SemVer distills it down to three cases:

- Patch change: this upgrade will not affect me.
- Minor change: this upgrade may introduce new features, but my code will continue to work.
- Major change: this upgrade makes API changes which may break my code.

This is by no means perfect; the distinction between minor and major changes
depends on how you define "API change" and "breakage", and no two ecosystems
agree on this. However, if you are able to agree on the meaning of these terms
within a particular group, then SemVer is an efficient way to communicate. In
other words, a language should decide what "API change" and "breakage" means for
them, stick to those definitions, and then use SemVer. We can leverage tooling
to help guide and enforce those definitions - more on that later.

## Version constraints

If you have a project with 20 dependencies, each of which are actively
developed, you don't want to have to keep updating the versions you have written
down for them every time they release a new patch. This is what version
constraints are for. Instead of `some-lib 1.2.3` you can write `some-lib >=
1.2.3 && < 2.0.0`. This is a version constraint, specifying a range of versions
that are considered valid. Crucially, the range describes an infinity of
versions that may not exist yet, but could be published in the future. The
constraint `>= 1.2.3 && < 2.0.0` covers the versions `1.2.3`, `1.2.4`, `1.3.0`,
`1.4.5`, etc. up to but not including `2.0.0`. This is another way of saying "I
want at least version `1.2.3`, because I've tested my project with that version,
but if any newer versions become available I want to use them instead, unless
they contain breaking API changes". The package manager is then responsible for
taking the set of version constraints and finding an exact version for each
dependency that satisfies all the constraints. Not just the constraints in your
project, but any constraints in your dependencies, and their dependencies, etc.
There are a number of algorithms to do this
([Molinillo](https://github.com/CocoaPods/Molinillo/blob/master/ARCHITECTURE.md), [MVS](https://golang.org/ref/mod#minimal-version-selection), [Pubgrub](https://github.com/dart-lang/pub/blob/master/doc/solver.md#overview)), and they
can yield different results, but the promise of version constraints is that it
shouldn't really matter. Provided the resulting set satisfies the version
constraints, the project should build. If it doesn't, the version constraints
are incorrect.

## Lockfiles

Version constraints are primarily there to make life easier for dependency
consumers. However, there's a downside: there's now no fixed set of dependencies
that should be used to build your project. As we've just mentioned, different
constraint resolution algorithms can yield different results. But the results
from a particular algorithm can change over time, as new versions of a
dependency are released. I've just said above that this shouldn't matter, so
what's the issue? Well, whilst it's arguably OK to get a different set of
dependencies when you run your package manager in development, it is clearly not
OK if you get a different set when running it on a production build. You want
your dependency set to be flexible when installing dependencies, so you don't
have to be precise with all your version numbers, but you want to be able to
freeze the resulting dependency set after that, so it remains the same when you
build your project in production or for others in your team.

This is what lockfiles are for. A lockfile is just a list of all your
dependencies along with a specific version for each one which satisfies your
version constraints. More accurately, it is a list of your transitive
dependencies - i.e. your dependencies but also your dependencies' dependencies,
etc. From a lockfile it should be possible to deterministically reproduce the
same dependency set, now or at any time in the future. The package manager
should generate a lockfile for you, and you should check it into version
control. When you add or remove a dependency, or modify a constraint, the
package manager should regenerate the lockfile.

## Package registry

Our package manager should have a centralised place to publish, update and
download packages. A lot of the features we'll talk about will build on this.
Nothing precludes having alternative registries, or private registries behind a
corporate firewall etc., but we'll assume there's a main one for open source
code. This is the equivalent of `rubygems.org`, `crates.io`, `pypi.org` etc.

# Reproducible builds

With the basics out of the way, let's talk about how we can enable reproducible
builds. This means that if your project builds now, on your machine, it will
build at any time in the future, on any machine. We can't guarantee this because
we don't have full control over your computer (for an attempt at this, see
[Nix](https://nixos.org/)/[Guix](https://guix.gnu.org/)), but within our language we can get pretty far.

## Canonical source code links

A package published to the registry should contain a link to a canonical source
code snapshot. This could be, for example, a git URL
`https://some-git-server.org/some-repo` combined with a particular commit hash
`abcd123` or tag `v1.2.3`. The package registry should guarantee that the code
in the package matches the code in the source code snapshot (at time of upload,
at least). This narrows a security hole, where an attacker can upload a
compromised version of a dependency straight to the package registry, and bypass
any controls or visibility on the source code repository. This happened, for
example, in the [`event-stream`
incident](https://schneider.dev/blog/event-stream-vulnerability-explained/).

## Store package code in a content-addressed store

It is also crucial that a package registry store a copy of the code for each
package in its own store, because third party source code hosting cannot be
relied on. This is not just a general availability concern: users on GitHub (as
an example) can delete commits, tags, and whole repositories. This caused
problems in the early days of the Go ecosystem, when it was common to reference
a package by a GitHub URL. Deleted repositories would lead to broken builds when
the library in question could no longer by found by `go get`.

As a bonus, packages can be broken down into individual files and each stored in
a content-addressed store. This significantly reduces the required storage space
compared to storing full tarballs of each version and allows the package manager
to perform similar sharing on developer machines, speeding up dependency
installation and upgrades. I think content-addressed code has the potential to
massively improve the developer experience by enabling a whole bunch of
quality-of-life features, but that’s a subject for another time.

## Do not allow packages to be removed

This is a big one. If a package version can be removed from the package
registry, any dependency sets containing that version will fail to build. This
breaks the guarantee we just made with lockfiles, and can cause great chaos if
the package is widely depended on. An infamous example of this is the [left-pad
incident](https://en.wikipedia.org/wiki/Npm_(software)#Notable_breakages).
The solution is simple: do not provide an API to remove package
versions. The package registry should be considered an append-only dictionary.
If you publish a version with a bug, you should publish a new version that fixes
the bug. If you accidentally publish a version containing a personal API key of
some sort, revoke the key and publish a new version. The package registry can
provide an API to mark a version as "bad", such that the package manager will
try not to choose it unless forced. There are of course legal cases where a
version must be removed, but the process for these should ideally always involve
manual review.

# Isolated effects

These features cannot be applied to all languages, but provide huge security
benefits. We need a language that can reliably enforce pure code - i.e. code
that does not perform side-effects like writing to disk or opening network
connections. Haskell, Elm and related functional languages fall into this
bucket. I hope that in future, more languages will join them.

## Distinguish between pure and effectful packages

Many dependencies are intrinsically pure: their purpose is to implement some
algorithm or provide a data structure, and they don't need to perform side
effects to do this. And yet, an attacker can compromise one and make it do all
sorts of nasty things. At a minimum, packages should be tagged as either pure or
effectful, and the compiler/interpreter should enforce those tags. Any change to
the tag can be flagged by the package manager, allowing you to see when a
`fibonacci-calculator` dependency suddenly needs network access. This closes the
single biggest security hole in package management: that an attacker can take
over a previously legitimate package and change it to do illegitimate things.

## Do not support running arbitrary code at build time

On a related note, the package manager should have no support for running
arbitrary code at build time. There's no real need for this, and it opens the
door for all manner of abuse. If absolutely necessary, the code should be pure.

# Batteries included

The theme here is we should have one tool to do as much for us as possible. This
may be against the Unix philosophy, but it has huge benefits in terms of
consistency across the ecosystem and the experience of beginners. We do not want
a hodge-podge of tools that don’t entirely cooperate with each other.

## Build and test on upload

A key health indicator for a package is whether it builds, and whether its tests
pass. Many open source projects embed badges that communicate this in their
READMEs. The package registry should determine and display this information. Not
only is it useful for people evaluating the package, but this information forms
the foundation upon which we can build more interesting features. We’ll talk
about this more later. For now, we will just say that the registry should
attempt to build and run tests for every package uploaded.

## Generate documentation on upload

We also want docs. High quality documentation is a key part of any good library.
API documentation should be automatically generated for every package uploaded
to the registry, and made available online. [docs.rs](http://docs.rs/) is a good example of
ergonomic generated API documentation. Some of its qualities are:

- A clear, consistent layout across all packages
- Separate documentation is stored for each package-version pair
- Each documented item has a link to its corresponding source code

For each package version published to the registry, HTML documentation should
automatically be generated and published. Users should be able to view the
documentation of any version of any package that has every been uploaded to the
registry. Documented items should link to a copy of their source code as well as
the canonical source (see Reproducible Builds, point 2).

## Global search

Many package repositories support searching for packages by name, but we can do
better. Our registry should support searching not only for packages, but for the
name of any public API item. For example, the name of a function or type.

Furthermore, we can support type-directed search. This allows search queries
which specify the type of the result we are looking for, rather than its name.
It is especially powerful for languages that support polymorphism, because we
can construct queries like `(a → b) → [a] → b` to find functions which have this
type, for any value of `a` and `b`. Running this query on
[Hoogle](https://hoogle.haskell.org/?hoogle=(a%20-%3E%20b)%20-%3E%20%5Ba%5D%20-%3E%20b), the Haskell
search engine, gives (among others) the result `mconcatMap :: (Monoid m) => (a
-> m) -> [a] -> m`, which is likely what we want. Only a few language ecosystems
have type-directed search engines. To my knowledge, no ecosystem offers
type-directed search of their entire package ecosystem, including all versions.

## Compiler manager

When building an application, we have to ensure we have the right versions of
our dependencies, but we also have to ensure we have the right version of the
compiler/interpreter for our language. New versions of the language add
features, fix bugs and make breaking changes, so we will want to rely on a
specific version or range of versions. This is no different from any other
dependency, yet most languages rely on separate tools to handle compiler
versions. Ruby has `rbenv` and `rvm`, Rust has `rustup`. Haskell’s `stack` does
manage the compiler version for you, but the alternative package manager `cabal`
does not.

In addition, most languages separate the compiler from the package manager.
`rustc` vs `cargo`, `ruby` vs `bundler`, `node` vs `npm`, `python` vs `pip`,
etc. There’s no need to do this, and it can confuse beginners and increase
friction even for experienced users.

To that end, we want the following:

- Our package manager will ship in the same tool as our compiler.
- Version constraints on the compiler will be specified with other dependencies and committed to lockfiles.
- Our package manager will be able to manage itself.

If our language is called `spork`, then we will ship a single executable called
`spork` which can manage application dependencies, compile and run code, and
manage versions of itself. Users will install only `spork` and `spork` will
handle everything else, forever.

# Novel features

Finally, we come to the features that no current ecosystem has. Here we attempt
to solve or mitigate one of the biggest outstanding issues in package
management: the limited time capacity of humans. Maintainers and downstream
consumers alike must carry out chores to keep their systems healthy, and much of
this can be automated by making our package tooling work harder.

## Build matrices

Our registry is able to build and run tests for every package uploaded to it,
but it can go further. We can build and test each package against a range of
version of its dependencies, and determine what combinations are successful and
what combinations lead to failure. For example, say that `lib-a` depends on
`lib-b >= 1.2 && < 2` and `lib-c >= 0.0.1 && < 0.1` . The registry can then
generate the following combinations (assuming these versions exist):

- `lib-b == 1.2.0 && lib-c == 0.0.1`
- `lib-b == 1.2.0 && lib-c == 0.0.2`
- `lib-b == 1.2.1 && lib-c == 0.0.1`
- `lib-b == 1.2.1 && lib-c == 0.0.2`

## Automatically resolve version ranges

Build matrices give us information on what combinations of dependency versions
are “good” for a given package. This allows the registry to automatically
determine a good range for the package’s dependencies. We can present this
information in several ways:

- Suggest narrowing a version range if the buildable range is much smaller than the specified range
- Suggest widening a version range if the buildable range is much greater than the specified range
- Allow uploads of packages that don’t specify a range, and determine it automatically.

The key idea is that the maintainer no longer has to guess or laboriously test
different dependency versions to determine a good range - the registry can do
this for them.

## Report breakage information for pre-release versions

A maintainer can upload a pre-release version of a package, and the registry
will not only build it but also build its dependents - packages that depend on
this package. By reporting build or test failures with those packages to the
upstream maintainer, the registry gives them an idea of how their new version
will impact the ecosystem.

As a maintainer, you might think that a change is innocuous until publishing it,
and it turns out that others were relying on the previous behaviour. Conversely,
you might be hesitant to make a breaking change because of the impact it will
have on downstream users, but in reality the impact might be much less than you
think. You could even reach out proactively to dependents and help them prepare
for the change. All this is possible when the registry can build and test every
package.

## API diffs

When a new version of a package you depend on is released, often the most
valuable information is whether it contains any API changes. These are the
changes that are most likely to break your project. Many maintainers will keep a
changelog, listing the changes for each version, but this is manually curated
and not always accurate.

As it builds each package version, the registry can mechanically determine if an
public interface has changed between versions. From this it can generate an API
diff, succinctly describing the change. For example:


```diff
- foo(int x, bool y) -> Bar
+ foo(bool y, int x) -> Bar

  Bar {
-   a : int,
    b : int,
  }
```

Here we see that the function `foo` has switched the order of its parameters,
and the field `a` in type `Bar` has been removed. API diffs can also communicate
whether a package has changed from pure to effectful, or vice versa.

## Enforced semantic versioning

We talked earlier about semantic versioning, and how it communicates important
compatibility information to downstream users. We can make use of API diffs to
enforce the correct use of semantic versioning. At the most basic level, if a
new version of a package changes its API in a backwards-incompatible way then
the registry will require it to have a major version number which is greater
than the previous version. We can also make use of breaking information from the
build matrix to infer whether a change is breaking in practice, even if the API
is unchanged.

## Upgrade help

Some API diffs are isomorphic, in the sense that no information is lost between
the two versions. The `foo` example from above is an isomorphic change:

```diff
- foo(int x, bool y) -> Bar
+ foo(bool y, int x) -> Bar
```

We can deal with this change by finding every call to `foo` in our codebase and
flipping the parameters around. This is a purely mechanical change, and one that
can be automated. There already exist tools that do this, for example Facebook’s
[Codemod](https://github.com/facebookarchive/codemod). By integrating this into the registry, we can allow downstream users to
update their code with one command - or even do it for them automatically.

# Costs

There are some downsides to this approach.

Firstly, it’s a huge undertaking. This feature set is effectively half a dozen
different tools merged into one. Building it all will take time, and integrating
it into a single application will be difficult. The benefits to users, I
believe, are worth this cost. Go is often praised for making most tooling
accessible from the `go` command. Having a new-user experience which boils down
to “install this one tool” is a massive improvement on almost all other
languages.

Many of our features require that the package registry can efficiently build and
test lots of different packages. This could be extremely costly in terms of
computing resources, and hence money. In a world where many language ecosystems
run on minimal budgets, this is a real deal-breaker. I see three solutions to
this:

1. get enough funding to pay for the necessary compute resources
2. offload the work to a provider that doesn’t charge for open-source use
3. design the language to run builds and tests very efficiently

My approach would be a combination of all of these, starting with (3). Making
builds fast not only helps with cost by also reduces the time developers spend
waiting. Fast builds are a feature commonly praised in languages that have it
(Go) and criticised in languages that don’t (Haskell, C++). (2) is achievable by
using a service like GitHub Actions or GitLab CI. (1) is a whole topic in
itself, and definitely out of scope of this post!

