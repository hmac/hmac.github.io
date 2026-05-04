---
title: A federated network for open source
publish: false
---

# A federated network for open source

GitHub is dying and people are starting to notice. Other forges are gaining popularity ([SourceHut](https://sr.ht), [Forgejo](https://forgejo.org/)), new ones are springing up ([ERSC](https://ersc.io), [code.storage](https://code.storage)) and there are some interesting decentralised options ([Radicle](https://radicle.xyz), [Tangled](https://tangled.org)).

I'm very interested in the decentralised approaches because I think depending on a single private entity for the open source ecosystem is a bad idea that we're only now coming to realise and regret as a community.

The core problems you need to solve with a decentralised approach are _who made this_ and _who do I trust_. Most of the complexity in these systems comes down to how they answer these questions. As an industry right now we can't reliably answer them, and this is at the heart of the supply chain security crisis that we find ourselves in.

It's not enough to solve this at the source code level, because we increasingly rely on public caches of built artefacts: docker images, pre-built binaries or shared libraries in package repositories, mirror servers, CI caches, Nix binary caches. It's just too inefficient to build everything from source, every time. So we need to know _who built this binary_ and _do I trust the builder_. There are projects that aim to tackle this as well, like [SLSA](https://slsa.dev/) and [Trustix](https://github.com/nix-community/trustix).

All of these projects are going in the right direction, but I think there's a core set of concepts that we could share so they can interoperate and we don't recreate the same thing N times. This is a sketch of those core concepts, as a starting point.

It breaks down into three networks: identity, source, and build.

## Identity
This is about who is who. An actor is identified by a public-private keypair. They don't have a permanent "name" of any sort. They can claim a particular alias via an assertion, for example I might claim hmac.dev as "me" by publishing a signed proof as a DNS record. The link between hmac.dev and my public key is not permanent or canonical, and the network doesn't trust it. Clients may choose to trust this, or not, based on their own policy (more on this later). As another example, I might post my public key on my GitHub account or Bluesky account or somewhere, and clients might choose to trust that if they can find N other corroborating posts on other platforms. This is essentially the [Keybase model](https://book.keybase.io/account#proofs).

## Source
This is about who wrote what. Source code is identified by the hash of its content. If I want to publish a library `mylib` to the network, I hash the library to get `sha256-abc123`, then publish an assertion (signed by my private key) that `mylib@1.2.3` is `sha256-abc123`.

This merely establishes that I (i.e. my public key) claim that `mylib@1.2.3` has hash `sha256-abc123`. There's no guarantee that I am the "rightful" maintainer of `mylib` or that `sha256-abc123` is really the hash of the "correct" that corresponds to `mylib@1.2.3` or even that `mylib` is a real project at all. The network makes no attempt to answer these questions. These are left to clients to decide. More on this later.

Once I've published this claim, a client who trusts me can use it to fetch the source of `mylib@1.2.3` from _any_ cache, without needing to trust the cache. It just needs to check that the hash of the fetched code matches the hash in my claim.

## Build
A client can fetch the source code of my library, if they trust me, but they still have to build it themselves. Extend this model to all dependencies, including your language toolchain, system libraries, your OS, and it's clear that this is not enough. We can't be compiling the world from scratch each time we build our application.

An actor in the network can fetch the source code for `mylib`, compile it themselves, and then share the compiled binary for others to use. They do this by publishing an assertion that they (identified by their public key) built `sha256-abc123` and it produced a binary with hash `sha256-def456`. The binary itself, like the source code, can be hosted by any cache.

To use this cached binary, a client must choose to trust the builder. If it trusts the builder and the author (me) then it can be happy that no other actor can have tampered with the result.

There's a lot more complexity here, around what environment and inputs are provided to a build. Some builders may care about this and impose constraints on library authors (e.g. Nix builders would require the source code to include a Nix file). Others might be more lax, and use "sane defaults" (e.g. a recent fresh Ubuntu VM). So there's no guarantee that two builders will produce the same result. The network doesn't care about this, but clients may choose to if they wish. For example they might only use binary Nix caches and require that at least two independent caches agree on a build hash in order to trust it (this is the Trustix model). Other clients may have a higher risk appetite and be happy to trust a particular builder, e.g. `ubuntu.com`.

## Trust
These networks rely on a fundamental model of trust. The network holds and propagates signed assertions of the form "(actor) claims (fact) about (object) at time (timestamp)". For example:

- `pubkey-789` claims `mylib@1.2.3` is `sha256-abc123` at time `1777895712055`
- `pubkey-678` claims `sha256-abc123` builds with output `sha256-def456` at time `1777895712056`
- `pubkey-345` claims that `pubkey-789` is an author of `mylib` at time `1777895712057`

There's no single log of these assertions - they are shared across the network via gossip. They are self-verifiable by their signature.

The last example shows how we can build a web of trust. A client may not trust me (`pubkey-789`) but they may trust `pubkey-345` because they know them, or because they appear to be ubuntu.com via a DNS record, or because they trust their organisation acme.org and acme.org trusts `pubkey-345`.

Clients can choose their trust policy based on their own risk appetite. The trust system applies to identity, source and build in the same way.

This system is general enough to support many different workflows, for example:

- Audits and code review: `pubkey-X` claims they have reviewed the source for `sha256-Y`
- Ownership transfer: `pubkey-X` claims `pubkey-Y` is an author of `mylib`
- Yanking: `pubkey-X` claims `mylib@1.2.3` IS NOT `sha256-abc123`
- Revoking trust (e.g. if a key is compromised): `pubkey-X` claims that `pubkey-Y` IS NOT an author of `mylib`
- Propagating trust in builders: `pubkey-X` claims that `pubkey-Y` is a trusted builder

The network doesn't define these assertions, it just propagates them. Clients must decide what assertions they will interpret and publish. This hopefully means the system can be extended to support any future workflow.

## Interop
The network can bridge between other ecosystems by modelling their level of trust. For example an actor (`pubkey-X`) can automatically publish facts about npm:

`pubkey-X` claims that `npmjs.com` has published `left-pad@1.2.3` as `sha256-ghi789` at time `1777895712058`

This is just another assertion by a particular actor in the network, and the client can choose whether to trusted it based on their particular trust policy. If npmjs.com chose to participate directly in the network in the future, it would publish its own public key and then start publishing assertions under that key.

## Summary

That's the general idea. The system is simple and generic enough to cover every use case I can think of. I see this as a sketch of a standard that multiple systems can build on and extend, so that they can all interoperate and contribute to a shared commons of software. I don't want a single forge to win, even if it is decentralised. I want lots of different approaches and I want them to be able to build on top of one another in a mutually beneficial way.

There are some areas I haven't thought about or intentionally am not solving:

- Mutation for security/legal reasons. I think this is solved by publishing negative assertions
- Privacy: some actors may not want to publish their trust assertions. I think this is probably solvable by running a node inside an internal network, and choosing how to communicate with public nodes.
- Economics: the infrastructure for this will cost money. But I don't think it's infeasible compared to other options, and the proper use of binary caching could actually save money over the long term.

I'm writing this less as a solid spec and more as an idea dump. I think there's decent overlap with some existing projects but I don't know if any of them have gone as far as this. If there is prior art I've missed or people already thinking about this, I'd love to know.
