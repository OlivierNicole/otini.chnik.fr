---
title: My personal Nix & NixOS FAQ
date: 2019-05-24
---

I use Nix and NixOS every day, but I still feel frustrated because I don't know
how to perform some basic tasks. I have decided to put the questions I ask
myself on this page, and hopefully fill in the answers little by little. This
will serve both as a future reference (to me and others) and as a motivation to
learn to use Nix properly.

Please do not hesitate to [contact me](/contact.html) to suggest answers or improvements.

* [How can I build a package locally instead of downloading the binary?](#build-local)
* [How can I get the Nix directory (containing the code and Nix files) of a package?](#get-nix-files)
* [How to find the Nix store path associated to an installed package?](#find-store-path)
* [Is there a command to delete a specific derivation from the Nix store?](#delete-deriv)
* [If an error occurs during the upgrading of a package, how can I temporarily
  ignore this package's updates in order to upgrade the rest of the
  packages?](#ignorepkg)

## How can I build a package locally instead of downloading the binary?  {#build-local}

Add `--option substitute false`, or set it globally. (search `man nix.conf` for
“substitute”).

## How can I get the Nix directory (containing the code and Nix files) of a package? {#get-nix-files}

To be filled

## How to find the Nix store path associated to an installed package?  {#find-store-path}

If the package installed the command `<command>` in the PATH, you can do:
```
$ realpath `which <command>`
```

Otherwise, you can use `nix-build` for this (though it has the side-effect of
downloading/building the package if you *didn't* have it already, it won't add
it to your current environment, so no practical difference except disk space).

```
$ nix-build '<nixpkgs>' -A hello
/nix/store/20liz5dns3dm13gbk5synzf5qjsdpf51-hello-2.10
```

## Is there a command to delete a specific derivation from the Nix store?  {#delete-deriv}

To be filled

## If an error occurs during the upgrading of a package, how can I temporarily ignore this package's updates in order to upgrade the rest of the packages? {#ignorepkg}

To be filled
