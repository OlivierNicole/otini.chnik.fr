This is my personal website built using Hakyll. It should be accessible at
http://otini.chnik.fr.

## How to build

Prerequisites:

- You must have Nix installed with Flake support activated, about which you will
  find extensive docs on the web.
- You need to add the Haskell.nix binary cache to your trusted substituters in
  your Nix config:
  ```nix
    # Binary Cache for haskell.nix
    nix.settings.trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    nix.settings.trusted-substituters = [
      "https://cache.iog.io"
    ];
  ```
  Note: this does not add the substituter globally as I don’t like that. It is
  only added in the `flake.nix` for this specific project. As a result, you may
  be prompted to accept the substituter when running commands like `nix run`.
- You may also have to do a thing regarding locales, if you’re not using NixOS:
  ```
  export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
  ```

You can now run `nix run . build` to build the website in the `_site`
subdirectory, or `nix run . watch` to view it at <http://localhost:8080>.
