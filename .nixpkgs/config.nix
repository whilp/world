{ pkgs }: {
  allowUnfree = true;
  packageOverrides = super: let self = super.pkgs; in with self; rec {
  all = pkgs.buildEnv {
    name = "all";
    paths = [
      cacert
      diffutils
      emacs
      git
      gitAndTools.hub
      go
      goPackages.godef
      goPackages.godep
      nix
      openssh
    ];
  };
};}
