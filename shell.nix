with (import <nixpkgs> {}).pkgs;
let 
  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      reactive-banana-gtk = self.callPackage ./. {};
      reactive-banana = self.callPackage ../reactive-banana/reactive-banana {};
    };
  };
in modifiedHaskellPackages.reactive-banana-gtk.env
