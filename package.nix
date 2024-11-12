{
  rustPlatform,
  buildPackages,
  SDL2,
  lib,
}:

rustPlatform.buildRustPackage {
  pname = "imghcprof";
  version =
    (builtins.listToAttrs (
      map (x: {
        name = x.name;
        value = x;
      }) ((builtins.fromTOML (builtins.readFile ./Cargo.lock)).package)
    )).imghcprof.version;

  src = ./.;
  cargoLock.lockFile = ./Cargo.lock;

  nativeBuildInputs = [
    # Cross pkg-config does not detect SDL2 when compiling to mingw
    buildPackages.pkg-config
  ];

  buildInputs = [
    SDL2
  ];

  # No tests so don't waste time
  doCheck = false;

  meta = {
    description = "Graphical viewer for GHC .prof files";
    homepage = "https://github.com/t4ccer/imghcprof";
    license = lib.licenses.agpl3Plus;
    maintainers = with lib.maintainers; [ t4ccer ];
    platforms = lib.platforms.all;
    mainProgram = "imghcprof";
  };
}
