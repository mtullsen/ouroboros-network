{ runCommand, fd, lib, stylish-haskell, haskell-nix }:

runCommand "check-stylish" {
  meta.platforms = with lib.platforms; [ linux ];
  buildInputs = [ fd stylish-haskell ];
  src = haskell-nix.haskellLib.cleanGit {
    name = "ouroboros-network-src";
    src = ../.;
  };
} ''
  unpackPhase
  cd $sourceRoot
  fd -p ouroboros-consensus* -e hs -E Setup.hs -E Ouroboros/Consensus/Mempool/TxLimits.hs -X stylish-haskell -c .stylish-haskell.yaml -i
  echo $? >> $out
''
