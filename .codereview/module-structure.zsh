#! /usr/bin/env zsh

# EDIT THIS:
NWTOP=~/src/iohk/ouroboros-network
TMPDIR=~/tmp

# Script assumptions

cd ${NWTOP}
mkdir -p ${NWTOP}/.codereview/{tm,tf,te}
ln -s .codereview/te .
ln -s .codereview/tm .
ln -s .codereview/tf .
 
setopt extendedglob
alias pr=print

# Most of the code below should work in any directory, so this file can
# typically be a source of cut & paste.

function section () {
    echo "\n\n"
    echo "----------" $1 "----------\n"
}

############################################################
# basic files of interest (as communicated by IOG)

export fsoi=$(pr -l */src/**/PeerSelection/PeerStateActions.hs \
                    */src/**/ConnectionManager/*.hs \
                    */src/**/Governor.hs \
                    */src/**/Governor/*.hs \
                    */src/**/InboundGovernor.hs \
                    */src/**/InboundGovernor/*.hs \
                    */src/**/Server2.hs \
                    */src/**/ConnectionHandler.hs \
                    */src/**/P2P.hs)

############################################################

# make lists of our 'true' src files, exclude test files
#  (we'll expand globs at references to)

net_srcfiles=ouroboros-network/src/**/*.hs~**/Hello/*.hs
  # a little adhoc, need to ignore 4 files!
netf_srcfiles=ouroboros-network-framework/src/**/*.hs

# debugging:
# print -l ${~net_srcfiles}
# exit 1

############################################################

section "generate tm/nwmodules.gr"
echo " (the graph of all modules in ouroboros-network/src/ and "
echo "  ouroboros-network-framework/src (no tests)"

(cd ${NWTOP};
 hsmodulegraph ${~net_srcfiles} ${~netf_srcfiles} > tm/nwmodules.gr)

echo "generate tm/nwmodules.ns (just the node/module names):"
cat tm/nwmodules.gr | sed -e 's/ : .*//;' | sort > tm/nwmodules.ns

echo "\n\n"
echo "... and display the graph with graphtool \n"
cat tm/nwmodules.gr |
  graphtool "nodes -ig < - | display"

############################################################
section "generate tm/allmodules.gr:"
echo "- so far, not too useful, includes the cardano-client/ pkg files)"
(cd ${NWTOP}; 
 hsmodulegraph cardano-client/src/**/*.hs \
               ${~net_srcfiles} ${~netf_srcfiles} > tm/allmodules.gr)

############################################################
# this useful for 'beginners' to the code:

section "create te/nwmodules.tagged-filenames"
echo " - labels the network files per which pkg they come from"
echo " - net: (ouroboros-network), netf: (ouroboros-network-framework)"

(cd ${NWTOP}/ouroboros-network/src;
 pr -l **/*.hs | sed 's/^/net:  /') >! ${TMPDIR}/T1
(cd ${NWTOP}/ouroboros-network-framework/src; pr -l **/*.hs | sed 's/^/netf: /') >! ${TMPDIR}/T2
cat ${TMPDIR}/{T1,T2} | sort -b -k2 > te/nwmodules.tagged-filenames

############################################################
section "define tm/key.ns & tm/key2.ns"

hsmodulegraph ${=fsoi} |        # note zsh word splitting
  graphtool "nodes -ig <- | print nodes" | sort >| tm/key.ns

echo "tm/key2.ns = tm/key.ns with highly-imported modules removed"
egrep -v "ConnectionManager.Types|Governor.Types" \
  tm/key.ns >| tm/key2.ns


############################################################
  
section "display 'key' modules and dependents"
cat tm/nwmodules.gr |
  graphtool \
    "nodes -ig < - | roots $(paste -sd ' ' tm/key.ns) | display"

echo "\ndisplay the same in 'node' form:\n"
cat tm/nwmodules.gr |
  graphtool \
    "nodes -ig < - | roots $(paste -sd ' ' tm/key.ns) | print nodes" |
  sed 's/^/  /'


############################################################

section "visualize least upper bound (LUB) of fsoi"
graphtool                                \
  " nodes -ig < tm/nwmodules.gr          \
  | swap                                 \
  | root $(paste -sd ' ' tm/key2.ns)     \
  | swap                                 \
  | display"

  # This isn't actually computing the LUB, it's just apparent that 
  # that the second line is the LUB:
  #  Ouroboros.Network.Diffusion
  #  ↳ Ouroboros.Network.Diffusion.P2P      *our LUB*
  #  ...
  # NOTE
  #  - One would expect to see everything that depends on these tm/key2.ns
  #  - BTW, Ouroboros.Network.Diffusion is "dead" in context of 2 pkgs
  #  - You are only nabbing nodes from the two packages (or 3)
  #  - You are ignoring that ouroboros-network exports a lot!

echo 
echo "the LUB is Ouroboros.Network.Diffusion.P2P"

############################################################
section "modules between P2P and 'key.ns' modules:"

graphtool                                \
  " nodes -ig < tm/nwmodules.gr          \
  | root Ouroboros.Network.Diffusion.P2P \
  | swap                                 \
  | root $(paste -sd ' ' tm/key.ns)      \
  | swap                                 \
  | display"


############################################################
section "modules from P2P down"
echo "... store results in tm/p2pdown.{gr,ns}:"

graphtool \
  "nodes -ig < tm/nwmodules.gr    \
  | root Ouroboros.Network.Diffusion.P2P \
  | print graph" >| tm/p2pdown.gr
graphtool "nodes -ig < tm/p2pdown.gr | print nodes" > tm/p2pdown.ns


echo "... and display the graph:"

graphtool "nodes -ig < tm/p2pdown.gr | display"

# create file list variable from above:
for f in $(sed "s#\.#/#g;s/$/.hs/" tm/p2pdown.ns); do
  echo */src/$f
  done > tm/p2pdown.filenames

export fsp2ptop=$(cat tm/p2pdown.filenames)

############################################################

section "modules which import (recursively) fsoi"
graphtool                                \
  " nodes -ig < tm/nwmodules.gr          \
  | swap                                 \
  | root $(paste -sd ' ' tm/key.ns)      \
  | swap                                 \
  | display"

#  Just one thing learned:
#   - one importer of Ouroboros.Network.Diffusion.P2P :
#     Ouroboros.Network.Diffusion

############################################################
section "module dependencies, each pkg separately"
echo "\n : ouroboros-network:\n"
hsmodulegraph ${~net_srcfiles} |
  graphtool "nodes -ig < - | di"

echo "\n : ouroboros-network-framework:\n"
hsmodulegraph ${~netf_srcfiles} |
  graphtool "nodes -ig < - | di"
