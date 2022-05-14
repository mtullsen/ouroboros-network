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
 
alias pr = print

# Most of the code below should work in any directory, so this file can
# typically be a source of cut & paste.

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
echo "generate tm/nwmodules.gr (the graph of all modules in"
echo "  ouroboros-network/src/ and ouroboros-network-framework/src (no tests)"

(cd ${NWTOP};
 hsmodulegraph ouroboros-network/src/**/*.hs \
               ouroboros-network-framework/src/**/*.hs  > tm/nwmodules.gr)

echo "generate tm/nwmodules.ns (just the node/module names):"
cat tm/nwmodules.gr | sed -e 's/ : .*//;' | sort > tm/nwmodules.ns

echo "... and display the graph with graphtool \n"
cat tm/nwmodules.gr |
  graphtool "nodes -ig < - | display"

############################################################
echo "generate tm/allmodules.gr:"
echo "- so far, not too useful, includes the cardano-client/ pkg files)"
(cd ${NWTOP}; 
 hsmodulegraph cardano-client/src/**/*.hs \
               ouroboros-network/src/**/*.hs \
               ouroboros-network-framework/src/**/*.hs  > tm/allmodules.gr)

############################################################
# this useful for 'beginners' to the code:

echo "create te/nwmodules.tagged-filenames"
pecho " - labels the network files per which pkg they come from"
echo " - net: (ouroboros-network), netf: (ouroboros-network-framework)"

(cd ${NWTOP}/ouroboros-network/src;
 pr -l **/*.hs | sed 's/^/net:  /') >! ${TMPDIR}/T1
(cd ${NWTOP}/ouroboros-network-framework/src; pr -l /**/*.hs | sed 's/^/netf: /') >! ${TMPDIR}/T2
cat ${TMPDIR}/{T1,T2} | sort -b -k2 > te/nwmodules.tagged-filenames

############################################################
echo "define tm/key.ns:"

hsmodulegraph $fsoi |
  graphtool "nodes -ig <- | print nodes" | sort >| tm/key0.ns

echo TODO: define tm/key.ns

echo "tm/key2.ns: tm/key.ns with highly-imported modules removed"
egrep -v "ConnectionManager.Types|Governor.Types" \
  tm/key.ns >| tm/key2.ns


############################################################
  
echo "display only 'key' modules (tm/key.ns) and dependents: \n"
cat tm/nwmodules.gr |
  graphtool \
    "nodes -ig < - | roots $(paste -sd ' ' tm/key.ns) | display"

echo "display the same in 'node' form:"
cat tm/nwmodules.gr |
  graphtool \
    "nodes -ig < - | roots $(paste -sd ' ' tm/key.ns) | print no"


############################################################

echo "let's visualize the least upper bound (LUB) of fsoi:"
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

echo "the LUB is Ouroboros.Network.Diffusion.P2P"
echo
echo "the subset of modules from P2P down to all the 'key.ns' modules:"

# Everything between P2P ("LUB of tm/key2.ns") and tm/key2.ns
#  - captured above via $fsoi
graphtool                                \
  " nodes -ig < tm/nwmodules.gr          \
  | root Ouroboros.Network.Diffusion.P2P \
  | swap                                 \
  | root $(paste -sd ' ' tm/key.ns)      \
  | swap                                 \
  | display"


############################################################

echo "the subset of modules from P2P down (big graph!):"
echo "... store results in tm/p2pdow.{gr,ns}:"

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

fsp2ptop=$(cat tm/p2pdown.filenames)

############################################################

echo "graph of modules 'above' (which import, recursively) fsoi":
graphtool                                \
  " nodes -ig < tm/nwmodules.gr          \
  | swap                                 \
  | root $(paste -sd ' ' tm/key.ns)      \
  | swap                                 \
  | display" | less

#  Just one thing learned:
#   - one importer of Ouroboros.Network.Diffusion.P2P :
#     Ouroboros.Network.Diffusion

############################################################
echo "less useful, but module dependencies, each pkg separately:"
echo ": ouroboros-network:"
hsmodulegraph ouroboros-network/src/**/*.hs |
  graphtool "nodes -ig < - | di"

echo ": ouroboros-network-framework:"
hsmodulegraph ouroboros-network-framework/src/**/*.hs |
  graphtool "nodes -ig < - | di"
