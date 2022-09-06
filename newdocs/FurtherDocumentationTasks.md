
                       Further Documentation Tasks

## More Code and Documentation Thereof

- Extensions of the Tutorial2.lhs code (along with documentation) that
  would make the code increasingly realistic:
  1. update `slotsInEpoch` to no longer be static and immutable (get from config, add to LedgerState, add ability to vote to change, etc.).
  2. Extend `SelectView PrtclD == BlockNo` to be more realistic (to allow for the
     case when two chains have the same `BlockNo` ...; see discussion in
     ToyLedgerD.hs).
  3. Extend `ChainDepState PrtclD == ()` to be more realistic.
  4. Have transactions that can fail. 
  5. Support more headers and hashes (Example2.lhs is very minimal here).

- Do further documentation of the hard fork combinator (elaborating on Edsco's
  blog post and capturing his two videos on the same).

## Haddock

- Integrate ... into the IOG haddock:    
  - Extend the Tutorial.lhs and the current haddock generation such that one can
    haddock-browse the tutorial and click into the existing haddocks (assuming
    this will *not* come for free).
  - Extend/modify cheatsheet.md so one can click and go to the haddocks.

## Client for Tutorial2.lhs and Running/Testing Tutorial2.lhs

- Write *client code* for Tutorial2.lhs.
  i.e., code that invokes the the code in Tutorial2.lhs; this would be "as
  simple as possible" version of the consensus outer loop, commented and
  documented.
    
- Using the above client code, extend so we can create and apply headers and
  blocks to it so as to explore and see the protocol in action.
  - even a single node could be useful, though ideally we should
    make this work with multiple nodes.
  - Question: Is there relevant/similar code in the existing testing code?
    
## Remaining Items From the Original /Gap Analysis/

The original gap analysis is here
https://docs.google.com/document/d/1KSXEdjS56hMZGSsdusi6nuuTHqwD6y3i_FKn04dOUpU/

There were many things that were not done, note these:
  - B. Gaps from a SWE Vantage point:
    - Create various architectural views and etc.
    - Some degree of requirements tracing.
  - C.1 Markdown documents in the repo: a little restructuring.
  - C.2 Haddock Generated Documentation: more at "top" of repo and fill in 
    some modules
    
