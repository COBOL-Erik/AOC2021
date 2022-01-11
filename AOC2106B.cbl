       identification division.
       program-id. AOC2106B.
       author. COBOL-Erik.
       environment division.
       configuration section.
       input-output section.
       file-control.
             select input-file assign to INFIL1.
       data division.
       file section.
       fd input-file.
       01 indata pic X(600).

       working-storage section.
       77 fish            pic X(600).
       01 counters.
          05 c0           pic S9(18) comp value zero.
          05 c1           pic S9(18) comp value zero.
          05 c2           pic S9(18) comp value zero.
          05 c3           pic S9(18) comp value zero.
          05 c4           pic S9(18) comp value zero.
          05 c5           pic S9(18) comp value zero.
          05 c6           pic S9(18) comp value zero.
          05 c7           pic S9(18) comp value zero.
          05 c8           pic S9(18) comp value zero.
          05 babies       pic S9(18) comp value zero.
       77 fish-count      pic S9(18) comp.

       procedure division.
       a-main.
           open input input-file
           read input-file into fish
           close input-file

           inspect fish tallying c0 for all '0'
           inspect fish tallying c1 for all '1'
           inspect fish tallying c2 for all '2'
           inspect fish tallying c3 for all '3'
           inspect fish tallying c4 for all '4'
           inspect fish tallying c5 for all '5'
           inspect fish tallying c6 for all '6'
           inspect fish tallying c7 for all '7'
           inspect fish tallying c8 for all '8'

           perform 256 times
              move c0 to babies
              move c1 to c0
              move c2 to c1
              move c3 to c2
              move c4 to c3
              move c5 to c4
              move c6 to c5
              compute c6 = babies + c7
              move c8 to c7
              move babies to c8
           end-perform
           compute fish-count = c0 + c1 + c2 + c3 + c4 + c5 + c6 + c7
                              + c8
           display fish-count

           goback
           .
