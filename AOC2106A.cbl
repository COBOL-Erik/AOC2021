       identification division.
       program-id. AOC2106A.
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
       77 fish            pic X(900000) value space.
       77 baby-boom-count pic S9(8) comp.
       77 fish-count      pic S9(8) comp value 1.

       procedure division.
       a-main.
           open input input-file 
           read input-file into fish(1:length of indata)
           close input-file

           perform 80 times
              move zero to baby-boom-count 
              inspect fish tallying baby-boom-count for all '0'
              inspect fish converting '012345678' to '601234567'
              perform baby-boom-count times
                 string fish delimited by space
                        ',8' delimited by size
                   into fish
              end-perform   
           end-perform
           inspect fish tallying fish-count for all ','
           display fish-count 

           goback
           .