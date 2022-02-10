       identification division.
       program-id. AOC2107B.
       author. COBOL-Erik.
       environment division.
       configuration section.
       input-output section.
       file-control.
             select input-file assign to INFIL1.
       data division.
       file section.
       fd input-file.
       01 indata pic X(4000).

       working-storage section.
       77 a-point pic s9(4) comp-4 value 1.
       77 a-count pic s99 comp-4.
       77 occix   pic s9(4) comp-4 value zero.
       77 trys    pic s9(4) comp-4.
       77 median  pic s9(4) comp-4.
       77 fuel    pic s9(8) comp-4 value zero.
       77 lowfuel pic s9(8) comp-4 value 99999999.
       77 acc     pic s9(8) comp-4.
       77 roof    pic s9(8) comp-4.
       77 num     pic s9(8) comp-4.
       77 qn5 pic x(30) value '16,1,2,0,4,2,7,1,2,14'. *> For test only
       01 t.
          05 occ occurs 1111 times.
             10 k1 pic 9999.

       procedure division.
           display 'start'
           open input input-file
           read input-file
      *     move qn5 to indata 
           move HIGH-VALUE to t
           perform until a-point > length of indata
              add 1 to occix
              unstring indata(a-point:) delimited by all ','
              into k1(occix) count in a-count
              end-unstring
              add a-count 1 to a-point
           end-perform
      * I am missing the last number because of spaces. Backtrack it:
           compute a-point = a-point - 1 - a-count
           compute k1(occix) = function numval(indata(a-point:))


           sort occ ascending key k1
           
      * Of course in B, median may NOT be the best bet. We try all
      * values <= the largest value in the list (= occ(occix))
           move 1 to trys
           perform until occ(trys) = occ(occix)
              compute median = trys
              move zero to fuel
              move 1 to occix
              perform until occ(occix) = HIGH-VALUE
                 compute roof = function abs(function numval(occ(occix))
                                                    - median)
                 perform SUMRANGE
                 compute fuel = fuel + acc
                 add 1 to occix
              end-perform
              if fuel < lowfuel
                 move fuel to lowfuel
              end-if
              add 1 to trys 
           end-perform 

           display lowfuel

           close input-file
           goback
           .

       SUMRANGE section.
           move zero to acc
           perform varying num from 1 by 1 until num > roof
              add num to acc
           end-perform
           .