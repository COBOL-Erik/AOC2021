       identification division.
       program-id. AOC2107A.
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
       77 middle  pic s9(4) comp-4.
       77 median  pic s9(4) comp-4.
       77 fuel    pic s9(8) comp-4 value zero.
       77 qn5 pic x(30) value '16,1,2,0,4,2,7,1,2,14'. *> For test only
       01 t.
          05 occ occurs 1111 times.
             10 k1 pic 9999.
       procedure division.
           display 'start'
           open input input-file
           read input-file

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
      * The median ought to be the best bet:     
           compute middle = occix / 2
           sort occ ascending key k1
           compute median = function numval(occ(middle))

      * Fuel calculation:     
           move zero to fuel
           move 1 to occix
           perform until occ(occix) = HIGH-VALUE
              compute fuel = fuel
                 + function abs(function numval(occ(occix)) - median)
              add 1 to occix
           end-perform

           display fuel

           close input-file
           goback
           .
