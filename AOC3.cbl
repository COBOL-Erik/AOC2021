       identification division.
       program-id. AOC3.
       author. COBOL-Erik.
       environment division.
       input-output section.
       file-control.
             select input-file
               assign to 'C:\WS\AOC2021\AOC3.txt'
               organization is line sequential.
       configuration section.
       data division.
       file section.
       fd input-file
          block 0 records
          label records omitted
          data record is input-record.
       01 input-record.
          05 cur-num pic 9(12).

       working-storage section.
       01 k-constants.
          05 k-roof  pic S9(2)  comp-4 value 12.

       77 a-dummy    pic X.
       77 a-work     pic S9(2)  comp-4 value zero.
       77 a-key      pic S9(12) comp-4 value zero.

       01 filler.
          05 sums occurs 12 times.
             10 gamma    pic S9(12) comp-4 value zero.
             10 epsilon  pic S9(12) comp-4 value zero.

       01 results.
          05 gamma-bin   pic 9(12).
          05 epsilon-bin pic 9(12).
          05 gamma-res   pic S9(12) comp-4 value zero.
          05 epsilon-res pic S9(12) comp-4 value zero.

       01 indexes.
          05 ix      pic S9(2) comp-4.

       01 file-eof   pic X(4)         value 'on'.
          88 eof-in value 'EOFi'.

       procedure division.
           open input input-file
           read input-file at end set eof-in to true end-read
      **Sums:
           perform until eof-in
              perform varying ix from 1 by 1 until ix > k-roof
                 if input-record(ix:1) = 1
                    add 1 to gamma(ix)
                 else
                    add 1 to epsilon(ix)
                 end-if
              end-perform
              read input-file at end set eof-in to true end-read
           end-perform
      **Bit rep. according to sums:
           perform varying ix from 1 by 1 until ix > k-roof
              if gamma(ix) > epsilon(ix)
                 move 1 to gamma-bin(ix:1)
                 move 0 to epsilon-bin(ix:1)
              else
                 move 0 to gamma-bin(ix:1)
                 move 1 to epsilon-bin(ix:1)
              end-if
           end-perform
      **Decimal rep.:
           perform varying ix from 1 by 1 until ix > k-roof
              move gamma-bin(ix:1) to a-work
              compute gamma-res = gamma-res
                                + a-work * 2**(k-roof - ix)
              move epsilon-bin(ix:1) to a-work
              compute epsilon-res = epsilon-res
                                + a-work * 2**(k-roof - ix)
           end-perform
           compute a-key = gamma-res * epsilon-res
           display a-key
           close input-file

           accept a-dummy *> To keep the console open
           goback.
