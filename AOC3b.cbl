       identification division.
       program-id. AOC3b.
       author. COBOL-Erik.
       environment division.
       input-output section.
       file-control.
             select input-file
               assign to var-input-file
               organization is line sequential.
             select out1-file
               assign to 'C:\WS\AOC2021\AOC3o1.txt'
               organization is line sequential.
             select out2-file
               assign to 'C:\WS\AOC2021\AOC3o2.txt'
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
       fd out1-file
          block 0 records
          label records omitted
          data record is out1-record.
       01 out1-record.
          05 cur-num pic 9(12).
       fd out2-file
          block 0 records
          label records omitted
          data record is out2-record.
       01 out2-record.
          05 cur-num pic 9(12).

       working-storage section.
       01 k-constants.
          05 k-roof  pic S9(2)  comp-4 value 12.

       77 var-input-file pic X(60) value 'C:\WS\AOC2021\AOC3.txt'

       77 a-dummy    pic X.
       77 a-bin-rep  pic X(12).
       77 a-dec-rep  pic S9(12) comp-4.
       77 a-work     pic S9(2)  comp-4 value zero.
       77 a-key      pic S9(12) comp-4 value zero.

       01 results.
          05 sum1    pic S9(12) comp-4 value zero.
          05 sum2    pic S9(12) comp-4 value zero.
          05 ox-dec  pic S9(12) comp-4 value zero.
          05 co2-dec pic S9(12) comp-4 value zero.

       01 indexes.
          05 ix      pic S9(2) comp-4.

       01 file-eof   pic X(4)              value 'on'.
          88 eof-no value 'on'.
          88 eof-in value 'EOFi'.

       procedure division.
      * Read input file
      ** Write 1-posts to one file and 0-posts to another
      *** For "oxygen", if sum of ones >= sum of zeroes, keep "1-file"
      ***                                                else "0-file"
      **** Copy that file to "input-file". Inc. pos. Start over.
           perform varying ix from 1 by 1 until ix > k-roof
              open input input-file
              open output out1-file out2-file
              move zero to sum1 sum2
              set eof-no to true
              read input-file at end set eof-in to true end-read
              perform until eof-in
                 if input-record(ix:1) = 1
                    add 1 to sum1
                    move input-record to out1-record
                    write out1-record
                 else
                    add 1 to sum2
                    move input-record to out2-record
                    write out2-record
                 end-if
                 read input-file at end set eof-in to true end-read
              end-perform
              close input-file out1-file out2-file
              perform create-new-input-file-1
           end-perform
           move out1-record to a-bin-rep
           display a-bin-rep
           perform get-decimal-rep
           move a-dec-rep to ox-dec

      * Again, for CO2 this time:
           move 'C:\WS\AOC2021\AOC3Copy.txt' to var-input-file
           perform varying ix from 1 by 1 until ix > k-roof
              open input input-file
              open output out1-file out2-file
              move zero to sum1 sum2
              set eof-no to true
              read input-file at end set eof-in to true end-read
              perform until eof-in
                 if input-record(ix:1) = 1
                    add 1 to sum1
                    move input-record to out1-record
                    write out1-record
                 else
                    add 1 to sum2
                    move input-record to out2-record
                    write out2-record
                 end-if
                 read input-file at end set eof-in to true end-read
              end-perform
              close input-file out1-file out2-file
              perform create-new-input-file-2
           end-perform
           move out2-record to a-bin-rep
           display a-bin-rep
           perform get-decimal-rep
           move a-dec-rep to co2-dec

           display ox-dec
           display co2-dec
           compute a-key = ox-dec * co2-dec
           display a-key

           accept a-dummy *> To keep the console open
           goback
           .


       create-new-input-file-1 section.
           set eof-no to true
           if sum1 >= sum2
              if sum1 = 1         *> We have found our number!
                 move 99999 to ix *> force end to loop
                 exit section
              end-if
              open output input-file
              open input out1-file
              read out1-file at end set eof-in to true end-read
              perform until eof-in
                 move out1-record to input-record
                 write input-record
                 read out1-file at end set eof-in to true end-read
              end-perform
              close out1-file input-file
           else
              open output input-file
              open input out2-file
              read out2-file at end set eof-in to true end-read
              perform until eof-in
                 move out2-record to input-record
                 write input-record
                 read out2-file at end set eof-in to true end-read
              end-perform
              close out2-file input-file
           end-if
           .

       create-new-input-file-2 section.
           set eof-no to true
           if sum1 < sum2
              open output input-file
              open input out1-file
              read out1-file at end set eof-in to true end-read
              perform until eof-in
                 move out1-record to input-record
                 write input-record
                 read out1-file at end set eof-in to true end-read
              end-perform
              close out1-file input-file
           else
              if sum2 = 1         *> We have found our number!
                 move 99999 to ix *> force end to loop
                 exit section
              end-if
              open output input-file
              open input out2-file
              read out2-file at end set eof-in to true end-read
              perform until eof-in
                 move out2-record to input-record
                 write input-record
                 read out2-file at end set eof-in to true end-read
              end-perform
              close out2-file input-file
           end-if
           .


       get-decimal-rep section.
           move zero to a-dec-rep
           perform varying ix from 1 by 1 until ix > k-roof
              move a-bin-rep(ix:1) to a-work
              compute a-dec-rep = a-dec-rep
                             + a-work * 2**(k-roof - ix)
           end-perform
           .

