       identification division.
       program-id. AOC1b.
       author. COBOL-Erik.
       environment division.
       input-output section.
       file-control.
             select input-file-1
               assign to 'C:\WS\AOC2021\AOC1.txt'
               organization is line sequential.
             select input-file-2
               assign to 'C:\WS\AOC2021\AOC1.txt'
               organization is line sequential.
             select input-file-3
               assign to 'C:\WS\AOC2021\AOC1.txt'
               organization is line sequential.
       configuration section.
       data division.
       file section.
       fd input-file-1
          block 0 records
          label records omitted
          data record is input-record-1.
       01 input-record-1.
          05 cur-num-1 pic X(4).
       fd input-file-2
          block 0 records
          label records omitted
          data record is input-record-2.
       01 input-record-2.
          05 cur-num-2 pic X(4).
       fd input-file-3
          block 0 records
          label records omitted
          data record is input-record-3.
       01 input-record-3.
          05 cur-num-3 pic X(4).

       working-storage section.
       77 a-dummy    pic X.
       77 a-work-num pic S9(4) comp-4.
       77 a-prev-num pic S9(4) comp-4 value zero.
       77 a-inc      pic S9(4) comp-4 value -1. *> To offset first comp.

       01 work-slide-sums.
          05 ssum1   pic S9(4) comp-4 value zero.
          05 ssum2   pic S9(4) comp-4 value zero.
          05 ssum3   pic S9(4) comp-4 value zero.
       01 done-slide-sums.
          05 dsum1   pic S9(4) comp-4 value zero.
          05 dsum2   pic S9(4) comp-4 value zero.
          05 dsum3   pic S9(4) comp-4 value zero.

       01 indexes.
          05 ix1     pic S9(2) comp-4 value zero.
          05 ix2     pic S9(2) comp-4 value zero.
          05 ix3     pic S9(2) comp-4 value zero.

       01 file-eof   pic X(4)         value 'on'.
          88 eof-in value 'EOFi'.

       procedure division.
           open input input-file-1 input-file-2 input-file-3
           read input-file-1 at end set eof-in to true end-read
           read input-file-2 at end set eof-in to true end-read
           read input-file-2 at end set eof-in to true end-read
           read input-file-3 at end set eof-in to true end-read
           read input-file-3 at end set eof-in to true end-read
           read input-file-3 at end set eof-in to true end-read
           perform until eof-in
              perform advance-1
              perform advance-2
              perform advance-3
           end-perform
           display a-inc
           close input-file-1
           close input-file-2
           close input-file-3

           accept a-dummy *> To keep the console open
           goback
           .

       advance-1 section.
           compute a-work-num = function numval(cur-num-1)
           add a-work-num to ssum1
           add 1 to ix1
           if ix1 = 3
              move zero to ix1
              move ssum1 to dsum1
              if dsum1 > dsum3
                 add 1 to a-inc
              end-if
              move zero to ssum1
           end-if
           read input-file-1 at end set eof-in to true end-read
           .

       advance-2 section.
           compute a-work-num = function numval(cur-num-2)
           add a-work-num to ssum2
           add 1 to ix2
           if ix2 = 3
              move zero to ix2
              move ssum2 to dsum2
              if dsum2 > dsum1
                 add 1 to a-inc
              end-if
              move zero to ssum2
           end-if
           read input-file-2 at end set eof-in to true end-read
           .

       advance-3 section.
           compute a-work-num = function numval(cur-num-3)
           add a-work-num to ssum3
           add 1 to ix3
           if ix3 = 3
              move zero to ix3
              move ssum3 to dsum3
              if dsum3 > dsum2
                 add 1 to a-inc
              end-if
              move zero to ssum3
           end-if
           read input-file-3 at end set eof-in to true end-read
           .
