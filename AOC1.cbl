       identification division.
       program-id. AOC1.
       author. COBOL-Erik.
       environment division.
       input-output section.
       file-control.
             select input-file
               assign to 'C:\WS\AOC2021\AOC1.txt'
               organization is line sequential.
       configuration section.
       data division.
       file section.
       fd input-file
          block 0 records
          label records omitted
          data record is input-record.
       01 input-record.
          05 cur-num pic X(4).

       working-storage section.
       77 a-dummy    pic X.
       77 a-work-num pic S9(4) comp-4.
       77 a-prev-num pic S9(4) comp-4 value zero.
       77 a-inc      pic S9(4) comp-4 value zero.

       01 file-eof   pic X(4)         value 'on'.
          88 eof-in value 'EOFi'.

       procedure division.
           open input input-file
           read input-file
             at end
                set eof-in to true
             not at end
                compute a-prev-num = function numval(cur-num)
                read input-file at end set eof-in to true end-read
           end-read
           perform until eof-in
              compute a-work-num = function numval(cur-num)
              if a-work-num > a-prev-num
                 add 1 to a-inc
              end-if
              move a-work-num to a-prev-num
      *       display cur-num ' ' a-inc
              read input-file at end set eof-in to true end-read
           end-perform
           display a-inc
           close input-file

           accept a-dummy *> To keep the console open
           goback.
