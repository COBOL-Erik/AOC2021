       identification division.
       program-id. AOC4.
       author. COBOL-Erik.
       environment division.
       input-output section.
       file-control.
             select input-file
               assign to 'C:\WS\AOC2021\AOC4atry.txt'
               organization is line sequential.
             select bingo-file
               assign to 'C:\WS\AOC2021\AOC4btry.txt'
               organization is line sequential.
       configuration section.
       data division.
       file section.
       fd input-file
          block 0 records
          label records omitted
          data record is input-record.
       01 input-record.
          05 numbers-drawn pic X(300).
       fd bingo-file
          block 0 records
          label records omitted
          data record is bingo-row.
       01 bingo-row.
          05 filler occurs 5 times.
             10 bingo-number-x.
                15 bingo-number  pic 99.
             10 filler           pic X. *> The space between numbers

       working-storage section.
       77 a-dummy    pic X.
       77 a-work-num pic S9(4) comp-4.
       77 a-prev-num pic S9(4) comp-4 value zero.
       77 a-inc      pic S9(4) comp-4 value zero.

       01 bingo-boards.
          05 bingo-board occurs 200 times.
             10 row occurs 5 times.
                15 kol occurs 5 times.
                   20 cur-num pic 99.
                   20 filler  pic X value space.
                      88 marked value 'X'.

       01 indices.
          05 inx pic S9(4) comp-4.
          05 bbx pic S9(4) comp-4.
          05 rwx pic S9(4) comp-4.
          05 klx pic S9(4) comp-4.

       01 file-eof   pic X(4)         value 'on'.
          88 eof-in value 'EOFi'.

       procedure division.
      * Read in numbers drawn:
           open input input-file
           read input-file
           close input-file

      * Read in bingo boards:
           move 1 to bbx rwx klx
           open input bingo-file
           read bingo-file at end set eof-in to true end-read
           perform until eof-in
              if bingo-row = space *> Empty row between boards
                 add 1 to bbx
                 move 1 to rwx
                 read bingo-file at end set eof-in to true end-read
                 exit perform cycle
              end-if
              perform varying inx from 1 by 1 until inx > 5
                 move bingo-number(inx) to cur-num(inx,rwx,bbx)
                 display function trim(bingo-number-x(inx))
              end-perform
              add 1 to rwx
              read bingo-file at end set eof-in to true end-read
           end-perform
           display bbx
           display rwx
           display cur-num(5,3,2)
           close bingo-file

           accept a-dummy *> To keep the console open
           goback.
