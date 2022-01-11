       identification division.
       program-id. AOC2104A.
       author. COBOL-Erik.
       environment division.
       configuration section.
       input-output section.
       file-control.
             select input-file assign to INFIL1.
             select bingo-file assign to BINGO1.
       data division.
       file section.
       fd input-file.
       01 input-record.
          05 numbers-drawn pic X(300).

       fd bingo-file.
       01 bingo-row-f.
          05 filler occurs 5 times.
             10 bingo-number-x-f.
                15 bingo-number-f  pic XX.
             10 filler             pic X. *> The space between numbers

       working-storage section.
       77 dn-string        pic X(300).
       77 drawn-number-x pic XX.
       77 dn-count pic S9(4) comp.
       77 dn-point pic S9(4) comp value 1.

       77 sum-part pic S9(4) comp value zero.
       77 ans      pic S9(8) comp.

       01 fstat pic X value ' '.
          88 eof value 'Y'.

       01 filler pic X value ' '.
          88 bingo value 'B'.

       01 bingo-boards.
          05 bingo-board           occurs 100 times indexed by bbx.
             10 bingo-row          occurs 5   times indexed by brx.
                15 bingo-number-x  occurs 5   times indexed by bnx.
                   20 bingo-number pic XX.
                   20 marker       pic X.
                      88 marked value 'X'.

       procedure division.
       a-main.
      * Create bingo boards:
           set bbx to 1
           set brx to 1
           open input bingo-file
           read bingo-file
             at end set eof to true
           end-read
           perform until eof
              if bingo-row-f = space *> Empty row between boards = space
                 set bbx up by 1
                 set brx to 1
              else
                 move bingo-row-f to bingo-row(bbx, brx)
                 set brx up by 1
              end-if
              read bingo-file
                at end set eof to true
              end-read
           end-perform
           close bingo-file

      * Draw numbers:
           open input input-file
           read input-file
           move numbers-drawn to dn-string
           close input-file *> This invalidates data in FD!
           perform draw
           perform draw until dn-point > length of dn-string
                           or dn-string(dn-point:) = space
                           or bingo
           if bingo
              display 'Bingo!'
           else
              display 'No bingo, sadly'
              goback
           end-if

      * Sum all unmarked numbers on winning board
           perform varying brx from 1 by 1 until brx > 5
              perform varying bnx from 1 by 1 until bnx > 5
                 if not marked(bbx, brx, bnx)
                    compute sum-part = sum-part
                       + function numval(bingo-number(bbx, brx, bnx))
                 end-if
              end-perform
           end-perform
           display 'Sum unmarked numbers on bingo board: ' sum-part
           display 'Latest number drawn: ' drawn-number-x
           compute ans = sum-part * function numval(drawn-number-x)
           display 'Answer: ' ans

           goback
           .

       draw section.
           unstring dn-string(dn-point:) delimited by ','
               into drawn-number-x count in dn-count
           end-unstring
           if dn-count not = 2 *> Make into number by shifting right
              move function reverse(drawn-number-x)
                to drawn-number-x
           end-if
           add dn-count 1 to dn-point

           perform varying bbx from 1 by 1 until bbx > 100
              perform varying brx from 1 by 1 until brx > 5
                 perform varying bnx from 1 by 1 until bnx > 5
                    if bingo-number(bbx, brx, bnx) = drawn-number-x
                       set marked(bbx, brx, bnx) to true
                    end-if
      * Check column for bingo:
                    if 'X' = marker(bbx, 1, bnx)
                       and = marker(bbx, 2, bnx)
                       and = marker(bbx, 3, bnx)
                       and = marker(bbx, 4, bnx)
                       and = marker(bbx, 5, bnx)
                       set bingo to true
                       exit section
                    end-if
                 end-perform
      * Check row for bingo:
                 if 'X' = marker(bbx, brx, 1)
                    and = marker(bbx, brx, 2)
                    and = marker(bbx, brx, 3)
                    and = marker(bbx, brx, 4)
                    and = marker(bbx, brx, 5)
                    set bingo to true
                    exit section
                 end-if
              end-perform
           end-perform
           .


      * Would be great if this worked!!
      *     set bbx to 1
      *     set brx to 1
      *     set bnx to 1
      *     search bingo-board
      *     at end
      *        display 'End search'
      *     when bingo-number(bbx, brx, bnx) = '17'
      *        display bingo-number(bbx, brx, bnx)
      *     end-search
      * But, alas, it does not. Search is limited to one index :-(
