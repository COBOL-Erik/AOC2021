       identification division.
       program-id. AOC2b.
       author. COBOL-Erik.
       environment division.
       input-output section.
       file-control.
             select input-file
               assign to 'C:\WS\AOC2021\AOC2.txt'
               organization is line sequential.
       configuration section.
       data division.
       file section.
       fd input-file.
       01 input-record pic X(9).

       working-storage section.
       77 a-dummy    pic X.
       77 a-func     pic X(7).
       77 a-val      pic 9.
       77 a-horizont pic S9(12) comp-4 value zero.
       77 a-depth    pic S9(12) comp-4 value zero.
       77 a-aim      pic S9(12) comp-4 value zero.
       77 a-key      pic S9(12) comp-4.

       01 file-eof   pic X(4)         value 'on'.
          88 eof-in value 'EOFi'.

       procedure division.
           open input input-file
           read input-file at end set eof-in to true end-read
           perform until eof-in
              unstring input-record
                delimited by all ' '
                into a-func a-val
              end-unstring
              evaluate a-func
              when 'forward'
                 add a-val to a-horizont
                 compute a-depth = a-depth + a-aim * a-val
              when 'down'
                 add a-val to a-aim
              when 'up'
                 subtract a-val from a-aim
              end-evaluate
              read input-file at end set eof-in to true end-read
           end-perform
           display a-horizont
           display a-depth
           display a-aim
           compute a-key = a-horizont * a-depth
           display a-key
           close input-file

           accept a-dummy *> To keep the console open
           goback.
