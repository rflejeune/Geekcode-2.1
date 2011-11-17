       IDENTIFICATION DIVISION.
       PROGRAM-ID. GEEKCODE.
      ******************************************************************
      *                                                                *
      *AUTHOR. RANDY LEJEUNE.                                          *
      *DATE-WRITTEN.  29  SEP 2010.                                    *
      *                                                                *
      ******************************************************************
      ******************************************************************
      *   This program is free software; you can redistribute it       *
      *   and/or modify it under the terms of the GNU General Public   *
      *   License as published by the Free Software Foundation; either *
      *   version 2 of the License, or at your option) any later       *
      *   version.                                                     *
      *                                                                *
      *   This program is distributed in the hope that it will be      *
      *   useful, but WITHOUT ANY WARRANTY; without even the implied   *
      *   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      *
      *   PURPOSE.  See the GNU General Public License for more        *
      *   details.                                                     *
      *                                                                *
      *   You should have received a copy of the GNU General Public    *
      *   License along with this program; if not, write to the Free   *
      *   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,*
      *                                                                *
      *   Interface Design taken from Chris Gushue's geekcode generator*
      *                                                                *
      ******************************************************************
      ******************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

           SOURCE-COMPUTER. IBM-386.
           OBJECT-COMPUTER. IBM-386.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT GEEK-SIG             ASSIGN TO "geekcode.sig"
                                       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.

       FILE SECTION.

       FD  GEEK-SIG. 
       01  GEEK-OUTPUT-REC             PIC X(80).

       WORKING-STORAGE SECTION.

       COPY "geekcode.cpy".

       77  WS-PRINT-LINE1              PIC X(80)     VALUE SPACES.
       77  WS-PRINT-LINE2              PIC X(80)     VALUE SPACES.
       77  WS-PRINT-LINE3              PIC X(80)     VALUE SPACES.
       77  WS-PRINT-LINE4              PIC X(80)     VALUE SPACES.

       77  WS-COMMAND                  PIC A(20)     VALUE SPACES.
       77  WS-CL-ARGS                  PIC X(10)     VALUE SPACES.
       77  WS-PAGE-CNT                 PIC 99        VALUE ZEROES.
       77  WS-CNT                      PIC XX        VALUE SPACES.
       77  WS-TOT-PAGE                 PIC 99        VALUE 45.
       77  WS-REC-CNT                  PIC 9         VALUE ZEROES.
       77  WS-RETURN-SYS-CODE          PIC 9(8) COMP VALUE ZEROES.
       77  WS-ENTRY                    PIC XX        VALUE ZEROES.

       01 WS-VALID-FLAG                PIC X         VALUE "N".
          88  WS-VALID-DATA                          VALUE "Y".
          88  WS-INVALID-DATA                        VALUE "N".

       01 WS-VALID-PENS                PIC X         VALUE "N".
          88  WS-HOW-MANY                            VALUE "Y".

       PROCEDURE DIVISION.

       00000-CONTROL.
           PERFORM 10000-SETUP
           PERFORM 20000-PROCESS
           PERFORM 30000-CLEANUP.

       10000-SETUP.
           ACCEPT WS-CL-ARGS FROM COMMAND-LINE END-ACCEPT
           PERFORM 93000-PARSE-CMDLN
           OPEN OUTPUT GEEK-SIG
           INITIALIZE  GEEK-OUTPUT-REC.

       20000-PROCESS.
           PERFORM 90000-CLEAR-SCREEN
           PERFORM 91000-PRINT-HEADING
           PERFORM 21000-CREATE.

       21000-CREATE.
           PERFORM 22100-TYPE      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 22200-DRESS     UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 22300-HAIR      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 22400-HEIGHT    UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 22450-WEIGHT    UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 22500-GLASSES   UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 22600-PENS      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 22670-SLIDES    UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 22700-AUTO      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 22800-AGE       UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 22900-WEIRD     UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 23000-VERBAGE   UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 23100-COMP      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 23200-FLAVOR    UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 23250-UNIX      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 23300-PERL      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 23400-LINUX     UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 23500-386BSD    UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 23600-NEWS      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 23700-WEB       UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 23800-EMACS     UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 23900-KIBO      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 24000-MS        UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 24100-MAC       UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 24200-VMS       UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 24400-POL       UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 24500-CP        UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 24700-TREK      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 24800-BAB       UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 25000-JEOP      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 25100-ROLE      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 25200-MAGIC     UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 25300-TV        UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 25400-BOOKS     UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 25500-DOOM      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 25600-BARNEY    UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 25700-EDUC      UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 25800-MUSIC     UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 25900-HOUSE     UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 26000-FRIENDS   UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 26100-REL       UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 26200-NUT       UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 26300-GENDER    UNTIL WS-VALID-DATA
           SET WS-VALID-FLAG TO "N"
           PERFORM 26350-SEX       UNTIL WS-VALID-DATA
           PERFORM 26500-PRINT.

       22100-TYPE.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT (2:1) TO WS-CNT

           DISPLAY "Geek Type
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
                  END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
               END-DISPLAY
           DISPLAY " 1 GB  - Geek of Business                15 GL  -
      -"Geek of Literature"
               END-DISPLAY
           DISPLAY " 2 GC  - Geek of Classics                16 GMC -
      -"Geek of Mass Communications"
               END-DISPLAY
           DISPLAY " 3 GCA - Geek of Commercial Arts         17 GM  -
      -"Geek of Math"
               END-DISPLAY
           DISPLAY " 4 GCM - Geek of Computer Management     18 GMD -
      -"Geek of Medicine"
               END-DISPLAY
           DISPLAY " 5 GCS - Geek of Computer Science        19 GMU -
      -"Geek of Music"
               END-DISPLAY
           DISPLAY " 6 GCC - Geek of Communications          20 GPA -
      -"Geek of Performing Arts"
               END-DISPLAY
           DISPLAY " 7 GE  - Geek of Engineering             21 GP  -
      -"Geek of Philosophy"
               END-DISPLAY
           DISPLAY " 8 GED - Geek of Education               22 GS  -
      -"Geek of Science"
               END-DISPLAY
           DISPLAY " 9 GFA - Geek of Fine Arts               23 GSS -
      -"Geek of Social Science"
               END-DISPLAY
           DISPLAY "10 GG  - Geek of Government              24 GTW -
      -"Geek of Technicial Writing"
               END-DISPLAY
           DISPLAY "11 GH  - Geek of Humanities              25 GO  -
      -"Geek of Other"
               END-DISPLAY
           DISPLAY "12 GIT - Geek of Information Technology  26 GU  -
      -"Geek of Undecided"
               END-DISPLAY
           DISPLAY "13 GJ  - Geek of Jurisprudence (Law)     27 G!  -
      -"Geek of No Qualifications"
               END-DISPLAY
           DISPLAY "14 GLS - Geek of Library Science         28 GAT -
      -"Geek of All Trades"
               END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Geek Type code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 28
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "GL" TO WS-TYPE
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "GC" TO WS-TYPE
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "GCA" TO WS-TYPE
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "GCM" TO WS-TYPE
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "GCS" TO WS-TYPE
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "GCC" TO WS-TYPE
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "GE" TO WS-TYPE
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "GED" TO WS-TYPE
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "GFA" TO WS-TYPE
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "GG" TO WS-TYPE
                   ELSE IF WS-ENTRY = 11 THEN
                       MOVE "GH" TO WS-TYPE
                   ELSE IF WS-ENTRY = 12 THEN
                       MOVE "GIT" TO WS-TYPE
                   ELSE IF WS-ENTRY = 13 THEN
                       MOVE "GJ" TO WS-TYPE
                   ELSE IF WS-ENTRY = 14 THEN
                       MOVE "GLS" TO WS-TYPE
                   ELSE IF WS-ENTRY = 15 THEN
                       MOVE "GL" TO WS-TYPE
                   ELSE IF WS-ENTRY = 16 THEN
                       MOVE "GMC" TO WS-TYPE
                   ELSE IF WS-ENTRY = 17 THEN
                       MOVE "GM" TO WS-TYPE
                   ELSE IF WS-ENTRY = 18 THEN
                       MOVE "GMD" TO WS-TYPE
                   ELSE IF WS-ENTRY = 19 THEN
                       MOVE "GMU" TO WS-TYPE
                   ELSE IF WS-ENTRY = 20 THEN
                       MOVE "GPA" TO WS-TYPE
                   ELSE IF WS-ENTRY = 21 THEN
                       MOVE "GP" TO WS-TYPE
                   ELSE IF WS-ENTRY = 22 THEN
                       MOVE "GS" TO WS-TYPE
                   ELSE IF WS-ENTRY = 23 THEN
                       MOVE "GSS" TO WS-TYPE
                   ELSE IF WS-ENTRY = 24 THEN
                       MOVE "GTW" TO WS-TYPE
                   ELSE IF WS-ENTRY = 25 THEN
                       MOVE "GO" TO WS-TYPE
                   ELSE IF WS-ENTRY = 26 THEN
                       MOVE "GU" TO WS-TYPE
                   ELSE IF WS-ENTRY = 27 THEN
                       MOVE "G!" TO WS-TYPE
                   ELSE IF WS-ENTRY = 28 THEN
                       MOVE "GAT" TO WS-TYPE
                   END-IF 
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22200-DRESS.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT (2:1) TO WS-CNT

           DISPLAY "Dress                                               
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1  d++  I tend to wear conservative dress such as "
              "a business suit or "
           END-DISPLAY
           DISPLAY "         worse, a tie." END-DISPLAY
           DISPLAY " 2  d+   Good leisure-wear. Slacks, button-shirt, "
               "etc. No jeans, tennis "
           END-DISPLAY
           DISPLAY "         shoes, or t-shirts." END-DISPLAY
           DISPLAY " 3  d    I dress a lot like those found in catalog "
               "ads. Bland, boring, "
           END-DISPLAY
           DISPLAY "         without life or meaning." END-DISPLAY
           DISPLAY " 4  d-   I'm usually in jeans and a t-shirt."
           END-DISPLAY
           DISPLAY " 5  d--  My t-shirts go a step further and have a "
               "trendy political "
           END-DISPLAY
           DISPLAY "         message on them." END-DISPLAY
           DISPLAY " 6  d--- Punk dresser, including, but not limited "
               "to, torn jeans and "
           END-DISPLAY
           DISPLAY "         shirts, body piercings, and prominent "
               "tattoos."
           END-DISPLAY
           DISPLAY " 7  dx   Cross dresser." END-DISPLAY
           DISPLAY " 8  d?   I have no idea what I am wearing now, "
               "let alone what I wore yesterday."
           END-DISPLAY
           DISPLAY " 9  !d   No clothing. Quite a fashion statement, "
               "don't you think?"
           END-DISPLAY
           DISPLAY "10  dpu  I wear the same clothes all the time, no "
               "matter the occasion, "
           END-DISPLAY
           DISPLAY "         forgetting to do laundry between wearings."
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Dress code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 10
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "d++" TO WS-DRESS
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "d+" TO WS-DRESS
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "d" TO WS-DRESS
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "d-" TO WS-DRESS
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "d--" TO WS-DRESS
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "d---" TO WS-DRESS
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "dx" TO WS-DRESS
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "d?" TO WS-DRESS
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "!d" TO WS-DRESS
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "dpu" TO WS-DRESS
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22300-HAIR.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT (2:1) TO WS-CNT

           DISPLAY "Hair                                                
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 H+++   My hair goes down past my waist."
           END-DISPLAY
           DISPLAY " 2 H++    My hair dangles to my mid-back."
           END-DISPLAY
           DISPLAY " 3 H+     It's down to about my shoulders."
           END-DISPLAY
           DISPLAY " 4 H      It's just pretty normal hair."
           END-DISPLAY
           DISPLAY " 5 H-     It's cut above the neck."
           END-DISPLAY
           DISPLAY " 6 H--    Above the neck AND ear (flattop)."
           END-DISPLAY
           DISPLAY " 7 H---   It's about 1/8 inch long."
           END-DISPLAY
           DISPLAY " 8 H----  I shave my head daily, otherwise it gets "
               "too long."
           END-DISPLAY
           DISPLAY " 9 !H     I'm bald."
           END-DISPLAY
           DISPLAY "10 H?     I have wigs that allow me to vary my "
               "hair."
           END-DISPLAY
           DISPLAY "11 H*     My hair is dyed funky flavors."
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Dress code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 11
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "H+++" TO WS-HAIR
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "H++" TO WS-HAIR
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "H+" TO WS-HAIR
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "H" TO WS-HAIR
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "H-" TO WS-HAIR
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "H--" TO WS-HAIR
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "H---" TO WS-HAIR
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "H----" TO WS-HAIR
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "!H" TO WS-HAIR
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "H?" TO WS-HAIR
                   ELSE IF WS-ENTRY = 11 THEN
                       MOVE "H*" TO WS-HAIR
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22400-HEIGHT.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT (2:1) TO WS-CNT

           DISPLAY "Height                                              
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 s+++   I usually have to duck through doors. "
           END-DISPLAY
           DISPLAY " 2 s++    I'm a basketball candidate. "
           END-DISPLAY
           DISPLAY " 3 s+     I'm a little taller than most. "
           END-DISPLAY
           DISPLAY " 4 s      I'm an average geek. "
           END-DISPLAY
           DISPLAY " 5 s-     I look up to most people. "
           END-DISPLAY
           DISPLAY " 6 s--    I look up to damn near everybody. "
           END-DISPLAY
           DISPLAY " 7 s---   I take a phone book with me when I go "
           END-DISPLAY
           DISPLAY "          out so I can eat dinner. "
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Height code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 07
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "s+++" TO WS-HEIGHT
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "s++" TO WS-HEIGHT
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "s+" TO WS-HEIGHT
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "s" TO WS-HEIGHT
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "s-" TO WS-HEIGHT
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "s--" TO WS-HEIGHT
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "s---" TO WS-HEIGHT
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22450-WEIGHT.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT (2:1) TO WS-CNT

           DISPLAY "Weight                                              
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 s+++   I take up three movie seats. "
           END-DISPLAY
           DISPLAY " 2 s++    I'm a linebacker candidate. "
           END-DISPLAY
           DISPLAY " 3 s+     I'm a little rounder than most."
           END-DISPLAY
           DISPLAY " 4 s      I'm an average geek."
           END-DISPLAY
           DISPLAY " 5 s-     Everybody tells me to gain a few pounds."
           END-DISPLAY
           DISPLAY " 6 s--    I tend to have to fight against a strong "
               "breeze."
           END-DISPLAY
           DISPLAY " 7 s---   My bones are poking through my skin. "
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Weight code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 07
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "+++" TO WS-WEIGHT
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "++" TO WS-WEIGHT
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "+" TO WS-WEIGHT
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE " " TO WS-WEIGHT
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "-" TO WS-WEIGHT
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "--" TO WS-WEIGHT
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "---" TO WS-WEIGHT
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22500-GLASSES.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT (2:1) TO WS-CNT

           DISPLAY "Glasses                                             
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 g+++   I have coke-bottle glasses that I can "
               "use to start leaves on "
           END-DISPLAY
           DISPLAY "          fire in the hot sun."
           END-DISPLAY
           DISPLAY " 2 g++    I've got four eyes and tape in the "
               "middle.  "
           END-DISPLAY
           DISPLAY " 3 g+     I've got four eyes, what's your point?"
           END-DISPLAY
           DISPLAY " 4 g-     I have contacts."
           END-DISPLAY
           DISPLAY " 5 g--    I have colored contacts I have contacts."
           END-DISPLAY
           DISPLAY " 6 g---   I have those funky contact that have "
              "interesting designs on"
           END-DISPLAY
           DISPLAY "          then such as happy faces or some such. "
           END-DISPLAY
           DISPLAY " 7 !g     I have no glasses."
           END-DISPLAY
           DISPLAY " 8 g?     I can't find my glasses."
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Glasses code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 08
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "g+++" TO WS-GLASSES
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "g++" TO WS-GLASSES
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "g+" TO WS-GLASSES
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "g-" TO WS-GLASSES
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "g--" TO WS-GLASSES
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "g---" TO WS-GLASSES
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "!g" TO WS-GLASSES
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "g?" TO WS-GLASSES
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22600-PENS.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT (2:1) TO WS-CNT

           DISPLAY "Pens                                          
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY "Do you have any pens in your pockets? "
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " 1 Yes." END-DISPLAY
           DISPLAY " 2 No." END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Pens code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 02
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "p" TO WS-PENS (1:1)
                       PERFORM 90000-CLEAR-SCREEN
                       PERFORM 22650-HOW-MANY UNTIL WS-HOW-MANY
                   ELSE IF WS-ENTRY = 02 THEN
                       PERFORM 90000-CLEAR-SCREEN
                       PERFORM 22660-NOPENS
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22650-HOW-MANY.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT (2:1) TO WS-CNT

           DISPLAY "Pens                                          
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY "How many pens do you have in your pockets? "
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " 1 One." END-DISPLAY
           DISPLAY " 2 Two." END-DISPLAY
           DISPLAY " 3 Three." END-DISPLAY
           DISPLAY " 4 Four." END-DISPLAY
           DISPLAY " 5 Five." END-DISPLAY
           DISPLAY " 6 Six." END-DISPLAY
           DISPLAY " 7 Seven." END-DISPLAY
           DISPLAY " 8 Eight." END-DISPLAY
           DISPLAY " 9 Nine." END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Pens Number code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 09
                   SET WS-VALID-PENS TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE 1 TO WS-PENS (2:1)
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE 2 TO WS-PENS (2:1)
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE 3 TO WS-PENS (2:1)
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE 4 TO WS-PENS (2:1)
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE 5 TO WS-PENS (2:1)
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE 6 TO WS-PENS (2:1)
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE 7 TO WS-PENS (2:1)
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE 8 TO WS-PENS (2:1)
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE 9 TO WS-PENS (2:1)
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22660-NOPENS.
           EXIT.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT (2:1) TO WS-CNT

           DISPLAY "Pens                                          
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 p?     I can't find a writing instrument."
           END-DISPLAY
           DISPLAY " 2 !p     Pens are obsolete. I have a Newton."
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your No Pens code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 02
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "?" TO WS-PENS (2:1)
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "!p" TO WS-PENS
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22670-SLIDES.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT (2:1) TO WS-CNT

           DISPLAY "Slide Rules, Etc.                                   
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY "Do you carry a slide rule, calculator or portable co
      -        "mputer along with you?"
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " 1 Yes." END-DISPLAY
           DISPLAY " 2 No." END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Extra Stuff code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 02
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "+" TO WS-PENS (3:1)
                   ELSE IF WS-ENTRY = 02 THEN
                       CONTINUE
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22700-AUTO.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT TO WS-CNT

           DISPLAY "Automobile                                          
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 au++++ I have my chauffeured limo take me "
               "everywhere."
           END-DISPLAY
           DISPLAY " 2 au+++  I own four different colored Mercedes."
           END-DISPLAY
           DISPLAY " 3 au++   I drive a brand new car that cost more "
               "than most houses"
           END-DISPLAY
           DISPLAY " 4 au+    I have a sporty-looking car which would "
               "be a babe-mobile if"
           END-DISPLAY
           DISPLAY "          I wasn't such a geek."
           END-DISPLAY
           DISPLAY " 5 au     I drive a car which I bought from my "
               " parents. It has four doors even "
           END-DISPLAY
           DISPLAY "          though I'm the only one who ever rides in 
      -        "it. "
           END-DISPLAY
           DISPLAY " 6 au-    I drive my parents' car. Hey, if I could "
               "afford my own I wouldn't "
           END-DISPLAY
           DISPLAY "          be living at home with them. "
           END-DISPLAY
           DISPLAY " 7 au--   My car has rust everywhere and the "
              "muffler drags along the ground.  "
           END-DISPLAY
           DISPLAY " 8 au---  I drive a '77 Pinto which went over "
               "100,000 miles two years ago.  "
           END-DISPLAY
           DISPLAY " 9 au---- I have a Yugo."
           END-DISPLAY
           DISPLAY "10 !au    I don't have a car."
           END-DISPLAY
           DISPLAY "11 au*    I have a motorcycle."
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Automobile code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 11
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "au++++" TO WS-AUTO
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "au+++" TO WS-AUTO
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "au++" TO WS-AUTO
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "au+" TO WS-AUTO
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "au" TO WS-AUTO
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "au-" TO WS-AUTO
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "au--" TO WS-AUTO
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "au---" TO WS-AUTO
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "au----" TO WS-AUTO
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "!au" TO WS-AUTO
                   ELSE IF WS-ENTRY = 11 THEN
                       MOVE "au*" TO WS-AUTO
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22800-AGE.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT TO WS-CNT

           DISPLAY "Age                                                 
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 a+++   60 and up." END-DISPLAY
           DISPLAY " 2 a++    50-59." END-DISPLAY
           DISPLAY " 3 a+     40-49." END-DISPLAY
           DISPLAY " 4 a      30-39." END-DISPLAY
           DISPLAY " 5 a-     20-29." END-DISPLAY
           DISPLAY " 6 a--    10-19." END-DISPLAY
           DISPLAY " 7 a---   9 and under." END-DISPLAY
           DISPLAY " 8 a?     Ageless." END-DISPLAY
           DISPLAY " 9 !a     It's none of your business how old I am."
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Age code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 09
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                               MOVE "a+++" TO WS-AGE
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "a++" TO WS-AGE
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "a+" TO WS-AGE
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "a" TO WS-AGE
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "a-" TO WS-AGE
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "a--" TO WS-AGE
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "a---" TO WS-AGE
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "a?" TO WS-AGE
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "!a" TO WS-AGE
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       22900-WEIRD.
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT TO WS-CNT

           DISPLAY "Weirdness                                           
      -       "             Page: " WS-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 w+++  Mainstream? I heard of that once, I think."
           END-DISPLAY
           DISPLAY " 2 w++   I am so weird, I make Al Yankovic look sane
      -        ".  "
           END-DISPLAY
           DISPLAY " 3 w+    So? What's your problem with weird."
           END-DISPLAY
           DISPLAY " 4 w     I am not weird. I'm perfectly normal."
           END-DISPLAY
           DISPLAY " 5 w-    I'm more normal that most people normally a
      -        "re."
           END-DISPLAY
           DISPLAY " 6 w--   I am so incredibly boring . . .  "
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Weirdness code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 06
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                               MOVE "w+++" TO WS-WEIRD
                   ELSE IF WS-ENTRY = 02 THEN
                               MOVE "w++" TO WS-WEIRD
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "w+" TO WS-WEIRD
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "w" TO WS-WEIRD
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "w-" TO WS-WEIRD
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "w--" TO WS-WEIRD
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       23000-VERBAGE.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Verbage                                             
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 v---   I don't talk. I just type. "
           END-DISPLAY
           DISPLAY " 2 v--    When I talk, people usually look mildly em
      -        "barrassed. "
           END-DISPLAY
           DISPLAY " 3 v-     I use words like 'grok' in everyday conver
      -        "sation."
           END-DISPLAY
           DISPLAY " 4 v      At least I speak in complete sentences. Us
      -        "ually. "
           END-DISPLAY
           DISPLAY " 5 v+     People compliment me on my vocabulary. "
           END-DISPLAY
           DISPLAY " 6 v++    People compliment me on my eloquence. "
           END-DISPLAY
           DISPLAY " 7 v+++   I was the regional forensics champ. "
           END-DISPLAY
           DISPLAY " 8 !v     Speech is irrelevant, I use telepathy. "
           END-DISPLAY
           DISPLAY " 9 v?     I mumble.  " END-DISPLAY
           DISPLAY "10 v*     I babble.  " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Verbage code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 10
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "v---" TO WS-VERBAGE
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "v--" TO WS-VERBAGE
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "v-" TO WS-VERBAGE
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "v" TO WS-VERBAGE
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "v+" TO WS-VERBAGE
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "v++" TO WS-VERBAGE
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "v+++" TO WS-VERBAGE
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "!v" TO WS-VERBAGE
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "v?" TO WS-VERBAGE
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "v*" TO WS-VERBAGE
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       23100-COMP.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Computers                                           
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 C++++  I'll be first in line to get the new cyber
      -       "netic interface installed "
           END-DISPLAY
           DISPLAY "          into my skull. " END-DISPLAY
           DISPLAY " 2 C+++   You mean there is life outside of Internet
      -        "? You're shittin' me! I "
           END-DISPLAY
           DISPLAY "          live for muds. I haven't dragged myself to
      -        " class in weeks. "
           END-DISPLAY
           DISPLAY " 3 C++    Computers are a large part of my existenc
      -        "ence. When I get up in the "
           END-DISPLAY
           DISPLAY "          morning, the first thing I do is log myse
      -        "lf in. I mud on weekends, "
           END-DISPLAY
           DISPLAY "          but still manage to stay off of academic p
      -        "robation."
           END-DISPLAY
           DISPLAY " 4 C+     Computers are fun and I enjoy using them.
      -        "I play a mean game of DOOM! "
           END-DISPLAY
           DISPLAY "          and can use a word processor without resor
      -        "ting to the manual too. "
           END-DISPLAY
           DISPLAY "          often. I know that a 3.5 inch disk is not
      -        " a hard disk. I also "
           END-DISPLAY
           DISPLAY "          know that when it says 'press any key' to 
      -        "continue, I don't have to "
           END-DISPLAY
           DISPLAY "          look for a key labeled 'ANY'.  "
           END-DISPLAY
           DISPLAY " 5 C      Computers are a tool, nothing more. I use 
      -        "it when it serves my "
           END-DISPLAY
           DISPLAY "          purpose." END-DISPLAY
           DISPLAY " 6 C-     Anything more complicated than my calculat
      -        "or and I'm screwed. "
           END-DISPLAY
           DISPLAY " 7 C--    Where's the on switch? " END-DISPLAY
           DISPLAY " 8 C---   If you even mention computers, I will rip
      -         "your head off!  "
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Computer code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 08
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "C++++" TO WS-COMP
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "C+++" TO WS-COMP
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "C++" TO WS-COMP
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "C+" TO WS-COMP
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "C" TO WS-COMP
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "C-" TO WS-COMP
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "C--" TO WS-COMP
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "C---" TO WS-COMP
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       23200-FLAVOR.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "UNIX Flavor                                         
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 B  BSD (use this unless your BSDish system is men
      -        "tioned below)." END-DISPLAY
           DISPLAY " 2 L  Linux" END-DISPLAY
           DISPLAY " 3 U  Ultrix" END-DISPLAY
           DISPLAY " 4 A  AIX" END-DISPLAY
           DISPLAY " 5 V  SysV" END-DISPLAY
           DISPLAY " 6 H  HP-UX" END-DISPLAY
           DISPLAY " 7 I  IRIX" END-DISPLAY
           DISPLAY " 8 O  OSF/1" END-DISPLAY
           DISPLAY " 9 S  SunOS / Solaris" END-DISPLAY
           DISPLAY "10 C  SCO UNIX" END-DISPLAY
           DISPLAY "11 N  NeXT" END-DISPLAY
           DISPLAY "12 ?  Some other one not listed." END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your UNIX Flavor code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 12
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "UB" TO WS-UNIX-FLAVOR
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "UL" TO WS-UNIX-FLAVOR
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "UU" TO WS-UNIX-FLAVOR
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "UA" TO WS-UNIX-FLAVOR
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "UV" TO WS-UNIX-FLAVOR
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "UH" TO WS-UNIX-FLAVOR
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "UI" TO WS-UNIX-FLAVOR
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "UO" TO WS-UNIX-FLAVOR
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "US" TO WS-UNIX-FLAVOR
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "UC" TO WS-UNIX-FLAVOR
                   ELSE IF WS-ENTRY = 11 THEN
                       MOVE "UN" TO WS-UNIX-FLAVOR
                   ELSE IF WS-ENTRY = 12 THEN
                       MOVE "U?" TO WS-UNIX-FLAVOR
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       23250-UNIX.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "UNIX Skill                                          
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 " WS-UNIX-FLAVOR "++++ I am the sysadmin. If you 
      -        "try and crack my machine don't be"
           END-DISPLAY
           DISPLAY "	      surprised if the municipal works department g
      -        "ets an 'accidental'"
           END-DISPLAY
           DISPLAY "          computer-generated order to start a new la
      -        "ndfill put on your front"
           END-DISPLAY
           DISPLAY "          lawn."
           END-DISPLAY
           DISPLAY " 2 " WS-UNIX-FLAVOR "+++  I don't need to crack /etc
      -        "/passwd because I just modified su"
           END-DISPLAY
           DISPLAY "	      so that it doesn't prompt me.  The admin staf
      -        "f doesn't even know"
           END-DISPLAY
           DISPLAY "          I'm here. If you don't understand what I j
      -        "ust said, this category"
           END-DISPLAY
           DISPLAY "          does NOT apply to you!" END-DISPLAY
           DISPLAY " 3 " WS-UNIX-FLAVOR "++   I've get the entire admin 
      -        "ticked off at me because I am always"
           END-DISPLAY
           DISPLAY "	      using all of the CPU time and trying to run p
      -        "rograms that I don't have"
           END-DISPLAY
           DISPLAY "          access to. I'm going to try cracking /etc/
      -        "passwd next week, just "
           END-DISPLAY
           DISPLAY "          don't tell anyone." END-DISPLAY
           DISPLAY " 4 " WS-UNIX-FLAVOR "+    I not only have a unix acc
      -        "ount, but I slam VMS any chance I get."
           END-DISPLAY
           DISPLAY " 5 " WS-UNIX-FLAVOR "     I have a unix account to d
      -        "o my stuff in."
           END-DISPLAY
           DISPLAY " 6 " WS-UNIX-FLAVOR "-    I have a VMS account."
           END-DISPLAY
           DISPLAY " 7 " WS-UNIX-FLAVOR "--   I've seen unix and didn't 
      -        "like it.  DEC rules!"
           END-DISPLAY
           DISPLAY " 8 " WS-UNIX-FLAVOR "---  Unix geeks are actually ne
      -        "rds in disguise. "
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your UNIX Skill code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 08
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "++++" TO WS-UNIX-GURU
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "+++" TO WS-UNIX-GURU
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "++" TO WS-UNIX-GURU
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "+" TO WS-UNIX-GURU
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE " " TO WS-UNIX-GURU
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "-" TO WS-UNIX-GURU
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "--" TO WS-UNIX-GURU
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "---" TO WS-UNIX-GURU
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       23300-PERL.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Perl                                                
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 P++++  I don't write Perl, I speak it. Perl has s
      -        "uperseded all"
           END-DISPLAY
           DISPLAY "          other programming languages. I firmly bel
      -        "ieve that all"
           END-DISPLAY
           DISPLAY "          programs can be reduced to a Perl one-line
      -        "r."
           END-DISPLAY
           DISPLAY " 2 P+++   Perl is a very powerful programming tool. 
      -        "Not only do I"
           END-DISPLAY
           DISPLAY "          no longer write shell scripts, I also no l
      -        "onger use awk or"
           END-DISPLAY
           DISPLAY "          sed. I use Perl for all programs of less t
      -        "han a thousand lines.  "
           END-DISPLAY
           DISPLAY " 3 P++    Perl is a powerful programming tool. I don
      -        "'t write shell"
           END-DISPLAY
           DISPLAY "          scripts anymore because I write them in Pe
      -        "rl."
           END-DISPLAY
           DISPLAY " 4 P+     I know of Perl. I like Perl. I just haven'
      -        "t learned much Perl,"
           END-DISPLAY
           DISPLAY "          but it is on my agenda. " END-DISPLAY
           DISPLAY " 5 P-     What's Perl got that awk and sed don't hav
      -        "e?  "
           END-DISPLAY
           DISPLAY " 6 P--    Perl users are sick, twisted programmers w
      -        "ho are just"
           END-DISPLAY
           DISPLAY "          showing off. " END-DISPLAY
           DISPLAY " 7 P---   Perl combines the power of sh, the clarity
      -        " of sed, and the"
           END-DISPLAY
           DISPLAY "          performance of awk with the simplicity of 
      -        "C. It should be banned."
           END-DISPLAY
           DISPLAY " 8 P?     What's Pearl?" END-DISPLAY
           DISPLAY " 9 !P     Our paranoid admin won't let us install pe
      -        "rl! Says it's"
           END-DISPLAY
           DISPLAY "          a 'hacking tool'."  END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Perl code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 09
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "P++++" TO WS-PERL
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "P+++" TO WS-PERL
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "P++" TO WS-PERL
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "P+" TO WS-PERL
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "P-" TO WS-PERL
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "P--" TO WS-PERL
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "P---" TO WS-PERL
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "P?" TO WS-PERL
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "!P" TO WS-PERL
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       23400-LINUX.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Linux                                               
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 L++++  I am Linus, hear me roar." END-DISPLAY
           DISPLAY " 2 L+++   I am a Linux wizard. I munch C code for br
      -        "eakfast and have enough "
           END-DISPLAY
           DISPLAY "          room left over for a kernel debugging. I h
      -        "ave so many patches "
           END-DISPLAY
           DISPLAY "          installed that I lost track about ten vers
      -        "ions ago. Linux"
           END-DISPLAY
           DISPLAY "          newbies consider me a net.god."
           END-DISPLAY
           DISPLAY " 3 L++    I use Linux almost exclusively on my syste
      -       "m. I monitor "
           END-DISPLAY
           DISPLAY "          comp.os.linux.* and even answer questions
      -        "some times. I've aliased "
           END-DISPLAY
           DISPLAY "          Linux FTP sites to make getting new softwa
      -        "re easier.  "
           END-DISPLAY
           DISPLAY " 4 L+     I've managed to get Linux installed and ev
      -        "en used it a few times. "
           END-DISPLAY
           DISPLAY "          It seems like it is just another OS."
           END-DISPLAY
           DISPLAY " 5 L      I know what Linux is, but that's about all
      -        "."
           END-DISPLAY
           DISPLAY " 6 L-     I have no desire to use Linux and frankly
      -       " don't give a rats patootie "
           END-DISPLAY
           DISPLAY "          about it.  " END-DISPLAY
           DISPLAY " 7 L--    Unix sucks. Because Linux = Unix. Linux S
      -        "ucks. I worship Bill Gates."
           END-DISPLAY
           DISPLAY " 8 L---   I am Bill Gates. " END-DISPLAY
           DISPLAY " 9 !L     I don't even know what Linux is!"
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Linux code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 09
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "L++++" TO WS-LINUX
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "L+++" TO WS-LINUX
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "L++" TO WS-LINUX
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "L+" TO WS-LINUX
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "L" TO WS-LINUX
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "L-" TO WS-LINUX
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "L--" TO WS-LINUX
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "L---" TO WS-LINUX
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "!L" TO WS-LINUX
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       23500-386BSD.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "386BSD                                              
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 3+++   I am a 386BSD wizard. I munch C code for b
      -        "reakfast and have enough"
           END-DISPLAY
           DISPLAY "          room left over for a kernel debugging. I h
      -        "ave so many patches"
           END-DISPLAY
           DISPLAY "          installed that I lost track about ten vers
      -        "ions ago. "
           END-DISPLAY
           DISPLAY "          386BSD newbies consider me a net.god."
           END-DISPLAY
           DISPLAY " 2 3++    I use 386BSD almost exclusively on my syst
      -       "em. I monitor "
           END-DISPLAY
           DISPLAY "          comp.os.386bsd.* and even answer questions
      -        "some times. I've aliased "
           END-DISPLAY
           DISPLAY "          386BSD FTP sites to make getting new softw
      -        "are easier.  "
           END-DISPLAY
           DISPLAY " 3 3+     I've managed to get 386BSD installed and e
      -        "ven used it a few times. "
           END-DISPLAY
           DISPLAY "          It seems like it is just another OS."
           END-DISPLAY
           DISPLAY " 4 3      I know what 386BSD is, but that's about al
      -        "l."
           END-DISPLAY
           DISPLAY " 5 3-     I have no desire to use 386BSD and frankly
      -       " don't give a rats patootie "
           END-DISPLAY
           DISPLAY "          about it.  " END-DISPLAY
           DISPLAY " 6 3--    Unix sucks. Because 386BSD = Unix. 386BSD 
      -        "Sucks. I worship Bill Gates."
           END-DISPLAY
           DISPLAY " 7 3---   I am USL's lawyer. " END-DISPLAY
           DISPLAY " 8 !3     I don't even know what Linux is!"
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your 386BSD code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 08
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "3+++" TO WS-386BSD
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "3++" TO WS-386BSD
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "3+" TO WS-386BSD
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "3" TO WS-386BSD
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "3-" TO WS-386BSD
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "3--" TO WS-386BSD
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "3---" TO WS-386BSD
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "!3" TO WS-386BSD
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       23600-NEWS.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "USENET                                              
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 N++++  I am Tim Pierce." END-DISPLAY
           DISPLAY " 2 N+++   I read so many news groups that the next b
      -        "atch of news "
           END-DISPLAY
           DISPLAY "          comes in before I finish reading the last 
      -        "batch, and I"
           END-DISPLAY
           DISPLAY "          have to read for about 2 hours straight be
      -        "fore I'm "
           END-DISPLAY
           DISPLAY "          caught up on the morning's news. Then ther
      -        "e's the afternoon...  "
           END-DISPLAY
           DISPLAY " 3 N++    I read all the news in a select handful of
      -        " groups. "
           END-DISPLAY
           DISPLAY " 4 N+     I read news recreationally when I have som
      -        "e time to kill."
           END-DISPLAY
           DISPLAY " 5 N      Usenet News? Sure, I read that once."
           END-DISPLAY
           DISPLAY " 6 N-     News is a waste of my time and I avoid it 
      -        "completely."
           END-DISPLAY
           DISPLAY " 7 N--    News sucks! 'Nuff said." END-DISPLAY
           DISPLAY " 8 N*     All I do is read news." END-DISPLAY
           DISPLAY " 9 !N     We don't have news." END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Usenet code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 09
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "N++++" TO WS-NEWS
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "N+++" TO WS-NEWS
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "N++" TO WS-NEWS
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "N+" TO WS-NEWS
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "N" TO WS-NEWS
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "N-" TO WS-NEWS
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "N--" TO WS-NEWS
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "N*" TO WS-NEWS
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "!N" TO WS-NEWS
                  END-IF
              END-IF
          END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       23700-WEB.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "World Wide Web                                      
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 W+++  I am a WebMaster . Don't even think about t
      -        "rying to view"
           END-DISPLAY
           DISPLAY "         my homepage without the latest version of N
      -        "etscape. When"
           END-DISPLAY
           DISPLAY "         I'm not on my normal net connection, I surf
      -        " the web using"
           END-DISPLAY
           DISPLAY "         my Newton and a cellular modem."
           END-DISPLAY
           DISPLAY " 2 W++   I have a homepage. I surf daily. My homepag
      -        "e is advertised"
           END-DISPLAY
           DISPLAY "         in my .signature."
           END-DISPLAY
           DISPLAY " 3 W+    I have the latest version of Netscape, and 
      -        "wander the web"
           END-DISPLAY
           DISPLAY "         only when there's something specific I'm lo
      -        "oking for."
           END-DISPLAY
           DISPLAY " 4 W     I have a browser and a connection. Occasio
      -        "nally I'll use them."
           END-DISPLAY
           DISPLAY " 5 W-    The web is really a pain. Life was so much
      -        " easier when"
           END-DISPLAY
           DISPLAY "         you could transfer information by simple A
      -        "SCII. Now everyone"
           END-DISPLAY
           DISPLAY "         won't even consider your ideas unless you 
      -        "spiff them up"
           END-DISPLAY
           DISPLAY "         with bandwidth-consuming pictures and poin
      -        "tless information links. "
           END-DISPLAY
           DISPLAY " 6 W--   A pox on the Web! It wastes time and bandw
      -        "idth and just"
           END-DISPLAY
           DISPLAY "         gives the uneducated morons a reason to cl
      -        "utter the Internet."
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your World Wide Web code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 06
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "W+++" TO WS-WEB
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "W++" TO WS-WEB
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "W+" TO WS-WEB
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "W" TO WS-WEB
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "W" TO WS-WEB
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "W--" TO WS-WEB
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       23800-EMACS.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Emacs                                               
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 E+++   Emacs is my login shell!! M-x doctor is my
      -         " psychologist! I use"
           END-DISPLAY
           DISPLAY "          emacs to control my TV and toaster oven! A
      -        "ll you vi people"
           END-DISPLAY
           DISPLAY "          don't know what you're missing! I read alt
      -        ".relgion.emacs,"
           END-DISPLAY
           DISPLAY "          alt.sex.emacs, and comp.os.emacs."
           END-DISPLAY
           DISPLAY " 2 E++    I know and use elisp regularly!"
           END-DISPLAY
           DISPLAY " 3 E+     Emacs is great! I read my mail and news wi
      -        "th it!"
           END-DISPLAY
           DISPLAY " 4 E      Yeah, I know what emacs is, and use it as
      -        "my regular editor."
           END-DISPLAY
           DISPLAY " 5 E-     Emacs is too big and bloated for my tastes
      -        "."
           END-DISPLAY
           DISPLAY " 6 E--    Emacs is just a fancy word processor."
           END-DISPLAY
           DISPLAY " 7 E---   Emacs sucks! vi forever!!!"
           END-DISPLAY
           DISPLAY " 8 E----  Emacs sucks! pico forever!!!"
           END-DISPLAY
           DISPLAY " 9 !E     Emacs? What's that?" END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Emacs code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 09
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "E+++" TO WS-EMACS
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "E++" TO WS-EMACS
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "E+" TO WS-EMACS
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "E" TO WS-EMACS
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "E-" TO WS-EMACS
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "E--" TO WS-EMACS
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "E---" TO WS-EMACS
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "E----" TO WS-EMACS
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "!E" TO WS-EMACS
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       23900-KIBO.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Kibo                                                
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 K++++++  I _am_ Kibo. " END-DISPLAY
           DISPLAY " 2 K+++++   I've had sex with Kibo." END-DISPLAY
           DISPLAY " 3 K++++    I've met Kibo." END-DISPLAY
           DISPLAY " 4 K+++     I've gotten mail from Kibo." END-DISPLAY
           DISPLAY " 5 K++      I've read Kibo." END-DISPLAY
           DISPLAY " 6 K+       I like Kibo." END-DISPLAY
           DISPLAY " 7 K        I know who Kibo is." END-DISPLAY
           DISPLAY " 8 K-       I don't know who Kibo is." END-DISPLAY
           DISPLAY " 9 K--      I dislike Kibo." END-DISPLAY
           DISPLAY "10 K---     I am Xibo." END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Kibo code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 10
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "K++++++" TO WS-KIBO
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "K+++++" TO WS-KIBO
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "K++++" TO WS-KIBO
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "K+++" TO WS-KIBO
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "K++" TO WS-KIBO
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "K+" TO WS-KIBO
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "K" TO WS-KIBO
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "K-" TO WS-KIBO
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "K--" TO WS-KIBO
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "K---" TO WS-KIBO
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       24000-MS.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Microsoft Windows                                   
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 w++++    I have Windows, Windows NT, and Windows
      -        "NT Advanced Server all "
           END-DISPLAY
           DISPLAY "            running on my SMP RISC machine. I haven'
      -        "t seen daylight in six"
           END-DISPLAY
           DISPLAY "            months." END-DISPLAY
           DISPLAY " 2 w+++     I am a MS Windows programming god. I wro
      -        "te a VxD driver to allow "
           END-DISPLAY
           DISPLAY "            MS Windows and DOS to share the use of m
      -        "y waffle iron. "
           END-DISPLAY
           DISPLAY "            P.S. Unix sux. " END-DISPLAY
           DISPLAY " 3 W++      I write MS Windows programs in C and thi
      -        "nk about using C++ "
           END-DISPLAY
           DISPLAY "            someday. I've written at least one DLL."
           END-DISPLAY
           DISPLAY " 4 w+       I have installed my own custom sounds, w
      -        "allpaper, and screen "
           END-DISPLAY
           DISPLAY "            savers so my PC walks and talks like a f
      -        "un house.  Oh yeah, I have "
           END-DISPLAY
           DISPLAY "            a hundred TrueType(tm) fonts that I've i
      -        "nstalled but never used. "
           END-DISPLAY
           DISPLAY " 5 w        Ok, so I use MS Windows, I don't have to
      -        " like it. "
           END-DISPLAY
           DISPLAY " 6 w-       I'm still trying to install MS Windows a
      -        "nd have at least one "
           END-DISPLAY
           DISPLAY "            peripheral that never works right."
           END-DISPLAY
           DISPLAY " 7 w--      MS Windows is a joke operating system. H
      -        "ell, its not even an "
           END-DISPLAY
           DISPLAY "            operating system. NT is Not Tough enough
      -        " for me either. "
           END-DISPLAY
           DISPLAY " 8 w---     Windows has set back the computing indus
      -        "try by at least 10 "
           END-DISPLAY
           DISPLAY "            years. Bill Gates should be drawn, quart
      -       "ered, hung, shot, poisoned, "
           END-DISPLAY
           DISPLAY "            disemboweled, and then REALLY hurt."
           END-DISPLAY
           DISPLAY " 9 !w       I don't do Windows. Got a problem with t
      -        "hat? "
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Microsoft code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 09
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "w++++" TO WS-MS
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "w+++" TO WS-MS
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "w++" TO WS-MS
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "w+" TO WS-MS
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "w" TO WS-MS
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "w-" TO WS-MS
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "w--" TO WS-MS
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "w---" TO WS-MS
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "!w" TO WS-MS
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       24100-MAC.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Macintosh                                           
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 M++    I am a Mac guru. Anything those dos putzes
      -        "and unix nerds"
           END-DISPLAY
           DISPLAY "          can do, i can do better, and if not, I'll
      -        " write the damn"
           END-DISPLAY
           DISPLAY "          software to do it. " END-DISPLAY
           DISPLAY " 2 M+     A Mac has it's uses and I use it quite oft
      -        "en."
           END-DISPLAY
           DISPLAY " 3 M      I use a Mac, but I'm pretty indifferent ab
      -        "out it."
           END-DISPLAY
           DISPLAY " 4 M-     Macs suck. All real geeks have a character
      -        " prompt."
           END-DISPLAY
           DISPLAY " 5 M--    Macs do more than suck. They make a user 
      -        "stupid by allowing"
           END-DISPLAY
           DISPLAY "          them to use the system without knowing wha
      -        "t they are doing."
           END-DISPLAY
           DISPLAY "          Mac weenies have lower IQs than the fuzz i
      -        "n my navel."
           END-DISPLAY
           DISPLAY " 6 !M     What's a Macintosh?" END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Macintosh code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 06
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "M++" TO WS-MAC
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "M+" TO WS-MAC
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "M" TO WS-MAC
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "M-" TO WS-MAC
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "M--" TO WS-MAC
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "!M" TO WS-MAC
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       24200-VMS.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "VMS                                                 
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 V++    Unix is a passing fad compared to the real
      -        " power in the universe, "
           END-DISPLAY
           DISPLAY "          my VMS system.  " END-DISPLAY
           DISPLAY " 2 V+     I tend to like VMS better than Unix."
           END-DISPLAY
           DISPLAY " 3 V      I've used VMS." END-DISPLAY
           DISPLAY " 4 V-     Unix is much better than VMS for my comput
      -        "ing needs. "
           END-DISPLAY
           DISPLAY " 5 V--    I would rather smash my head repeatedly in
      -        "to a brick wall than "
           END-DISPLAY
           DISPLAY "          suffer the agony of working with VMS. It's
      -        "reminiscent of a dead "
           END-DISPLAY
           DISPLAY "          and decaying pile of moose droppings. Unix
      -        " rules the universe. "
           END-DISPLAY
           DISPLAY " 6 !V     I've not ever used VMS." END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your VMS code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 06
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "V++" TO WS-VMS
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "V+" TO WS-VMS
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "V" TO WS-VMS
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "V-" TO WS-VMS
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "V--" TO WS-VMS
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "!V" TO WS-VMS
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       24400-POL.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Politics                                            
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 po+++  Fuckin' Minorities! Adolf Hitler is my her
      -        "o! And so is"
           END-DISPLAY
           DISPLAY "          Rush Limbaugh!"
           END-DISPLAY
           DISPLAY " 2 po++   All in favor of eliminating free speech, s
      -        "ay aye!"
           END-DISPLAY
           DISPLAY " 3 po+    Let's get the government off of big-busine
      -        "ss' back."
           END-DISPLAY
           DISPLAY " 4 po     Politics? I've heard of that somewhere but
      -        " in all honesty I "
           END-DISPLAY
           DISPLAY "          really don't give a shit. "
           END-DISPLAY
           DISPLAY " 5 po-    Bring back the 60's."
           END-DISPLAY
           DISPLAY " 6 po--   I'm still living in the 60's."
           END-DISPLAY
           DISPLAY " 7 po---  No taxes through no government."
           END-DISPLAY
           DISPLAY " 8 -po+   Don't label me you moron! Both sides are e
      -        "qually fucked up!"
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Politics code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 08
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "po+++" TO WS-POL
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "po++" TO WS-POL
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "po+" TO WS-POL
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "po" TO WS-POL
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "po-" TO WS-POL
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "po--" TO WS-POL
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "po---" TO WS-POL
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "-po+" TO WS-POL
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       24500-CP.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Cypherpunk                                          
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 Y+++   I am T.C. May" END-DISPLAY
           DISPLAY " 2 Y++    I am on the cypherpunks mailing list and a
      -        "ctive around"
           END-DISPLAY
           DISPLAY "          Usenet. I never miss an opportunity to tal
      -        "k about the"
           END-DISPLAY
           DISPLAY "          evils of Clipper and the NSA. Orwells' 198
      -        "4 is more than"
           END-DISPLAY
           DISPLAY "          a story, it is a warning to ours' and futu
      -        "re generations."
           END-DISPLAY
           DISPLAY "          I'm a member of the EFF." END-DISPLAY
           DISPLAY " 3 Y+     I have an interest and concern in privacy 
      -        "issues, but in"
           END-DISPLAY
           DISPLAY "          reality I am not really all that active or
      -       " vocal."
           END-DISPLAY
           DISPLAY " 4 Y      I'm pretty indifferent on the whole issue"
           END-DISPLAY
           DISPLAY " 5 Y-     It seems to me that all of these concerns 
      -        "are a little "
           END-DISPLAY
           DISPLAY "          extreme. I mean, the government must be ab
      -        "le to protect"
           END-DISPLAY
           DISPLAY "          itself from criminals." END-DISPLAY
           DISPLAY " 6 Y--    Get a life. The only people that need this
      -        " kind of protection"
           END-DISPLAY
           DISPLAY "          are people with something to hide. I think
      -        " cypherpunks are "
           END-DISPLAY
           DISPLAY "          just a little paranoid."
           END-DISPLAY
           DISPLAY " 7 Y---   I am L. Dietweiller." END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Cypherpunk code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 07
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "Y+++" TO WS-CP
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "Y++" TO WS-CP
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "Y+" TO WS-CP
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "Y" TO WS-CP
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "Y-" TO WS-CP
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "Y--" TO WS-CP
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "Y---" TO WS-CP
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       24700-TREK.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Star Trek                                           
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 t+++ It's not just a TV show, its a religion. I k
      -        "now all about"
           END-DISPLAY
           DISPLAY "        warp field dynamics and the principles behin
      -        "d the transporter."
           END-DISPLAY
           DISPLAY "        I have memorized the TECH manual. I speak Kl
      -        "ingon. I go to"
           END-DISPLAY
           DISPLAY "        cons with Vulcan ears on. I have no life. It
      -        "'s not just a "
           END-DISPLAY
           DISPLAY "        TV show, its a religion. " END-DISPLAY
           DISPLAY " 2 t++  It's the best show around. I have all the ep
      -        "isodes and the"
           END-DISPLAY
           DISPLAY "        movies on tape and can quote entire scenes v
      -        "erbatim.  I've "
           END-DISPLAY
           DISPLAY "        built a few of the model kits too. But you l
      -        "l never catch"
           END-DISPLAY
           DISPLAY "        me at one of those conventions. Those people
      -        " are kooks."
           END-DISPLAY
           DISPLAY " 3 t+   It's a damn fine TV show and is one of the o
      -        "nly things "
           END-DISPLAY
           DISPLAY "        good on television any more. " END-DISPLAY
           DISPLAY " 4 t    It's just another TV show." END-DISPLAY
           DISPLAY " 5 t-   Maybe it is just me, but I have no idea what
      -        " the big deal"
           END-DISPLAY
           DISPLAY "        with Star Trek is. Perhaps I'm missing somet
      -        "hing but I "
           END-DISPLAY
           DISPLAY "        just think it is bad drama. " END-DISPLAY
           DISPLAY " 6 t--  Star Trek is just another Space Opera."
           END-DISPLAY
           DISPLAY " 7 t--- Star Trek SUCKS! It is the worst crap I ha
      -        "ve ever seen!"
           END-DISPLAY
           DISPLAY "        Hey, all you trekkies out there, GET A LIFE
      -        "!!!"
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Star Trek code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 07
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "t+++" TO WS-TREK
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "t++" TO WS-TREK
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "t+" TO WS-TREK
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "t" TO WS-TREK
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "t-" TO WS-TREK
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "t--" TO WS-TREK
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "t---" TO WS-TREK
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       24800-BAB.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Babylon 5                                           
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 5+++   I am a True Worshipper of the Church of Jo
      -        "e who lives"
           END-DISPLAY
           DISPLAY "          eats breathes and thinks Babylon 5, and ha
      -        "s Evil thoughts"
           END-DISPLAY
           DISPLAY "          about stealing Joe's videotape archives ju
      -        "st to see "
           END-DISPLAY
           DISPLAY "          episodes earlier." END-DISPLAY
           DISPLAY " 2 5++    Finally a show that shows what a real futu
      -        "re would look"
           END-DISPLAY
           DISPLAY "          like. None of this Picardian 'Let's talk a
      -        "bout it and"
           END-DISPLAY
           DISPLAY "          be friends' crap. And what's this? We fina
      -        "lly get to "
           END-DISPLAY
           DISPLAY "          see a bathroom!  Over on that Enterprise, 
      -        "they've been "
           END-DISPLAY
           DISPLAY "          holding it for over seven years.  "
           END-DISPLAY
           DISPLAY " 3 5+     Babylon 5 certainly presents a fresh persp
      -        "ective in the"
           END-DISPLAY
           DISPLAY "          Sci-Fi universe. I watch it weekly."
           END-DISPLAY
           DISPLAY " 4 5      I've seen it, I am pretty indifferent to i
      -        "t."
           END-DISPLAY
           DISPLAY " 5 5-     This show is sub-par. The acting is wooden
      -        ", the special "
           END-DISPLAY
           DISPLAY "          effects are obviously poor quality. In gen
      -        "eral, it"
           END-DISPLAY
           DISPLAY "          seems like a very cheap Star Trek ripoff."
           END-DISPLAY
           DISPLAY " 6 5--    You call this Sci-Fi? That is such a load 
      -        "of crap! This"
           END-DISPLAY
           DISPLAY "          show is just a soap with bad actors, piss-
      -        "poor effects,"
           END-DISPLAY
           DISPLAY "          and lame storylines. Puh-leese."
           END-DISPLAY
           DISPLAY " 7 !5     I've never seen Babylon 5."
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Babylon 5 code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 07
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "5+++" TO WS-BAB
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "5++" TO WS-BAB
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "5+" TO WS-BAB
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "5" TO WS-BAB
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "5-" TO WS-BAB
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "5--" TO WS-BAB
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "!5" TO WS-BAB
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       25000-JEOP.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Jeopardy                                            
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 j+++   I dress like Art Fleming, practice Alex Tr
      -        "ebek's vocal"
           END-DISPLAY
           DISPLAY "          nuances, and make a pilgrimage to the Jeop
      -        "ardy studio"
           END-DISPLAY
           DISPLAY "          every six months to either take the contes
      -        "tant test or"
           END-DISPLAY
           DISPLAY "          to cheer from the audience. "
           END-DISPLAY
           DISPLAY " 2 j++    I watch Jeopardy regularly, and annoy othe
      -        "rs in the college"
           END-DISPLAY
           DISPLAY "          rec center by shouting out the answers."
           END-DISPLAY
           DISPLAY " 3 j+     I watch Jeopardy regularly."
           END-DISPLAY
           DISPLAY " 4 j      Sure I watch it, but, hey, it's only a sho
      -        "w."
           END-DISPLAY
           DISPLAY " 5 j-     Jeopardy? That's show's for a bunch of no-
      -        "life eggheads. "
           END-DISPLAY
           DISPLAY " 6 j--    I annoy others in the college rec center b
      -        "y shouting out"
           END-DISPLAY
           DISPLAY "          the *wrong* answers." END-DISPLAY
           DISPLAY " 7 !j     I've never seen Jeopardy or don't watch it
      -        "."
           END-DISPLAY
           DISPLAY " 8 j$     I've won money on the show." END-DISPLAY
           DISPLAY " 9 jP     I've gotten the d*mn Lee Press-On Nails on
      -        " the show (or"
           END-DISPLAY
           DISPLAY "          some other lame-o consolation prize). "
           END-DISPLAY
           DISPLAY "10 jx     I don't watch Jeopardy because it's too ea
      -        "sy."
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Jeopardy code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 10
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "j+++" TO WS-JEOP
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "j++" TO WS-JEOP
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "j+" TO WS-JEOP
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "j" TO WS-JEOP
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "j-" TO WS-JEOP
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "j--" TO WS-JEOP
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "!j" TO WS-JEOP
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "j$" TO WS-JEOP
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "jP" TO WS-JEOP
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "jx" TO WS-JEOP
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       25100-ROLE.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Role Playing                                        
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 R+++   I've written and publish my own gaming mat
      -        "erials. "
           END-DISPLAY
           DISPLAY " 2 R++    There is no life outside the role of the d
      -        "ie. I know all"
           END-DISPLAY
           DISPLAY "          of piddly rules of (chosen game). _MY_ own
      -        " warped rules"
           END-DISPLAY
           DISPLAY "          scare the rest of the players."
           END-DISPLAY
           DISPLAY " 3 R+     I've got my weekly sessions set up and a c
      -        "haracter that"
           END-DISPLAY
           DISPLAY "          I know better than I know myself. "
           END-DISPLAY
           DISPLAY " 4 R      Role-Playing? That's just something to do
      -        " to kill a"
           END-DISPLAY
           DISPLAY "          Saturday afternoon." END-DISPLAY
           DISPLAY " 5 R-     Gosh, what an utter waste of time!"
           END-DISPLAY
           DISPLAY " 6 R--    Role-Players are instruments of pure evil"
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Jeopardy code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY < 07
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "R+++" TO WS-ROLE
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "R++" TO WS-ROLE
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "R+" TO WS-ROLE
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "R" TO WS-ROLE
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "R-" TO WS-ROLE
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "R--" TO WS-ROLE
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       25200-MAGIC.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "MAGIC: The Gathering                                
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 G++++  I am considered a Magic(tm) god. I have n
      -        "icknames for"
           END-DISPLAY
           DISPLAY "          every card and know just about every strat
      -        "egy there is."
           END-DISPLAY
           DISPLAY " 2 G+++   I have a Lord of the Pit, a Black Lotus an
      -        "d a Reverse"
           END-DISPLAY
           DISPLAY "          Damage. I play for hours every night."
           END-DISPLAY
           DISPLAY " 3 G++    I've spent almost $100 on cards. A good ch
      -        "unk of my"
           END-DISPLAY
           DISPLAY "          spare time goes into playing or constructi
      -        "ng decks and"
           END-DISPLAY
           DISPLAY "          keeping up my checklist." END-DISPLAY
           DISPLAY " 4 G+     Ok, ok, so I bought a few packs of cards.
      -        " Big deal."
           END-DISPLAY
           DISPLAY " 5 G      I play Magic, if I can borrow a deck. It's
      -        " an ok game."
           END-DISPLAY
           DISPLAY " 6 G-     I don't even play anymore. I just collect.
      -       " My cards fill "
           END-DISPLAY
           DISPLAY "          three shoeboxes." END-DISPLAY
           DISPLAY " 7 G--    I don't go to class/work anymore.  Sometim
      -        "es I don't sleep."
           END-DISPLAY
           DISPLAY " 8 G---   I have 3 Lords of the Pit, Armageddon, Wra
      -        "th of God,"
           END-DISPLAY
           DISPLAY "          and two Reverse Damages. I also have all f
      -        "ive of the"
           END-DISPLAY
           DISPLAY "          Greater Legends Dragons. I can quote the e
      -        "xact wording"
           END-DISPLAY
           DISPLAY "          and, in some cases, casting cost, of any c
      -        "ard on demand."
           END-DISPLAY
           DISPLAY "          I've memorized the PPG. I am a Magic munch
      -        "kin. "
           END-DISPLAY
           DISPLAY " 9 G----  Some friends and I are trying to get boxes
      -        " of booster"
           END-DISPLAY
           DISPLAY "          packs at cost so we can sell them at a pro
      -        "fit and buy more"
           END-DISPLAY
           DISPLAY "          cards at cost that we can sell for profit 
      -        "and buy more"
           END-DISPLAY
           DISPLAY "          cards at.... " END-DISPLAY
           DISPLAY "10 G?     What the hell _IS_ Magic?"
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your MAGIC code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 10
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "G++++" TO WS-MAGIC
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "G+++" TO WS-MAGIC
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "G++" TO WS-MAGIC
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "G+" TO WS-MAGIC
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "G" TO WS-MAGIC
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "G-" TO WS-MAGIC
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "G--" TO WS-MAGIC
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "G---" TO WS-MAGIC
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "G----" TO WS-MAGIC
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "G?" TO WS-MAGIC
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       25300-TV.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Television                                          
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 tv+++  There's nothing I can experience 'out ther
      -        "e' that I can't"
           END-DISPLAY
           DISPLAY "          see coming over my satellite dish. I wish 
      -        "there were"
           END-DISPLAY
           DISPLAY "          MORE channels. " END-DISPLAY
           DISPLAY " 2 tv++   I just leave the tv on, to make sure I don
      -        "'t miss anything."
           END-DISPLAY
           DISPLAY " 3 tv+    I watch some tv every day. "
           END-DISPLAY
           DISPLAY " 4 tv     I watch only the shows that are actually w
      -        "orth while."
           END-DISPLAY
           DISPLAY " 5 tv-    I watch tv for the news and 'special progr
      -        "amming.' "
           END-DISPLAY
           DISPLAY " 6 tv--   I turn my tv on during natural disasters."
           END-DISPLAY
           DISPLAY " 7 !tv    I do not own a television. " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Jeopardy code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 07
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "tv+++" TO WS-TV
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "tv++" TO WS-TV
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "tv+" TO WS-TV
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "tv" TO WS-TV
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "tv-" TO WS-TV
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "tv--" TO WS-TV
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "!tv" TO WS-TV
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       25400-BOOKS.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Books                                               
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 b+++   I consume a few books a week as part of a 
      -        "staple diet."
           END-DISPLAY
           DISPLAY " 2 b++    I find the time to get through at least on
      -        "e new book a month. "
           END-DISPLAY
           DISPLAY " 3 b+     I enjoy reading, but don't get the time ve
      -        "ry often.  "
           END-DISPLAY
           DISPLAY " 4 b      I read the newspaper and the occasional bo
      -        "ok. "
           END-DISPLAY
           DISPLAY " 5 b-     I read when there is no other way to get t
      -        "he information. "
           END-DISPLAY
           DISPLAY " 6 b--    I did not actually READ the geek code, "
           END-DISPLAY
           DISPLAY "          I just had someone tell me. "
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Books code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 06
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "b+++" TO WS-BOOKS
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "b++" TO WS-BOOKS
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "b+" TO WS-BOOKS
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "b" TO WS-BOOKS
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "b-" TO WS-BOOKS
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "b--" TO WS-BOOKS
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       25500-DOOM.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "DOOM!                                               
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 D+++   I crank out PWAD files daily, complete wit
      -        "h new monsters,"
           END-DISPLAY
           DISPLAY "          weaponry, sounds and maps. I'm a DOOM God.
      -        " I can solve the"
           END-DISPLAY
           DISPLAY "          original maps in nightmare mode with my ey
      -        "es closed. "
           END-DISPLAY
           DISPLAY " 2 D++    I've played the shareware version and boug
      -        "ht the real one"
           END-DISPLAY
           DISPLAY "          and I'm actually pretty good at the game. 
      -        "I occasionally "
           END-DISPLAY
           DISPLAY "          download PWAD files and play them too. "
           END-DISPLAY
           DISPLAY " 3 D+     It's a fun, action game that is a nice div
      -        "ersion on a "
           END-DISPLAY
           DISPLAY "          lazy afternoon." END-DISPLAY
           DISPLAY " 4 D      I've played the game and I'm pretty indiff
      -        "erent."
           END-DISPLAY
           DISPLAY " 5 D-     I've played the game and really didn't thi
      -        "nk it was"
           END-DISPLAY
           DISPLAY "          all that impressive." END-DISPLAY
           DISPLAY " 6 D--    It's an overly-violent game and pure crap"
           END-DISPLAY
           DISPLAY " 7 D---   I've seen better on my Atari 2600."
           END-DISPLAY
           DISPLAY " 8 !D     I've never played Doom!" END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Books code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 08
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "D+++" TO WS-DOOM
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "D++" TO WS-DOOM
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "D+" TO WS-DOOM
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "D" TO WS-DOOM
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "D-" TO WS-DOOM
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "D--" TO WS-DOOM
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "D---" TO WS-DOOM
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "!D" TO WS-DOOM
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       25600-BARNEY.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Barney                                              
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 B+++   I worship the ground He walks on. I wish t
      -        "o erect a"
           END-DISPLAY
           DISPLAY "          shrine for Him in my front yard. I feel a
      -        " need to sell"
           END-DISPLAY
           DISPLAY "          all my worldly belongings, shave my head,
      -        "and go to "
           END-DISPLAY
           DISPLAY "          airports where I will hand out Barney doll
      -        "s and spread"
           END-DISPLAY
           DISPLAY "          His message of universal love for everyone
      -        " regardless of"
           END-DISPLAY
           DISPLAY "          race, creed, color, sexual preference, or 
      -        "species. "
           END-DISPLAY
           DISPLAY " 2 B++    I don't miss an episode, except when I hav
      -        "e to work"
           END-DISPLAY
           DISPLAY "          or go in for a root canal. Barney loves me
      -        ". "
           END-DISPLAY
           DISPLAY " 3 B+     I like him. He has a nice, wholesome messa
      -        "ge. He's"
           END-DISPLAY
           DISPLAY "          good for the country. " END-DISPLAY
           DISPLAY " 4 B      Hey, the little tykes love him, they don't
      -       " go around "
           END-DISPLAY
           DISPLAY "          karate-chopping each other any more; what'
      -        "s the big deal?"
           END-DISPLAY
           DISPLAY " 5 B-     Barney is annoying." END-DISPLAY
           DISPLAY " 6 B--    Don't talk to me about him. I'm getting si
      -        "ck of his "
           END-DISPLAY
           DISPLAY "          smarmy message. He makes me ill."
           END-DISPLAY
           DISPLAY " 7 B---   He's sick. He's polluting our children's m
      -       "inds with this"
           END-DISPLAY
           DISPLAY "          love and tolerance crap."
           END-DISPLAY
           DISPLAY " 8 !B     Who's Barney?" END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Barney code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 08
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "B+++" TO WS-BARNEY
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "B++" TO WS-BARNEY
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "B+" TO WS-BARNEY
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "B" TO WS-BARNEY
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "B-" TO WS-BARNEY
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "B--" TO WS-BARNEY
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "B---" TO WS-BARNEY
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "!B" TO WS-BARNEY
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       25700-EDUC.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Education                                           
      -       "              age: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 e++++  Still pretty stupid, over qualified to wor
      -        "k any"
           END-DISPLAY
           DISPLAY "          job, went and got my Ph.D. "
           END-DISPLAY
           DISPLAY " 2 e+++   Had not learned enough to know better not 
      -        "to go back"
           END-DISPLAY
           DISPLAY "          and try for a master's degree."
           END-DISPLAY
           DISPLAY " 3 e++    Managed to finish my bachelors. " 
           END-DISPLAY
           DISPLAY " 4 e+     Started a degree, plan to finish it some d
      -        "ay.  "
           END-DISPLAY
           DISPLAY " 5 e      K-12, been on a college campus."
           END-DISPLAY
           DISPLAY " 6 e-     Got my bachelors, escaped alive, and am ma
      -        "king hoards"
           END-DISPLAY
           DISPLAY "          of money writing unmaintainable (except by
      -        " me) software. "
           END-DISPLAY
           DISPLAY " 7 e--    The company I work for was dumb enough to 
      -        "fund my way"
           END-DISPLAY
           DISPLAY "          through a masters degree, then started pay
      -        "ing me even more money."
           END-DISPLAY
           DISPLAY " 8 e---   Achieved a Ph.D, have devoted my life to i
      -        "nsignificant"
           END-DISPLAY
           DISPLAY "          research, which my employer pays dearly fo
      -        "r.  "
           END-DISPLAY
           DISPLAY " 9 !e     Flunked high school, learned life the hard
      -        " way."
           END-DISPLAY
           DISPLAY "10 e*     I learned everything there is to know abou
      -        "t life from"
           END-DISPLAY
           DISPLAY "          the 'Hitchhiker's Trilogy'. "
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Education code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 10
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "e++++" TO WS-EDUC
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "e+++" TO WS-EDUC
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "e++" TO WS-EDUC
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "e+" TO WS-EDUC
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "e" TO WS-EDUC
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "e-" TO WS-EDUC
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "e--" TO WS-EDUC
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "e---" TO WS-EDUC
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "!e" TO WS-EDUC
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "e*" TO WS-EDUC
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       25800-MUSIC.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Music                                               
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 u+++   I consider myself over-refined and grok th
      -        "at heavy-duty"
           END-DISPLAY
           DISPLAY "          elevator music." END-DISPLAY
           DISPLAY " 2 u++    I consider myself refined and enjoy classi
      -        "cal and"
           END-DISPLAY
           DISPLAY "          new-age selections." END-DISPLAY
           DISPLAY " 3 u+     I own a tape or CD collection (records als
      -        "o count,"
           END-DISPLAY
           DISPLAY "          but you would be admitting how old you rea
      -        "lly are)."
           END-DISPLAY
           DISPLAY " 4 u      I occasionally listen to the radio."
           END-DISPLAY
           DISPLAY " 5 u-     Just play it loud." END-DISPLAY
           DISPLAY " 6 u--    I play air-guitar better than anyone else.
      -    " "
           END-DISPLAY
           DISPLAY " 7 u---   LISTEN! I SAID TO PLAY IT LOUD!" 
           END-DISPLAY
           DISPLAY " 8 u*     I listen to music that no one else has eve
      -        "r heard of." 
           END-DISPLAY
           DISPLAY " 9 u**    I listen to so many types of music that I 
      -        "can't even"
           END-DISPLAY
           DISPLAY "          keep them straight." END-DISPLAY
           DISPLAY "10 -u     I like _both_ kinds of music: Country AND 
      -        "Western."
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Music code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 10
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "u+++" TO WS-MUSIC
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "u++" TO WS-MUSIC
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "u+" TO WS-MUSIC
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "u" TO WS-MUSIC
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "u-" TO WS-MUSIC
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "u--" TO WS-MUSIC
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "u---" TO WS-MUSIC
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "u*" TO WS-MUSIC
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "e**" TO WS-MUSIC
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "-u" TO WS-MUSIC
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       25900-HOUSE.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Housing                                             
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 h++    Living in a cave with 47 computers and an 
      -        "Internet feed,"
           END-DISPLAY
           DISPLAY "          located near a Dominoes pizza. See !d."
           END-DISPLAY
           DISPLAY " 2 h+     Living alone, get out once a week to buy f
      -        "ood, no more"
           END-DISPLAY
           DISPLAY "          than once a month to do laundry. All surfa
      -        "ces covered."
           END-DISPLAY
           DISPLAY " 3 h      Friends come over to visit every once in a
      -        " while to talk"
           END-DISPLAY
           DISPLAY "          about Geek things. There is a place for th
      -        "em to sit."
           END-DISPLAY
           DISPLAY " 4 h-     Living with one or more registered Geeks."
           END-DISPLAY
           DISPLAY " 5 h--    Living with one or more people who know no
      -        "thing about"
           END-DISPLAY
           DISPLAY "          being a Geek and refuse to watch 'Star Tre
      -        "k'. "
           END-DISPLAY
           DISPLAY " 6 h---   Married, with the potential for children. 
      -        " (persons living"
           END-DISPLAY
           DISPLAY "          with a fiance might as well label themselv
      -        "es h---,"
           END-DISPLAY
           DISPLAY "          you're as good as there already.)"
           END-DISPLAY
           DISPLAY " 7 h----  Married with children - Al Bundy can sympa
      -        "thize ."
           END-DISPLAY
           DISPLAY " 8 !h     I am stuck living with my parents!"
           END-DISPLAY
           DISPLAY " 9 h*     I'm not sure where I live anymore. This la
      -        "b/workplace"
           END-DISPLAY
           DISPLAY "          seems like home to me. " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Housing code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 09
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "h++" TO WS-HOUSE
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "h+" TO WS-HOUSE
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "h" TO WS-HOUSE
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "h-" TO WS-HOUSE
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "h--" TO WS-HOUSE
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "h---" TO WS-HOUSE
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "h----" TO WS-HOUSE
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "!h" TO WS-HOUSE
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "h*" TO WS-HOUSE
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       26000-FRIENDS.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Friends                                             
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 f++    I have so many friends, I make other peopl
      -       "e jealous."
           END-DISPLAY
           DISPLAY " 2 f+     I have quite a few really close friends. W
      -        "e get along great. They "
           END-DISPLAY
           DISPLAY "          are all other geeks, though."
           END-DISPLAY
           DISPLAY " 3 f      Yeah, I have friends. Who told you?"
           END-DISPLAY
           DISPLAY " 4 f-     I have a few friends. They barely seem to 
      -        "speak to me anymore."
           END-DISPLAY
           DISPLAY " 5 f--    I've got about one friend left in the worl
      -        "d, who probably wants to "
           END-DISPLAY
           DISPLAY "          shoot me. " END-DISPLAY
           DISPLAY " 6 f---   I used to have friends, but I didn't like 
      -        "it ."
           END-DISPLAY
           DISPLAY " 7 f?     I *think* I have friends." END-DISPLAY
           DISPLAY " 8 f*     Everyone is my friend." END-DISPLAY
           DISPLAY " 9 !f     I have no friends. Get lost." 
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Friends code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF Ws-ENTRY <= 09
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF Ws-ENTRY = 01 THEN
                       MOVE "f++" TO WS-FRIENDS
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "f+" TO WS-FRIENDS
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "f" TO WS-FRIENDS
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "f-" TO WS-FRIENDS
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "f--" TO WS-FRIENDS
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "f---" TO WS-FRIENDS
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "f?" TO WS-FRIENDS
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "f*" TO WS-FRIENDS
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "!f" TO WS-FRIENDS
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       26100-REL.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Relationships                                       
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 r+++   Found someone, dated, and am now married."
           END-DISPLAY
           DISPLAY " 2 r++    I've dated my current SO for a long time."
           END-DISPLAY
           DISPLAY " 3 r+     I bounce from one relationship to another,
      -        " but I have quite a few. "
           END-DISPLAY
           DISPLAY " 4 r      I date periodically." END-DISPLAY
           DISPLAY " 5 r-     I have difficulty maintaining a relationsh
      -        "ip."
           END-DISPLAY
           DISPLAY " 6 r--    Most people aren't interested in dating me
      -        "." 
           END-DISPLAY
           DISPLAY " 7 r---   I'm beginning to think I'm a leper or some
      -        "thing, the way"
           END-DISPLAY
           DISPLAY "          people avoid me like the plague."
           END-DISPLAY
           DISPLAY " 8 !r     I've never had a relationship."
           END-DISPLAY
           DISPLAY " 9 r*     signifying membership in the SBCA (Sour Ba
      -        "chelor(ette)'s"
           END-DISPLAY
           DISPLAY "          Club of America). The motto is 'Bitter, bu
      -        "t not Desperate'."
           END-DISPLAY
           DISPLAY "          First founded at Caltech. " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Relationships code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 09
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "r+++" TO WS-REL
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "r++" TO WS-REL
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "r+" TO WS-REL
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "r" TO WS-REL
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "r-" TO WS-REL
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "r--" TO WS-REL
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "r---" TO WS-REL
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "!r" TO WS-REL
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "r*" TO WS-REL
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       26200-NUT.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Nutrition                                           
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 n+++   I graze like a bunny - pass me a carrot!"
           END-DISPLAY
           DISPLAY " 2 n++    I like the fibers in food."
           END-DISPLAY
           DISPLAY " 3 n+     I like food - especially when it is health
      -        "y. "
           END-DISPLAY
           DISPLAY " 4 n-     Food? I just grab something from the shelv
      -        "es with meat in it. "
           END-DISPLAY
           DISPLAY " 5 n--    I eat only the cheap things - even with ar
      -        "tificial meat and"
           END-DISPLAY
           DISPLAY "          vegetables." END-DISPLAY
           DISPLAY " 6 n---   I eat meat - seen Jurassic Park?"
           END-DISPLAY
           DISPLAY " 7 n----  I _live_ on snacks and coke."
           END-DISPLAY
           DISPLAY " 8 !n     Eh what? never mind the menu, give me some
      -        "thing to eat!"
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Nutrition code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 08
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "n+++" TO WS-NUT
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "n++" TO WS-NUT
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "n+" TO WS-NUT
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "n-" TO WS-NUT
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "n--" TO WS-NUT
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "n---" TO WS-NUT
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "n----" TO WS-NUT
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "!n" TO WS-NUT
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       26300-GENDER.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Gender                                              
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 x   I am female." END-DISPLAY
           DISPLAY " 2 y   I am male." END-DISPLAY
           DISPLAY " 3 z   Its none of your business what sex I am."
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Gender code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 03
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "x" TO WS-SEX-GENDER
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "y" TO WS-SEX-GENDER
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "z" TO WS-SEX-GENDER
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       26350-SEX.
           ADD 1 TO WS-PAGE-CNT

           DISPLAY "Sexuality                                           
      -       "             Page: " WS-PAGE-CNT " of " WS-TOT-PAGE
           END-DISPLAY
           DISPLAY "====================================================
      -"==========================="
           END-DISPLAY
           DISPLAY " 1 " WS-SEX-GENDER "++++  I have a few little rug ra
      -        "ts to prove I've"
           END-DISPLAY
           DISPLAY "          been there. Besides, with kids around, who
      -        " has time for sex? "
           END-DISPLAY
           DISPLAY " 2 " WS-SEX-GENDER "+++   I'm married, so I can get 
      -        "it"
           END-DISPLAY
           DISPLAY "          (theoretically) whenever I want."
           END-DISPLAY
           DISPLAY " 3 " WS-SEX-GENDER "++    I was once referred to as 
      -        "'easy'."
           END-DISPLAY
           DISPLAY "          I have no idea where that might have come
      -        " from though."
           END-DISPLAY
           DISPLAY " 4 " WS-SEX-GENDER "+     I've had real, live sex."
           DISPLAY " 5 " WS-SEX-GENDER "-     I prefer computer sex to r
      -        "eal sex."
           END-DISPLAY
           DISPLAY " 6 " WS-SEX-GENDER "--    I was once referred to a
      -        "s a 'cyberslut',"
           END-DISPLAY
           DISPLAY "          but I have no idea where that might have c
      -        "ome from."
           END-DISPLAY
           DISPLAY " 7 " WS-SEX-GENDER "*     I'm a pervert."
           END-DISPLAY
           DISPLAY " 8 " WS-SEX-GENDER "**    I've been known to make pe
      -        "rverts look like angels."
           END-DISPLAY
           DISPLAY " 9 !" WS-SEX-GENDER "     Sex? What's that? I've had
      -        " no sexual experiences."
           END-DISPLAY
           DISPLAY "10 " WS-SEX-GENDER "?     It's none of your business
      -        " what my sex life is like."
           END-DISPLAY
           DISPLAY "11 +" WS-SEX-GENDER "?    Sex? What's that? No exper
      -        "ience, willing to learn! "
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY "Enter your Sexuality code "
               "number here [0 to quit]: " WITH NO ADVANCING
           END-DISPLAY

           ACCEPT WS-ENTRY END-ACCEPT

           IF WS-ENTRY (2:1) = SPACE
               MOVE WS-ENTRY (1:1) TO WS-ENTRY (2:1)
               MOVE 0              TO WS-ENTRY (1:1)   
           END-IF

           IF WS-ENTRY NOT NUMERIC
               CONTINUE 
           ELSE
               IF WS-ENTRY <= 11
                   SET WS-VALID-FLAG TO "Y"
                   IF WS-ENTRY = 00 THEN
                       PERFORM 92000-ABEND
                   ELSE IF WS-ENTRY = 01 THEN
                       MOVE "++++" TO WS-SEX
                   ELSE IF WS-ENTRY = 02 THEN
                       MOVE "+++" TO WS-SEX
                   ELSE IF WS-ENTRY = 03 THEN
                       MOVE "++" TO WS-SEX
                   ELSE IF WS-ENTRY = 04 THEN
                       MOVE "+" TO WS-SEX
                   ELSE IF WS-ENTRY = 05 THEN
                       MOVE "-" TO WS-SEX
                   ELSE IF WS-ENTRY = 06 THEN
                       MOVE "--" TO WS-SEX
                   ELSE IF WS-ENTRY = 07 THEN
                       MOVE "*" TO WS-SEX
                   ELSE IF WS-ENTRY = 08 THEN
                       MOVE "**" TO WS-SEX
                   ELSE IF WS-ENTRY = 09 THEN
                       MOVE "!" TO WS-SEX-PRE
                       MOVE " " TO WS-SEX
                   ELSE IF WS-ENTRY = 10 THEN
                       MOVE "?" TO WS-SEX
                   ELSE IF WS-ENTRY = 11 THEN
                       MOVE "+" TO WS-SEX-PRE
                       MOVE "?" TO WS-SEX
                   END-IF
               END-IF
           END-IF.

           PERFORM 90000-CLEAR-SCREEN.

       26500-PRINT.
           MOVE  "-----BEGIN GEEK CODE BLOCK-----" TO   WS-HEAD
           WRITE GEEK-OUTPUT-REC                   FROM WS-HEAD 
           END-WRITE
           ADD 1 TO WS-REC-CNT
           WRITE GEEK-OUTPUT-REC                   FROM WS-VER
           END-WRITE
           ADD 1 TO WS-REC-CNT

           STRING WS-TYPE           DELIMITED BY SPACE  
                  SPACE             DELIMITED BY SIZE
                  WS-DRESS          DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-HAIR           DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-SHAPE          DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-GLASSES        DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-PENS           DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-AUTO           DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-AGE            DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-WEIRD          DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-VERBAGE        DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-COMP           DELIMITED BY SPACE
           INTO WS-PRINT-LINE1
           END-STRING 

           WRITE GEEK-OUTPUT-REC FROM WS-PRINT-LINE1
           END-WRITE
           ADD 1 TO WS-REC-CNT

           STRING WS-UNIX           DELIMITED BY SPACE  
                  SPACE             DELIMITED BY SIZE
                  WS-PERL           DELIMITED BY SPACE  
                  SPACE             DELIMITED BY SIZE
                  WS-LINUX          DELIMITED BY SPACE  
                  SPACE             DELIMITED BY SIZE
                  WS-386BSD         DELIMITED BY SPACE  
                  SPACE             DELIMITED BY SIZE
                  WS-NEWS           DELIMITED BY SPACE  
                  SPACE             DELIMITED BY SIZE
                  WS-WEB            DELIMITED BY SPACE  
                  SPACE             DELIMITED BY SIZE
                  WS-MAC            DELIMITED BY SPACE  
                  SPACE             DELIMITED BY SIZE
                  WS-VMS            DELIMITED BY SPACE  
                  SPACE             DELIMITED BY SIZE
                  WS-POL            DELIMITED BY SPACE  
                  SPACE             DELIMITED BY SIZE
                  WS-CP             DELIMITED BY SPACE  
           INTO WS-PRINT-LINE2
           END-STRING 

           WRITE GEEK-OUTPUT-REC FROM WS-PRINT-LINE2
           END-WRITE
           ADD 1 TO WS-REC-CNT

           STRING WS-TREK           DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-BAB            DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-JEOP           DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-ROLE           DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-MAGIC          DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-EMACS          DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-KIBO           DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-MS             DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-TV             DELIMITED BY SPACE
           INTO WS-PRINT-LINE3
           END-STRING 

           WRITE GEEK-OUTPUT-REC FROM WS-PRINT-LINE3
           END-WRITE
           ADD 1 TO WS-REC-CNT

           STRING WS-BOOKS          DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-DOOM           DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-BARNEY         DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-EDUC           DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-MUSIC          DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-HOUSE          DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-REL            DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-FRIENDS        DELIMITED BY SPACE
                  SPACE             DELIMITED BY SIZE
                  WS-NUT            DELIMITED BY SPACE  
                  SPACE             DELIMITED BY SIZE
                  WS-CODE-SEX       DELIMITED BY SIZE  
           INTO WS-PRINT-LINE4
           END-STRING 

           WRITE GEEK-OUTPUT-REC FROM WS-PRINT-LINE4
           END-WRITE
           ADD 1 TO WS-REC-CNT

           MOVE  "-----END GEEK CODE BLOCK-----" TO   WS-END
           WRITE GEEK-OUTPUT-REC                 FROM WS-END
           END-WRITE
           ADD 1 TO WS-REC-CNT

           DISPLAY "-----BEGIN GEEK CODE BLOCK-----" END-DISPLAY
           DISPLAY "Version: 2.1" END-DISPLAY
           DISPLAY WS-PRINT-LINE1 END-DISPLAY
           DISPLAY WS-PRINT-LINE2 END-DISPLAY
           DISPLAY WS-PRINT-LINE3 END-DISPLAY
           DISPLAY WS-PRINT-LINE4 END-DISPLAY
           DISPLAY "------END GEEK CODE BLOCK------" END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY WS-REC-CNT " records written to 'geekcode.sig'"
           END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY.

       30000-CLEANUP.
           CLOSE GEEK-SIG
           PERFORM 94000-TERMINATE.

       90000-CLEAR-SCREEN.
           MOVE "clear" TO WS-COMMAND
           CALL "system" USING WS-COMMAND GIVING WS-RETURN-SYS-CODE
               END-CALL
           DISPLAY " " END-DISPLAY
           DISPLAY " " END-DISPLAY.

       91000-PRINT-HEADING.
           DISPLAY "Geek Code Generator v0.1 - Generates your geek code"
           END-DISPLAY
           DISPLAY "Copyright (C) 2010 Randy LeJeune" END-DISPLAY
           DISPLAY " " END-DISPLAY.

       92000-ABEND.
           PERFORM 30000-CLEANUP.

       93000-PARSE-CMDLN.
           PERFORM 90000-CLEAR-SCREEN
           IF WS-CL-ARGS = "-h" OR "-H" OR "--help" OR "/h" OR "/?"
               DISPLAY "Usage: geekcode2.1 [options] file..."
               END-DISPLAY
               DISPLAY " " END-DISPLAY
               DISPLAY "Options: " END-DISPLAY
               DISPLAY "    -h, --help            Display this message"
               END-DISPLAY
               DISPLAY "    -v, --version         Display version"
               END-DISPLAY
               PERFORM 94000-TERMINATE
           ELSE IF WS-CL-ARGS = "-v" OR "-V" OR "--version"
               DISPLAY "geekcode generator 0.1" END-DISPLAY
               DISPLAY "Copyright (C) 2010 Randy LeJeune"
               END-DISPLAY
               DISPLAY "License GPLv3+: GNU GPL version 3 or later - <ht
      -            "tp://gnu.org/licenses/gpl.html>."
               END-DISPLAY
               DISPLAY "This is free software: you are free to change an
      -            "d redistribute it."
               END-DISPLAY
               DISPLAY "here is NO WARRANTY, to the extent permitted by
      -            "law."
               END-DISPLAY
               DISPLAY " " END-DISPLAY
               DISPLAY " " END-DISPLAY
               DISPLAY "Written by Randy LeJeune." END-DISPLAY
               PERFORM 94000-TERMINATE
           ELSE IF WS-CL-ARGS = SPACES
               CONTINUE
           ELSE
               DISPLAY "geekcode: invalid option.'" END-DISPLAY
               DISPLAY "Try `geekcode -h' for more information."
               END-DISPLAY
               PERFORM 94000-TERMINATE
           END-IF.

       94000-TERMINATE.
           GOBACK.
