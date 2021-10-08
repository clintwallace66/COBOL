       IDENTIFICATION DIVISION.
       PROGRAM-ID. PR2FA21.
       AUTHOR. JOHN WALLACE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-PC.
       OBJECT-COMPUTER.  IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BENNETT-FILE
               ASSIGN TO 'PR2FA21-AL.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT BENNETT-REPORT-FILE
               ASSIGN TO PRINTER 'PR2FA21-Print'.


       DATA DIVISION.
       FILE SECTION.

       FD  BENNETT-FILE
           RECORD CONTAINS 85 CHARACTERS.

       01  BENNETT-RECORD.
           05  BR-STORE-ID              PIC A(4).
           05  BR-EMPLOYEE-ID           PIC X(5).
           05  BR-EMPLOYEE-POS          PIC A(2).
           05  BR-EMPLOYEE-L            PIC X(10).
           05  BR-EMPLOYEE-F            PIC X(10).
           05  BR-EMPLOYEE-M            PIC X(3).
           05  BR-HIRE-DATE             PIC 9(8).
           05  BR-EMPLOYEE-STATUS       PIC A(1).
           05  BR-SEP-DATE              PIC 9(8).
           05  BR-START-SAL             PIC 9(8).
           05  BR-PAY-INC               PIC 9(8).
           05  BR-CURR-SAL              PIC 9(6)v99.
           05  BR-NUM-DEP               PIC 9(2).
           05  BR-HEAL-PLAN             PIC A(1).
           05  BR-HEAL-COST             PIC 9(3).
           05  BR-DEN-PLAN              PIC A(1).
           05  BR-DEN-COST              PIC 9(3).

       FD  BENNETT-REPORT-FILE
           RECORD CONTAINS 85 CHARACTERS.

       01  REPORT-RECORD                PIC X(85).

       WORKING-STORAGE SECTION.
       01  BLANK-LINES                  PIC X(132)    VALUE SPACES.

       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                 PIC X         VALUE ' '.
               88  NO-MORE-DATA                       VALUE 'N'.
           05  FIRST-RECORD             PIC X         VALUE 'Y'.

       01  HOLD-FIELD.
           05  STORE-HOLD               PIC X(4)      VALUE SPACES.

       01  WS-CURRENT-DATE.
           05  WS-YEAR                  PIC 9999.
           05  WS-MONTH                 PIC 99.
           05  WS-DAY                   PIC 99.

       01  TOTAL-FIELDS.
           05  TF-SAL-INC               PIC 9(6)V99.
           05  TF-HEL-INC               PIC 9(4)V99.
           05  TF-DEN-INC               PIC 9(4)V99.
           05  TF-SALARY-TOTAL          PIC 9(7)V99.
           05  TF-HEALTH-TOTAL          PIC 9(5)V99.
           05  TF-DENTAL-TOTAL          PIC 9(5)V99.
           05  TF-GT-SAL                PIC 9(8)V99.
           05  TF-GT-HEL                PIC 9(8)V99.
           05  TF-GT-DEN                PIC 9(8)V99.
      **************        OUTPUT AREA        ********************

       01  REPORT-TITLE-ONE.
           05  RT1-DATE.
               10  H1-MONTH             PIC 99.
               10                       PIC X         VALUE '/'.
               10  H1-DAY               PIC 99.
               10                       PIC X         VALUE '/'.
               10  H1-YEAR              PIC 9999.
           05                           PIC X(25)     VALUE SPACES.
           05                           PIC X(33)     VALUE
           'BENNETT SHOES'.
           05                           PIC X(11)     VALUE
           'JCW'.

       01  REPORT-TITLE-TWO.
           05                           PIC X(29)     VALUE SPACES.
           05                           PIC X(50)     VALUE
           'ALABAMA EMPLOYEE REPORT'.

       01  REPORT-TITLE-THREE.
           05                           PIC X(2)      VALUE SPACES.
           05                           PIC X(7)      VALUE
           'STORE:'.
           05 STORE-NAME                PIC X(12).

       01  REPORT-TITLE-FOUR.
           05                           PIC X(3)      VALUE SPACES.
           05                           PIC X(10)     VALUE 'EMP'.
           05                           PIC X(12)     VALUE 'EMP'.
           05                           PIC X(11)     VALUE 'EMP'.
           05                           PIC X(13)     VALUE 'INCREASED'.
           05                           PIC X(13)     VALUE 'INCREASED'.
           05                           PIC X(17)     VALUE 'INCREASED'.

       01  REPORT-TITLE-FIVE.
           05                           PIC X(3)      VALUE SPACES.
           05                           PIC X(10)     VALUE 'ID'.
           05                           PIC X(9)      VALUE 'POS'.
           05                           PIC X(15)     VALUE 'LAST NAME'.
           05                           PIC X(13)     VALUE 'SALARY'.
           05                           PIC X(13)     VALUE 'HEALTH'.
           05                           PIC X(16)     VALUE 'DENTAL'.


       01  DETAIL-LINE.
           05                           PIC X(2)      VALUE SPACES.
           05  DL-EMPLOYEE-ID           PIC X(5).
           05                           PIC X(2)      VALUE SPACES.
           05  DL-EMPLOYEE-POS          PIC A(10).
           05                           PIC X(3)      VALUE SPACES.
           05  DL-EMPLOYEE-L            PIC X(13).
           05                           PIC X(1)      VALUE '$'.
           05  DL-INC-SAL               PIC ZZZ,ZZ9.99.
           05                           PIC X(4)      VALUE SPACES.
           05                           PIC X(1)      VALUE '$'.
           05  DL-INC-HEALTH            PIC Z,ZZ9.99.
           05                           PIC X(4)      VALUE SPACES.
           05                           PIC X(1)      VALUE '$'.
           05  DL-INC-DENTAL            PIC Z,ZZ9.99.
           05                           PIC X(7)      VALUE SPACES.

       01  TOTAL-LINE.
           05                           PIC X(19)     VALUE SPACES.
           05                           PIC X(14)     VALUE
           'STORE TOTALS:'.
           05                           PIC X(1)      VALUE '$'.
           05  TL-SALARY-TOTAL          PIC Z,ZZZ,ZZ9.99.
           05                           PIC X(3)      VALUE SPACES.
           05                           PIC X(1)      VALUE '$'.
           05  TL-HEALTH-TOTAL          PIC ZZ,ZZ9.99.
           05                           PIC X(3)      VALUE SPACES.
           05                           PIC X(1)      VALUE '$'.
           05  TL-DENTAL-TOTAL          PIC ZZ,ZZ9.99.
           05                           PIC X(7)      VALUE SPACES.

       01  GRAND-TOTAL-LINE.
           05                           PIC X(18)     VALUE SPACES.
           05                           PIC X(14)     VALUE
           'GRAND TOTALS:'.
           05                           PIC X(1)      VALUE '$'.
           05  GTL-SALARY-TOTALS        PIC ZZ,ZZZ,ZZ9.99.
           05                           PIC X(2)      VALUE SPACES.
           05                           PIC X(1)      VALUE '$'.
           05  GTL-HEALTH-TOTALS        PIC ZZZ,ZZ9.99.
           05                           PIC X(2)      VALUE SPACES.
           05                           PIC X(1)      VALUE '$'.
           05  GTL-DENTAL-TOTALS        PIC ZZZ,ZZ9.99.
           05                           PIC X(7)      VALUE SPACES.


       PROCEDURE DIVISION.

       10-CONTROL-MODULE.

           PERFORM 15-OPEN-ROUTINE
           PERFORM 25-PROCESS-EMPLOYEE-ROUTINE
           PERFORM 50-EOF-ROUTINE

           .
       15-OPEN-ROUTINE.

           OPEN INPUT BENNETT-FILE
               OUTPUT BENNETT-REPORT-FILE

           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR

           PERFORM 20-HEADER-ROUTINE

           .

       20-HEADER-ROUTINE.

           WRITE REPORT-RECORD FROM REPORT-TITLE-ONE
               AFTER ADVANCING 1 LINE

           WRITE REPORT-RECORD FROM REPORT-TITLE-TWO
               AFTER ADVANCING 2 LINE

           PERFORM 35-STORE-NAME

           WRITE REPORT-RECORD FROM REPORT-TITLE-FOUR
               AFTER ADVANCING 2 LINE

           WRITE REPORT-RECORD FROM REPORT-TITLE-FIVE
               AFTER ADVANCING 1 LINE

           WRITE REPORT-RECORD FROM BLANK-LINES

           .
       25-PROCESS-EMPLOYEE-ROUTINE.

           PERFORM UNTIL NO-MORE-DATA
               READ BENNETT-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 30-BENNETT-SALARY-ROUTINE
               END-READ
           END-PERFORM

           .
       30-BENNETT-SALARY-ROUTINE.

           MOVE BR-EMPLOYEE-ID TO DL-EMPLOYEE-ID
           EVALUATE BR-EMPLOYEE-POS
               WHEN = 'SE'
                   MOVE 'SECURITY' to DL-EMPLOYEE-POS
               WHEN = 'SM'
                   MOVE 'MANAGER' to DL-EMPLOYEE-POS
               WHEN = 'SS'
                   MOVE 'SUPERVISOR' to DL-EMPLOYEE-POS
               WHEN = 'OW'
                   MOVE 'OFFICE' to DL-EMPLOYEE-POS
               WHEN = 'SA'
                   MOVE 'SALES' to DL-EMPLOYEE-POS
           END-EVALUATE
           MOVE BR-EMPLOYEE-L TO DL-EMPLOYEE-L
           EVALUATE TRUE

              WHEN FIRST-RECORD ='Y'
                   MOVE BR-STORE-ID TO STORE-HOLD
                   MOVE 'N' TO FIRST-RECORD

              WHEN BR-STORE-ID NOT = STORE-HOLD

                   PERFORM 45-STORE-BREAK

           END-EVALUATE

           COMPUTE TF-SAL-INC ROUNDED = ((BR-CURR-SAL * 5) / 100) +
           BR-CURR-SAL

           MOVE TF-SAL-INC TO DL-INC-SAL
           ADD TF-SAL-INC TO TF-SALARY-TOTAL
           MOVE 0 TO TF-SAL-INC

           COMPUTE TF-HEL-INC = (BR-HEAL-COST * 2.5) / 100 +
           BR-HEAL-COST

           MOVE TF-HEL-INC TO DL-INC-HEALTH
           ADD TF-HEL-INC TO TF-HEALTH-TOTAL
           MOVE 0 TO TF-HEL-INC

           COMPUTE TF-DEN-INC = (BR-DEN-COST * 1.5) / 100 +
           BR-DEN-COST

           MOVE TF-DEN-INC TO DL-INC-DENTAL
           ADD TF-DEN-INC TO TF-DENTAL-TOTAL
           MOVE 0 TO TF-DEN-INC
           WRITE REPORT-RECORD FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE

           .
       35-STORE-NAME.

           IF BR-STORE-ID = 'HUNT'
             MOVE "HUNTSVILLE" TO STORE-NAME

           ELSE
             MOVE "BIRMINGHAM" TO STORE-NAME
           END-IF


           WRITE REPORT-RECORD FROM REPORT-TITLE-THREE
               AFTER ADVANCING 2 LINE

           .
       45-STORE-BREAK.

           MOVE BR-STORE-ID TO STORE-HOLD
           MOVE TF-SALARY-TOTAL TO TL-SALARY-TOTAL
           MOVE TF-HEALTH-TOTAL TO TL-HEALTH-TOTAL
           MOVE TF-DENTAL-TOTAL TO TL-DENTAL-TOTAL

           WRITE REPORT-RECORD FROM TOTAL-LINE
               AFTER ADVANCING 2 LINES
           WRITE REPORT-RECORD FROM BLANK-LINES

           PERFORM 35-STORE-NAME.

           WRITE REPORT-RECORD FROM BLANK-LINES

           ADD TF-SALARY-TOTAL TO TF-GT-SAL
           ADD TF-HEALTH-TOTAL TO TF-GT-HEL
           ADD TF-DENTAL-TOTAL TO TF-GT-DEN

           MOVE 0 TO TF-SALARY-TOTAL
           MOVE 0 TO TF-HEALTH-TOTAL
           MOVE 0 TO TF-DENTAL-TOTAL

           .
       50-EOF-ROUTINE.
           ADD TF-SALARY-TOTAL TO TF-GT-SAL
           ADD TF-HEALTH-TOTAL TO TF-GT-HEL
           ADD TF-DENTAL-TOTAL TO TF-GT-DEN
           MOVE TF-GT-SAL TO GTL-SALARY-TOTALS
           MOVE TF-GT-HEL TO GTL-HEALTH-TOTALS
           MOVE TF-GT-DEN TO GTL-DENTAL-TOTALS
           MOVE TF-SALARY-TOTAL TO TL-SALARY-TOTAL
           MOVE TF-HEALTH-TOTAL TO TL-HEALTH-TOTAL
           MOVE TF-DENTAL-TOTAL TO TL-DENTAL-TOTAL
           WRITE REPORT-RECORD FROM TOTAL-LINE
               AFTER ADVANCING 3 LINES

           WRITE REPORT-RECORD FROM GRAND-TOTAL-LINE
               AFTER ADVANCING 3 LINES

           CLOSE BENNETT-FILE
               BENNETT-REPORT-FILE
           STOP RUN
           .
