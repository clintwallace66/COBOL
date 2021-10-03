       IDENTIFICATION DIVISION.
       PROGRAM-ID. PR1FA21.
       AUTHOR. JOHN WALLACE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-PC.
       OBJECT-COMPUTER.  IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KNOX-FILE
               ASSIGN TO 'PR1FA21-Knox.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT KNOX-REPORT-FILE
               ASSIGN TO PRINTER 'PR1FA21-Print'.


       DATA DIVISION.
       FILE SECTION.

       FD  KNOX-FILE
           RECORD CONTAINS 75 CHARACTERS.

       01  KNOX-RECORD.
           05  KR-STORE-ID            PIC A(4).
           05  KR-EMPLOYEE-ID         PIC X(5).
           05  KR-EMPLOYEE-POS        PIC A(2).
           05  KR-EMPLOYEE-L          PIC X(10).
           05  KR-EMPLOYEE-F          PIC X(10).
           05  KR-EMPLOYEE-M          PIC X(3).
           05  KR-HIRE-DATE           PIC 9(8).
           05  KR-EMPLOYEE-STATUS     PIC A(1).
           05  KR-SEP-DATE            PIC 9(8).
           05  KR-START-SAL           PIC 9(8).
           05  KR-PAY-INC             PIC 9(8).
           05  KR-CURR-SAL            PIC 9(6).

       FD  KNOX-REPORT-FILE
           RECORD CONTAINS 75 CHARACTERS.

       01  REPORT-RECORD               PIC X(75).

       WORKING-STORAGE SECTION.
       01  BLANK-LINES             PIC X(132)      VALUE SPACES.

       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                PIC X         VALUE ' '.
               88  NO-MORE-DATA                      VALUE 'N'.

       01  TOTAL-FIELDS.

           05  TF-SALARY-TOTAL         PIC 9(9).


      **************        OUTPUT AREA        ********************

       01  REPORT-TITLE-ONE.
           05  RT1-DATE                 PIC 9999/99/99.
           05                           PIC X(25)     VALUE SPACES.
           05                           PIC X(32)    VALUE
           'BENNETT SHOES'.
           05                           PIC X(7)     VALUE
           'JCW'.

       01  REPORT-TITLE-TWO.
           05                           PIC X(34)      VALUE SPACES.
           05                          PIC X(40)      VALUE
           'EMPLOYEE REPORT'.

       01  REPORT-TITLE-THREE.
           05                          PIC X(35)      VALUE SPACES.
           05                          PIC X(39)      VALUE
           'KNOXVILLE, TN'.

       01  REPORT-TITLE-FOUR.
           05                          PIC X(3)      VALUE SPACES.
           05                           PIC X(7)      VALUE 'EMP'.
           05                           PIC X(9)      VALUE 'EMP'.
           05                            PIC X(12)     VALUE 'EMP'.
           05                           PIC X(11)     VALUE 'EMP'.
           05                            PIC X(9)      VALUE 'EMP'.
           05                           PIC X(12)     VALuE 'LAST'.
           05                           PIC X(11)     VALUE 'CURRENT'.

       01  REPORT-TITLE-FIVE.
           05                          PIC X(3)      VALUE SPACES.
           05                           PIC X(7)      VALUE 'ID'.
           05                           PIC X(5)      VALUE 'POS'.
           05                           PIC X(13)     VALUE'FIRST NAME'.
           05                           PIC X(12)     VALUE 'LAST NAME'.
           05                           PIC X(9)      VALUE 'STATUS'.
           05                           PIC X(14)     VALUE 'INCREASE'.
           05                           PIC X(11)     VALUE 'SALARY'.

       01  DETAIL-LINE.
           05                          PIC X(2)      VALUE SPACES.
           05  DL-EMPLOYEE-ID          PIC X(5).
           05                          PIC X(3)      VALUE SPACES.
           05  DL-EMPLOYEE-POS         PIC A(2).
           05                          PIC X(3)      VALUE SPACES.
           05  DL-EMPLOYEE-F           PIC X(10).
           05                          PIC X(3)      VALUE SPACES.
           05  DL-EMPLOYEE-L           PIC X(10).
           05                          PIC X(4)      VALUE SPACES.
           05  DL-EMPLOYEE-STATUS      PIC A(1).
           05                          PIC X(5)      VALUE SPACES.
           05  DL-PAY-INC              PIC 99/99/9999.
           05                          PIC X(3)      VALUE SPACES.
           05                           PIC X(1)      VALUE '$'.
           05  DL-CURR-SAL             PIC 999,999.99.
           05                           PIC X(2)         VALUE SPACES.

       01  TOTAL-LINE.
           05                          PIC X(44)     VALUE SPACES.
           05                             PIC X(13)  VALUE
           'SALARY TOTAL:'.
           05                          PIC X(2)      VALUE SPACES.
           05                           PIC X(1)      VALUE '$'.
           05  TL-SALARY-TOTAL         PIC 9,999,999.99.
      /
       PROCEDURE DIVISION.
      *                                Y3I
       10-CONTROL-MODULE.

           PERFORM 15-OPEN-ROUTINE
           PERFORM 25-PROCESS-EMPLOYEE-ROUTINE

           PERFORM 40-EOF-ROUTINE

           .
       15-OPEN-ROUTINE.

           OPEN INPUT KNOX-FILE
               OUTPUT KNOX-REPORT-FILE
           ACCEPT RT1-DATE FROM DATE YYYYMMDD
           PERFORM 20-HEADER-ROUTINE
           .

       20-HEADER-ROUTINE.

           WRITE REPORT-RECORD FROM REPORT-TITLE-ONE
               AFTER ADVANCING 1 LINE

           WRITE REPORT-RECORD FROM REPORT-TITLE-TWO
               AFTER ADVANCING 2 LINE

           WRITE REPORT-RECORD FROM REPORT-TITLE-THREE
               AFTER ADVANCING 2 LINE

           WRITE REPORT-RECORD FROM REPORT-TITLE-FOUR
               AFTER ADVANCING 2 LINE

           WRITE REPORT-RECORD FROM REPORT-TITLE-FIVE
               AFTER ADVANCING 1 LINE

           WRITE REPORT-RECORD FROM BLANK-LINES


           .
       25-PROCESS-EMPLOYEE-ROUTINE.

           PERFORM UNTIL NO-MORE-DATA
               READ KNOX-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 30-KNOX-SALARY-ROUTINE
               END-READ
           END-PERFORM
           .
       30-KNOX-SALARY-ROUTINE.


           MOVE KR-EMPLOYEE-ID TO DL-EMPLOYEE-ID
           MOVE KR-EMPLOYEE-POS TO DL-EMPLOYEE-POS
           MOVE KR-EMPLOYEE-F TO DL-EMPLOYEE-F
           MOVE KR-EMPLOYEE-L TO DL-EMPLOYEE-L
           MOVE KR-EMPLOYEE-STATUS TO DL-EMPLOYEE-STATUS
           MOVE KR-PAY-INC TO DL-PAY-INC
           MOVE KR-CURR-SAL TO DL-CURR-SAL
           ADD KR-CURR-SAL TO TF-SALARY-TOTAL
           WRITE REPORT-RECORD FROM DETAIL-LINE
               AFTER ADVANCING 1 LINES

           .

       40-EOF-ROUTINE.

           MOVE TF-SALARY-TOTAL TO TL-SALARY-TOTAL
           WRITE REPORT-RECORD FROM TOTAL-LINE
               AFTER ADVANCING 3 LINES

           CLOSE KNOX-FILE
               KNOX-REPORT-FILE
           STOP RUN
           .
