       IDENTIFICATION DIVISION.
       PROGRAM-ID. PR3FA21.
       AUTHOR. JOHN WALLACE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-PC.
       OBJECT-COMPUTER.  IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *    INPUT FILE
           SELECT BENNETT-FILE
               ASSIGN TO 'PR3FA21-MESS.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

      *    BIRMINGHAM FILES
           SELECT BHAM-FILE
               ASSIGN TO 'EMP-FILE-B100.TXT'.

           SELECT BHAM-INVENTORY-FILE
               ASSIGN TO 'NEW-INV-FILE-B100.TXT'.

      *    HUNTSVILLE FILES
           SELECT HUNT-FILE
               ASSIGN TO 'EMP-FILE-B200.TXT'.
           SELECT HUNT-INVENTORY-FILE
               ASSIGN TO 'NEW-INV-FILE-B200.TXT'.

      *    ANNISTON FILES
           SELECT ANNI-FILE
               ASSIGN TO 'EMP-FILE-B300.TXT'.
           SELECT ANNI-INVENTORY-FILE
               ASSIGN TO 'NEW-INV-FILE-B300.TXT'.

      *    MONTGOMERY FILES
           SELECT MONT-FILE
               ASSIGN TO 'EMP-FILE-B400.TXT'.
           SELECT MONT-INVENTORY-FILE
               ASSIGN TO 'NEW-INV-FILE-B400.TXT'.

      *    ERROR FILE
           SELECT ERROR-FILE
               ASSIGN TO 'ERR-WH-ID.TXT'.


       DATA DIVISION.
       FILE SECTION.

       FD  BENNETT-FILE
           RECORD CONTAINS 224 CHARACTERS.

       01  BENNETT-RECORD.
           05  BR-STORE-ID              PIC X(4).
           05  BR-EMPLOYEE-ID           PIC X(5).
           05  BR-EMPLOYEE-POS          PIC A(2).
           05  BR-EMPLOYEE-L            PIC X(10).
           05  BR-EMPLOYEE-F            PIC X(10).
           05  BR-EMPLOYEE-M            PIC X(3).
           05  BR-HIRE-DATE             PIC 9(8).
           05  FILLER                   PIC A(25).
           05  BR-CURR-SAL              PIC 9(6)v99.
           05  BR-NUM-DEP               PIC 9(2).
           05  BR-HEAL-PLAN             PIC A(1).
           05  BR-HEAL-COST             PIC 9(3).
           05  FILLER                   PIC X(4).
           05  BR-VENDOR-ID             PIC A(1).
           05  BR-CANDY-ID              PIC X(3).
           05  BR-CANDY-DATA OCCURS 5 TIMES.
               10  BR-CANDY-NAME        PIC X(15).
               10  BR-BOX-SIZE          PIC A.
               10  BR-CANDY-TYPE        PIC AA.
               10  BR-NUM-CASES         PIC S9(4).
               10  BR-PURCHASE-PRICE    PIC S999V99.



       FD  BHAM-FILE
           RECORD CONTAINS 85 CHARACTERS.
       01  BHAM-REC.
           05  BH-STORE-ID              PIC X(4).
           05  BH-EMPLOYEE-ID           PIC X(5).
           05  BH-EMPLOYEE-POS          PIC A(2).
           05  BH-EMPLOYEE-L            PIC X(10).
           05  BH-EMPLOYEE-F            PIC X(10).
           05  BH-EMPLOYEE-M            PIC X(3).
           05  BH-HIRE-DATE             PIC 9(8).
           05  FILLER                   PIC A(25).
           05  BH-CURR-SAL              PIC 9(6)v99.
           05  BH-NUM-DEP               PIC 9(2).
           05  BH-HEAL-PLAN             PIC A(1).
           05  BH-HEAL-COST             PIC 9(3).
           05  FILLER                   PIC X(4).
       FD  BHAM-INVENTORY-FILE
           RECORD CONTAINS 143 CHARACTERS.
       01  BHAM-INVENTORY-REC.
           05  BHM-STORE-ID             PIC X(4).
           05  BH-VENDOR-ID             PIC A(1).
           05  BH-CANDY-ID              PIC X(3).
           05  BH-CANDY-DATA OCCURS 5 TIMES.
               10  BH-CANDY-NAME        PIC X(15).
               10  BH-BOX-SIZE          PIC A.
               10  BH-CANDY-TYPE        PIC AA.
               10  BH-NUM-CASES         PIC S9(4).
               10  BH-PURCHASE-PRICE    PIC S999V99.



       FD  HUNT-FILE
           RECORD CONTAINS 85 CHARACTERS.
       01  HUNT-REC.
           05  HU-STORE-ID              PIC X(4).
           05  HU-EMPLOYEE-ID           PIC X(5).
           05  HU-EMPLOYEE-POS          PIC A(2).
           05  HU-EMPLOYEE-L            PIC X(10).
           05  HU-EMPLOYEE-F            PIC X(10).
           05  HU-EMPLOYEE-M            PIC X(3).
           05  HU-HIRE-DATE             PIC 9(8).
           05  FILLER                   PIC A(25).
           05  HU-CURR-SAL              PIC 9(6)v99.
           05  HU-NUM-DEP               PIC 9(2).
           05  HU-HEAL-PLAN             PIC A(1).
           05  HU-HEAL-COST             PIC 9(3).
           05  FILLER                   PIC X(4).
       FD  HUNT-INVENTORY-FILE
           RECORD CONTAINS 143 CHARACTERS.
       01  HUNT-INVENTORY-REC.
           05  HUT-STORE-ID             PIC X(4).
           05  HU-VENDOR-ID             PIC A(1).
           05  HU-CANDY-ID              PIC X(3).
           05  HU-CANDY-DATA OCCURS 5 TIMES.
               10  HU-CANDY-NAME        PIC X(15).
               10  HU-BOX-SIZE          PIC A.
               10  HU-CANDY-TYPE        PIC AA.
               10  HU-NUM-CASES         PIC S9(4).
               10  HU-PURCHASE-PRICE    PIC S999V99.



       FD  ANNI-FILE
           RECORD CONTAINS 85 CHARACTERS.
       01  ANNI-REC.
           05  AI-STORE-ID              PIC X(4).
           05  AI-EMPLOYEE-ID           PIC X(5).
           05  AI-EMPLOYEE-POS          PIC A(2).
           05  AI-EMPLOYEE-L            PIC X(10).
           05  AI-EMPLOYEE-F            PIC X(10).
           05  AI-EMPLOYEE-M            PIC X(3).
           05  AI-HIRE-DATE             PIC 9(8).
           05  FILLER                   PIC A(25).
           05  AI-CURR-SAL              PIC 9(6)v99.
           05  AI-NUM-DEP               PIC 9(2).
           05  AI-HEAL-PLAN             PIC A(1).
           05  AI-HEAL-COST             PIC 9(3).
           05  FILLER                   PIC X(4).
       FD  ANNI-INVENTORY-FILE
               RECORD CONTAINS 143 CHARACTERS.
       01  ANNI-INVENTORY-REC.
           05  ANI-STORE-ID             PIC X(4).
           05  AI-VENDOR-ID             PIC A(1).
           05  AI-CANDY-ID              PIC X(3).
           05  AI-CANDY-DATA OCCURS 5 TIMES.
               10  AI-CANDY-NAME        PIC X(15).
               10  AI-BOX-SIZE          PIC A.
               10  AI-CANDY-TYPE        PIC AA.
               10  AI-NUM-CASES         PIC S9(4).
               10  AI-PURCHASE-PRICE    PIC S999V99.



       FD  MONT-FILE
           RECORD CONTAINS 85 CHARACTERS.
       01  MONT-REC.
           05  MT-STORE-ID              PIC X(4).
           05  MT-EMPLOYEE-ID           PIC X(5).
           05  MT-EMPLOYEE-POS          PIC A(2).
           05  MT-EMPLOYEE-L            PIC X(10).
           05  MT-EMPLOYEE-F            PIC X(10).
           05  MT-EMPLOYEE-M            PIC X(3).
           05  MT-HIRE-DATE             PIC 9(8).
           05  FILLER                   PIC A(25).
           05  MT-CURR-SAL              PIC 9(6)v99.
           05  MT-NUM-DEP               PIC 9(2).
           05  MT-HEAL-PLAN             PIC A(1).
           05  MT-HEAL-COST             PIC 9(3).
           05  FILLER                   PIC X(4).
       FD  MONT-INVENTORY-FILE
               RECORD CONTAINS 143 CHARACTERS.
       01  MONT-INVENTORY-REC.
           05  MOT-STORE-ID             PIC X(4).
           05  MT-VENDOR-ID             PIC A(1).
           05  MT-CANDY-ID              PIC X(3).
           05  MT-CANDY-DATA OCCURS 5 TIMES.
               10  MT-CANDY-NAME        PIC X(15).
               10  MT-BOX-SIZE          PIC A.
               10  MT-CANDY-TYPE        PIC AA.
               10  MT-NUM-CASES         PIC S9(4).
               10  MT-PURCHASE-PRICE    PIC S999V99.



       FD  ERROR-FILE
           RECORD CONTAINS 224 CHARACTERS.
       01  ERROR-REC                    PIC X(224).



       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                 PIC X     VALUE 'Y'.

      *    THE TABLE REDEFINES THE DATA IN WAREHOUSE-TEXT, SO IT CAN BE
      *    SEARCHED LATER ON.
       01  WAREHOUSE-TEXT.
           05  PIC X(4)    VALUE 'BHAM'.
           05  PIC X(4)    VALUE 'HUNT'.
           05  PIC X(4)    VALUE 'ANNI'.
           05  PIC X(4)    VALUE 'MONT'.
       01  WAREHOUSE-TABLE REDEFINES
           WAREHOUSE-TEXT OCCURS 4 TIMES INDEXED
             BY WAREHOUSE-INDEX.
           05  STORE-ID                 PIC X(4).


       PROCEDURE DIVISION.

       10-CONTROL-MODULE.

           PERFORM 15-OPEN-ROUTINE
           PERFORM 20-READ
           PERFORM 30-CLOSE-ROUTINE
           .

       15-OPEN-ROUTINE.

      *    FILES ARE OPENED
           OPEN INPUT BENNETT-FILE
               OUTPUT BHAM-FILE, HUNT-FILE, ANNI-FILE, MONT-FILE,
               BHAM-INVENTORY-FILE, HUNT-INVENTORY-FILE,
               ANNI-INVENTORY-FILE, MONT-INVENTORY-FILE, ERROR-FILE
           .

       20-READ.

      *    READS IN THE SOURCE FILE AND STARTS THE DATA SORTING.
           PERFORM UNTIL EOF-FLAG = 'N'
            READ BENNETT-FILE
                AT END
                     MOVE 'N' TO EOF-FLAG
                NOT AT END
                     PERFORM 25-FILE-CREATION
            END-READ
           END-PERFORM
           .

       25-FILE-CREATION.

      *    TABLE IS SEARCHED FOR MATCHING DATA THROUGH THE INPUT RECORD
      *    BR-STORE-ID. WHEN MATCHING DATA IS FOUND IT OUTPUTS THAT
      *    RECORD TO THE APPROPRIATE FILE. ALSO CONVERTS THE ORIGINAL
      *    STORE ID TO THE NEW NUMERICAL ONE GIVEN BY THE PROGRAM 3
      *    DESCRIPTION.
           SET WAREHOUSE-INDEX TO 1
           SEARCH WAREHOUSE-TABLE

                 AT END
                   MOVE BENNETT-RECORD TO ERROR-REC
                   WRITE ERROR-REC
                   BEFORE ADVANCING 1 LINE

      *    BIRMINGHAM FILES ARE PROCESSED
                 WHEN BR-STORE-ID = STORE-ID (1)
                       MOVE "B100" TO BR-STORE-ID
                       WRITE BHAM-REC FROM BENNETT-RECORD
                       BEFORE ADVANCING 1 LINE
                       MOVE BR-STORE-ID TO BHM-STORE-ID
                       MOVE BR-VENDOR-ID TO BH-VENDOR-ID
                       MOVE BR-CANDY-ID TO BH-CANDY-ID
                       MOVE BR-CANDY-DATA (1) TO BH-CANDY-DATA (1)
                       MOVE BR-CANDY-DATA (2) TO BH-CANDY-DATA (2)
                       MOVE BR-CANDY-DATA (3) TO BH-CANDY-DATA (3)
                       MOVE BR-CANDY-DATA (4) TO BH-CANDY-DATA (4)
                       MOVE BR-CANDY-DATA (5) TO BH-CANDY-DATA (5)
                       WRITE BHAM-INVENTORY-REC
                       BEFORE ADVANCING 1 LINE

      *    HUNTSVILLE FILES ARE PROCESSED
                 WHEN BR-STORE-ID = STORE-ID (2)
                       MOVE "B200" TO BR-STORE-ID
                       WRITE HUNT-REC FROM BENNETT-RECORD
                       BEFORE ADVANCING 1 LINE
                       MOVE BR-STORE-ID TO HUT-STORE-ID
                       MOVE BR-VENDOR-ID TO HU-VENDOR-ID
                       MOVE BR-CANDY-ID TO HU-CANDY-ID
                       MOVE BR-CANDY-DATA (1) TO HU-CANDY-DATA (1)
                       MOVE BR-CANDY-DATA (2) TO HU-CANDY-DATA (2)
                       MOVE BR-CANDY-DATA (3) TO HU-CANDY-DATA (3)
                       MOVE BR-CANDY-DATA (4) TO HU-CANDY-DATA (4)
                       MOVE BR-CANDY-DATA (5) TO HU-CANDY-DATA (5)
                       WRITE HUNT-INVENTORY-REC
                       BEFORE ADVANCING 1 LINE

      *    ANNISTON FILES ARE PROCESSED
                 WHEN BR-STORE-ID = STORE-ID (3)
                       MOVE "B300" TO BR-STORE-ID
                       WRITE ANNI-REC FROM BENNETT-RECORD
                       BEFORE ADVANCING 1 LINE
                       MOVE BR-STORE-ID TO ANI-STORE-ID
                       MOVE BR-VENDOR-ID TO AI-VENDOR-ID
                       MOVE BR-CANDY-ID TO AI-CANDY-ID
                       MOVE BR-CANDY-DATA (1) TO AI-CANDY-DATA (1)
                       MOVE BR-CANDY-DATA (2) TO AI-CANDY-DATA (2)
                       MOVE BR-CANDY-DATA (3) TO AI-CANDY-DATA (3)
                       MOVE BR-CANDY-DATA (4) TO AI-CANDY-DATA (4)
                       MOVE BR-CANDY-DATA (5) TO AI-CANDY-DATA (5)
                       WRITE ANNI-INVENTORY-REC
                       BEFORE ADVANCING 1 LINE

      *    MONTGOMERY FILES ARE PROCESSED
                 WHEN BR-STORE-ID = STORE-ID (4)
                       MOVE "B400" TO BR-STORE-ID
                       WRITE MONT-REC FROM BENNETT-RECORD
                       BEFORE ADVANCING 1 LINE
                       MOVE BR-STORE-ID TO MOT-STORE-ID
                       MOVE BR-VENDOR-ID TO MT-VENDOR-ID
                       MOVE BR-CANDY-ID TO MT-CANDY-ID
                       MOVE BR-CANDY-DATA (1) TO MT-CANDY-DATA (1)
                       MOVE BR-CANDY-DATA (2) TO MT-CANDY-DATA (2)
                       MOVE BR-CANDY-DATA (3) TO MT-CANDY-DATA (3)
                       MOVE BR-CANDY-DATA (4) TO MT-CANDY-DATA (4)
                       MOVE BR-CANDY-DATA (5) TO MT-CANDY-DATA (5)
                       WRITE MONT-INVENTORY-REC
                       BEFORE ADVANCING 1 LINE


           END-SEARCH
           .

       30-CLOSE-ROUTINE.

      *    FILES ARE CLOSED AND PROGRAM ENDS
           CLOSE BENNETT-FILE, BHAM-FILE, HUNT-FILE, ANNI-FILE,
                 MONT-FILE, BHAM-INVENTORY-FILE, HUNT-INVENTORY-FILE,
                 ANNI-INVENTORY-FILE, MONT-INVENTORY-FILE, ERROR-FILE


           STOP RUN
           .
