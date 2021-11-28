       IDENTIFICATION DIVISION.
       PROGRAM-ID. PR4FA21.
       AUTHOR. JOHN WALLACE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-PC.
       OBJECT-COMPUTER.  IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *    UNSORTED INPUT FILES
           SELECT UNSORTED-B100
               ASSIGN TO 'NEW-INV-FILE-B100.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT UNSORTED-B200
               ASSIGN TO 'NEW-INV-FILE-B200.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT UNSORTED-B300
               ASSIGN TO 'NEW-INV-FILE-B300.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT UNSORTED-B400
               ASSIGN TO 'NEW-INV-FILE-B400.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.


      *    FILE USED FOR SORTING INPUT FILES
           SELECT SORTING-FILE
               ASSIGN TO 'SORTINGFILE.TMP'.


      *    SORTED INPUT FILES
           SELECT SORTED-B100
               ASSIGN TO 'SORTED-B100.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORTED-B200
               ASSIGN TO 'SORTED-B200.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORTED-B300
               ASSIGN TO 'SORTED-B300.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORTED-B400
               ASSIGN TO 'SORTED-B400.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.


      *    FILE USED FOR MERGING ALL SORTED FILES
           SELECT MERGE-FILE
               ASSIGN TO 'MERGEFILE.TMP'.


      *    MERGED SORTED INPUT FILES INTO 1
           SELECT MERGED-SORTED-FILE
               ASSIGN TO 'MERGED-SORTED-INVENTORY.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.

      *    REPORT FILE USED FOR OUTPUT
           SELECT BENNETT-REPORT-FILE
               ASSIGN TO PRINTER 'REPORT.TXT'.

       DATA DIVISION.
       FILE SECTION.

      *    UNSORTED FILE RECORDS
       FD  UNSORTED-B100
               RECORD CONTAINS 143 CHARACTERS.
       01  UNSORTED-B100-REC.
           05  UN1-STORE-ID              PIC X(4).
           05  UN1-VENDOR-ID             PIC A(1).
           05  UN1-CANDY-ID              PIC X(3).
           05  FILLER                    PIC X(135).
       FD  UNSORTED-B200
               RECORD CONTAINS 143 CHARACTERS.
       01  UNSORTED-B200-REC.
           05  UN2-STORE-ID              PIC X(4).
           05  UN2-VENDOR-ID             PIC A(1).
           05  UN2-CANDY-ID              PIC X(3).
           05  FILLER                    PIC X(135).
       FD  UNSORTED-B300
               RECORD CONTAINS 143 CHARACTERS.
       01  UNSORTED-B300-REC.
           05  UN3-STORE-ID              PIC X(4).
           05  UN3-VENDOR-ID             PIC A(1).
           05  UN3-CANDY-ID              PIC X(3).
           05  FILLER                    PIC X(135).
       FD  UNSORTED-B400
               RECORD CONTAINS 143 CHARACTERS.
       01  UNSORTED-B400-REC.
           05  UN4-STORE-ID              PIC X(4).
           05  UN4-VENDOR-ID             PIC A(1).
           05  UN4-CANDY-ID              PIC X(3).
           05  FILLER                    PIC X(135).

      *    SORTING FILE RECORD
       SD  SORTING-FILE
               RECORD CONTAINS 143 CHARACTERS.
       01  SORT-REC.
           05  SORT-STORE-ID              PIC X(4).
           05  SORT-VENDOR-ID             PIC A(1).
           05  SORT-CANDY-ID              PIC X(3).
           05  FILLER                     PIC X(135).

      *    SORTED FILE RECORDS
       FD  SORTED-B100
               RECORD CONTAINS 143 CHARACTERS.
       01  SORTED-B100-REC.
           05  SR1-STORE-ID              PIC X(4).
           05  SR1-VENDOR-ID             PIC A(1).
           05  SR1-CANDY-ID              PIC X(3).
           05  FILLER                    PIC X(135).
       FD  SORTED-B200
               RECORD CONTAINS 143 CHARACTERS.
       01  SORTED-B200-REC.
           05  SR2-STORE-ID              PIC X(4).
           05  SR2-VENDOR-ID             PIC A(1).
           05  SR2-CANDY-ID              PIC X(3).
           05  FILLER                    PIC X(135).
       FD  SORTED-B300
               RECORD CONTAINS 143 CHARACTERS.
       01  SORTED-B300-REC.
           05  SR3-STORE-ID              PIC X(4).
           05  SR3-VENDOR-ID             PIC A(1).
           05  SR3-CANDY-ID              PIC X(3).
           05  FILLER                    PIC X(135).
       FD  SORTED-B400
               RECORD CONTAINS 143 CHARACTERS.
       01  SORTED-B400-REC.
           05  SR4-STORE-ID              PIC X(4).
           05  SR4-VENDOR-ID             PIC A(1).
           05  SR4-CANDY-ID              PIC X(3).
           05  FILLER                    PIC X(135).

      *    MERGE FILE RECORD
       SD  MERGE-FILE
               RECORD CONTAINS 143 CHARACTERS.
       01  MERGE-REC.
           05  MERGE-STORE-ID              PIC X(4).
           05  MERGE-VENDOR-ID             PIC A(1).
           05  MERGE-CANDY-ID              PIC X(3).
           05  FILLER                      PIC X(135).

      *    MERGED AND SORTED FILE RECORD
       FD  MERGED-SORTED-FILE
               RECORD CONTAINS 143 CHARACTERS.
       01  MERGED-SORTED-REC.
           05  MS-STORE-ID              PIC X(4).
           05  MS-VENDOR-ID             PIC A(1).
           05  MS-CANDY-ID              PIC X(3).
           05  MS-CANDY-DATA OCCURS 5 TIMES.
               10  MS-CANDY-NAME        PIC X(15).
               10  MS-BOX-SIZE           PIC A.
               10  MS-CANDY-TYPE        PIC AA.
               10  MS-NUM-CASES          PIC S9(4).
               10  MS-PURCHASE-PRICE     PIC S999V99.

      *    REPORT RECORD FOR OUTPUT
       FD  BENNETT-REPORT-FILE
               RECORD CONTAINS 300 CHARACTERS.
       01  REPORT-REC                   PIC X(300).


       WORKING-STORAGE SECTION.
      *    SWITCHES USED FOR LOOPS
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                 PIC X         VALUE 'Y'.
           05  FIRST-RECORD             PIC X         VALUE 'Y'.
           05  FIRST-TIME               PIC X         VALUE 'Y'.

      *    SUBSCRIPT FOR ARRAY LOOP
       01  SUBSCIPTS.
           05 SUB                       PIC 9         VALUE ZERO.


       01  TEMP-FIELDS.
           05 HEADER-TEMP               PIC 9         VALUE 1.

      *    USED TO STRING SIZE AND CONCATENATE BAD- WITH INCORRECT SIZE
       01  SPACING.
           05 SIZE-OUT                  PIC X(5).

      *    HOLD FIELDS USED FOR CONTROL BREAKS
       01  HOLD-FIELD.
           05  WAREHOUSE-HOLD           PIC X(4)      VALUE SPACES.
           05  VENDOR-HOLD              PIC X(4)      VALUE SPACES.
           05  CANDY-HOLD               PIC X(4)      VALUE SPACES.

      *    DATE STORAGE SECTION
       01  WS-CURRENT-DATE.
           05  WS-YEAR                  PIC 9999.
           05  WS-MONTH                 PIC 99.
           05  WS-DAY                   PIC 99.

      *    STORAGE SECTION FOR TOTALS OF CONTROL BREAKS
       01  TOTAL-FIELDS.
           05 CANDY-STOCK               PIC 9(5).
           05 VENDOR-CANDY-STOCK        PIC 9(6).
           05 WAREHOUSE-CANDY-STOCK     PIC 9(7).
           05 CANDY-COST                PIC 9(7)V99.
           05 VENDOR-CANDY-COST         PIC 9(8)V99.
           05 WAREHOUSE-CANDY-COST      PIC 9(9)V99.
           05 GRAND-STOCK               PIC 9(8).
           05 GRAND-COST                PIC 9(10)V99.
      **************        OUTPUT AREA        ********************

      *    MAIN HEADERS FOR REPORT
       01  H1.
           05                           PIC X(28)     VALUE SPACES.
           05                           PIC X(45)     VALUE
           'BENNETT SWEETS AND MORE'.
       01  H2.
           05                           PIC X(32)     VALUE SPACES.
           05                           PIC X(41)     VALUE
           'BENNETT CANDIES:'.
       01  H8.
           05                           PIC X(32)     VALUE SPACES.
           05                           PIC X(41)     VALUE
           'BENNETT SWEETS:'.
       01  H9.
           05                           PIC X(32)     VALUE SPACES.
           05                           PIC X(41)     VALUE
           'BENNETT DESSERTS:'.


       01  H3.
           05                           PIC X(7)      VALUE SPACES.
           05  H2-DATE.
               10  H2-DAY               PIC 99.
               10                       PIC X         VALUE '/'.
               10  H2-MONTH             PIC 99.
               10                       PIC X         VALUE '/'.
               10  H2-YEAR              PIC 9999.
           05                           PIC X(15)     VALUE SPACES.
           05                           PIC X(30)     VALUE
           'INVENTORY REPORT'.
           05                           PIC X(11)     VALUE
           'JCW'.

      *    CONTROL BREAK HEADERS
       01  H4-WAREHOUSE.
           05                           PIC XX        VALUE SPACES.
           05                           PIC X(11)     VALUE
           'WAREHOUSE:'.
           05 WAREHOUSE                 PIC X(4).
       01  H5-VENDOR.
           05                           PIC X(5)      VALUE SPACES.
           05                           PIC X(8)      VALUE
           'VENDOR:'.
           05 VENDOR                    PIC X(18).
       01  H6-CANDY.
           05                           PIC X(6)      VALUE SPACES.
           05                           PIC X(7)      VALUE
           'CANDY:'.
           05 CANDY                     PIC X(3).

      *    COLUMN TITLES FOR CANDY DATA
       01  H7-TITLES.
           05                           PIC X(6)      VALUE SPACES.
           05                           PIC X(18)     VALUE
           'CANDY NAME'.
           05                           PIC X(12)     VALUE
           'SIZE'.
           05                           PIC X(7)      VALUE
           'TYPE'.
           05                           PIC X(14)     VALUE
           'IN STOCK'.
           05                           PIC X(16)     VALUE
           'TOTAL COST'.

      *    DETAIL LINES FOR REPORT DATA
      *    FIRST LINE OF DATA
       01  DETAIL-LINE1 .
           05                           PIC X(3)      VALUE SPACES.
           05 DL1-CANDY-NAME            PIC X(15).
           05                           PIC X(4)      VALUE SPACES.
           05 DL1-SIZE                  PIC X(10).
           05                           PIC X(5)      VALUE SPACES.
           05 DL1-TYPE                  PIC X(2).
           05                           PIC X(5)      VALUE SPACES.
           05 DL1-INSTOCK               PIC Z,ZZ9.
           05                           PIC X(8)      VALUE SPACES.
           05 DL1-TOTAL                 PIC $$$,$$$.99.
           05                           PIC X(6)      VALUE SPACES.
      *    USED FOR THE REMAINING LINES OF DATA
       01  DETAIL-LINE2.
           05 FILLER                     PIC X(22)     VALUE SPACES.
           05 DL2-SIZE                   PIC X(10).
           05                           PIC X(5)      VALUE SPACES.
           05 DL2-TYPE                     PIC X(2).
           05                           PIC X(5)      VALUE SPACES.
           05 DL2-INSTOCK                 PIC Z,ZZ9.
           05                           PIC X(8)      VALUE SPACES.
           05 DL2-TOTAL                 PIC $$$,$$$.99.
           05                           PIC X(6)      VALUE SPACES.

      *    TOTAL LINES USED FOR EACH CONTROL BREAK
       01  TOTAL-CANDY-LINE.
           05                           PIC X(8)      VALUE SPACES.
           05                           PIC X(14)     VALUE
           'TOTAL CANDY:'.
           05 TCL-CANDY                 PIC X(15).
           05                           PIC X(6)      VALUE SPACES.
           05 TCL-INSTOCK               PIC ZZ,ZZ9.
           05                           PIC X(6)      VALUE SPACES.
           05 TCL-TOTAL                 PIC $,$$$,$$$.99.
           05                           PIC X(6)      VALUE SPACES.
       01  TOTAL-VENDOR-LINE.
           05                           PIC X(3)      VALUE SPACES.
           05                           PIC X(19)     VALUE
           'TOTAL FOR VENDOR:'.
           05 TVL-VENDOR                PIC X(18).
           05                           PIC X(2)      VALUE SPACES.
           05 TVL-INSTOCK               PIC ZZZ,ZZ9.
           05                           PIC X(5)      VALUE SPACES.
           05 TVL-TOTAL                 PIC $$,$$$,$$$.99.
           05                           PIC X(6)      VALUE SPACES.
       01  TOTAL-WAREHOUSE-LINE.
           05                           PIC X(22)     VALUE
           'TOTAL FOR WAREHOUSE:'.
           05 TWL-WAREHOUSE             PIC X(4).
           05                           PIC X(14)     VALUE SPACES.
           05 TWL-INSTOCK               PIC Z,ZZZ,ZZ9.
           05                           PIC X(4)      VALUE SPACES.
           05 TWL-TOTAL                 PIC $$$,$$$,$$$.99.
           05                           PIC X(6)      VALUE SPACES.

      *    GRAND TOTAL LINE USED AT THE END OF THE REPORT
       01  GRAND-TOTAL-LINE.
           05                           PIC X(25)     VALUE SPACES.
           05                           PIC X(14)     VALUE
           'GRAND TOTAL:'.
           05 GTL-INSTOCK               PIC ZZ,ZZZ,ZZ9.
           05                           PIC X(2)      VALUE SPACES.
           05 GTL-TOTAL                 PIC $,$$$,$$$,$$$.99.
           05                           PIC X(6)      VALUE SPACES.
       PROCEDURE DIVISION.

      *    CONTROL MODULE USED FOR PROGRAM FLOW
       10-CONTROL-MODULE.

           PERFORM 15-SORT-MERGE
           PERFORM 20-OPEN-ROUTINE
           PERFORM 30-READ
           PERFORM 55-FINAL

       .
      *    WHERE THE SORTING AND MERGING OF THE FILES HAPPENS
       15-SORT-MERGE.
      *    SORTS FOUR INPUT FILES
           SORT SORTING-FILE
               ON ASCENDING KEY SORT-STORE-ID SORT-VENDOR-ID
               SORT-CANDY-ID
               USING UNSORTED-B100
               GIVING SORTED-B100
           SORT SORTING-FILE
               ON ASCENDING KEY SORT-STORE-ID SORT-VENDOR-ID
               SORT-CANDY-ID
               USING UNSORTED-B200
               GIVING SORTED-B200
           SORT SORTING-FILE
               ON ASCENDING KEY SORT-STORE-ID SORT-VENDOR-ID
               SORT-CANDY-ID
               USING UNSORTED-B300
               GIVING SORTED-B300
           SORT SORTING-FILE
               ON ASCENDING KEY SORT-STORE-ID SORT-VENDOR-ID
               SORT-CANDY-ID
               USING UNSORTED-B400
               GIVING SORTED-B400

      *    MERGES FOUR INPUT FILES INTO ONE
           MERGE MERGE-FILE
               ON ASCENDING KEY MERGE-STORE-ID MERGE-VENDOR-ID
               MERGE-CANDY-ID
               USING SORTED-B100, SORTED-B200, SORTED-B300,
               SORTED-B400
               GIVING MERGED-SORTED-FILE

           .
      *    OPENS REQUIRED FILES, PULLS IN DATE INFO, AND STARTS HEADER
       20-OPEN-ROUTINE.

           OPEN INPUT MERGED-SORTED-FILE
               OUTPUT BENNETT-REPORT-FILE


           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-YEAR TO H2-YEAR
           MOVE WS-MONTH TO H2-MONTH
           MOVE WS-DAY TO H2-DAY

           PERFORM 25-HEADER-ROUTINE


           .

      *    WRITES HEADERS TO REPORT
       25-HEADER-ROUTINE.
           EVALUATE HEADER-TEMP
           WHEN = 1
               WRITE REPORT-REC FROM H1
               WRITE REPORT-REC FROM H3
               AFTER ADVANCING  1 LINE

           WHEN = 2
               WRITE REPORT-REC FROM H2
               AFTER ADVANCING  PAGE
               WRITE REPORT-REC FROM H3
               AFTER ADVANCING 2 LINES

           WHEN = 3
               WRITE REPORT-REC FROM H8
               AFTER ADVANCING  PAGE
               WRITE REPORT-REC FROM H3
               AFTER ADVANCING 2 LINES

           WHEN = 4
               WRITE REPORT-REC FROM H9
               AFTER ADVANCING  PAGE
               WRITE REPORT-REC FROM H3
               AFTER ADVANCING 2 LINES

           END-EVALUATE

           ADD 1 TO HEADER-TEMP
           .
      *    READS IN MERGE FILE DATA UNTIL EOF-FLAG
       30-READ.

           PERFORM UNTIL EOF-FLAG = 'N'
               READ MERGED-SORTED-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                   IF MS-STORE-ID NOT = 'B100' OR 'B200' OR 'B300' OR
                       'B400'
                       CONTINUE
                   END-IF
                       PERFORM 35-BENNETT-PROCESS
               END-READ
           END-PERFORM

           .
      *    MAIN PROCESS OF MOVING INPUT DATA TO REPORT
       35-BENNETT-PROCESS.
      *    DETERMINES IF STORE ID IS VALID AND BEGINES CONTROL BREAK
           IF MS-STORE-ID = 'B100' OR 'B200' OR 'B300' OR 'B400'
           EVALUATE TRUE
               WHEN FIRST-RECORD = 'Y'
               PERFORM 65-WAREHOUSE-HEADER
               PERFORM 60-VENDOR-NAME
               PERFORM 70-VENDOR-HEADER
               PERFORM 75-CANDY-HEADER
                   MOVE 'N' TO FIRST-RECORD
                   MOVE MS-STORE-ID TO WAREHOUSE-HOLD
                   MOVE MS-VENDOR-ID TO VENDOR-HOLD
                   MOVE MS-CANDY-ID TO CANDY-HOLD
               WHEN MS-STORE-ID NOT = WAREHOUSE-HOLD
                   PERFORM 50-MAJOR-BREAK
                   PERFORM 25-HEADER-ROUTINE
                   PERFORM 65-WAREHOUSE-HEADER
                   PERFORM 60-VENDOR-NAME
                   PERFORM 70-VENDOR-HEADER
                   PERFORM 75-CANDY-HEADER
               WHEN MS-VENDOR-ID NOT = VENDOR-HOLD
                   PERFORM 45-INT-BREAK
                   PERFORM 60-VENDOR-NAME
                   WRITE REPORT-REC FROM H5-VENDOR
                   AFTER ADVANCING 3 LINES
                   PERFORM 75-CANDY-HEADER
               WHEN MS-CANDY-ID NOT = CANDY-HOLD
                   PERFORM 40-MINOR-BREAK
                   PERFORM 75-CANDY-HEADER

           END-EVALUATE



           PERFORM 60-VENDOR-NAME


      *    HANDLES ARRAY DATA WHIOLE ONLY MOVING CANDY NAME TO FIRST LINE
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
           IF SUB = 1
               MOVE MS-CANDY-NAME (1) TO DL1-CANDY-NAME
               PERFORM 80-SIZE
               PERFORM 85-TYPE
               PERFORM 90-NUMBERS
               WRITE REPORT-REC FROM DETAIL-LINE1
               AFTER ADVANCING 2 LINES
           END-IF
           IF SUB > 1
               PERFORM 80-SIZE
               PERFORM 85-TYPE
               PERFORM 90-NUMBERS
               WRITE REPORT-REC FROM DETAIL-LINE2
               AFTER ADVANCING 1 LINE
           END-IF

           END-PERFORM

           END-IF



           .

      *    CONTROL BREAK FOR EACH CANDY
       40-MINOR-BREAK.
           MOVE CANDY-HOLD TO TCL-CANDY
           MOVE CANDY-STOCK TO TCL-INSTOCK
           ADD CANDY-STOCK TO VENDOR-CANDY-STOCK
           MOVE CANDY-COST TO TCL-TOTAL
           ADD CANDY-COST TO VENDOR-CANDY-COST
           WRITE REPORT-REC FROM TOTAL-CANDY-LINE
           AFTER ADVANCING 2 LINES
           MOVE 0 TO CANDY-STOCK
           MOVE 0 TO CANDY-COST
           MOVE MS-CANDY-ID TO CANDY-HOLD


           .

      *    CONTROL BREAK FOR EACH VENDOR
       45-INT-BREAK.
           PERFORM 40-MINOR-BREAK
           MOVE VENDOR TO TVL-VENDOR
           MOVE VENDOR-CANDY-STOCK TO TVL-INSTOCK
           ADD VENDOR-CANDY-STOCK TO WAREHOUSE-CANDY-STOCK
           MOVE VENDOR-CANDY-COST TO TVL-TOTAL
           ADD VENDOR-CANDY-COST TO WAREHOUSE-CANDY-COST
           WRITE REPORT-REC FROM TOTAL-VENDOR-LINE
           AFTER ADVANCING 2 LINES
           MOVE 0 TO VENDOR-CANDY-STOCK
           MOVE 0 TO VENDOR-CANDY-COST
           MOVE MS-VENDOR-ID TO VENDOR-HOLD

           .

      *    CONTROL BREAK FOR EACH WAREHOUSE
       50-MAJOR-BREAK.
           PERFORM 45-INT-BREAK
           MOVE WAREHOUSE-HOLD TO TWL-WAREHOUSE
           MOVE WAREHOUSE-CANDY-STOCK TO TWL-INSTOCK
           ADD WAREHOUSE-CANDY-STOCK TO GRAND-STOCK
           MOVE WAREHOUSE-CANDY-COST TO TWL-TOTAL
           ADD WAREHOUSE-CANDY-COST TO GRAND-COST
           WRITE REPORT-REC FROM TOTAL-WAREHOUSE-LINE
           AFTER ADVANCING 2 LINES
           MOVE 0 TO WAREHOUSE-CANDY-STOCK
           MOVE 0 TO WAREHOUSE-CANDY-COST
           MOVE MS-STORE-ID TO WAREHOUSE-HOLD

           .

      *    OUTPUTS THE GRAND TOTAL DATA
       55-FINAL.
           PERFORM 50-MAJOR-BREAK
           MOVE GRAND-COST TO GTL-TOTAL
           MOVE GRAND-STOCK TO GTL-INSTOCK
           WRITE REPORT-REC FROM GRAND-TOTAL-LINE
           AFTER ADVANCING 3 LINES
           CLOSE MERGED-SORTED-FILE
                 BENNETT-REPORT-FILE
           STOP RUN

           .

      *    EXPANDS VENDOR NAME TO ITS FULL NAME
       60-VENDOR-NAME.

           IF MS-VENDOR-ID = 'A'
                   MOVE 'ATOMIC SWEETS' TO VENDOR
           ELSE
               IF MS-VENDOR-ID = 'B'
                   MOVE 'BOSTON SWEETS' TO VENDOR
           ELSE
               IF MS-VENDOR-ID = 'N'
                   MOVE 'NELLIES SWEET SHOP' TO VENDOR
           ELSE
               IF MS-VENDOR-ID = 'T'
                   MOVE 'TIGER TREATS' TO VENDOR
           ELSE
               IF MS-VENDOR-ID = 'U'
                   MOVE 'UNITY CANDY' TO VENDOR
           ELSE
               IF MS-VENDOR-ID = 'V'
                   MOVE 'XTRA CANDIES' TO VENDOR
           ELSE
               MOVE 'INVALID VEND CODE' TO VENDOR
           END-IF
           .

      *    WRITES WAREHOUSE HEADER TO REPORT
       65-WAREHOUSE-HEADER.

           MOVE MS-STORE-ID TO WAREHOUSE
           WRITE REPORT-REC FROM H4-WAREHOUSE
           AFTER ADVANCING 2 LINES

           .

      *    WRITES VENDOR HEADER TO REPORT
       70-VENDOR-HEADER.

           WRITE REPORT-REC FROM H5-VENDOR
           AFTER ADVANCING 2 LINES
           .

      *    WRITES CANDY HEADER AND COLUMN TITLES TO REPORT
       75-CANDY-HEADER.
           MOVE MS-CANDY-ID TO CANDY
           WRITE REPORT-REC FROM H6-CANDY
           AFTER ADVANCING 2 LINES

           WRITE REPORT-REC FROM H7-TITLES
           AFTER ADVANCING 2 LINES
           .

      *    EXPANDS SIZE AND VALIDATES THE DATA. CONCATENATES BAD- WITH
      *    INVALID DATA
       80-SIZE.
           EVALUATE MS-BOX-SIZE (SUB)
              WHEN = 'L'
               MOVE 'LARGE' TO DL2-SIZE DL1-SIZE
              WHEN = 'M'
               MOVE 'MEDIUM' TO DL2-SIZE DL1-SIZE
              WHEN = 'S'
               MOVE 'SMALL' TO DL2-SIZE DL1-SIZE
              WHEN = 'F'
               MOVE 'FUNDRAISER' TO DL2-SIZE DL1-SIZE
              WHEN = 'X'
               MOVE 'SAMPLE' TO DL2-SIZE DL1-SIZE
              WHEN = ' '
               MOVE 'NO DATA' TO DL2-SIZE DL1-SIZE
              WHEN NOT = 'L' OR 'M' OR 'S' OR 'F' OR 'X' OR ' '
                STRING
                'BAD-' DELIMITED BY ' '
                MS-BOX-SIZE (SUB) DELIMITED BY SIZE
                INTO SIZE-OUT
                END-STRING
                MOVE SIZE-OUT TO DL2-SIZE DL1-SIZE
           END-EVALUATE

           .

      *    VALIDATES TYPE AND OUTPUTS ACCORDINGLY
       85-TYPE.

           EVALUATE MS-CANDY-TYPE (SUB)
               WHEN = 'SU' OR 'SF'
                MOVE MS-CANDY-TYPE (SUB) TO DL1-TYPE DL2-TYPE
               WHEN NOT = 'SU' OR ' SF'
                MOVE '**' TO DL1-TYPE DL2-TYPE
           END-EVALUATE

           .

      *    INSPECTS CANDY COST AND CASE NUMBER FOR INVALID DATA.
      *    REPLACES WITH A 0.
       90-NUMBERS.
           INSPECT MS-PURCHASE-PRICE (SUB)  REPLACING
               ALL '$' BY '0'
               ALL 'o' BY '0'
               ALL '%' BY '0'
               ALL '#' BY '0'
               ALL ' ' BY '0'
               ALL 'P' BY '0'
               ALL 'W' BY '0'
           INSPECT MS-NUM-CASES (SUB) REPLACING
               ALL 'X' BY '0'
               ALL ' ' BY '0'
               ALL '$' BY '0'
               ALL '%' BY '0'
           ADD MS-NUM-CASES(SUB) TO CANDY-STOCK
           ADD MS-PURCHASE-PRICE(SUB) TO CANDY-COST
           MOVE MS-NUM-CASES(SUB) TO DL1-INSTOCK DL2-INSTOCK
           MOVE MS-PURCHASE-PRICE (SUB) TO DL1-TOTAL DL2-TOTAL


           .
