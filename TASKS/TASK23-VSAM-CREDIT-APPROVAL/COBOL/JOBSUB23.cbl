      ******************************************************************
      * CREDIT APPROVAL SYSTEM - VSAM LOOKUP WITH CREDITCHECK CALL     *
      *                                                                *
      * PURPOSE:                                                       *
      * READS LOAN REQUESTS FROM INPUT FILE, LOOKS UP EACH CUSTOMER    *
      * IN VSAM KSDS, CALLS SUBPROGRAM TO CHECK CREDITWORTHINESS,      *
      * AND WRITES APPROVE/REJECT DECISION TO OUTPUT FILE.             *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   PHASE 1 - READ: READ LOAN-REC FROM LOAN-FILE.                *
      *   PHASE 2 - VSAM LOOKUP: RANDOM READ BY LOAN-CUST-ID.          *
      *     STATUS '23': CUSTOMER NOT FOUND -> REJECTED.               *
      *     OTHER NON-ZERO: VSAM ERROR -> STOP RUN.                    *
      *     FOUND: CALL SUB1JB23 FOR CREDIT DECISION.                  *
      *   PHASE 3 - OUTPUT: WRITE DECISION LINE TO APPROVAL-FILE.      *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2026/01/21                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT:  VSAMDD (CREDIT.MASTER) - VSAM KSDS CUSTOMER MASTER     *
      *         INDD   (LOAN.REQUESTS) - SEQUENTIAL LOAN REQUEST FILE  *
      * OUTPUT: OUTDD  (APPROVAL.LOG)  - SEQUENTIAL DECISION LOG       *
      *                                                                *
      * SUBPROGRAMS CALLED:                                            *
      * SUB1JB23 - EVALUATES CREDIT SCORE, DEBT AND PAYMENT HISTORY    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOBSUB23.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT VSAM-FILE ASSIGN TO VSAMDD
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS CUST-ID
              FILE STATUS IS VSAM-STATUS.

           SELECT LOAN-FILE ASSIGN TO INDD
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS LOAN-STATUS.

           SELECT APPROVAL-FILE ASSIGN TO OUTDD
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS APPROVAL-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD VSAM-FILE.
       01 CUST-REC.
          05 CUST-ID PIC X(6).
          05 CUST-NAME PIC X(25).
          05 CREDIT-SCORE PIC 9(3).
          05 LATE-PAYMENTS PIC 9(2).
          05 CURRENT-DEBT PIC 9(5)V99.
          05 FILLER PIC X(37).

       FD LOAN-FILE RECORDING MODE IS F.
       01 LOAN-REC.
          05 LOAN-REQUEST-ID PIC X(6).
          05 LOAN-CUST-ID PIC X(6).
          05 LOAN-AMOUNT PIC 9(5)V99.
          05 FILLER PIC X(61) VALUE SPACES.

       FD APPROVAL-FILE RECORDING MODE IS F.
       01 APPROVAL-REC PIC X(80).

       WORKING-STORAGE SECTION.

      * FILE STATUS VARIABLES
       01 F-STATUS.
          05 VSAM-STATUS PIC X(2).
          05 LOAN-STATUS PIC X(2).
          05 APPROVAL-STATUS PIC X(2).

      * CONTROL FLAGS
       01 WS-FLAGS.
          05 WS-EOF PIC X(1) VALUE 'N'.
             88 EOF VALUE 'Y'.
             88 NOT-EOF VALUE 'N'.

      * PROCESSING COUNTERS
       01 WS-COUNTERS.
          05 TOTAL-PROCESS-COUNTER PIC 9(5) VALUE 0.
          05 ERROR-COUNTER PIC 9(5) VALUE 0.
          05 SUCCESS-COUNTER PIC 9(5) VALUE 0.

      * STATISTICS COUNTERS
       01 WS-DISP-COUNTERS.
          05 TOTAL-PROCESS-COUNTER-DISP PIC Z(4)9.
          05 ERROR-COUNTER-DISP PIC Z(4)9.
          05 SUCCESS-COUNTER-DISP PIC Z(4)9.

      * DECISION RESULTS
       01 WS-DECISION PIC X(10) VALUE SPACES.
       01 WS-REASON PIC X(25) VALUE SPACES.

      **********************************************
      * OPENS FILES, PROCESSES ALL RECORDS, WRITES
      * SUMMARY TO SYSOUT, THEN CLOSES FILES.
      **********************************************
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM OPEN-ALL-FILES.
           PERFORM PROCESS-ALL-RECORDS.
           PERFORM CLOSE-ALL-FILES.
           STOP RUN.

      **********************************************
      * OPEN ALL FILES AND CHECK STATUS
      **********************************************
       OPEN-ALL-FILES.
           OPEN INPUT VSAM-FILE.
           IF VSAM-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING VSAM FILE: ' VSAM-STATUS
              STOP RUN
           END-IF.

           OPEN INPUT LOAN-FILE.
           IF LOAN-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING INPUT FILE: ' LOAN-STATUS
              STOP RUN
           END-IF.

           OPEN OUTPUT APPROVAL-FILE.
           IF APPROVAL-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING OUT STATUS: ' APPROVAL-STATUS
              STOP RUN
           END-IF.

      **********************************************
      * READS LOAN-FILE UNTIL EOF. PER RECORD:
      * RESETS DECISION FIELDS,
      * CALLS PROCESS-LOAN-REQUEST FOR VSAM LOOKUP AND CREDIT DECISION.
      **********************************************
       PROCESS-ALL-RECORDS.
           PERFORM UNTIL EOF
              READ LOAN-FILE
                AT END
                   SET EOF TO TRUE
                NOT AT END
                   IF LOAN-STATUS NOT = '00'
                      DISPLAY 'ERROR READING FILE: ' LOAN-STATUS
                      STOP RUN
                   END-IF
                   ADD 1 TO TOTAL-PROCESS-COUNTER
                   MOVE SPACES TO WS-DECISION
                   MOVE SPACES TO WS-REASON
                   PERFORM READ-VSAM-PARA
              END-READ
           END-PERFORM.

      **********************************************
      * MOVES LOAN-CUST-ID TO VSAM KEY AND READS.
      * STATUS '23': CUSTOMER NOT FOUND -> REJECTED.
      * OTHER NON-ZERO: VSAM ERROR -> STOP RUN.
      * FOUND: CALL SUB1JB23 FOR CREDIT DECISION.
      * ALWAYS CALLS WRITE-DECISION-LINE AFTER.
      **********************************************
       READ-VSAM-PARA.
           MOVE LOAN-CUST-ID TO CUST-ID.
           READ VSAM-FILE
               INVALID KEY
                  IF VSAM-STATUS = '23'
                     ADD 1 TO ERROR-COUNTER
                     MOVE 'REJECTED' TO WS-DECISION
                     MOVE 'CUSTOMER NOT FOUND' TO WS-REASON
                  ELSE
                     DISPLAY 'VSAM ERROR: ' VSAM-STATUS
                     DISPLAY 'CUSTOMER ID: ' LOAN-CUST-ID
                     STOP RUN
                  END-IF
               NOT INVALID KEY
                  CALL 'SUB1JB23' USING
                       CREDIT-SCORE,
                       LATE-PAYMENTS,
                       CURRENT-DEBT,
                       LOAN-AMOUNT,
                       WS-DECISION,
                       WS-REASON
                  END-CALL
                  IF WS-DECISION = 'APPROVED'
                     ADD 1 TO SUCCESS-COUNTER
                  ELSE
                     ADD 1 TO ERROR-COUNTER
                  END-IF
           END-READ
           PERFORM WRITE-OUTPUT-LINE.

      **********************************************
      * BUILD AND WRITE OUTPUT DECISION LINE
      **********************************************
       WRITE-OUTPUT-LINE.
           MOVE SPACES TO APPROVAL-REC.
           STRING LOAN-REQUEST-ID DELIMITED BY SIZE
                  ' ' DELIMITED BY SIZE
                  FUNCTION TRIM(WS-DECISION) DELIMITED BY SIZE
                  ' ' DELIMITED BY SIZE
                  FUNCTION TRIM(WS-REASON) DELIMITED BY SIZE
                  INTO APPROVAL-REC
           END-STRING.
           WRITE APPROVAL-REC.
           IF APPROVAL-STATUS NOT = '00'
              DISPLAY 'ERROR WRITING OUTPUT: ' APPROVAL-STATUS
              STOP RUN
           END-IF.

      **********************************************
      * DISPLAY SUMMARY COUNTERS AND CLOSE ALL FILES
      **********************************************
       CLOSE-ALL-FILES.
           MOVE TOTAL-PROCESS-COUNTER TO TOTAL-PROCESS-COUNTER-DISP.
           MOVE ERROR-COUNTER TO ERROR-COUNTER-DISP.
           MOVE SUCCESS-COUNTER TO SUCCESS-COUNTER-DISP.

           DISPLAY 'TOTAL: ' FUNCTION TRIM(TOTAL-PROCESS-COUNTER-DISP).
           DISPLAY 'ERROR: ' FUNCTION TRIM(ERROR-COUNTER-DISP).
           DISPLAY 'SUCCESS: ' FUNCTION TRIM(SUCCESS-COUNTER-DISP).
           CLOSE VSAM-FILE.
           IF VSAM-STATUS NOT = '00'
              DISPLAY 'WARNING: ERROR CLOSING VSAM FILE: ' VSAM-STATUS
           END-IF.
           CLOSE LOAN-FILE.
           IF LOAN-STATUS NOT = '00'
              DISPLAY 'WARNING: ERROR CLOSING INPUT FILE: ' LOAN-STATUS
           END-IF.
           CLOSE APPROVAL-FILE.
           IF APPROVAL-STATUS NOT = '00'
              DISPLAY 'WARNING: ERROR CLOSING OUTPUT FILE: '
                       APPROVAL-STATUS
           END-IF.
