      ******************************************************************
      * ACCOUNT ARCHIVE BATCH                                          *
      *                                                                *
      * PURPOSE:                                                       *
      * READS DAILY ACCOUNT FILE (PS), LOOKS UP TRANSACTION HISTORY    *
      * IN VSAM KSDS, ROUTES EACH ACCOUNT TO ONE OF THREE GDG OUTPUT   *
      * FILES (ACTIVE / ARCHIVED / UNMATCHED) AND WRITES SUMMARY       *
      * REPORT. SETS RETURN-CODE BASED ON UNMATCHED COUNT.             *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   PHASE 1 - RANDOM READ VSAM BY ACCT-ID:                       *
      *     STATUS '23' (NOT FOUND): ROUTE TO GDG UNMATCHED,           *
      *       INCREMENT UNMATCH-COUNT AND ERROR-COUNT.                 *
      *     OTHER NON-ZERO: LOG ERROR, STOP RUN (FATAL).               *
      *   PHASE 2 - ROUTE FOUND ACCOUNT BY LAST TRANSACTION DATE:      *
      *     HIST-LAST-TXN-DATE < WS-CUTOFF-DATE:                       *
      *       ROUTE TO GDG ARCHIVED, INCREMENT ARCHIVE-COUNT.          *
      *     HIST-LAST-TXN-DATE >= WS-CUTOFF-DATE:                      *
      *       ROUTE TO GDG ACTIVE, INCREMENT ACTIVE-COUNT.             *
      *   PHASE 3 - FINAL RETURN-CODE:                                 *
      *     ERROR-COUNT =  0: RC=0.                                    *
      *     ERROR-COUNT <  10: RC=4.                                   *
      *     ERROR-COUNT >= 10: RC=12.                                  *
      *                                                                *
      * CUTOFF: TODAY - 180 DAYS (COMPUTED VIA INTEGER-OF-DATE).       *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2026/01/29                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT:  INPSDD  (ACCT.DATA)    - PS DAILY ACCOUNT FILE         *
      *         VSAMDD  (ACCT.HISTORY) - VSAM KSDS TRANSACTION HISTORY *
      * OUTPUT: GDGDD1  (ACCT.ACTIVE)  - GDG ACTIVE ACCOUNTS           *
      *         GDGDD2  (ACCT.ARCHIVE) - GDG ARCHIVED ACCOUNTS         *
      *         GDGDD3  (ACCT.UNMATCH) - GDG UNMATCHED ACCOUNTS        *
      *         REPPSDD (PROCESS.REP)  - PS PROCESSING REPORT          *
      ******************************************************************
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. GDGJOB27.                                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT ACCT-DATA-FILE ASSIGN TO INPSDD                       
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS ACCT-DATA-STATUS.                          
                                                                        
           SELECT ACCT-HISTORY ASSIGN TO VSAMDD                         
              ORGANIZATION IS INDEXED                                   
              ACCESS MODE IS RANDOM                                     
              RECORD KEY IS HIST-ACCT-ID                                
              FILE STATUS IS ACCT-HIST-STATUS.                          
                                                                        
           SELECT ACCT-ACTIVE ASSIGN TO GDGDD1                          
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS ACCT-ACT-STATUS.                           
                                                                        
           SELECT ARCHIVE-OLD ASSIGN TO GDGDD2                          
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS ARCHIVE-STATUS.                            
                                                                        
           SELECT PROCESS-REP ASSIGN TO REPPSDD                         
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS REP-STATUS.                                
                                                                        
           SELECT ACCT-UNMATCHED ASSIGN TO GDGDD3                       
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS UNMATCH-STATUS.                            
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD ACCT-DATA-FILE RECORDING MODE IS F.                           
       01 ACCT-DATA.                                                    
          05 DATA-ACCT-ID PIC X(6).                                     
          05 DATA-CUST-NAME PIC X(25).                                  
          05 DATA-LAST-ACTV-DATE PIC 9(8).                              
          05 DATA-BALANCE PIC 9(7)V99.                                  
          05 FILLER PIC X(10).                                          
                                                                        
       FD ACCT-HISTORY.                                                 
       01 ACCT-HIST.                                                    
          05 HIST-ACCT-ID PIC X(6).                                     
          05 HIST-LAST-TRNS-DATE PIC 9(8).                              
          05 HIST-TRNS-COUNT PIC 9(4).                                  
                                                                        
       FD ACCT-ACTIVE RECORDING MODE IS F.                              
       01 ACCT-ACTV.                                                    
          05 ACTV-ACCT-ID PIC X(6).                                     
          05 ACTV-CUST-NAME PIC X(25).                                  
          05 ACTV-LAST-ACTV-DATE PIC 9(8).                              
          05 ACTV-BALANCE PIC 9(7)V99.                                  
                                                                        
       FD ARCHIVE-OLD RECORDING MODE IS F.                              
       01 ARCH-OLD.                                                     
          05 ARCH-ACCT-ID PIC X(6).                                     
          05 ARCH-CUST-NAME PIC X(25).                                  
          05 ARCH-LAST-ACTV-DATE PIC 9(8).                              
          05 ARCH-BALANCE PIC 9(7)V99.                                  
                                                                        
       FD PROCESS-REP RECORDING MODE IS V.                              
       01 PROC-REP PIC X(50).                                           
                                                                        
       FD ACCT-UNMATCHED RECORDING MODE IS F.                           
       01 ACCT-UNMATCH.                                                 
          05 UNMATCH-ACCT-ID PIC X(6).                                  
          05 UNMATCH-CUST-NAME PIC X(25).                               
          05 UNMATCH-LAST-ACTV-DATE PIC 9(8).                           
          05 UNMATCH-BALANCE PIC 9(7)V99.                               
                                                                        
       WORKING-STORAGE SECTION.                                         

      * FILE STATUSES VARIABLES
       01 WS-FILE-STATUSES.                                             
          05 ACCT-DATA-STATUS PIC X(2).                                 
          05 ACCT-HIST-STATUS PIC X(2).                                 
          05 ACCT-ACT-STATUS PIC X(2).                                  
          05 ARCHIVE-STATUS PIC X(2).                                   
          05 REP-STATUS PIC X(2).                                       
          05 UNMATCH-STATUS PIC X(2).                                   
                                                                        
      * CONTROL FLAGS
       01 WS-FLAGS.                                                     
          05 WS-EOF PIC X(1) VALUE 'N'.                                 
             88 EOF VALUE 'Y'.                                          
                                                                        
      * PROCESSING COUNTERS
       01 WS-COUNTERS.                                                  
          05 TOTAL-COUNT PIC 9(5) VALUE 0.                              
          05 ACTIVE-COUNT PIC 9(5) VALUE 0.                             
          05 ARCHIVE-COUNT PIC 9(5) VALUE 0.                            
          05 ERROR-COUNT PIC 9(5) VALUE 0.                              
          05 UNMATCH-COUNT PIC 9(5) VALUE 0.                            
                                                                        
      * FORMATTED DISPLAY COUNTERS FOR REPORT
       01 WS-DISP-COUNTERS.                                             
          05 TOTAL-COUNT-DISP PIC Z(4)9.                                
          05 ACTIVE-COUNT-DISP PIC Z(4)9.                               
          05 ARCHIVE-COUNT-DISP PIC Z(4)9.                              
          05 ERROR-COUNT-DISP PIC Z(4)9.                                
          05 UNMATCH-COUNT-DISP PIC Z(4)9.                              
                                                                        
      * DATE VARIABLES FOR CUTOFF CALCULATION
       01 WS-CURR-DATE PIC 9(8).                                        
       01 WS-CUTOFF-DATE PIC 9(8).                                      

      * STATUS MESSAGE AND RETURN-CODE DISPLAY
       01 WS-MSG PIC X(50).                                             
       77 WS-RC-DISP PIC -Z9.                                           
                                                                        
      **********************************************
      * OPEN -> INIT -> READ/ROUTE -> REPORT -> CLOSE
      **********************************************
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
           PERFORM OPEN-ALL-FILES.                                      
           PERFORM INITIALIZE-DATA.                                     
           PERFORM READ-ACCT-DATA.                                      
           PERFORM WRITE-FINAL-REPORT.                                  
           PERFORM CLOSE-ALL-FILES.                                     
           STOP RUN.                                                    
                                                                        
      **********************************************
      * ZEROES ALL COUNTERS AND MESSAGE BUFFER.
      * ACCEPTS TODAY'S DATE (YYYYMMDD).
      * COMPUTES CUTOFF = TODAY - 180 DAYS VIA
      * INTEGER-OF-DATE / DATE-OF-INTEGER FUNCTIONS.
      **********************************************
       INITIALIZE-DATA.                                                 
           MOVE ZEROES TO WS-COUNTERS.                                  
           MOVE SPACES TO WS-MSG.                                       
           MOVE ALL SPACES TO PROC-REP.                                 
           ACCEPT WS-CURR-DATE FROM DATE YYYYMMDD.                      
           COMPUTE WS-CUTOFF-DATE = FUNCTION DATE-OF-INTEGER(           
                   FUNCTION INTEGER-OF-DATE(WS-CURR-DATE) - 180).       
                                                                        
      **********************************************
      * OPEN ALL FILES AND CHECK STATUS
      **********************************************
       OPEN-ALL-FILES.                                                  
           OPEN INPUT ACCT-DATA-FILE.                                   
           IF ACCT-DATA-STATUS NOT = '00'                               
              DISPLAY 'ERROR OPENING ACCT-DATA FILE: ' ACCT-DATA-STATUS 
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN INPUT ACCT-HISTORY.                                     
           IF ACCT-HIST-STATUS NOT = '00'                               
              DISPLAY 'ERROR OPENING ACCT-HIST FILE: ' ACCT-HIST-STATUS 
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT ACCT-ACTIVE.                                     
           IF ACCT-ACT-STATUS NOT = '00'                                
              DISPLAY 'ERROR OPENING ACCT-ACTV FILE: ' ACCT-ACT-STATUS  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT ARCHIVE-OLD.                                     
           IF ARCHIVE-STATUS NOT = '00'                                 
              DISPLAY 'ERROR OPENING ARCH-OLD FILE: ' ARCHIVE-STATUS    
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT PROCESS-REP.                                     
           IF REP-STATUS NOT = '00'                                     
              DISPLAY 'ERROR OPENING PROC-REP FILE: ' REP-STATUS        
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT ACCT-UNMATCHED.                                  
           IF UNMATCH-STATUS NOT = '00'                                 
              DISPLAY 'ERROR OPENING ACCT-UNMTCH FILE: ' UNMATCH-STATUS 
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************
      * READS ACCT-DATA-FILE SEQUENTIALLY UNTIL EOF.
      * PER RECORD: INCREMENTS TOTAL-COUNT,
      * THEN CALLS CHECK-ACCT-HIST TO ROUTE ACCOUNT.
      * STOPS ON ANY NON-ZERO READ STATUS.
      **********************************************
       READ-ACCT-DATA.                                                  
           PERFORM UNTIL EOF                                            
              READ ACCT-DATA-FILE                                       
                AT END                                                  
                   SET EOF TO TRUE                                      
                NOT AT END                                              
                   IF ACCT-DATA-STATUS NOT = '00'                       
                      DISPLAY 'ERROR READING ACCT-DATA FILE: '          
                              ACCT-DATA-STATUS                          
                      STOP RUN                                          
                   END-IF                                               
                   ADD 1 TO TOTAL-COUNT                                 
                   PERFORM CHECK-ACCT-HIST                              
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
      **********************************************
      * RANDOM READ VSAM BY ACCT-ID.
      * STATUS '23' (NOT FOUND): WRITE TO UNMATCHED,
      *   INCREMENT UNMATCH-COUNT AND ERROR-COUNT.
      * OTHER NON-ZERO STATUS: LOG ERROR, STOP RUN.
      * FOUND: COMPARE HIST-LAST-TRNS-DATE TO CUTOFF.
      *   DATE < CUTOFF  -> WRITE-ARCHIVE (INACTIVE).
      *   DATE >= CUTOFF -> WRITE-ACTIVE  (ACTIVE).
      **********************************************
       CHECK-ACCT-HIST.                                                 
           MOVE DATA-ACCT-ID TO HIST-ACCT-ID.                           
           READ ACCT-HISTORY                                            
             INVALID KEY                                                
                 ADD 1 TO UNMATCH-COUNT                                 
                 ADD 1 TO ERROR-COUNT
                 PERFORM WRITE-UNMATCHED                                
             NOT INVALID KEY                                            
                 IF HIST-LAST-TRNS-DATE < WS-CUTOFF-DATE                
                    ADD 1 TO ARCHIVE-COUNT                              
                    PERFORM WRITE-ARCHIVE                               
                 ELSE                                                   
                    ADD 1 TO ACTIVE-COUNT                               
                    PERFORM WRITE-ACTIVE                                
                 END-IF                                                 
           END-READ.                                                    
           IF ACCT-HIST-STATUS NOT = '00' AND                           
              ACCT-HIST-STATUS NOT = '23'
              DISPLAY 'VSAM READ ERROR: ' ACCT-HIST-STATUS              
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************
      * COPIES DATA-REC FIELDS TO ACCT-UNMATCH.
      * WRITES RECORD TO GDG UNMATCHED FILE (GDGDD3).
      * STOPS ON ANY NON-ZERO WRITE STATUS.
      **********************************************
       WRITE-UNMATCHED.                                                 
           MOVE DATA-ACCT-ID TO UNMATCH-ACCT-ID.                        
           MOVE DATA-CUST-NAME TO UNMATCH-CUST-NAME.                    
           MOVE DATA-LAST-ACTV-DATE TO UNMATCH-LAST-ACTV-DATE.          
           MOVE DATA-BALANCE TO UNMATCH-BALANCE.                        
           WRITE ACCT-UNMATCH.                                          
           IF UNMATCH-STATUS NOT = '00'                                 
              DISPLAY 'ERROR WRITING UNMATCH FILE: ' UNMATCH-STATUS     
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
                                                                        
      **********************************************
      * COPIES DATA-REC FIELDS TO ARCH-OLD.
      * TRIGGERED WHEN HIST-LAST-TRNS-DATE < CUTOFF.
      * WRITES RECORD TO GDG ARCHIVE FILE (GDGDD2).
      * STOPS ON ANY NON-ZERO WRITE STATUS.
      **********************************************
       WRITE-ARCHIVE.                                                   
           MOVE DATA-ACCT-ID TO ARCH-ACCT-ID.                           
           MOVE DATA-CUST-NAME TO ARCH-CUST-NAME.                       
           MOVE DATA-LAST-ACTV-DATE TO ARCH-LAST-ACTV-DATE.             
           MOVE DATA-BALANCE TO ARCH-BALANCE.                           
           WRITE ARCH-OLD.                                              
           IF ARCHIVE-STATUS NOT = '00'                                 
              DISPLAY 'ERROR WRITING ARCHIVE FILE: ' ARCHIVE-STATUS     
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************
      * COPIES DATA-REC FIELDS TO ACCT-ACTV.
      * TRIGGERED WHEN HIST-LAST-TRNS-DATE >= CUTOFF.
      * WRITES RECORD TO GDG ACTIVE FILE (GDGDD1).
      * STOPS ON ANY NON-ZERO WRITE STATUS.
      **********************************************
       WRITE-ACTIVE.                                                    
           MOVE DATA-ACCT-ID TO ACTV-ACCT-ID.                           
           MOVE DATA-CUST-NAME TO ACTV-CUST-NAME.                       
           MOVE DATA-LAST-ACTV-DATE TO ACTV-LAST-ACTV-DATE.             
           MOVE DATA-BALANCE TO ACTV-BALANCE.                           
           WRITE ACCT-ACTV.                                             
           IF ACCT-ACT-STATUS NOT = '00'                                
              DISPLAY 'ERROR WRITING ACTIVE FILE: ' ACCT-ACT-STATUS     
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************
      * MOVES COUNTERS TO DISPLAY VARIABLES.
      * SETS RETURN-CODE: 0=OK, 4=ERRORS<10, 12=10+.
      * WRITES 6 LINES TO PROCESS-REP (REPPSDD)
      **********************************************
       WRITE-FINAL-REPORT.                                              
           MOVE TOTAL-COUNT TO TOTAL-COUNT-DISP.
           MOVE ACTIVE-COUNT TO ACTIVE-COUNT-DISP.
           MOVE ARCHIVE-COUNT TO ARCHIVE-COUNT-DISP.
           MOVE ERROR-COUNT TO ERROR-COUNT-DISP.
           MOVE UNMATCH-COUNT TO UNMATCH-COUNT-DISP.

           EVALUATE TRUE                                                
               WHEN ERROR-COUNT = 0                                     
                 MOVE 0 TO RETURN-CODE                                  
               WHEN ERROR-COUNT < 10                                    
                 MOVE 4 TO RETURN-CODE                                  
               WHEN OTHER                                               
                 MOVE 12 TO RETURN-CODE                                 
           END-EVALUATE.                                                
                                                                        
           IF ERROR-COUNT = 0                                           
              MOVE 'COMPLETED SUCCESSFULLY' TO WS-MSG                   
           ELSE                                                         
              MOVE 'COMPLETED WITH ERRORS' TO WS-MSG                    
           END-IF.                                                      
                                                                        
           STRING 'PROCESSING DATE: ' DELIMITED BY SIZE                 
                  WS-CURR-DATE DELIMITED BY SIZE                        
                  INTO PROC-REP                                         
           END-STRING.                                                  
           WRITE PROC-REP.                                              
           IF REP-STATUS NOT = '00'
              DISPLAY 'ERROR WRITING PROCESSING DATE TO REPORT FILE: '
                  REP-STATUS
              STOP RUN
           END-IF.
           MOVE ALL SPACES TO PROC-REP.                                 
                                                                        
           STRING 'TOTAL RECORDS READ: ' DELIMITED BY SIZE              
                  FUNCTION TRIM(TOTAL-COUNT-DISP) DELIMITED BY SIZE     
                  INTO PROC-REP                                         
           END-STRING.                                                  
           WRITE PROC-REP.                                              
           IF REP-STATUS NOT = '00'
              DISPLAY 'ERROR WRITING TOTAL RECORD READ TO REPORT'
                      ' FILE: ' REP-STATUS
              STOP RUN
           END-IF.
           MOVE ALL SPACES TO PROC-REP.                                 
                                                                        
           STRING 'ACTIVE ACCOUNTS: ' DELIMITED BY SIZE                 
                  FUNCTION TRIM(ACTIVE-COUNT-DISP) DELIMITED BY SIZE    
                  INTO PROC-REP                                         
           END-STRING.                                                  
           WRITE PROC-REP.                                              
           IF REP-STATUS NOT = '00'
              DISPLAY 'ERROR WRITING ACTIVE ACCOUNTS TO REPORT FILE: '
                   REP-STATUS
              STOP RUN
           END-IF.
           MOVE ALL SPACES TO PROC-REP.                                 
                                                                        
           STRING 'ARCHIVED ACCOUNTS: ' DELIMITED BY SIZE               
                  FUNCTION TRIM(ARCHIVE-COUNT-DISP) DELIMITED BY SIZE   
                  INTO PROC-REP                                         
           END-STRING.                                                  
           WRITE PROC-REP.                                              
           IF REP-STATUS NOT = '00'
              DISPLAY 'ERROR WRITING ARCHIVED ACCOUNT TO REPORT FILE: '
                   REP-STATUS
              STOP RUN
           END-IF.
           MOVE ALL SPACES TO PROC-REP.                                 
                                                                        
           STRING 'UNMATCHED ACCOUNTS: '  DELIMITED BY SIZE             
                  FUNCTION TRIM(UNMATCH-COUNT-DISP) DELIMITED BY SIZE   
                  INTO PROC-REP                                         
           END-STRING.                                                  
           WRITE PROC-REP.                                              
           IF REP-STATUS NOT = '00'
              DISPLAY 'ERROR WRITING UNMATCHED ACCOUNTS TO REPORT'
                      ' FILE: ' REP-STATUS
              STOP RUN
           END-IF.
           MOVE ALL SPACES TO PROC-REP.                                 
                                                                        
           STRING 'PROCESSING STATUS: ' DELIMITED BY SIZE               
                  WS-MSG DELIMITED BY SIZE                              
                  INTO PROC-REP                                         
           END-STRING.                                                  
           WRITE PROC-REP.                                              
           IF REP-STATUS NOT = '00'
              DISPLAY 'ERROR WRITING PROCESSING STATUS TO REPORT'
                      ' FILE: ' REP-STATUS
              STOP RUN
           END-IF.
           MOVE ALL SPACES TO PROC-REP.                                 
                                                                        
           MOVE RETURN-CODE TO WS-RC-DISP                               
           STRING 'RETURN CODE: ' DELIMITED BY SIZE                     
                  FUNCTION TRIM(WS-RC-DISP) DELIMITED BY SIZE           
                  INTO PROC-REP                                         
           END-STRING.                                                  
           WRITE PROC-REP.                                              
           IF REP-STATUS NOT = '00'
              DISPLAY 'ERROR WRITING RETURN CODE TO REPORT FILE: '
                  REP-STATUS
              STOP RUN
           END-IF.
           MOVE ALL SPACES TO PROC-REP.                                 
                                                                        
      **********************************************
      * CLOSE ALL FILES WITH WARNING ON BAD STATUS
      **********************************************
       CLOSE-ALL-FILES.                                                 
           CLOSE ACCT-DATA-FILE.                                        
           IF ACCT-DATA-STATUS NOT = '00'                               
              DISPLAY 'WARNING: ERROR CLOSING ACCT-DATA FILE: '         
                  ACCT-DATA-STATUS                                      
           END-IF.                                                      
                                                                        
           CLOSE ACCT-HISTORY.                                          
           IF ACCT-HIST-STATUS NOT = '00'
              DISPLAY 'WARNING: ERROR CLOSING ACCT-HIST FILE: '
                  ACCT-HIST-STATUS
           END-IF.

           CLOSE ACCT-ACTIVE.                                           
           IF ACCT-ACT-STATUS NOT = '00'
              DISPLAY 'WARNING: ERROR CLOSING ACCT-ACTIVE FILE: '
                  ACCT-ACT-STATUS
           END-IF.

           CLOSE ARCHIVE-OLD.                                           
           IF ARCHIVE-STATUS NOT = '00'
              DISPLAY 'WARNING: ERROR CLOSING ARCHIVE FILE: '
                  ARCHIVE-STATUS
           END-IF.

           CLOSE PROCESS-REP.                                           
           IF REP-STATUS NOT = '00'
              DISPLAY 'WARNING: ERROR CLOSING PROCESS REPORT FILE: '
                  REP-STATUS
           END-IF.

           CLOSE ACCT-UNMATCHED.                                        
           IF UNMATCH-STATUS NOT = '00'
              DISPLAY 'WARNING: ERROR CLOSING UNMATCHED FILE: '
                  UNMATCH-STATUS
           END-IF.
