      ******************************************************************
      * ESDS TRANSACTION LOG - CLIENT REPORT                           *
      *                                                                *
      * PURPOSE:                                                       *
      * READS CLIENT LIST (PS), FOR EACH CLIENT PERFORMS FULL          *
      * SEQUENTIAL SCAN OF ESDS TRANSACTION LOG, ACCUMULATES DEBIT     *
      * AND CREDIT TOTALS, COMPUTES NET RESULT AND WRITES SUMMARY      *
      * LINE TO OUTPUT REPORT FILE (PS).                               *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   PHASE 1 - READ ACCT-LIST (PS):                               *
      *     FOR EACH ACCT-ID: RESET TOTALS, PERFORM FULL ESDS SCAN.    *
      *   PHASE 2 - SCAN TRANS-LOG (ESDS) PER CLIENT:                  *
      *     OPEN ESDS, READ ALL RECORDS SEQUENTIALLY.                  *
      *     TRANS-ACCT-ID = WS-ACCT-ID:                                *
      *       TRANS-TYPE 'D' -> ADD TRANS-AMOUNT TO WS-TOTAL-DEBIT.    *
      *       TRANS-TYPE 'C' -> ADD TRANS-AMOUNT TO WS-TOTAL-CREDIT.   *
      *       OTHER TYPE -> SILENTLY IGNORED.                          *
      *     CLOSE ESDS AFTER EACH CLIENT SCAN.                         *
      *   PHASE 3 - COMPUTE AND WRITE REPORT LINE:                     *
      *     WS-NET = WS-TOTAL-CREDIT - WS-TOTAL-DEBIT.                 *
      *     BOTH TOTALS = 0 -> STATUS 'NO TRANS'.                      *
      *     ANY TOTAL  != 0 -> STATUS 'OK'.                            *
      *     WRITE: ACCT-ID, DEBIT, CREDIT, NET, STATUS.                *
      *                                                                *
      * NOTE: TRANS-LOG IS OPENED/CLOSED PER CLIENT IN PROCESS-TRANS-LOG
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2026/02/08                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT: ACCT (ACCT.LIST) - PS CLIENT ID LIST                    *
      *        AS-TRNS (TRANS.LOG.ESDS) - ESDS TRANSACTION LOG         *
      * OUTPUT: ACCTREP (ACCT.REPORT) - PS CLIENT SUMMARY REPORT       *
      ******************************************************************
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. ESDS28.                                              
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT TRANS-LOG ASSIGN TO AS-TRNS                           
             ORGANIZATION IS SEQUENTIAL                                 
             ACCESS MODE IS SEQUENTIAL                                  
             FILE STATUS IS TRANS-LOG-STATUS.                           
                                                                        
           SELECT ACCT-LIST ASSIGN TO ACCT                              
             ORGANIZATION IS SEQUENTIAL                                 
             FILE STATUS IS ACCT-LIST-STATUS.                           
                                                                        
           SELECT ACCT-REPORT ASSIGN TO ACCTREP                         
             ORGANIZATION IS SEQUENTIAL                                 
             FILE STATUS IS ACCT-REPORT-STATUS.                         
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD TRANS-LOG.                                                    
       01 TRANS-REC.                                                    
          05 TRANS-ACCT-ID PIC X(6).                                    
          05 TRANS-DATE PIC X(8).                                       
          05 TRANS-TYPE PIC X(1).                                       
          05 TRANS-AMOUNT PIC 9(7)V99.                                  
          05 FILLER PIC X(56).                                          
                                                                        
       FD ACCT-LIST RECORDING MODE IS F.                                
       01 ACCT-LIST-REC.                                                
          05 ACCT-LIST-ID PIC X(6).                                     
          05 FILLER PIC X(74).                                          
                                                                        
       FD ACCT-REPORT RECORDING MODE IS V.                              
       01 ACCT-REPORT-REC PIC X(60).                                    
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
          05 ACCT-LIST-FLAG PIC X(1) VALUE 'N'.                         
             88 ACCT-LIST-EOF VALUE 'Y'.                                
          05 TRANS-LOG-FLAG PIC X(1) VALUE 'N'.                         
             88 TRANS-LOG-EOF VALUE 'Y'.                                
             88 TRANS-LOG-NOT-EOF VALUE 'N'.                            
                                                                        
      * FILE STATUS VARIABLES                                           
       01 WS-FILE-STATUSES.                                             
          05 TRANS-LOG-STATUS PIC X(2).                                 
          05 ACCT-LIST-STATUS PIC X(2).                                 
          05 ACCT-REPORT-STATUS PIC X(2).                               
                                                                        
      * PROCESSING VARIABLES                                            
       01 WS-ACCT-ID PIC X(6).                                          
       01 WS-TOTAL-DEBIT PIC S9(7)V99 COMP-3.                           
       01 WS-TOTAL-CREDIT PIC S9(7)V99 COMP-3.                          
       01 WS-NET PIC S9(7)V99.                                          
       01 WS-REPORT-STATUS PIC X(10).                                   
                                                                        
      * FORMATTED DISPLAY VARIABLES FOR REPORT                          
       01 DISP-TOTAL-DEBIT PIC Z(6)9.99.                                
       01 DISP-TOTAL-CREDIT PIC Z(6)9.99.                               
       01 DISP-NET PIC ++++++9.99.                                      
                                                                        
      **********************************************                    
      * OPEN -> READ ACCT-LIST -> CLOSE                                 
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
           PERFORM OPEN-ALL-FILES.                                      
           PERFORM READ-ACCT-LIST.                                      
           PERFORM CLOSE-ALL-FILES.                                     
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * READS ACCT-LIST SEQUENTIALLY UNTIL EOF.                         
      * PER RECORD: MOVES ACCT-LIST-ID TO WS-ACCT-ID,                   
      * THEN CALLS PROCESS-TRANS-LOG FOR FULL ESDS SCAN.                
      * STOPS ON ANY NON-ZERO READ STATUS.                              
      **********************************************                    
       READ-ACCT-LIST.                                                  
           PERFORM UNTIL ACCT-LIST-EOF                                  
              READ ACCT-LIST                                            
                AT END                                                  
                   SET ACCT-LIST-EOF TO TRUE                            
                NOT AT END                                              
                   IF ACCT-LIST-STATUS NOT = '00'                       
                      DISPLAY 'ERROR READ ACCT-LIST FILE: '             
                               ACCT-LIST-STATUS                         
                      STOP RUN                                          
                   END-IF                                               
                   MOVE ACCT-LIST-ID TO WS-ACCT-ID                      
                   PERFORM PROCESS-TRANS-LOG                            
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
      **********************************************                    
      * RESETS ALL TOTALS AND BUFFERS FOR CURRENT CLIENT.               
      * OPENS TRANS-LOG (ESDS), READS ALL RECORDS UNTIL EOF.            
      * CALLS PROCESS-TRANS-TYPE FOR MATCHING ACCT-ID ONLY.             
      * CLOSES TRANS-LOG AFTER FULL SCAN.                               
      * CALLS COMPUTE-NET-STATUS AND WRITE-ACCT-REPORT.                 
      **********************************************                    
       PROCESS-TRANS-LOG.                                               
           MOVE ZERO TO WS-TOTAL-DEBIT.                                 
           MOVE ZERO TO WS-TOTAL-CREDIT.                                
           MOVE SPACES TO WS-REPORT-STATUS.                             
           MOVE SPACES TO ACCT-REPORT-REC.                              
                                                                        
           OPEN INPUT TRANS-LOG.                                        
           IF TRANS-LOG-STATUS NOT = '00'                               
              DISPLAY 'ERROR OPENING TRANS-LOG FILE: ' TRANS-LOG-STATUS 
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           SET TRANS-LOG-NOT-EOF TO TRUE.                               
           PERFORM UNTIL TRANS-LOG-EOF                                  
              READ TRANS-LOG                                            
                AT END                                                  
                   SET TRANS-LOG-EOF TO TRUE                            
                NOT AT END                                              
                   IF TRANS-LOG-STATUS NOT = '00'                       
                      DISPLAY 'ERROR READ TRANS-LOG FILE: '             
                               TRANS-LOG-STATUS                         
                      STOP RUN                                          
                   END-IF                                               
                   IF TRANS-ACCT-ID = WS-ACCT-ID                        
                      PERFORM PROCESS-TRANS-TYPE                        
                   END-IF                                               
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
           CLOSE TRANS-LOG.                                             
           IF TRANS-LOG-STATUS NOT = '00'                               
              DISPLAY 'WARNING: ERROR CLOSING TRANS-LOG FILE: '         
                       TRANS-LOG-STATUS                                 
           END-IF.                                                      
                                                                        
           PERFORM COMPUTE-NET-STATUS.                                  
           PERFORM WRITE-ACCT-REPORT.                                   
                                                                        
      **********************************************                    
      * MOVES TOTALS TO DISPLAY VARIABLES.                              
      * BUILDS REPORT LINE VIA STRING:                                  
      * WRITES TO ACCT-REPORT.                                          
      * STOPS ON ANY NON-ZERO WRITE STATUS.                             
      **********************************************                    
       WRITE-ACCT-REPORT.                                               
           MOVE WS-TOTAL-DEBIT TO DISP-TOTAL-DEBIT.                     
           MOVE WS-TOTAL-CREDIT TO DISP-TOTAL-CREDIT.                   
           MOVE WS-NET TO DISP-NET.                                     
           STRING WS-ACCT-ID DELIMITED BY SIZE                          
                  ' ' DELIMITED BY SIZE                                 
                  FUNCTION TRIM(DISP-TOTAL-DEBIT) DELIMITED BY SIZE     
                  ' ' DELIMITED BY SIZE                                 
                  FUNCTION TRIM(DISP-TOTAL-CREDIT) DELIMITED BY SIZE    
                  ' ' DELIMITED BY SIZE                                 
                  FUNCTION TRIM(DISP-NET) DELIMITED BY SIZE             
                  ' ' DELIMITED BY SIZE                                 
                  WS-REPORT-STATUS DELIMITED BY SIZE                    
                  INTO ACCT-REPORT-REC                                  
           END-STRING.                                                  
           WRITE ACCT-REPORT-REC.                                       
           IF ACCT-REPORT-STATUS NOT = '00'                             
              DISPLAY 'ERROR WRITING ACCT-REPORT FILE: '                
                       ACCT-REPORT-STATUS                               
              DISPLAY 'ACCT ID: ' WS-ACCT-ID                            
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * COMPUTES WS-NET = WS-TOTAL-CREDIT - WS-TOTAL-DEBIT.             
      * BOTH TOTALS = 0 -> STATUS 'NO TRANS'.                           
      * ANY TOTAL  != 0 -> STATUS 'OK'.                                 
      **********************************************                    
       COMPUTE-NET-STATUS.                                              
           COMPUTE WS-NET = WS-TOTAL-CREDIT - WS-TOTAL-DEBIT.           
           IF WS-TOTAL-CREDIT = 0 AND WS-TOTAL-DEBIT = 0                
              MOVE 'NO TRANS' TO WS-REPORT-STATUS                       
           ELSE                                                         
              MOVE 'OK' TO WS-REPORT-STATUS                             
           END-IF.                                                      
                                                                        
      **********************************************                    
      * ACCUMULATES DEBIT OR CREDIT BY TRANS-TYPE.                      
      * 'D' -> ADD TRANS-AMOUNT TO WS-TOTAL-DEBIT.                      
      * 'C' -> ADD TRANS-AMOUNT TO WS-TOTAL-CREDIT.                     
      * OTHER TRANS-TYPE -> SILENTLY IGNORED.                           
      **********************************************                    
       PROCESS-TRANS-TYPE.                                              
           EVALUATE TRUE                                                
              WHEN TRANS-TYPE = 'D'                                     
                ADD TRANS-AMOUNT TO WS-TOTAL-DEBIT                      
              WHEN TRANS-TYPE = 'C'                                     
                ADD TRANS-AMOUNT TO WS-TOTAL-CREDIT                     
           END-EVALUATE.                                                
                                                                        
      **********************************************                    
      * OPEN ALL FILES AND CHECK STATUS                                 
      **********************************************                    
       OPEN-ALL-FILES.                                                  
           OPEN INPUT ACCT-LIST.                                        
           IF ACCT-LIST-STATUS NOT = '00'                               
              DISPLAY 'ERROR OPENING ACCT-LIST FILE: ' ACCT-LIST-STATUS 
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT ACCT-REPORT.                                     
           IF ACCT-REPORT-STATUS NOT = '00'                             
              DISPLAY 'ERROR OPENING ACCT-REPORT FILE: '                
                       ACCT-REPORT-STATUS                               
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * CLOSE ALL FILES AND CHECK STATUS                                
      **********************************************                    
       CLOSE-ALL-FILES.                                                 
           CLOSE ACCT-LIST.                                             
           IF ACCT-LIST-STATUS NOT = '00'                               
              DISPLAY 'WARNING: ERROR CLOSING ACCT-LIST FILE: '         
                       ACCT-LIST-STATUS                                 
           END-IF.                                                      
                                                                        
           CLOSE ACCT-REPORT.                                           
           IF ACCT-REPORT-STATUS NOT = '00'                             
              DISPLAY 'WARNING: ERROR CLOSING ACCT-REPORT FILE: '       
                       ACCT-REPORT-STATUS                               
           END-IF.                                                      
