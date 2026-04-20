      ******************************************************************
      * PRICE UPDATE SYSTEM - PS INPUT + VSAM UPDATE + DB2 AUDIT LOG   *
      *                                                                *
      * PURPOSE:                                                       *
      * READS DAILY PRICE UPDATE FILE, UPDATES PRODUCT PRICES IN VSAM  *
      * MASTER, AND INSERTS CHANGE HISTORY INTO DB2 FOR AUDIT TRAIL.   *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   PHASE 1 - READ PS PRICE UPDATE FILE                          *
      *   PHASE 2 - RANDOM READ VSAM BY PRODUCT-ID:                    *
      *     STATUS '23': PRODUCT NOT FOUND -> LOG AND SKIP.            *
      *     OTHER NON-ZERO: VSAM ERROR -> ROLLBACK, STOP RUN.          *
      *     FOUND: SAVE OLD PRICE, REWRITE VSAM WITH NEW PRICE.        *
      *   PHASE 3 - INSERT OLD/NEW PRICE INTO DB2 HISTORY TABLE        *
      *     SQLCODE 0: LOG UPDATED, INCREMENT COMMIT-COUNT.            *
      *     OTHER -> LOG ERROR, ROLLBACK, STOP RUN.                    *
      *   PHASE 4 - COMMIT EVERY 50 RECORDS (COMMIT-COUNT >= 50).      *
      *             FINAL COMMIT IN CLOSE-ALL-FILES IF COUNT > 0.      *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2026/01/23                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT:  INDD   (PRICE.UPDATE)   - PS DAILY PRICE UPDATES       *
      *         VSAMDD (PRODUCT.MASTER) - VSAM KSDS PRODUCT MASTER     *
      * OUTPUT: OUTDD  (UPDATE.LOG)     - PS UPDATE RESULT LOG         *
      *                                                                *
      * DB2 OBJECTS:                                                   *
      * TB_PRICE_HISTORY - AUDIT TABLE (PROD_ID,OLD_PRICE,NEW_PRICE)   *
      * DCLGEN: TASK25   - HOST VARIABLE DECLARATIONS                  *
      ******************************************************************
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. DB2VSM25.                                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT IN-FILE ASSIGN TO INDD                                
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS IN-STATUS.                                 
                                                                        
      * VSAM KSDS OPENED I-O FOR RANDOM READ AND REWRITE.               
           SELECT VSAM-FILE ASSIGN TO VSAMDD                            
              ORGANIZATION IS INDEXED                                   
              ACCESS MODE IS RANDOM                                     
              RECORD KEY IS VSAM-PROD-ID                                
              FILE STATUS IS VSAM-STATUS.                               
                                                                        
           SELECT OUT-FILE ASSIGN TO OUTDD                              
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS OUT-STATUS.                                
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD IN-FILE RECORDING MODE IS F.                                  
       01 IN-REC.                                                       
          05 IN-PROD-ID PIC X(5).                                       
          05 IN-NEW-PRICE PIC 9(5)V99.                                  
          05 FILLER PIC X(68).                                          
                                                                        
       FD VSAM-FILE.                                                    
       01 VSAM-REC.                                                     
          05 VSAM-PROD-ID PIC X(5).                                     
          05 VSAM-PROD-NAME PIC X(20).                                  
          05 VSAM-CURR-PRICE PIC 9(5)V99.                               
          05 FILLER PIC X(48).                                          
                                                                        
       FD OUT-FILE RECORDING MODE IS F.                                 
       01 OUT-REC PIC X(80).                                            
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * DB2 COMMUNICATION AREA                                          
           EXEC SQL                                                     
             INCLUDE SQLCA                                              
           END-EXEC.                                                    
                                                                        
      * DCLGEN HOST VARIABLES                                           
           EXEC SQL                                                     
             INCLUDE TASK25                                             
           END-EXEC.                                                    
                                                                        
      * FILE STATUS VARIABLES                                           
       01 FILE-STATUSES.                                                
          05 IN-STATUS PIC X(2).                                        
          05 VSAM-STATUS PIC X(2).                                      
          05 OUT-STATUS PIC X(2).                                       
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
          05 WS-EOF PIC X(1) VALUE 'N'.                                 
             88 EOF VALUE 'Y'.                                          
                                                                        
      * COMMIT COUNTER                                                  
       01 WS-COUNTERS.                                                  
          05 COMMIT-COUNT PIC 9(5) VALUE 0.                             
          05 WS-CNT-COMMITS PIC 9(5) VALUE 0.                           
          05 WS-CNT-PROCESSED PIC 9(5) VALUE 0.                         
          05 WS-CNT-UPDATED PIC 9(5) VALUE 0.                           
          05 WS-CNT-NOT-FOUND PIC 9(5) VALUE 0.                         
                                                                        
      * DISPLAY-FORMATTED COUNTERS                                      
       01 WS-DISP-COUNTERS.                                             
          05 WS-CNT-COMMITS-DISP PIC ZZZZ9.                             
          05 WS-CNT-PROCESSED-DISP PIC ZZZZ9.                           
          05 WS-CNT-UPDATED-DISP PIC ZZZZ9.                             
          05 WS-CNT-NOT-FOUND-DISP PIC ZZZZ9.                           
                                                                        
      * EDITED PRICE VALUES FOR OUTPUT                                  
       01 WS-CONVERT.                                                   
          05 WS-OLD-PRICE-CONV PIC ZZZZ9.99.                            
          05 WS-IN-NEW-PRICE-CONV PIC ZZZZ9.99.                         
                                                                        
      * OLD PRICE SAVED BEFORE REWRITE                                  
       01 WS-OLD-PRICE PIC S9(5)V9(2) COMP-3.                           
                                                                        
      * OUTPUT BUFFER                                                   
       01 WS-MSG PIC X(80).                                             
                                                                        
      **********************************************                    
      * OPEN -> PROCESS UPDATES -> CLOSE                                
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
           PERFORM OPEN-ALL-FILES.                                      
           PERFORM PROCESS-PRICE-UPDATES.                               
           PERFORM CLOSE-ALL-FILES.                                     
           PERFORM DISPLAY-SUMMARY.                                     
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * OPEN ALL FILES AND CHECK STATUS                                 
      **********************************************                    
       OPEN-ALL-FILES.                                                  
           OPEN INPUT IN-FILE.                                          
           IF IN-STATUS NOT = '00'                                      
              DISPLAY 'ERROR OPENING INPUT FILE: ' IN-STATUS            
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN I-O VSAM-FILE.                                          
           IF VSAM-STATUS NOT = '00'                                    
              DISPLAY 'ERROR OPENING VSAM FILE: ' VSAM-STATUS           
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT OUT-FILE.                                        
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR OPENING OUTPUT FILE: ' OUT-STATUS          
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * READS IN-FILE TO EOF. PER RECORD:                               
      * CALLS READ-VSAM-PARA, INCREMENTS RECORDS-PROCESSED.             
      * COMMITS EVERY 50 RECORDS (COMMIT-COUNT >= 50).                  
      * ROLLBACK ON ANY ERROR.                                          
      **********************************************                    
       PROCESS-PRICE-UPDATES.                                           
           PERFORM UNTIL EOF                                            
              MOVE SPACES TO OUT-REC                                    
              MOVE SPACES TO WS-MSG                                     
              READ IN-FILE                                              
                AT END                                                  
                   SET EOF TO TRUE                                      
                NOT AT END                                              
                   IF IN-STATUS NOT = '00'                              
                      DISPLAY 'ERROR READING INPUT FILE: ' IN-STATUS    
                      EXEC SQL                                          
                        ROLLBACK WORK                                   
                      END-EXEC                                          
                      STOP RUN                                          
                   END-IF                                               
                   PERFORM READ-VSAM-PARA                               
                   ADD 1 TO WS-CNT-PROCESSED                            
                   IF COMMIT-COUNT >= 50                                
                      EXEC SQL                                          
                        COMMIT WORK                                     
                      END-EXEC                                          
                      IF SQLCODE NOT = 0                                
                         DISPLAY 'BATCH COMMIT ERROR: ' SQLCODE         
                         EXEC SQL                                       
                           ROLLBACK WORK                                
                         END-EXEC                                       
                         STOP RUN                                       
                      END-IF                                            
                      ADD 1 TO WS-CNT-COMMITS                           
                      MOVE 0 TO COMMIT-COUNT                            
                   END-IF                                               
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
      **********************************************                    
      * RANDOM READ VSAM BY IN-PROD-ID.                                 
      * STATUS '23': LOG NOT FOUND, SKIP.                               
      * OTHER NON-ZERO: ROLLBACK, STOP RUN.                             
      * FOUND: SAVE OLD PRICE, REWRITE WITH NEW PRICE,                  
      * THEN CALL WRITE-DB2-PARA.                                       
      * DB2 INSERT INTENTIONALLY PRECEDES VSAM REWRITE.                 
      * IF DB2 INSERT FAILS -> ROLLBACK DB2, STOP RUN.                  
      * VSAM IS NOT TOUCHED SINCE REWRITE NOT YET CALLED.               
      **********************************************                    
       READ-VSAM-PARA.                                                  
           MOVE IN-PROD-ID TO VSAM-PROD-ID.                             
           READ VSAM-FILE                                               
               INVALID KEY                                              
                   IF VSAM-STATUS = '23'                                
                      ADD 1 TO WS-CNT-NOT-FOUND                         
                      MOVE 'PRODUCT NOT FOUND IN VSAM' TO WS-MSG        
                      PERFORM WRITE-LOG-PARA                            
                   ELSE                                                 
                      DISPLAY 'VSAM READ ERROR: ' VSAM-STATUS           
                      EXEC SQL                                          
                        ROLLBACK WORK                                   
                      END-EXEC                                          
                      STOP RUN                                          
                   END-IF                                               
               NOT INVALID KEY                                          
                   MOVE VSAM-CURR-PRICE TO WS-OLD-PRICE                 
                   MOVE IN-NEW-PRICE TO VSAM-CURR-PRICE                 
                   PERFORM WRITE-DB2-PARA                               
                   PERFORM REWRITE-VSAM-PARA                            
           END-READ.                                                    

      **********************************************                    
      * REWRITES VSAM-REC WITH UPDATED PRICE.                           
      * INVALID KEY SHOULD NOT OCCUR AFTER A SUCCESSFUL READ -          
      * CAUGHT BY STATUS CHECK.                                         
      * ROLLBACK AND STOP ON ANY ERROR.                                 
      **********************************************                    
       REWRITE-VSAM-PARA.                                               
           REWRITE VSAM-REC                                             
               INVALID KEY                                              
                   DISPLAY 'REWRITE ERROR STATUS: ' VSAM-STATUS         
                   EXEC SQL                                             
                     ROLLBACK WORK                                      
                   END-EXEC                                             
                   STOP RUN                                             
               NOT INVALID KEY                                          
                   CONTINUE                                             
           END-REWRITE.                                                 
                                                                        
      **********************************************                    
      * INSERTS PRICE CHANGE ROW INTO TB_PRICE_HISTORY.                 
      * SQLCODE 0: INCREMENT RECORDS-UPDATED AND                        
      * COMMIT-COUNT, WRITE LOG LINE.                                   
      * OTHER: WRITE ERROR LOG LINE,                                    
      * ROLLBACK, STOP RUN.                                             
      **********************************************                    
       WRITE-DB2-PARA.                                                  
           MOVE IN-PROD-ID TO PROD-ID.                                  
           MOVE WS-OLD-PRICE TO OLD-PRICE.                              
           MOVE IN-NEW-PRICE TO NEW-PRICE.                              
                                                                        
           EXEC SQL                                                     
             INSERT INTO TB_PRICE_HISTORY                               
             (PROD_ID, OLD_PRICE, NEW_PRICE)                            
             VALUES                                                     
             (:PROD-ID,                                                 
              :OLD-PRICE,                                               
              :NEW-PRICE)                                               
           END-EXEC.                                                    
                                                                        
           IF SQLCODE = 0                                               
              ADD 1 TO WS-CNT-UPDATED                                   
              ADD 1 TO COMMIT-COUNT                                     
              MOVE WS-OLD-PRICE TO WS-OLD-PRICE-CONV                    
              MOVE IN-NEW-PRICE TO WS-IN-NEW-PRICE-CONV                 
              STRING 'OLD_PRICE: ' DELIMITED BY SIZE                    
                     FUNCTION TRIM(WS-OLD-PRICE-CONV) DELIMITED BY SIZE 
                     '    NEW_PRICE: ' DELIMITED BY SIZE                
                     FUNCTION TRIM(WS-IN-NEW-PRICE-CONV) DELIMITED  BY  
                         SIZE                                           
                     ' UPDATED' DELIMITED BY SIZE                       
                     INTO WS-MSG                                        
              END-STRING                                                
              PERFORM WRITE-LOG-PARA                                    
           ELSE                                                         
              DISPLAY 'DB2 INSERT ERROR: ' SQLCODE                      
              MOVE 'DB2 INSERT FAILED' TO WS-MSG                        
              PERFORM WRITE-LOG-PARA                                    
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * WRITES ONE LOG LINE TO UPDATE-LOG-FILE:                         
      * FORMAT: PROD-ID + SPACE + LOG MESSAGE.                          
      * ON WRITE FAILURE: ROLLBACK, STOP RUN.                           
      **********************************************                    
       WRITE-LOG-PARA.                                                  
           STRING IN-PROD-ID DELIMITED BY SIZE                          
                  ' ' DELIMITED BY SIZE                                 
                  WS-MSG DELIMITED BY SIZE                              
                  INTO OUT-REC                                          
           END-STRING.                                                  
           WRITE OUT-REC.                                               
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR WRITING OUTPUT: ' OUT-STATUS               
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * FINAL COMMIT AND CLOSE ALL FILES                                
      **********************************************                    
       CLOSE-ALL-FILES.                                                 
           IF COMMIT-COUNT > 0                                          
              EXEC SQL                                                  
                COMMIT WORK                                             
              END-EXEC                                                  
              IF SQLCODE NOT = 0                                        
                 DISPLAY 'FINAL COMMIT ERROR: ' SQLCODE                 
                 EXEC SQL                                               
                   ROLLBACK WORK                                        
                 END-EXEC                                               
                 STOP RUN                                               
              END-IF                                                    
              ADD 1 TO WS-CNT-COMMITS                                   
              MOVE 0 TO COMMIT-COUNT                                    
           END-IF.                                                      
                                                                        
           CLOSE IN-FILE.                                               
           IF IN-STATUS NOT = '00'                                      
              DISPLAY 'WARNING: ERROR CLOSING INPUT FILE: ' IN-STATUS   
           END-IF.                                                      
                                                                        
           CLOSE VSAM-FILE.                                             
           IF VSAM-STATUS NOT = '00'                                    
              DISPLAY 'WARNING: ERROR CLOSING VSAM FILE: ' VSAM-STATUS  
           END-IF.                                                      
                                                                        
           CLOSE OUT-FILE.                                              
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'WARNING: ERROR CLOSING OUTPUT FILE: ' OUT-STATUS 
           END-IF.                                                      
                                                                        
       DISPLAY-SUMMARY.                                                 
           MOVE WS-CNT-COMMITS TO WS-CNT-COMMITS-DISP.                  
           MOVE WS-CNT-PROCESSED TO WS-CNT-PROCESSED-DISP.              
           MOVE WS-CNT-UPDATED TO WS-CNT-UPDATED-DISP.                  
           MOVE WS-CNT-NOT-FOUND TO WS-CNT-NOT-FOUND-DISP.              
                                                                        
           DISPLAY '========================================'.          
           DISPLAY 'PRICE UPDATE SUMMARY'.                              
           DISPLAY '========================================'.          
           DISPLAY 'COMMIT COUNT: '                                     
                   FUNCTION TRIM(WS-CNT-COMMITS-DISP).                  
           DISPLAY 'RECORDS PROCESSED: '                                
                   FUNCTION TRIM(WS-CNT-PROCESSED-DISP).                
           DISPLAY 'RECORDS UPDATED: '                                  
                   FUNCTION TRIM(WS-CNT-UPDATED-DISP).                  
           DISPLAY 'RECORDS NOT FOUND: '                                
                   FUNCTION TRIM(WS-CNT-NOT-FOUND-DISP).                
           DISPLAY '========================================'.          
