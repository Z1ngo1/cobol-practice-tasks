      ******************************************************************
      * CREDIT CARD TRANSACTION VALIDATION SYSTEM                      *
      *                                                                *
      * PURPOSE:                                                       *
      * VALIDATES DAILY CARD TRANSACTIONS AGAINST CARD MASTER FILE.    *
      * CHECKS CARD EXISTENCE, STATUS (ACTIVE/BLOCKED), AND EXPIRY     *
      * DATE. SPLITS TRANSACTIONS INTO APPROVED AND DECLINED FILES.    *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   1. READ TRANSACTION RECORD (TRANS-ID, CARD-NUM, AMOUNT)      *
      *   2. LOOKUP CARD IN VSAM BY CARD-NUM (RANDOM ACCESS)           *
      *   3. VALIDATION CHECKS:                                        *
      *      - CHECK 1: CARD EXISTS (FILE STATUS 23 = NOT FOUND)       *
      *      - CHECK 2: CARD STATUS = 'A' (ACTIVE, NOT BLOCKED)        *
      *      - CHECK 3: CARD NOT EXPIRED (EXP-DATE >= CURRENT DATE)    *
      *   4. IF ALL CHECKS PASS: WRITE TO APPROVED FILE                *
      *   5. IF ANY CHECK FAILS: WRITE TO DECLINED FILE WITH REASON    *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2025/12/27                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT:  VSAMDD (CARD.MASTER) - CARD MASTER FILE (VSAM KSDS)    *
      * INPUT:  TRNSDD (TRANS.DAILY) - DAILY TRANSACTIONS (PS, 80 B)   *
      * OUTPUT: APRVDD (APPROVED.FILE) - APPROVED TRANS (PS, 80 B)     *
      * OUTPUT: DECLDD (DECLINED.FILE) - DECLINED TRANS (PS, 80 B)     *
      ******************************************************************
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. VSMJOB11.                                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT CARD-MASTER-FILE ASSIGN TO VSAMDD                     
               ORGANIZATION IS INDEXED                                  
               ACCESS MODE IS RANDOM                                    
               RECORD KEY IS CARD-NUMBER                                
               FILE STATUS IS VSAM-STATUS.                              
                                                                        
           SELECT DAILY-TRANS-FILE ASSIGN TO TRNSDD                     
               ORGANIZATION IS SEQUENTIAL                               
               FILE STATUS IS TRANS-STATUS.                             
                                                                        
           SELECT APPROVED-TRANS-FILE ASSIGN TO APRVDD                  
               ORGANIZATION IS SEQUENTIAL                               
               FILE STATUS IS APRV-STATUS.                              
                                                                        
           SELECT DECLINED-TRANS-FILE ASSIGN TO DECLDD                  
               ORGANIZATION IS SEQUENTIAL                               
               FILE STATUS IS DECL-STATUS.                              
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD CARD-MASTER-FILE.                                             
       01 CARD-MASTER-REC.                                              
          05 CARD-NUMBER PIC 9(16).                                     
          05 CARD-OWNER-NAME PIC X(20).                                 
          05 CARD-EXPIRY-DATE PIC X(4).                                 
          05 CARD-STATUS PIC X(1).                                      
                                                                        
       FD DAILY-TRANS-FILE RECORDING MODE IS F.                         
       01 TRANSACTION-REC.                                              
          05 TRANSACTION-ID PIC X(5).                                   
          05 TRANSACTION-CARD-NUM PIC 9(16).                            
          05 TRANSACTION-AMOUNT PIC 9(5)V99.                            
          05 FILLER PIC X(52).                                          
                                                                        
       FD APPROVED-TRANS-FILE RECORDING MODE IS F.                      
       01 APPROVED-REC.                                                 
          05 APPROVED-TRANS-ID PIC X(5).                                
          05 FILLER PIC X(1).                                           
          05 APPROVED-CARD-NUM PIC 9(16).                               
          05 FILLER PIC X(1).                                           
          05 APPROVED-AMOUNT PIC $$$$9.99.                              
          05 FILLER PIC X(49).                                          
                                                                        
       FD DECLINED-TRANS-FILE RECORDING MODE IS F.                      
       01 DECLINED-REC.                                                 
          05 DECLINED-TRANS-ID PIC X(5).                                
          05 FILLER PIC X(1).                                           
          05 DECLINED-CARD-NUM PIC 9(16).                               
          05 FILLER PIC X(1).                                           
          05 DECLINED-AMOUNT PIC $$$$9.99.                              
          05 FILLER PIC X(1).                                           
          05 DECLINE-REASON PIC X(10).                                  
          05 FILLER PIC X(38).                                          
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * FILE-STATUS VARIABLES                                           
       01 FILE-STATUSES.                                                
          05 VSAM-STATUS PIC X(2).                                      
          05 TRANS-STATUS PIC X(2).                                     
          05 APRV-STATUS PIC X(2).                                      
          05 DECL-STATUS PIC X(2).                                      
                                                                        
      * CONTROL FLAGS                                                   
       01 FLAGS.                                                        
          05 WS-EOF PIC X(1) VALUE 'N'.                                 
             88 EOF VALUE 'Y'.                                          
                                                                        
      * CURRENT DATE (YYYYMMDD FROM SYSTEM)                             
       01 WS-CUR-DATE-GROUP.                                            
          05 WS-CUR-YYYY PIC 9(4).                                      
          05 WS-CUR-MM PIC 9(2).                                        
          05 WS-CUR-DD PIC 9(2).                                        
                                                                        
      * DATE COMPARISON VARIABLES (YY FORMAT)                           
       01 WS-COMPARE-VARS.                                              
          05 WS-CUR-YY PIC 9(2).                                        
          05 WS-CARD-MM PIC 9(2).                                       
          05 WS-CARD-YY PIC 9(2).                                       
                                                                        
      * DECLINE REASON CODE                                             
       01 WS-REASON PIC X(10).                                          
                                                                        
      * STATISTICS COUNTERS                                             
       01 WS-COUNTERS.                                                  
          05 TOTAL-TRANSACTIONS PIC 9(5) VALUE 0.                       
          05 TOTAL-APPROVED PIC 9(5) VALUE 0.                           
          05 TOTAL-DECLINED PIC 9(5) VALUE 0.                           
          05 TOTAL-NOT-FOUND PIC 9(5) VALUE 0.                          
          05 TOTAL-BLOCKED PIC 9(5) VALUE 0.                            
          05 TOTAL-EXPIRED PIC 9(5) VALUE 0.                            
                                                                        
      * DISPLAY-FORMATTED COUNTERS                                      
       01 WS-DISP-COUNTERS.                                             
          05 TOTAL-TRANSACTIONS-DISP PIC Z(4)9.                         
          05 TOTAL-APPROVED-DISP PIC Z(4)9.                             
          05 TOTAL-DECLINED-DISP PIC Z(4)9.                             
          05 TOTAL-NOT-FOUND-DISP PIC Z(4)9.                            
          05 TOTAL-BLOCKED-DISP PIC Z(4)9.                              
          05 TOTAL-EXPIRED-DISP PIC Z(4)9.                              
                                                                        
      **********************************************                    
      * MAIN FLOW: INIT -> OPEN -> PROCESS -> CLOSE -> REPORT           
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
           PERFORM INIT-PROCESS.                                        
           PERFORM OPEN-ALL-FILES.                                      
           PERFORM PROCESS-TRANS.                                       
           PERFORM CLOSE-ALL-FILES.                                     
           PERFORM DISPLAY-SUMMARY.                                     
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * INITIALIZE: GET CURRENT DATE FROM SYSTEM                        
      **********************************************                    
       INIT-PROCESS.                                                    
           ACCEPT WS-CUR-DATE-GROUP FROM DATE YYYYMMDD.                 
           MOVE WS-CUR-YYYY(3:2) TO WS-CUR-YY.                          
           DISPLAY 'CURRENT DATE: ' WS-CUR-YYYY '/' WS-CUR-MM '/'       
                    WS-CUR-DD.                                          
           DISPLAY 'COMPARE YEAR: ' WS-CUR-YY.                          
           DISPLAY 'COMPARE MONTH: ' WS-CUR-MM.                         
                                                                        
      **********************************************                    
      * OPEN ALL FILES AND CHECK STATUS                                 
      **********************************************                    
       OPEN-ALL-FILES.                                                  
           OPEN INPUT CARD-MASTER-FILE.                                 
           IF VSAM-STATUS NOT = '00'                                    
              DISPLAY 'ERROR OPENING CARD MASTER FILE: '                
                       VSAM-STATUS                                      
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN INPUT DAILY-TRANS-FILE.                                 
           IF TRANS-STATUS NOT = '00'                                   
              DISPLAY 'ERROR OPENING TRANSACTIONS FILE: ' TRANS-STATUS  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT APPROVED-TRANS-FILE.                             
           IF APRV-STATUS NOT = '00'                                    
              DISPLAY 'ERROR OPENING APPROVED FILE: ' APRV-STATUS       
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT DECLINED-TRANS-FILE.                             
           IF DECL-STATUS NOT = '00'                                    
              DISPLAY 'ERROR OPENING DECLINED FILE: ' DECL-STATUS       
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * READ TRANSACTIONS AND PROCESS EACH RECORD                       
      **********************************************                    
       PROCESS-TRANS.                                                   
           PERFORM UNTIL EOF                                            
              MOVE SPACES TO APPROVED-REC                               
              MOVE SPACES TO DECLINED-REC                               
              READ DAILY-TRANS-FILE                                     
                AT END                                                  
                   SET EOF TO TRUE                                      
                NOT AT END                                              
                   IF TRANS-STATUS = '00'                               
                      ADD 1 TO TOTAL-TRANSACTIONS                       
                      PERFORM PROCESS-TRANSACTION                       
                   ELSE                                                 
                      DISPLAY 'ERROR READING TRANS FILE: ' TRANS-STATUS 
                      STOP RUN                                          
                   END-IF                                               
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
      **********************************************                    
      * PROCESS SINGLE TRANSACTION: LOOKUP AND VALIDATE                 
      **********************************************                    
       PROCESS-TRANSACTION.                                             
           MOVE SPACES TO WS-REASON.                                    
           MOVE TRANSACTION-CARD-NUM TO CARD-NUMBER.                    
           READ CARD-MASTER-FILE                                        
           IF VSAM-STATUS = '23'                                        
              MOVE 'NOT FOUND' TO WS-REASON                             
              PERFORM WRITE-DECLINED-TRANS                              
           ELSE                                                         
             IF VSAM-STATUS = '00'                                      
                PERFORM VALIDATE-STATUS                                 
             ELSE                                                       
                DISPLAY 'CRITICAL VSAM READ ERROR: ' VSAM-STATUS        
                DISPLAY 'TRANSACTION ID: ' TRANSACTION-ID               
                DISPLAY 'CARD NUMBER: ' TRANSACTION-CARD-NUM            
                STOP RUN                                                
             END-IF                                                     
           END-IF.                                                      
                                                                        
      **********************************************                    
      * VALIDATE CARD STATUS (ACTIVE OR BLOCKED)                        
      **********************************************                    
       VALIDATE-STATUS.                                                 
           IF CARD-STATUS = 'B'                                         
              MOVE 'BLOCKED' TO WS-REASON                               
              PERFORM WRITE-DECLINED-TRANS                              
           ELSE                                                         
              PERFORM VALIDATE-EXPIRY                                   
           END-IF.                                                      
                                                                        
      **********************************************                    
      * VALIDATE CARD EXPIRY DATE (MMYY FORMAT)                         
      **********************************************                    
       VALIDATE-EXPIRY.                                                 
           MOVE CARD-EXPIRY-DATE(1:2) TO WS-CARD-MM.                    
           MOVE CARD-EXPIRY-DATE(3:2) TO WS-CARD-YY.                    
           IF WS-CARD-YY < WS-CUR-YY                                    
              MOVE 'EXPIRED' TO WS-REASON                               
              PERFORM WRITE-DECLINED-TRANS                              
           ELSE                                                         
              IF WS-CARD-YY = WS-CUR-YY                                 
                 IF WS-CARD-MM < WS-CUR-MM                              
                    MOVE 'EXPIRED' TO WS-REASON                         
                    PERFORM WRITE-DECLINED-TRANS                        
                 ELSE                                                   
                    PERFORM WRITE-APPROVED-TRANS                        
                 END-IF                                                 
              ELSE                                                      
                 PERFORM WRITE-APPROVED-TRANS                           
              END-IF                                                    
           END-IF.                                                      
                                                                        
      **********************************************                    
      * WRITE APPROVED TRANSACTION TO OUTPUT FILE                       
      **********************************************                    
       WRITE-APPROVED-TRANS.                                            
           MOVE TRANSACTION-ID TO APPROVED-TRANS-ID.                    
           MOVE TRANSACTION-CARD-NUM TO APPROVED-CARD-NUM.              
           MOVE TRANSACTION-AMOUNT TO APPROVED-AMOUNT.                  
           WRITE APPROVED-REC.                                          
           IF APRV-STATUS NOT = '00'                                    
              DISPLAY 'ERROR WRITING APPROVED FILE: ' APRV-STATUS       
              DISPLAY 'TRANSACTION ID: ' TRANSACTION-ID                 
              STOP RUN                                                  
           ELSE                                                         
              ADD 1 TO TOTAL-APPROVED                                   
           END-IF.                                                      
                                                                        
      **********************************************                    
      * WRITE DECLINED TRANSACTION WITH REASON CODE                     
      **********************************************                    
       WRITE-DECLINED-TRANS.                                            
           MOVE TRANSACTION-ID TO DECLINED-TRANS-ID.                    
           MOVE TRANSACTION-CARD-NUM TO DECLINED-CARD-NUM.              
           MOVE TRANSACTION-AMOUNT TO DECLINED-AMOUNT.                  
           MOVE WS-REASON TO DECLINE-REASON.                            
           WRITE DECLINED-REC.                                          
           IF DECL-STATUS NOT = '00'                                    
              DISPLAY 'ERROR WRITING DECLINED FILE: ' DECL-STATUS       
              DISPLAY 'TRANSACTION ID: ' TRANSACTION-ID                 
              STOP RUN                                                  
           ELSE                                                         
              ADD 1 TO TOTAL-DECLINED                                   
                                                                        
              EVALUATE WS-REASON                                        
                  WHEN 'NOT FOUND'                                      
                    ADD 1 TO TOTAL-NOT-FOUND                            
                  WHEN 'BLOCKED'                                        
                    ADD 1 TO TOTAL-BLOCKED                              
                  WHEN 'EXPIRED'                                        
                    ADD 1 TO TOTAL-EXPIRED                              
              END-EVALUATE                                              
           END-IF.                                                      
                                                                        
      **********************************************                    
      * CLOSE ALL FILES AND CHECK STATUS                                
      **********************************************                    
       CLOSE-ALL-FILES.                                                 
           CLOSE CARD-MASTER-FILE.                                      
           IF VSAM-STATUS NOT = '00'                                    
              DISPLAY 'WARNING: ERROR CLOSING CARD MASTER: '            
                       VSAM-STATUS                                      
           END-IF.                                                      
                                                                        
           CLOSE DAILY-TRANS-FILE.                                      
           IF TRANS-STATUS NOT = '00'                                   
              DISPLAY 'WARNING: ERROR CLOSING TRANSACTIONS: '           
                       TRANS-STATUS                                     
           END-IF.                                                      
                                                                        
           CLOSE APPROVED-TRANS-FILE.                                   
           IF APRV-STATUS NOT = '00'                                    
              DISPLAY 'WARNING: ERROR CLOSING APPROVED: ' APRV-STATUS   
           END-IF.                                                      
                                                                        
           CLOSE DECLINED-TRANS-FILE.                                   
           IF DECL-STATUS NOT = '00'                                    
              DISPLAY 'WARNING: ERROR CLOSING DECLINED: ' DECL-STATUS   
           END-IF.                                                      
                                                                        
      **********************************************                    
      * DISPLAY SUMMARY STATISTICS TO SYSOUT                            
      **********************************************                    
       DISPLAY-SUMMARY.                                                 
           MOVE TOTAL-TRANSACTIONS TO TOTAL-TRANSACTIONS-DISP.          
           MOVE TOTAL-APPROVED TO TOTAL-APPROVED-DISP.                  
           MOVE TOTAL-DECLINED TO TOTAL-DECLINED-DISP.                  
           MOVE TOTAL-NOT-FOUND TO TOTAL-NOT-FOUND-DISP.                
           MOVE TOTAL-BLOCKED TO TOTAL-BLOCKED-DISP.                    
           MOVE TOTAL-EXPIRED TO TOTAL-EXPIRED-DISP.                    
                                                                        
           DISPLAY '========================================'.          
           DISPLAY 'CARD VALIDATION SUMMARY'.                           
           DISPLAY '========================================'.          
           DISPLAY 'TOTAL TRANSACTIONS: ' TOTAL-TRANSACTIONS-DISP.      
           DISPLAY 'APPROVED:           ' TOTAL-APPROVED-DISP.          
           DISPLAY 'DECLINED:           ' TOTAL-DECLINED-DISP.          
           DISPLAY '  NOT FOUND:        ' TOTAL-NOT-FOUND-DISP.         
           DISPLAY '  BLOCKED:          ' TOTAL-BLOCKED-DISP.           
           DISPLAY '  EXPIRED:          ' TOTAL-EXPIRED-DISP.           
           DISPLAY '========================================'.          
