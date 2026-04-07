      ******************************************************************
      *         BANKING TRANSACTION VSAM UPDATE SYSTEM                 *
      *                                                                *
      * PURPOSE:                                                       *
      * READS TRANSACTION FILE AND UPDATES CUSTOMER ACCOUNT BALANCES   *
      * IN VSAM KSDS MASTER. GENERATES ERROR REPORT FOR REJECTS.       *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   DEPOSIT  (D): NEW-BAL = CURRENT-BAL + AMOUNT                 *
      *   WITHDRAW (W): IF BAL >= AMOUNT -> SUBTRACT, REWRITE          *
      *                 ELSE             -> REJECT, WRITE ERROR        *
      *   ACCT NOT FOUND                 -> REJECT, WRITE ERROR        *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2025/12/11                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT:   INDD (TRANS.FILE) - TRANSACTION INPUT FILE (PS)       *
      * I-O:     EMPDD (ACCT.MASTER) - ACCOUNT MASTER FILE (KSDS)      *
      * OUTPUT:  REPDD (REPORT.FILE) - ERROR REPORT FILE (PS)          *
      ******************************************************************
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. VSAMJO5.                                             
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT ACCT-MASTER ASSIGN TO EMPDD                           
               ORGANIZATION IS INDEXED                                  
               ACCESS MODE IS RANDOM                                    
               RECORD KEY IS ACCT-ID                                    
               FILE STATUS IS ACCT-STATUS.                              
                                                                        
           SELECT TRANS-FILE ASSIGN TO INDD                             
               ORGANIZATION IS SEQUENTIAL                               
               FILE STATUS IS TRANS-STATUS.                             
                                                                        
           SELECT ERROR-FILE ASSIGN TO REPDD                            
               ORGANIZATION IS SEQUENTIAL                               
               FILE STATUS IS ERROR-STATUS.                             
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD ACCT-MASTER.                                                  
       01 ACCT-REC.                                                     
          05 ACCT-ID PIC X(5).                                          
          05 ACCT-NAME PIC X(20).                                       
          05 ACCT-BAL PIC 9(5)V99.                                      
                                                                        
       FD TRANS-FILE RECORDING MODE IS F.                               
       01 TRANS-REC.                                                    
          05 TRANS-ACCT-ID PIC X(5).                                    
          05 TRANS-TYPE PIC X(1).                                       
          05 TRANS-AMOUNT PIC 9(5)V99.                                  
          05 FILLER PIC X(67).                                          
                                                                        
       FD ERROR-FILE RECORDING MODE IS F.                               
       01 ERROR-REC.                                                    
          05 REP-MSG-CONST    PIC X(13) VALUE 'TRANS ERROR: '.          
          05 REP-ID           PIC X(5).                                 
          05 FILLER           PIC X(1).                                 
          05 REP-DESC         PIC X(61).                                
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * FILE STATUS VARIABLES                                           
       01 ACCT-STATUS PIC X(2).                                         
       01 TRANS-STATUS PIC X(2).                                        
       01 ERROR-STATUS PIC X(2).                                        
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
           05 WS-EOF PIC X(1) VALUE 'N'.                                
              88 EOF VALUE 'Y'.                                         
           05 FOUND-SW PIC X(1) VALUE 'N'.                              
              88 FOUND VALUE 'Y'.                                       
              88 NOT-FOUND VALUE 'N'.                                   
                                                                        
      **********************************************                    
      * OPEN FILES, PROCESS ALL TRANSACTIONS, CLOSE FILES.              
      * STOPS ON ANY UNEXPECTED FILE STATUS.                            
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
                                                                        
           PERFORM OPEN-FILES.                                          
                                                                        
           IF ACCT-STATUS NOT = '00'                                    
              DISPLAY 'ERROR OPENING ACCT-MASTER FILE: ' ACCT-STATUS    
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           IF TRANS-STATUS NOT = '00'                                   
              DISPLAY 'ERROR OPENING TRANS-FILE: ' TRANS-STATUS         
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           IF ERROR-STATUS NOT = '00'                                   
              DISPLAY 'ERROR OPENING ERROR-FILE: ' ERROR-STATUS         
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      * READ EACH TRANSACTION AND PROCESS UNTIL EOF.                    
           PERFORM UNTIL EOF                                            
              READ TRANS-FILE                                           
                AT END                                                  
                   SET EOF TO TRUE                                      
                NOT AT END                                              
                   IF TRANS-STATUS NOT = '00'                           
                      DISPLAY 'READ INPUT FILE ERROR: ' TRANS-STATUS    
                      STOP RUN                                          
                   END-IF                                               
                   PERFORM PROCESS-TRANSACTION                          
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
           PERFORM CLOSE-FILES.                                         
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * OPEN ALL FILES                                                  
      **********************************************                    
       OPEN-FILES.                                                      
           OPEN I-O ACCT-MASTER.                                        
                                                                        
           OPEN INPUT TRANS-FILE.                                       
                                                                        
           OPEN OUTPUT ERROR-FILE.                                      
                                                                        
      **********************************************                    
      * CLOSES ALL FILES AND CHECKS STATUS                              
      **********************************************                    
       CLOSE-FILES.                                                     
           CLOSE ACCT-MASTER.                                           
           IF ACCT-STATUS NOT = '00'                                    
              DISPLAY 'WARNING: ERROR CLOSING ACCT-MASTER FILE : '      
                       ACCT-STATUS                                      
           END-IF.                                                      
                                                                        
           CLOSE TRANS-FILE.                                            
           IF TRANS-STATUS NOT = '00'                                   
              DISPLAY 'WARNING: ERROR CLOSING TRANS-FILE: ' TRANS-STATUS
           END-IF.                                                      
                                                                        
           CLOSE ERROR-FILE.                                            
           IF ERROR-STATUS NOT = '00'                                   
              DISPLAY 'WARNING: ERROR CLOSING ERROR-FILE: ' ERROR-STATUS
           END-IF.                                                      
                                                                        
      **********************************************                    
      * RANDOM READ VSAM BY ACCT-ID.                                    
      * STATUS '23' (NOT FOUND) -> WRITE ERROR RECORD AND EXIT.         
      * FOUND -> APPLY DEPOSIT OR WITHDRAWAL,                           
      * THEN REWRITE UPDATED RECORD TO VSAM.
      * NOTE: UNKNOWN TRANS-TYPE (NOT 'D' OR 'W')
      *       IS SILENTLY IGNORED - NO UPDATE,
      *       NO ERROR LOGGED. ADD VALIDATION IF NEEDED.
      **********************************************                    
       PROCESS-TRANSACTION.                                             
           SET NOT-FOUND TO TRUE.                                       
           MOVE TRANS-ACCT-ID TO ACCT-ID.                               
           MOVE SPACES TO ERROR-REC.                                    
           MOVE 'TRANS ERROR: ' TO REP-MSG-CONST.                       
                                                                        
           READ ACCT-MASTER                                             
               INVALID KEY                                              
                   SET NOT-FOUND TO TRUE                                
                   MOVE TRANS-ACCT-ID TO REP-ID                         
                   MOVE 'ACCOUNT NOT FOUND' TO REP-DESC                 
                   WRITE ERROR-REC                                      
                   END-WRITE                                            
                   IF ERROR-STATUS NOT = '00'                           
                      DISPLAY 'ERROR WRITING ERROR-FILE: ' ERROR-STATUS 
                      STOP RUN                                          
                   END-IF                                               
               NOT INVALID KEY                                          
                   SET FOUND TO TRUE                                    
           END-READ.                                                    
                                                                        
           IF ACCT-STATUS NOT = '00' AND ACCT-STATUS NOT = '23'         
              DISPLAY 'READ VSAM FILE ERROR: ' ACCT-STATUS              
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           IF FOUND                                                     
               IF TRANS-TYPE = 'D'                                      
                  ADD TRANS-AMOUNT TO ACCT-BAL                          
                  PERFORM REWRITE-ACCOUNT                               
               END-IF                                                   
                                                                        
               IF TRANS-TYPE = 'W'                                      
                  IF ACCT-BAL >= TRANS-AMOUNT                           
                     SUBTRACT TRANS-AMOUNT FROM ACCT-BAL                
                     PERFORM REWRITE-ACCOUNT                            
                  ELSE                                                  
                     MOVE ACCT-ID TO REP-ID                             
                     MOVE 'INSUFFICIENT FUNDS' TO REP-DESC              
                     WRITE ERROR-REC                                    
                     END-WRITE                                          
                     IF ERROR-STATUS NOT = '00'                         
                        DISPLAY 'ERROR WRITING ERROR-FILE: '            
                                 ERROR-STATUS                           
                        STOP RUN                                        
                     END-IF                                             
                  END-IF                                                
               END-IF                                                   
           END-IF.                                                      
                                                                        
      **********************************************                    
      * REWRITE UPDATED ACCT-REC BACK TO VSAM.                          
      * REQUIRES PRIOR SUCCESSFUL READ ON SAME KEY.                     
      **********************************************                    
       REWRITE-ACCOUNT.                                                 
           REWRITE ACCT-REC                                             
               INVALID KEY                                              
                 CONTINUE                                               
           END-REWRITE.                                                 
                                                                        
           IF ACCT-STATUS NOT = '00'                                    
              DISPLAY 'REWRITE FAILED: ' ACCT-STATUS                    
              STOP RUN                                                  
           END-IF.                                                      
