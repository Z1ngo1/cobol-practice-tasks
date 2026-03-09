      ******************************************************************
      * SALES BONUS INDEXING SYSTEM (DB2)                              *
      *                                                                *
      * PURPOSE:                                                       *
      * READS SALES BONUS DATA FROM DB2 TABLE, CALCULATES NEW BONUS    *
      * BASED ON REGION AND SALES VOLUME, APPLIES CAP LIMIT,           *
      * UPDATES TABLE AND GENERATES REPORT.                            *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   BASE INCREASE BY REGION:                                     *
      *     EU: +12%, NE: +10%, AS: +8%, SW: +5%                       *
      *    ADDITIONAL BONUS IF YEAR_SALES >= 150000:                   *
      *      CALCULATED_BONUS = CALCULATED_BONUS * 1.05                *
      *    CAP LIMIT: IF NEW_BONUS > 20000 THEN SET TO 20000           *
      *                                                                *
      *   STATUS CODES:                                                *
      *     HIGHSAL - HIGH SALES (>= 150000), BONUS BOOST APPLIED      *
      *     CAP     - BONUS CAPPED AT 20000                            *
      *     LOW     - FINAL BONUS < 2000                               *
      *     OK      - STANDARD INCREASE                                *
      *   COMMIT EVERY 50 RECORDS FOR OPTIMAL PERFORMANCE              *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2025/12/22                                               *
      *                                                                *
      * DB2:    TB_SALES_BONUS - SALES BONUS TABLE (SOURCE/TARGET)     *
      * OUTPUT: OUTDD (REPORT.FILE) - BONUS CHANGE REPORT (PS,80 BYTES)*
      ******************************************************************
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. DB2TASK8.                                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT BONUS-REPORT-FILE ASSIGN TO OUTDD                     
             ORGANIZATION IS SEQUENTIAL                                 
             FILE STATUS IS OUT-STATUS.                                 
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD BONUS-REPORT-FILE RECORDING MODE IS F.                        
       01 REPORT-REC PIC X(80).                                         
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * FILE STATUS CODE                                                
       01 OUT-STATUS PIC X(2).                                          
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
          05 WS-EOF PIC X(1) VALUE 'N'.                                 
             88 EOF VALUE 'Y'.                                          
             88 NOT-EOF VALUE 'N'.                                      
                                                                        
      * COUNTERS AND WORKING VARIABLES                                  
       01 COMMIT-COUNT PIC 9(5) VALUE 0.                                
       01 TOTAL-REC-UPDATED PIC 9(5) VALUE 0.                           
       01 TOTAL-REC-UPDATED-DISP PIC ZZZZ9.                             
       01 OLD-BONUS PIC S9(7)V9(2) USAGE COMP-3.                        
       01 CALC-BONUS PIC S9(7)V9(2) USAGE COMP-3.                       
       01 WS-STATUS-TEXT PIC X(7).                                      
       01 OLD-BONUS-DISP PIC ZZZZ9.99.                                  
       01 CALC-BONUS-DISP PIC ZZZZ9.99.                                 
       77 WS-SQLCODE-DISP PIC -Z(8)9.                                   
                                                                        
      * DB2 SQL COMMUNICATION AREA                                      
           EXEC SQL                                                     
             INCLUDE SQLCA                                              
           END-EXEC.                                                    
                                                                        
      * DB2 DCLGEN - GENERATED FROM TB_SALES_BONUS TABLE                
           EXEC SQL                                                     
             INCLUDE TASK8                                              
           END-EXEC.                                                    
                                                                        
      * CURSOR DEFINITION FOR BONUS TABLE                               
           EXEC SQL                                                     
             DECLARE CUR-BONUS CURSOR WITH HOLD FOR                     
             SELECT * FROM TB_SALES_BONUS                               
             FOR UPDATE OF BONUS_AMT                                    
           END-EXEC.                                                    
                                                                        
      **********************************************                    
      * MAIN FLOW: OPEN -> FETCH -> UPDATE -> COMMIT -> CLOSE           
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
                                                                        
           PERFORM OPEN-FILES.                                          
           PERFORM UNTIL EOF                                            
              PERFORM FETCH-EMPLOYEE                                    
              IF NOT EOF                                                
                 PERFORM PROCESS-BONUS                                  
              END-IF                                                    
           END-PERFORM.                                                 
           PERFORM CLOSE-FILES.                                         
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * OPEN FILES AND CURSOR                                           
      **********************************************                    
       OPEN-FILES.                                                      
           OPEN OUTPUT BONUS-REPORT-FILE.                               
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR OPENING REPORT FILE: ' OUT-STATUS          
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           MOVE 'EMPID  REGION OLD_BONUS NEW_BONUS STATUS'              
                 TO REPORT-REC.                                         
           WRITE REPORT-REC.                                            
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR WRITING REPORT HEADER: ' OUT-STATUS        
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           EXEC SQL                                                     
             OPEN CUR-BONUS                                             
           END-EXEC.                                                    
           IF SQLCODE NOT = 0                                           
               DISPLAY 'ERROR OPENING CURSOR: ' SQLCODE                 
               STOP RUN                                                 
           END-IF.                                                      
                                                                        
      **********************************************                    
      * CLOSE FILES, COMMIT AND GENERATE SUMMARY                        
      **********************************************                    
       CLOSE-FILES.                                                     
           EXEC SQL                                                     
             COMMIT WORK                                                
           END-EXEC.                                                    
           IF SQLCODE NOT = 0                                           
               MOVE SQLCODE TO WS-SQLCODE-DISP                          
               DISPLAY 'FINAL COMMIT ERROR: ' WS-SQLCODE-DISP           
               EXEC SQL                                                 
                 ROLLBACK WORK                                          
               END-EXEC                                                 
               IF SQLCODE NOT = 0                                       
                  MOVE SQLCODE TO WS-SQLCODE-DISP                       
                  DISPLAY 'ROLLBACK ERROR: ' WS-SQLCODE-DISP            
               END-IF                                                   
               STOP RUN                                                 
           END-IF.                                                      
                                                                        
           EXEC SQL                                                     
             CLOSE CUR-BONUS                                            
           END-EXEC.                                                    
           IF SQLCODE NOT = 0 AND SQLCODE NOT = -501                    
               DISPLAY 'CURSOR CLOSE ERROR: ' SQLCODE                   
           END-IF.                                                      
                                                                        
           MOVE SPACES TO REPORT-REC.                                   
           MOVE TOTAL-REC-UPDATED TO TOTAL-REC-UPDATED-DISP.            
           STRING 'TOTAL: ' DELIMITED BY SIZE                           
                  TOTAL-REC-UPDATED-DISP DELIMITED BY SIZE              
                  ' ROWS UPDATED' DELIMITED BY SIZE                     
                  INTO REPORT-REC                                       
           END-STRING.                                                  
           WRITE REPORT-REC.                                            
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR WRITING REPORT FOOTER: ' OUT-STATUS        
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           CLOSE BONUS-REPORT-FILE.                                     
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR CLOSING FILE: ' OUT-STATUS                 
           END-IF.                                                      
           DISPLAY 'BONUS UPDATE COMPLETED: ' TOTAL-REC-UPDATED.        
                                                                        
      **********************************************                    
      * FETCH NEXT EMPLOYEE RECORD FROM DB2                             
      **********************************************                    
       FETCH-EMPLOYEE.                                                  
           EXEC SQL                                                     
             FETCH CUR-BONUS INTO :DCLTB-SALES-BONUS                    
           END-EXEC.                                                    
                                                                        
           EVALUATE TRUE                                                
               WHEN SQLCODE = 0                                         
                 SET NOT-EOF TO TRUE                                    
               WHEN SQLCODE = 100                                       
                 SET EOF TO TRUE                                        
               WHEN OTHER                                               
                 MOVE SQLCODE TO WS-SQLCODE-DISP                        
                 DISPLAY 'FETCH ERROR: ' WS-SQLCODE-DISP                
                 SET EOF TO TRUE                                        
                 EXEC SQL                                               
                   ROLLBACK WORK                                        
                 END-EXEC                                               
                 IF SQLCODE NOT = 0                                     
                    MOVE SQLCODE TO WS-SQLCODE-DISP                     
                    DISPLAY 'ROLLBACK ERROR: ' WS-SQLCODE-DISP          
                 END-IF                                                 
                 STOP RUN                                               
           END-EVALUATE.                                                
                                                                        
      **********************************************                    
      * CALCULATE NEW BONUS AND UPDATE DB2                              
      **********************************************                    
       PROCESS-BONUS.                                                   
           MOVE BONUS-AMT OF DCLTB-SALES-BONUS TO OLD-BONUS.            
                                                                        
           EVALUATE TRUE                                                
               WHEN REGION-CODE OF DCLTB-SALES-BONUS = 'EU'             
                 COMPUTE CALC-BONUS = OLD-BONUS * 1.12                  
               WHEN REGION-CODE OF DCLTB-SALES-BONUS = 'NE'             
                 COMPUTE CALC-BONUS = OLD-BONUS * 1.10                  
               WHEN REGION-CODE OF DCLTB-SALES-BONUS = 'AS'             
                 COMPUTE CALC-BONUS = OLD-BONUS * 1.08                  
               WHEN REGION-CODE OF DCLTB-SALES-BONUS = 'SW'             
                 COMPUTE CALC-BONUS = OLD-BONUS * 1.05                  
           END-EVALUATE.                                                
                                                                        
           IF YEAR-SALES OF DCLTB-SALES-BONUS >= 150000.00              
              COMPUTE CALC-BONUS = CALC-BONUS * 1.05                    
           END-IF.                                                      
                                                                        
           MOVE 'OK' TO WS-STATUS-TEXT.                                 
                                                                        
           EVALUATE TRUE                                                
               WHEN CALC-BONUS > 20000                                  
                 MOVE 20000 TO CALC-BONUS                               
                 MOVE 'CAP' TO WS-STATUS-TEXT                           
               WHEN CALC-BONUS < 2000                                   
                 MOVE 'LOW' TO WS-STATUS-TEXT                           
               WHEN YEAR-SALES OF DCLTB-SALES-BONUS >= 150000.00        
                 MOVE 'HIGHSAL' TO WS-STATUS-TEXT                       
               WHEN OTHER                                               
                 MOVE 'OK' TO WS-STATUS-TEXT                            
               END-EVALUATE.                                            
                                                                        
           EXEC SQL                                                     
             UPDATE TB_SALES_BONUS                                      
             SET BONUS_AMT = :CALC-BONUS                                
             WHERE CURRENT OF CUR-BONUS                                 
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT = 0                                           
              MOVE SQLCODE TO WS-SQLCODE-DISP                           
              DISPLAY 'UPDATE ERROR FOR EMPLOYEE: '                     
                       EMP-ID OF DCLTB-SALES-BONUS                      
              DISPLAY 'SQLCODE: ' WS-SQLCODE-DISP                       
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              IF SQLCODE NOT = 0                                        
                 MOVE SQLCODE TO WS-SQLCODE-DISP                        
                 DISPLAY 'ROLLBACK ERROR: ' WS-SQLCODE-DISP             
              END-IF                                                    
              DISPLAY 'ALL CHANGES ROLLED BACK DUE TO UPDATE ERROR'     
              STOP RUN                                                  
           ELSE                                                         
              ADD 1 TO TOTAL-REC-UPDATED                                
              ADD 1 TO COMMIT-COUNT                                     
           END-IF.                                                      
                                                                        
           PERFORM WRITE-REPORT-LINE.                                   
                                                                        
           IF COMMIT-COUNT >= 50                                        
              PERFORM INTERMEDIATE-COMMIT                               
           END-IF.                                                      
                                                                        
      **********************************************                    
      * PERFORM INTERMEDIATE COMMIT EVERY 50 RECORDS                    
      **********************************************                    
       INTERMEDIATE-COMMIT.                                             
           EXEC SQL                                                     
             COMMIT WORK                                                
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT = 0                                           
              MOVE SQLCODE TO WS-SQLCODE-DISP                           
              DISPLAY 'INTERMEDIATE COMMIT ERROR: ' WS-SQLCODE-DISP     
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              IF SQLCODE NOT = 0                                        
                 MOVE SQLCODE TO WS-SQLCODE-DISP                        
                 DISPLAY 'ROLLBACK ERROR: ' WS-SQLCODE-DISP             
              END-IF                                                    
              STOP RUN                                                  
           ELSE                                                         
              DISPLAY 'COMMIT AFTER ' COMMIT-COUNT ' RECORDS'           
              MOVE 0 TO COMMIT-COUNT                                    
           END-IF.                                                      
                                                                        
      **********************************************                    
      * FORMAT AND WRITE REPORT LINE                                    
      **********************************************                    
       WRITE-REPORT-LINE.                                               
           MOVE OLD-BONUS TO OLD-BONUS-DISP.                            
           MOVE CALC-BONUS TO CALC-BONUS-DISP.                          
                                                                        
           MOVE SPACES TO REPORT-REC                                    
           STRING EMP-ID OF DCLTB-SALES-BONUS DELIMITED BY SPACE        
                  ' ' DELIMITED BY SIZE                                 
                  REGION-CODE OF DCLTB-SALES-BONUS DELIMITED BY SIZE    
                  '     ' DELIMITED BY SIZE                             
                  OLD-BONUS-DISP DELIMITED BY SIZE                      
                  '  ' DELIMITED BY SIZE                                
                  CALC-BONUS-DISP DELIMITED BY SIZE                     
                  '  ' DELIMITED BY SIZE                                
                  WS-STATUS-TEXT DELIMITED BY SIZE                      
                  INTO REPORT-REC                                       
           END-STRING.                                                  
                                                                        
           WRITE REPORT-REC.                                            
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR WRITING REPORT LINE: ' OUT-STATUS          
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              IF SQLCODE NOT = 0                                        
                 MOVE SQLCODE TO WS-SQLCODE-DISP                        
                 DISPLAY 'ROLLBACK ERROR: ' WS-SQLCODE-DISP             
              END-IF                                                    
              STOP RUN                                                  
           END-IF.                                                      
