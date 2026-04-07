      ******************************************************************
      * EMPLOYEE SALARY INDEXING SYSTEM (DB2)                          *
      *                                                                *
      * PURPOSE:                                                       *
      * READS EMPLOYEE SALARY DATA FROM DB2 TABLE, APPLIES SALARY      *
      * INCREASES BY DEPARTMENT, APPLIES MAXIMUM CAP LIMIT,            *
      * UPDATES TABLE AND GENERATES REPORT.                            *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   IT DEPT:  NEW_SALARY = CURRENT_SALARY * 1.10 (+10%)          *
      *   SAL DEPT: NEW_SALARY = CURRENT_SALARY * 1.05 (+5%)           *
      *   OTHER:    NEW_SALARY = CURRENT_SALARY * 1.03 (+3%)           *
      *   IF NEW_SALARY > 100000 -> CAP AT 100000, STATUS = MAXCAP     *
      *   COMMIT EVERY 100 RECORDS, FINAL COMMIT AFTER LOOP            *
      *   CURSOR WITH HOLD - KEEPS CURSOR OPEN ACROSS COMMITS          *
      *   ANY SQLCODE ERROR -> ROLLBACK AND STOP                       *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2025/12/21                                               *
      *                                                                *
      * DB2 OBJECTS:                                                   *
      * TB_EMP_SALARY - EMPLOYEE SALARY TABLE (SOURCE/TARGET)          *
      * DCLGEN: TASK7 - HOST VARIABLE DECLARATIONS                     *
      *                                                                *
      * OUTPUT: OUTDD (REPORT.FILE) - SALARY CHANGE REPORT (PS, 80 B)  *
      ******************************************************************
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. DB2TASK7.                                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT SALARY-REPORT-FILE ASSIGN TO OUTDD                    
             ORGANIZATION IS SEQUENTIAL                                 
             FILE STATUS IS OUT-STATUS.                                 
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD SALARY-REPORT-FILE RECORDING MODE IS F.                       
       01 REPORT-REC PIC X(80).                                         
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * DB2 SQL COMMUNICATION AREA                                      
           EXEC SQL                                                     
             INCLUDE SQLCA                                              
           END-EXEC.                                                    
                                                                        
      * DB2 DCLGEN - GENERATED FROM TB_EMP_SALARY TABLE                 
           EXEC SQL                                                     
             INCLUDE TASK7                                              
           END-EXEC.                                                    
                                                                        
      * CURSOR DEFINITION FOR SALARY TABLE                              
           EXEC SQL                                                     
             DECLARE CUR-SALARY CURSOR WITH HOLD FOR                    
             SELECT * FROM TB_EMP_SALARY                                
             FOR UPDATE OF SALARY                                       
           END-EXEC.                                                    
                                                                        
      * FILE STATUS CODE                                                
       01 OUT-STATUS PIC X(2).                                          
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
          05 WS-EOF PIC X(1) VALUE 'N'.                                 
             88 EOF VALUE 'Y'.                                          
             88 NOT-EOF VALUE 'N'.                                      
                                                                        
      * COUNTERS AND WORKING VARIABLES                                  
       01 COMMIT-COUNT PIC 9(3) VALUE 0.                                
       01 TOTAL-RECORDS-UPDATED PIC 9(3) VALUE 0.                       
       01 OLD-SALARY PIC S9(7)V99 COMP-3.                               
       01 NEW-SALARY PIC S9(7)V99 COMP-3.                               
       01 STATUS-TEXT  PIC X(7).                                        
       01 OLD-SAL-DISP PIC Z(7).99.                                     
       01 NEW-SAL-DISP PIC Z(7).99.                                     
       01 TOTAL-RECORDS-UPDATED-DISP PIC ZZ9.                           
       01 WS-SQLCODE-SAVE PIC S9(9) COMP.                               
       77 WS-SQLCODE-DISP PIC -Z(8)9.                                   
                                                                        
      **********************************************                    
      * OPENS REPORT FILE, OPENS DB2 CURSOR, PROCESSES ALL ROWS,        
      * FINAL COMMIT, CLOSES CURSOR AND REPORT FILE.                    
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
                                                                        
           OPEN OUTPUT SALARY-REPORT-FILE.                              
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR OPENING FILE' OUT-STATUS                   
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           MOVE 'EMPID   OLD_SAL    NEW_SAL  STATUS' TO REPORT-REC.     
           WRITE REPORT-REC.                                            
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR WRITING REPORT: ' OUT-STATUS               
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           EXEC SQL                                                     
             OPEN CUR-SALARY                                            
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT = 0                                           
               MOVE SQLCODE TO WS-SQLCODE-DISP                          
               DISPLAY 'CURSOR OPEN ERROR: ' WS-SQLCODE-DISP            
               STOP RUN                                                 
           END-IF.                                                      
                                                                        
           PERFORM UNTIL EOF                                            
               PERFORM FETCH-EMPLOYEE                                   
               IF NOT EOF                                               
                  PERFORM UPDATE-EMPLOYEE-SALARY                        
               END-IF                                                   
           END-PERFORM.                                                 
                                                                        
      * FINAL COMMIT AFTER ALL RECORDS PROCESSED.                       
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
             CLOSE CUR-SALARY                                           
           END-EXEC.                                                    
           IF SQLCODE NOT = 0                                           
              MOVE SQLCODE TO WS-SQLCODE-DISP                           
              DISPLAY 'ERROR CLOSING CURSOR: ' WS-SQLCODE-DISP          
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           MOVE SPACES TO REPORT-REC.                                   
           MOVE TOTAL-RECORDS-UPDATED TO TOTAL-RECORDS-UPDATED-DISP     
           STRING 'TOTAL: ' DELIMITED BY SIZE                           
                  TOTAL-RECORDS-UPDATED-DISP DELIMITED BY SIZE          
                  ' RECORDS UPDATED' DELIMITED BY SIZE                  
                  INTO REPORT-REC                                       
           END-STRING.                                                  
           WRITE REPORT-REC.                                            
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR WRITING REPORT: ' OUT-STATUS               
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           CLOSE SALARY-REPORT-FILE.                                    
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR CLOSING FILE: ' OUT-STATUS                 
           END-IF.                                                      
           DISPLAY 'SALARY INDEXING COMPLETED: '                        
                   TOTAL-RECORDS-UPDATED-DISP.                          
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * FETCHES NEXT ROW FROM CUR-SALARY.                               
      * SQLCODE 0 -> CONTINUE, 100 -> EOF, OTHER -> ROLLBACK AND STOP.  
      **********************************************                    
       FETCH-EMPLOYEE.                                                  
           EXEC SQL                                                     
             FETCH CUR-SALARY INTO :DCLTB-EMP-SALARY                    
           END-EXEC.                                                    
                                                                        
           EVALUATE TRUE                                                
               WHEN SQLCODE = 0                                         
                 SET NOT-EOF TO TRUE                                    
               WHEN SQLCODE = 100                                       
                 SET EOF TO TRUE                                        
               WHEN OTHER                                               
                 MOVE SQLCODE TO WS-SQLCODE-SAVE                        
                 MOVE WS-SQLCODE-SAVE TO WS-SQLCODE-DISP                
                 DISPLAY 'FETCH ERROR: ' WS-SQLCODE-DISP                
                 EXEC SQL                                               
                   ROLLBACK WORK                                        
                 END-EXEC                                               
                 IF SQLCODE NOT = 0                                     
                    MOVE SQLCODE TO WS-SQLCODE-DISP                     
                    DISPLAY 'ROLLBACK ERROR: ' WS-SQLCODE-DISP          
                 END-IF                                                 
                 SET EOF TO TRUE                                        
                 STOP RUN                                               
           END-EVALUATE.                                                
                                                                        
      **********************************************                    
      * CALCULATES NEW SALARY BY DEPT, APPLIES CAP,                     
      * UPDATES CURRENT ROW VIA CURSOR,                                 
      * WRITES REPORT LINE, COMMITS EVERY 100 RECORDS.                  
      **********************************************                    
       UPDATE-EMPLOYEE-SALARY.                                          
           MOVE SALARY OF DCLTB-EMP-SALARY TO OLD-SALARY.               
           MOVE 'OK' TO STATUS-TEXT.                                    
                                                                        
           EVALUATE TRUE                                                
              WHEN DEPT-CODE OF DCLTB-EMP-SALARY = 'IT'                 
                 COMPUTE NEW-SALARY = OLD-SALARY * 1.10                 
              WHEN DEPT-CODE OF DCLTB-EMP-SALARY = 'SAL'                
                 COMPUTE NEW-SALARY = OLD-SALARY * 1.05                 
              WHEN OTHER                                                
                 COMPUTE NEW-SALARY = OLD-SALARY * 1.03                 
           END-EVALUATE.                                                
                                                                        
           IF NEW-SALARY > 100000                                       
              MOVE 100000 TO NEW-SALARY                                 
              MOVE 'MAXCAP' TO STATUS-TEXT                              
           END-IF.                                                      
                                                                        
           EXEC SQL                                                     
             UPDATE TB_EMP_SALARY                                       
             SET SALARY = :NEW-SALARY                                   
             WHERE CURRENT OF CUR-SALARY                                
           END-EXEC.                                                    
                                                                        
           IF SQLCODE NOT = 0                                           
              MOVE SQLCODE TO WS-SQLCODE-SAVE                           
              MOVE WS-SQLCODE-SAVE TO WS-SQLCODE-DISP                   
              DISPLAY 'UPDATE ERROR: ' EMP-ID OF DCLTB-EMP-SALARY       
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
              ADD 1 TO TOTAL-RECORDS-UPDATED                            
              ADD 1 TO COMMIT-COUNT                                     
           END-IF.                                                      
                                                                        
           PERFORM WRITE-REPORT-LINE.                                   
                                                                        
      * INTERMEDIATE COMMIT TO AVOID LONG-RUNNING UNIT OF WORK.         
           IF COMMIT-COUNT >= 100                                       
               EXEC SQL                                                 
                 COMMIT WORK                                            
               END-EXEC                                                 
               IF SQLCODE NOT = 0                                       
                   MOVE SQLCODE TO WS-SQLCODE-DISP                      
                   DISPLAY 'COMMIT ERROR: ' WS-SQLCODE-DISP             
                   EXEC SQL                                             
                     ROLLBACK WORK                                      
                   END-EXEC                                             
                   IF SQLCODE NOT = 0                                   
                      MOVE SQLCODE TO WS-SQLCODE-DISP                   
                      DISPLAY 'ROLLBACK ERROR: ' WS-SQLCODE-DISP        
                   END-IF                                               
                   STOP RUN                                             
               END-IF                                                   
               DISPLAY 'INTERMEDIATE COMMIT AT: ' TOTAL-RECORDS-UPDATED 
               MOVE 0 TO COMMIT-COUNT                                   
           END-IF.                                                      
                                                                        
      **********************************************                    
      * FORMATS EMP-ID, OLD AND NEW SALARY, STATUS                      
      * INTO REPORT LINE AND WRITES TO REPORT FILE.                     
      **********************************************                    
       WRITE-REPORT-LINE.                                               
           MOVE OLD-SALARY TO OLD-SAL-DISP.                             
           MOVE NEW-SALARY TO NEW-SAL-DISP.                             
                                                                        
           MOVE SPACES TO REPORT-REC.                                   
           STRING EMP-ID OF DCLTB-EMP-SALARY DELIMITED BY SPACE         
                  ' '                            DELIMITED BY SIZE      
                  OLD-SAL-DISP                DELIMITED BY SIZE         
                  ' '                            DELIMITED BY SIZE      
                  NEW-SAL-DISP                DELIMITED BY SIZE         
                  ' '                            DELIMITED BY SIZE      
                  STATUS-TEXT                 DELIMITED BY SIZE         
                  INTO REPORT-REC                                       
           END-STRING.                                                  
                                                                        
           WRITE REPORT-REC.                                            
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR WRITING REPORT: ' OUT-STATUS               
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              IF SQLCODE NOT = 0                                        
                 MOVE SQLCODE TO WS-SQLCODE-DISP                        
                 DISPLAY 'ROLLBACK ERROR: ' WS-SQLCODE-DISP             
              END-IF                                                    
              STOP RUN                                                  
           END-IF.                                                      
