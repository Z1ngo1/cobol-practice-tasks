      ******************************************************************
      * DB2 UPSERT - EMPLOYEE SYNC WITH UPDATE OR INSERT               *
      *                                                                *
      * PURPOSE:                                                       *
      * SYNCHRONIZES EMPLOYEE DATA FROM EXTERNAL FILE TO DB2 TABLE.    *
      * IF EMPLOYEE EXISTS - UPDATE; IF NEW - INSERT (UPSERT LOGIC).   *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   PHASE 1 - VALIDATE INPUT DATA:                               *
      *     1. CHECK EMP-ID NOT EMPTY                                  *
      *     2. CHECK EMP-NAME NOT EMPTY                                *
      *     3. CHECK SALARY > 0                                        *
      *     4. CHECK STATUS IN ('A','I')                               *
      *     5. IF ANY VALIDATION FAILS: LOG ERROR, SKIP RECORD         *
      *                                                                *
      *   PHASE 2 - CHECK IF EMPLOYEE EXISTS:                          *
      *     1. SELECT EMP_NAME, SALARY FROM TB_EMPLOYEES               *
      *     2. IF SQLCODE = 0: EMPLOYEE FOUND PERFORM UPDATE           *
      *     3. IF SQLCODE = 100: NOT FOUND PERFORM INSERT              *
      *     4. IF OTHER SQLCODE: DB2 ERROR LOG AND SKIP                *
      *                                                                *
      *   PHASE 3 - UPDATE EXISTING EMPLOYEE:                          *
      *     1. UPDATE ALL FIELDS (NAME, DEPT, SALARY, DATE, STATUS)    *
      *     2. COMPARE OLD VS NEW SALARY                               *
      *     3. LOG CHANGE DETAILS IF SALARY CHANGED                    *
      *     4. LOG UPDATE EVEN IF NO SALARY CHANGE                     *
      *                                                                *
      *   PHASE 4 - INSERT NEW EMPLOYEE:                               *
      *     1. INSERT ALL FIELDS INTO TB_EMPLOYEES                     *
      *     2. LOG SUCCESS OR ERROR                                    *
      *                                                                *
      *   PHASE 5 - BATCH COMMIT STRATEGY:                             *
      *     1. COMMIT EVERY 50 SUCCESSFUL OPERATIONS                   *
      *     2. FINAL COMMIT AT END FOR REMAINING RECORDS               *
      *     3. ROLLBACK ON CRITICAL ERRORS                             *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2026/01/16                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT:  INDD (EMP.UPDATE) - EMPLOYEE DATA (PS, 44 B)           *
      * OUTPUT: OUTDD (SYNC.LOG) - SYNC RESULTS LOG (PS, V)            *
      ******************************************************************
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. DB2JOB20.                                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT INP-FILE ASSIGN TO INDD                               
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS IN-STATUS.                                 
                                                                        
           SELECT OUT-FILE ASSIGN TO OUTDD                              
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS OUT-STATUS.                                
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD INP-FILE RECORDING MODE IS F.                                 
       01 INP-REC.                                                      
          05 INP-ID PIC X(5).                                           
          05 INP-NAME PIC X(20).                                        
          05 INP-DEPT PIC X(3).                                         
          05 INP-SALARY PIC S9(5)V99.                                   
          05 INP-HIRE-DATE PIC 9(8).                                    
          05 INP-STATUS PIC X(1).                                       
                                                                        
       FD OUT-FILE RECORDING MODE IS V.                                 
       01 OUT-REC PIC X(80).                                            
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * FILE STATUS VARIABLES                                           
       01 FILE-STATUSES.                                                
          05 OUT-STATUS PIC X(2).                                       
          05 IN-STATUS PIC X(2).                                        
                                                                        
      * CONTROL FLAGS                                                   
       01 FLAGS.                                                        
          05 WS-EOF PIC X(1) VALUE 'N'.                                 
             88 EOF VALUE 'Y'.                                          
          05 WS-ERROR-FIND PIC X(1) VALUE 'N'.                          
             88 ERROR-FIND VALUE 'Y'.                                   
             88 ERROR-NOT-FIND VALUE 'N'.                               
                                                                        
      * STATISTICS COUNTERS                                             
       01 COUNTERS.                                                     
          05 RECORDS-PROCESSED PIC 9(5) VALUE 0.                        
          05 RECORDS-INSERTED PIC 9(5) VALUE 0.                         
          05 RECORDS-UPDATED PIC 9(5) VALUE 0.                          
          05 RECORDS-ERRORS PIC 9(5) VALUE 0.                           
          05 COMMIT-COUNTER PIC 9(5) VALUE 0.                           
          05 COMMIT-BATCHES PIC 9(5) VALUE 0.                           
                                                                        
      * DISPLAY-FORMATTED COUNTERS                                      
       01 DISP-COUNTERS.                                                
          05 RECORDS-PROCESSED-DISP PIC Z(4)9.                          
          05 RECORDS-INSERTED-DISP PIC Z(4)9.                           
          05 RECORDS-UPDATED-DISP PIC Z(4)9.                            
          05 RECORDS-ERRORS-DISP PIC Z(4)9.                             
          05 COMMIT-BATCHES-DISP PIC Z(4)9.                             
          05 COMMIT-COUNTER-DISP PIC Z(4)9.                             
                                                                        
      * SALARY DISPLAY VARIABLES                                        
       01 WS-OLD-SALARY-DISP PIC Z(6)9.99.                              
       01 WS-NEW-SALARY-DISP PIC Z(6)9.99.                              
                                                                        
      * OUTPUT MESSAGE BUFFER                                           
       01 WS-OUT-MSG PIC X(80).                                         
                                                                        
      * VARCHAR LENGTH CALCULATOR                                       
       01 WS-EMP-NAME-LEN PIC 9(2) VALUE 0.                             
                                                                        
      * FORMATTED SQLCODE FOR DISPLAY                                   
       77 WS-SQLCODE-DISP  PIC -Z(9)9.                                  
                                                                        
      * DATE FORMATTING VARIABLES                                       
       01 WS-DATE-PARTS.                                                
          05 WS-YEAR     PIC X(4).                                      
          05 WS-MONTH    PIC X(2).                                      
          05 WS-DAY      PIC X(2).                                      
                                                                        
      * SQL COMMUNICATION AREA                                          
           EXEC SQL                                                      
             INCLUDE SQLCA                                               
           END-EXEC.                                                     
                                                                        
      * DB2 HOST VARIABLES                                              
       01 HV-EMP-ID PIC X(5).                                           
       01 HV-EMP-NAME.                                                  
          49 HV-EMP-NAME-LEN PIC S9(4) COMP-5.                          
          49 HV-EMP-NAME-TEXT PIC X(20).                                
       01 HV-EMP-DEPT PIC X(3).                                         
       01 HV-SALARY PIC S9(7)V99 COMP-3.                                
       01 HV-HIRE-DATE PIC X(10).                                       
       01 HV-STATUS PIC X(1).                                           
                                                                        
      * DB2 HOST VARIABLES FOR OLD VALUES                               
       01 HV-OLD-NAME.                                                  
          49 HV-OLD-NAME-LEN  PIC S9(4) COMP-5.                         
          49 HV-OLD-NAME-TEXT PIC X(20).                                
       01 HV-OLD-SALARY PIC S9(7)V99 COMP-3.                            
                                                                        
      **********************************************                    
      * MAIN FLOW: OPEN -> PROCESS -> CLOSE -> REPORT                   
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
           PERFORM OPEN-ALL-FILES.                                      
           PERFORM PROCESS-ALL-RECORDS.                                 
           PERFORM CLOSE-ALL-FILES.                                     
           PERFORM DISPLAY-SUMMARY.                                     
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * OPEN ALL FILES AND CHECK STATUS                                 
      **********************************************                    
       OPEN-ALL-FILES.                                                  
           OPEN INPUT INP-FILE.                                         
           IF IN-STATUS NOT = '00'                                      
              DISPLAY 'ERROR OPENING INP STATUS: ' IN-STATUS            
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT OUT-FILE.                                        
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR OPENING OUT STATUS: ' OUT-STATUS           
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * READ AND PROCESS ALL INPUT RECORDS                              
      **********************************************                    
       PROCESS-ALL-RECORDS.                                             
           PERFORM UNTIL EOF                                            
              SET ERROR-NOT-FIND TO TRUE                                
              READ INP-FILE                                             
                AT END                                                  
                   SET EOF TO TRUE                                      
                NOT AT END                                              
                   IF IN-STATUS NOT = '00' AND IN-STATUS NOT = '10'     
                      DISPLAY 'ERROR READING INPUT FILE: ' IN-STATUS    
                      DISPLAY 'RECORDS PROCESSED: '                     
                               RECORDS-PROCESSED-DISP                   
                      EXEC SQL                                          
                        ROLLBACK WORK                                   
                      END-EXEC                                          
                      STOP RUN                                          
                   END-IF                                               
                   ADD 1 TO RECORDS-PROCESSED                           
                   PERFORM VALIDATE-ID                                  
                   PERFORM VALIDATE-NAME                                
                   PERFORM  VALIDATE-SALARY                             
                   PERFORM VALIDATE-STATUS                             
                   IF WS-ERROR-FIND = 'Y'                               
                      ADD 1 TO RECORDS-ERRORS                           
                   ELSE                                                 
                      PERFORM PROCESS-EMPLOYEE                          
                      IF COMMIT-COUNTER >= 50                           
                         EXEC SQL                                       
                           COMMIT WORK                                  
                         END-EXEC                                       
                         IF SQLCODE NOT = 0                             
                            MOVE SQLCODE TO WS-SQLCODE-DISP             
                            DISPLAY 'BATCH COMMIT ERROR: '              
                                     WS-SQLCODE-DISP                    
                            EXEC SQL                                    
                              ROLLBACK WORK                             
                            END-EXEC                                    
                            STOP RUN                                    
                         END-IF                                         
                         ADD 1 TO COMMIT-BATCHES                        
                         MOVE 0 TO COMMIT-COUNTER                       
                      END-IF                                            
                   END-IF                                               
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
      **********************************************                    
      * VALIDATE EMPLOYEE ID CHECK NOT EMPTY                            
      **********************************************                    
       VALIDATE-ID.                                                     
           IF INP-ID = SPACES                                           
              SET ERROR-FIND TO TRUE                                    
              MOVE 'ID VALIDATION ERROR: ID IS EMPTY' TO WS-OUT-MSG     
              PERFORM WRITE-LOG-MESSAGE                                 
           END-IF.                                                      
                                                                        
      **********************************************                    
      * VALIDATE EMPLOYEE NAME CHECK NOT EMPTY                          
      **********************************************                    
       VALIDATE-NAME.                                                   
           IF INP-NAME = SPACES                                         
              SET ERROR-FIND TO TRUE                                    
              STRING 'VALIDATION ERROR: NAME IS EMPTY (ID='             
                      DELIMITED BY SIZE                                 
                      INP-ID DELIMITED BY SIZE                          
                      ')' DELIMITED BY SIZE                             
                      INTO WS-OUT-MSG                                   
              END-STRING                                                
              PERFORM WRITE-LOG-MESSAGE                                 
           END-IF.                                                      
                                                                        
      **********************************************                    
      * VALIDATE SALARY CHECK NUMERIC, POSITIVE, NON-ZERO               
      **********************************************                    
        VALIDATE-SALARY.                                                
           EVALUATE TRUE                                                
               WHEN INP-SALARY IS NOT NUMERIC                           
                 SET ERROR-FIND TO TRUE                                 
                 MOVE 'SALARY VALIDATION ERROR: NON-NUMERIC' TO         
                       WS-OUT-MSG                                       
                 PERFORM WRITE-LOG-MESSAGE                              
               WHEN INP-SALARY IS NEGATIVE                              
                 SET ERROR-FIND TO TRUE                                 
                 MOVE 'SALARY VALIDATION ERROR: NEGATIVE VALUE' TO      
                       WS-OUT-MSG                                       
                 PERFORM WRITE-LOG-MESSAGE                              
               WHEN INP-SALARY IS ZERO                                  
                 SET ERROR-FIND TO TRUE                                 
                 MOVE 'SALARY VALIDATION ERROR: ZERO VALUE' TO          
                       WS-OUT-MSG                                       
                 PERFORM WRITE-LOG-MESSAGE                              
           END-EVALUATE.                                                
                                                                        
      **********************************************                    
      * VALIDATE STATUS CHECK 'A' OR 'I' ONLY                           
      **********************************************                    
       VALIDATE-STATUS.                                                
           IF INP-STATUS NOT = 'A' AND INP-STATUS NOT = 'I'             
              SET ERROR-FIND TO TRUE                                    
              MOVE 'STATUS VALIDATION ERROR' TO WS-OUT-MSG              
              PERFORM WRITE-LOG-MESSAGE                                 
           END-IF.                                                      
                                                                        
      **********************************************                    
      * WRITE ERROR/SUCCESS MESSAGE TO LOG FILE                         
      **********************************************                    
       WRITE-LOG-MESSAGE.                                               
           STRING INP-ID DELIMITED BY SIZE                              
                  ' ' DELIMITED BY SIZE                                 
                  WS-OUT-MSG DELIMITED BY SIZE                          
                  INTO OUT-REC                                          
           END-STRING.                                                  
           WRITE OUT-REC.                                               
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR WRITING OUTPUT FILE IN WRITE-LOG-MESSAGE: '
                       OUT-STATUS                                       
              DISPLAY 'EMPLOYEE ID: ' INP-ID                            
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              STOP RUN                                                  
           END-IF.                                                      
           MOVE SPACES TO OUT-REC.                                      
           MOVE SPACES TO WS-OUT-MSG.                                   
                                                                        
      **********************************************                    
      * PROCESS EMPLOYEE: PREPARE DATA AND CHECK EXISTENCE              
      **********************************************                    
       PROCESS-EMPLOYEE.                                                
           MOVE INP-ID TO HV-EMP-ID.                                    
           MOVE INP-DEPT TO HV-EMP-DEPT.                                
           MOVE INP-SALARY TO HV-SALARY.                                
           MOVE INP-STATUS TO HV-STATUS.                                
           MOVE SPACES TO HV-EMP-NAME-TEXT.                             
           MOVE INP-NAME TO HV-EMP-NAME-TEXT.                           
           MOVE 0 TO WS-EMP-NAME-LEN.                                   
           INSPECT FUNCTION REVERSE(HV-EMP-NAME-TEXT)                   
                   TALLYING WS-EMP-NAME-LEN FOR LEADING SPACES.         
           COMPUTE HV-EMP-NAME-LEN = 20 - WS-EMP-NAME-LEN.              
           PERFORM FORMAT-HIRE-DATE.                                    
                                                                        
           EXEC SQL                                                     
             SELECT EMP_NAME,SALARY                                     
               INTO :HV-OLD-NAME, :HV-OLD-SALARY                        
               FROM TB_EMPLOYEES                                        
             WHERE EMP_ID = :HV-EMP-ID                                  
           END-EXEC.                                                    
                                                                        
           IF SQLCODE = 0                                               
              PERFORM UPDATE-EMPLOYEE                                   
           ELSE                                                         
             IF SQLCODE = 100                                           
                PERFORM INSERT-EMPLOYEE                                 
             ELSE                                                       
                ADD 1 TO RECORDS-ERRORS                                 
                MOVE SPACES TO WS-OUT-MSG                               
                MOVE SQLCODE TO WS-SQLCODE-DISP                         
                STRING 'SELECT ERROR: SQLCODE=' DELIMITED BY SIZE       
                        WS-SQLCODE-DISP DELIMITED BY SIZE               
                        INTO WS-OUT-MSG                                 
                END-STRING                                              
                PERFORM WRITE-LOG-MESSAGE                               
             END-IF                                                     
           END-IF.                                                      
                                                                        
      **********************************************                    
      * FORMAT HIRE DATE FROM YYYYMMDD TO YYYY-MM-DD                    
      **********************************************                    
       FORMAT-HIRE-DATE.                                                
           MOVE INP-HIRE-DATE(1:4) TO WS-YEAR                           
           MOVE INP-HIRE-DATE(5:2) TO WS-MONTH                          
           MOVE INP-HIRE-DATE(7:2) TO WS-DAY                            
           STRING WS-YEAR DELIMITED BY SIZE                             
                  '-' DELIMITED BY SIZE                                 
                  WS-MONTH DELIMITED BY SIZE                            
                  '-' DELIMITED BY SIZE                                 
                  WS-DAY DELIMITED BY SIZE                              
                  INTO HV-HIRE-DATE                                     
           END-STRING.                                                  
                                                                        
      **********************************************                    
      * UPDATE EXISTING EMPLOYEE IN DB2 TABLE                           
      **********************************************                    
       UPDATE-EMPLOYEE.                                                 
           EXEC SQL                                                     
             UPDATE TB_EMPLOYEES                                        
             SET EMP_NAME = :HV-EMP-NAME,                               
                 DEPT = :HV-EMP-DEPT,                                   
                 SALARY = :HV-SALARY,                                   
                 HIRE_DATE = :HV-HIRE-DATE,                             
                 STATUS = :HV-STATUS                                    
             WHERE EMP_ID = :HV-EMP-ID                                  
           END-EXEC.                                                    
                                                                        
           IF SQLCODE = 0                                               
              ADD 1 TO RECORDS-UPDATED                                  
              ADD 1 TO COMMIT-COUNTER                                   
              MOVE HV-OLD-SALARY TO WS-OLD-SALARY-DISP                  
              MOVE HV-SALARY TO WS-NEW-SALARY-DISP                      
              IF HV-OLD-SALARY NOT = HV-SALARY                          
                 MOVE SPACES TO WS-OUT-MSG                              
                 STRING 'UPDATE(SALARY CHANGE FROM ' DELIMITED BY SIZE  
                        FUNCTION TRIM(WS-OLD-SALARY-DISP)               
                                 DELIMITED BY SIZE                      
                        ' TO ' DELIMITED BY SIZE                        
                        FUNCTION TRIM(WS-NEW-SALARY-DISP)               
                                 DELIMITED BY SIZE                      
                        ')' DELIMITED BY SIZE                           
                        INTO WS-OUT-MSG                                 
                 END-STRING                                             
                 PERFORM WRITE-LOG-MESSAGE                              
              ELSE                                                      
                 MOVE SPACES TO WS-OUT-MSG                              
                 STRING 'UPDATED (NO SALARY CHANGE: ' DELIMITED BY SIZE 
                     FUNCTION TRIM(WS-OLD-SALARY-DISP)                  
                              DELIMITED BY SIZE                         
                     ')' DELIMITED BY SIZE                              
                     INTO WS-OUT-MSG                                    
                 END-STRING                                             
                 PERFORM WRITE-LOG-MESSAGE                              
              END-IF                                                    
           ELSE                                                         
              MOVE SQLCODE TO WS-SQLCODE-DISP                           
              IF SQLCODE < -900 OR SQLCODE = -911 OR SQLCODE = -913     
                 DISPLAY 'CRITICAL UPDATE ERROR: ' WS-SQLCODE-DISP      
                 DISPLAY 'EMPLOYEE ID: ' INP-ID                         
                 EXEC SQL                                               
                   ROLLBACK WORK                                        
                 END-EXEC                                               
                 STOP RUN                                               
              END-IF                                                    
              ADD 1 TO RECORDS-ERRORS                                   
              MOVE SPACES TO WS-OUT-MSG                                 
              STRING 'UPDATE ERROR: SQLCODE=' DELIMITED BY SIZE         
                      WS-SQLCODE-DISP DELIMITED BY SIZE                 
                      INTO WS-OUT-MSG                                   
              END-STRING                                                
              PERFORM WRITE-LOG-MESSAGE                                 
           END-IF.                                                      
                                                                        
      **********************************************                    
      * INSERT NEW EMPLOYEE INTO DB2 TABLE                              
      **********************************************                    
       INSERT-EMPLOYEE.                                                 
           EXEC SQL                                                     
             INSERT INTO TB_EMPLOYEES                                   
                (EMP_ID, EMP_NAME,DEPT,SALARY,HIRE_DATE,STATUS)         
             VALUES                                                     
                (:HV-EMP-ID,:HV-EMP-NAME,:HV-EMP-DEPT,:HV-SALARY,       
                 :HV-HIRE-DATE,:HV-STATUS)                              
           END-EXEC.                                                    
                                                                        
           IF SQLCODE = 0                                               
              ADD 1 TO RECORDS-INSERTED                                 
              ADD 1 TO COMMIT-COUNTER                                   
              MOVE SPACES TO WS-OUT-MSG                                 
              MOVE 'INSERT SUCCESS' TO WS-OUT-MSG                       
              PERFORM WRITE-LOG-MESSAGE                                 
           ELSE                                                         
              MOVE SQLCODE TO WS-SQLCODE-DISP                           
              IF SQLCODE < -900 OR SQLCODE = -911 OR SQLCODE = -913     
                 DISPLAY 'CRITICAL INSERT ERROR: ' WS-SQLCODE-DISP      
                 DISPLAY 'EMPLOYEE ID: ' INP-ID                         
                 EXEC SQL                                               
                   ROLLBACK WORK                                        
                 END-EXEC                                               
                 STOP RUN                                               
              END-IF                                                    
              ADD 1 TO RECORDS-ERRORS                                   
              MOVE SPACES TO WS-OUT-MSG                                 
              STRING 'INSERT ERROR: SQLCODE=' DELIMITED BY SIZE         
                     WS-SQLCODE-DISP DELIMITED BY SIZE                  
                     INTO WS-OUT-MSG                                    
              END-STRING                                                
              PERFORM WRITE-LOG-MESSAGE                                 
           END-IF.                                                      
                                                                        
      **********************************************                    
      * CLOSE ALL FILES AND FINAL COMMIT                                
      **********************************************                    
       CLOSE-ALL-FILES.                                                 
           IF COMMIT-COUNTER > 0                                        
              EXEC SQL                                                  
                COMMIT WORK                                             
              END-EXEC                                                  
              IF SQLCODE NOT = 0                                        
                 MOVE SQLCODE TO WS-SQLCODE-DISP                        
                 DISPLAY 'FINAL COMMIT ERROR: ' WS-SQLCODE-DISP         
                 EXEC SQL                                               
                   ROLLBACK WORK                                        
                 END-EXEC                                               
                 STOP RUN                                               
              END-IF                                                    
              ADD 1 TO COMMIT-BATCHES                                   
              MOVE 0 TO COMMIT-COUNTER                                  
           END-IF.                                                      
                                                                        
           CLOSE INP-FILE.                                              
           IF IN-STATUS NOT = '00'                                      
              DISPLAY 'WARNING: ERROR CLOSING INPUT FILE: '             
                       IN-STATUS                                        
           END-IF.                                                      
                                                                        
           CLOSE OUT-FILE.                                              
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'WARNING: ERROR CLOSING OUTPUT FILE: '            
                       OUT-STATUS                                       
           END-IF.                                                      
                                                                        
      **********************************************                    
      * DISPLAY SUMMARY STATISTICS TO SYSOUT                            
      **********************************************                    
       DISPLAY-SUMMARY.                                                 
           MOVE RECORDS-PROCESSED TO RECORDS-PROCESSED-DISP.            
           MOVE RECORDS-INSERTED TO RECORDS-INSERTED-DISP.              
           MOVE RECORDS-UPDATED TO RECORDS-UPDATED-DISP.                
           MOVE RECORDS-ERRORS TO RECORDS-ERRORS-DISP.                  
           MOVE COMMIT-BATCHES TO COMMIT-BATCHES-DISP.                  
           MOVE COMMIT-COUNTER TO COMMIT-COUNTER-DISP.                  
                                                                        
           DISPLAY '========================================'.          
           DISPLAY 'EMPLOYEE UPSERT SUMMARY'.                           
           DISPLAY '========================================'.          
           DISPLAY 'RECORDS PROCESSED:   ' RECORDS-PROCESSED-DISP.      
           DISPLAY 'RECORDS INSERTED:    ' RECORDS-INSERTED-DISP.       
           DISPLAY 'RECORDS UPDATED:     ' RECORDS-UPDATED-DISP.        
           DISPLAY 'RECORDS ERRORS:      ' RECORDS-ERRORS-DISP.         
           DISPLAY 'COMMIT BATCHES:      ' COMMIT-BATCHES-DISP.         
           DISPLAY 'UNCOMMITTED RECORDS: ' COMMIT-COUNTER-DISP.         
           DISPLAY '========================================'.          
