      ******************************************************************
      * DB2 UPSERT - EMPLOYEE SYNC WITH UPDATE OR INSERT               *
      *                                                                *
      * PURPOSE:                                                       *
      * SYNCHRONIZES EMPLOYEE DATA FROM EXTERNAL FILE TO DB2 TABLE.    *
      * IF EMPLOYEE EXISTS - UPDATE; IF NEW - INSERT (UPSERT LOGIC).   *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   PHASE 1 - VALIDATE:                                          *
      *     EMP-ID NOT SPACES, EMP-NAME NOT SPACES,                    *
      *     EMP-SALARY > 0 AND NUMERIC,                                *
      *     EMP-STATUS IN ('A','I').                                   *
      *     ANY FAILURE: LOG ERROR, SET VALID-ERROR, SKIP DB2 WORK.    *
      *   PHASE 2 - CHECK EXISTENCE:                                   *
      *     SELECT EMP_NAME, SALARY FROM TB_EMPLOYEES WHERE EMP_ID.    *
      *     SQLCODE  0  : EXISTS   -> PERFORM UPDATE-EMPLOYEE.         *
      *       LOG SALARY CHANGE IF HV-OLD-SALARY != HV-SALARY.         *
      *     SQLCODE 100 : NOT FOUND -> PERFORM INSERT-EMPLOYEE.        *
      *     OTHER       : LOG ERROR, SKIP.                             *
      *   PHASE 3 - UPDATE: UPDATE ALL FIELDS. LOG SALARY CHANGE       *
      *     IF HV-OLD-SALARY != HV-SALARY, ELSE LOG NO-CHANGE.         *
      *   PHASE 4 - INSERT: INSERT ALL FIELDS. LOG SUCCESS OR ERROR.   *
      *   PHASE 5 - COMMIT: EVERY 50 SUCCESSFUL OPERATIONS.            *
      *     FINAL COMMIT IN CLOSE-ALL-FILES FOR REMAINING RECORDS.     *
      *     ROLLBACK ON CRITICAL SQLCODES (< -900).                    *
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
           SELECT EMP-FILE ASSIGN TO INDD                               
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS EMP-FILE-STATUS.                           
                                                                        
           SELECT LOG-FILE ASSIGN TO OUTDD                              
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS LOG-STATUS.                                
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD EMP-FILE RECORDING MODE IS F.                                 
       01 EMP-REC.                                                      
          05 EMP-ID PIC X(5).                                           
          05 EMP-NAME PIC X(20).                                        
          05 EMP-DEPT PIC X(3).                                         
          05 EMP-SALARY PIC S9(5)V99.                                   
          05 EMP-HIRE-DATE PIC 9(8).                                    
          05 EMP-STATUS PIC X(1).                                       
                                                                        
       FD LOG-FILE RECORDING MODE IS V.                                 
       01 LOG-REC PIC X(80).                                            
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
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
                                                                        
      * FILE STATUS VARIABLES                                           
       01 FILE-STATUSES.                                                
          05 LOG-STATUS PIC X(2).                                       
          05 EMP-FILE-STATUS PIC X(2).                                  
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
          05 WS-EOF PIC X(1) VALUE 'N'.                                 
             88 EOF VALUE 'Y'.                                          
          05 WS-ERROR-FIND PIC X(1) VALUE 'N'.                          
             88 ERROR-FIND VALUE 'Y'.                                   
             88 ERROR-NOT-FIND VALUE 'N'.                               
                                                                        
      * STATISTICS COUNTERS                                             
       01 WS-COUNTERS.                                                  
          05 RECORDS-PROCESSED PIC 9(5) VALUE 0.                        
          05 RECORDS-INSERTED PIC 9(5) VALUE 0.                         
          05 RECORDS-UPDATED PIC 9(5) VALUE 0.                          
          05 RECORDS-ERRORS PIC 9(5) VALUE 0.                           
          05 COMMIT-COUNTER PIC 9(5) VALUE 0.                           
          05 COMMIT-BATCHES PIC 9(5) VALUE 0.                           
                                                                        
      * DISPLAY-FORMATTED COUNTERS                                      
       01 WS-DISP-COUNTERS.                                             
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
                                                                        
      **********************************************                    
      * OPENS FILES, PROCESSES ALL RECORDS, CLOSES,                     
      * RUNS FINAL COMMIT, THEN DISPLAYS SUMMARY.                       
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
           PERFORM OPEN-ALL-FILES.                                      
           PERFORM PROCESS-ALL-RECORDS.                                 
           PERFORM CLOSE-ALL-FILES.                                     
           PERFORM DISPLAY-SUMMARY.                                     
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * OPENS EMP-FILE (INPUT) AND LOG-FILE (OUTPUT).                   
      * STOPS ON ANY NON-ZERO STATUS.                                   
      **********************************************                    
       OPEN-ALL-FILES.                                                  
           OPEN INPUT EMP-FILE.                                         
           IF EMP-FILE-STATUS NOT = '00'                                
              DISPLAY 'ERROR OPENING INP STATUS: ' EMP-FILE-STATUS      
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT LOG-FILE.                                        
           IF LOG-STATUS NOT = '00'                                     
              DISPLAY 'ERROR OPENING OUT STATUS: ' LOG-STATUS           
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * READS EMP-FILE UNTIL EOF.                                       
      * RESETS ERROR-NOT-FIND FLAG PER RECORD.                          
      * RUNS ALL 4 VALIDATIONS REGARDLESS - EACH                        
      * LOGS ITS OWN ERROR IF TRIGGERED.                                
      * NOTE: ALL ERRORS PER RECORD ARE LOGGED BEFORE SKIP.             
      * IF ANY VALIDATION SET ERROR-NOT-FIND: SKIP DB2.                 
      * ELSE: PROCESS-EMPLOYEE + BATCH COMMIT CHECK.                    
      **********************************************                    
       PROCESS-ALL-RECORDS.                                             
           PERFORM UNTIL EOF                                            
              SET ERROR-NOT-FIND TO TRUE                                
              READ EMP-FILE                                             
                AT END                                                  
                   SET EOF TO TRUE                                      
                NOT AT END                                              
                   IF EMP-FILE-STATUS NOT = '00'                        
                      DISPLAY 'ERROR READING INPUT FILE: '              
                          EMP-FILE-STATUS                               
                      MOVE RECORDS-PROCESSED TO RECORDS-PROCESSED-DISP  
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
                   PERFORM VALIDATE-SALARY                              
                   PERFORM VALIDATE-STATUSS                             
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
      * VALIDATES EMP-ID NOT SPACES.                                    
      * SETS ERROR-FIND AND LOGS IF EMPTY.                              
      **********************************************                    
       VALIDATE-ID.                                                     
           IF EMP-ID = SPACES                                           
              SET ERROR-FIND TO TRUE                                    
              MOVE 'ID VALIDATION ERROR: ID IS EMPTY' TO WS-OUT-MSG     
              PERFORM WRITE-LOG-MESSAGE                                 
           END-IF.                                                      
                                                                        
      **********************************************                    
      * VALIDATES EMP-NAME NOT SPACES.                                  
      * SETS ERROR-FIND AND LOGS WITH ID IF EMPTY.                      
      **********************************************                    
       VALIDATE-NAME.                                                   
           IF EMP-NAME = SPACES                                         
              SET ERROR-FIND TO TRUE                                    
              STRING 'VALIDATION ERROR: NAME IS EMPTY (ID='             
                      DELIMITED BY SIZE                                 
                      EMP-ID DELIMITED BY SIZE                          
                      ')' DELIMITED BY SIZE                             
                      INTO WS-OUT-MSG                                   
              END-STRING                                                
              PERFORM WRITE-LOG-MESSAGE                                 
           END-IF.                                                      
                                                                        
      **********************************************                    
      * VALIDATES EMP-SALARY: NUMERIC, NOT NEGATIVE, NOT ZERO.          
      * SETS ERROR-FIND ON ANY FAILURE.                                 
      **********************************************                    
        VALIDATE-SALARY.                                                
           EVALUATE TRUE                                                
               WHEN EMP-SALARY IS NOT NUMERIC                           
                 SET ERROR-FIND TO TRUE                                 
                 MOVE 'SALARY VALIDATION ERROR: NON-NUMERIC' TO         
                       WS-OUT-MSG                                       
                 PERFORM WRITE-LOG-MESSAGE                              
               WHEN EMP-SALARY IS NEGATIVE                              
                 SET ERROR-FIND TO TRUE                                 
                 MOVE 'SALARY VALIDATION ERROR: NEGATIVE VALUE' TO      
                       WS-OUT-MSG                                       
                 PERFORM WRITE-LOG-MESSAGE                              
               WHEN EMP-SALARY IS ZERO                                  
                 SET ERROR-FIND TO TRUE                                 
                 MOVE 'SALARY VALIDATION ERROR: ZERO VALUE' TO          
                       WS-OUT-MSG                                       
                 PERFORM WRITE-LOG-MESSAGE                              
           END-EVALUATE.                                                
                                                                        
      **********************************************                    
      * VALIDATES EMP-STATUS IS 'A' OR 'I'.                             
      * SETS ERROR-FIND AND LOGS IF INVALID.                            
      **********************************************                    
       VALIDATE-STATUSS.                                                
           IF EMP-STATUS NOT = 'A' AND EMP-STATUS NOT = 'I'             
              SET ERROR-FIND TO TRUE                                    
              MOVE 'STATUS VALIDATION ERROR' TO WS-OUT-MSG              
              PERFORM WRITE-LOG-MESSAGE                                 
           END-IF.                                                      
                                                                        
      **********************************************                    
      * WRITES WS-OUT-MSG TO LOG-FILE PREFIXED WITH EMP-ID.             
      * CLEARS BOTH BUFFERS AFTER WRITE.                                
      * ROLLBACK AND STOP ON WRITE FAILURE.                             
      **********************************************                    
       WRITE-LOG-MESSAGE.                                               
           STRING EMP-ID DELIMITED BY SIZE                              
                  ' ' DELIMITED BY SIZE                                 
                  WS-OUT-MSG DELIMITED BY SIZE                          
                  INTO LOG-REC                                          
           END-STRING.                                                  
           WRITE LOG-REC.                                               
           IF LOG-STATUS NOT = '00'                                     
              DISPLAY 'ERROR WRITING OUTPUT FILE IN WRITE-LOG-MESSAGE: '
                       LOG-STATUS                                       
              DISPLAY 'EMPLOYEE ID: ' EMP-ID                            
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              STOP RUN                                                  
           END-IF.                                                      
           MOVE SPACES TO LOG-REC.                                      
           MOVE SPACES TO WS-OUT-MSG.                                   
                                                                        
      **********************************************                    
      * MOVES EMP-REC TO HOST VARIABLES.                                
      * COMPUTES HV-EMP-NAME-LEN VIA REVERSE/INSPECT.                   
      * CALLS FORMAT-HIRE-DATE TO BUILD YYYY-MM-DD.                     
      * SELECTS OLD VALUES FROM DB2:                                    
      *   SQLCODE  0  : EMPLOYEE EXISTS - UPDATE.                       
      *   SQLCODE 100 : NOT FOUND - INSERT.                             
      *   OTHER       : LOG SELECT ERROR, SKIP.                         
      **********************************************                    
       PROCESS-EMPLOYEE.                                                
           MOVE EMP-ID TO HV-EMP-ID.                                    
           MOVE EMP-DEPT TO HV-EMP-DEPT.                                
           MOVE EMP-SALARY TO HV-SALARY.                                
           MOVE EMP-STATUS TO HV-STATUS.                                
           MOVE SPACES TO HV-EMP-NAME-TEXT.                             
           MOVE EMP-NAME TO HV-EMP-NAME-TEXT.                           
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
      * CONVERTS EMP-HIRE-DATE FROM YYYYMMDD                            
      * TO YYYY-MM-DD FORMAT IN HV-HIRE-DATE.                           
      **********************************************                    
       FORMAT-HIRE-DATE.                                                
           MOVE EMP-HIRE-DATE(1:4) TO WS-YEAR                           
           MOVE EMP-HIRE-DATE(5:2) TO WS-MONTH                          
           MOVE EMP-HIRE-DATE(7:2) TO WS-DAY                            
           STRING WS-YEAR DELIMITED BY SIZE                             
                  '-' DELIMITED BY SIZE                                 
                  WS-MONTH DELIMITED BY SIZE                            
                  '-' DELIMITED BY SIZE                                 
                  WS-DAY DELIMITED BY SIZE                              
                  INTO HV-HIRE-DATE                                     
           END-STRING.                                                  
                                                                        
      **********************************************                    
      * UPDATES ALL FIELDS FOR EXISTING EMPLOYEE.                       
      * SQLCODE 0: LOG SALARY CHANGE OR NO-CHANGE.                      
      * CRITICAL SQLCODE: ROLLBACK AND STOP RUN.                        
      * OTHER ERROR: LOG ERROR LINE.                                    
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
                 STRING 'UPDATE (SALARY CHANGE FROM ' DELIMITED BY SIZE 
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
              IF SQLCODE < -900                                         
                 DISPLAY 'CRITICAL UPDATE ERROR: ' WS-SQLCODE-DISP      
                 DISPLAY 'EMPLOYEE ID: ' EMP-ID                         
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
      * SQLCODE 0: LOG SUCCESS, INCREMENT COUNTERS.                     
      * CRITICAL SQLCODE: ROLLBACK AND STOP RUN.                        
      * OTHER ERROR: LOG ERROR LINE.                                    
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
              MOVE 'INSERTED (NEW EMPLOYEE)' TO WS-OUT-MSG              
              PERFORM WRITE-LOG-MESSAGE                                 
           ELSE                                                         
              MOVE SQLCODE TO WS-SQLCODE-DISP                           
              IF SQLCODE < -900                                         
                 DISPLAY 'CRITICAL INSERT ERROR: ' WS-SQLCODE-DISP      
                 DISPLAY 'EMPLOYEE ID: ' EMP-ID                         
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
      * COMMITS REMAINING UNCOMMITTED OPERATIONS.                       
      * ROLLBACK AND STOP ON COMMIT FAILURE.                            
      * THEN CLOSES BOTH FILES (NON-ZERO = WARNING).                    
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
                                                                        
           CLOSE EMP-FILE.                                              
           IF EMP-FILE-STATUS NOT = '00'                                
              DISPLAY 'WARNING: ERROR CLOSING INPUT FILE: '             
                       EMP-FILE-STATUS                                  
           END-IF.                                                      
                                                                        
           CLOSE LOG-FILE.                                              
           IF LOG-STATUS NOT = '00'                                     
              DISPLAY 'WARNING: ERROR CLOSING OUTPUT FILE: '            
                       LOG-STATUS                                       
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
                                                                        
           DISPLAY '========================================'.          
           DISPLAY 'EMPLOYEE UPSERT SUMMARY'.                           
           DISPLAY '========================================'.          
           DISPLAY 'RECORDS PROCESSED:   ' RECORDS-PROCESSED-DISP.      
           DISPLAY 'RECORDS INSERTED:    ' RECORDS-INSERTED-DISP.       
           DISPLAY 'RECORDS UPDATED:     ' RECORDS-UPDATED-DISP.        
           DISPLAY 'RECORDS ERRORS:      ' RECORDS-ERRORS-DISP.         
           DISPLAY 'COMMIT BATCHES:      ' COMMIT-BATCHES-DISP.         
           DISPLAY '========================================'.          
