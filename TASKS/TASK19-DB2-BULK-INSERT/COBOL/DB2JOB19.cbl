      ******************************************************************
      * DB2 BULK INSERT - CUSTOMER DATA IMPORT WITH VALIDATION         *
      *                                                                *
      * PURPOSE:                                                       *
      * READS NEW CUSTOMER DATA FROM SEQUENTIAL FILE, VALIDATES EACH   *
      * RECORD, AND INSERTS INTO DB2 TABLE TB_CUSTOMERS.               *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   PHASE 1 - READ: READ CUSTOMER-FILE ONE RECORD AT A TIME.     *
      *   PHASE 2 - VALIDATE:                                          *
      *     INP-ID NOT SPACES  (MANDATORY).                            *
      *     INP-EMAIL CONTAINS EXACTLY ONE '@' SYMBOL.                 *
      *     INP-PHONE IS NUMERIC AND LENGTH = 10.                      *
      *     ANY FAILURE: WRITE ERROR LINE TO LOG-FILE, SKIP INSERT.    *
      *   PHASE 3 - INSERT: MOVE TO HOST VARIABLES, EXEC SQL INSERT.   *
      *     SQLCODE  0   : SUCCESS - WRITE OK LINE, INCREMENT PENDING. *
      *     SQLCODE -803 : DUPLICATE KEY - WRITE ERROR LINE.           *
      *     OTHER        : CRITICAL IF < -900 - ROLLBACK.              *
      *   PHASE 4 - COMMIT:                                            *
      *     COMMIT EVERY 100 SUCCESSFUL INSERTS (COMMIT-PENDING >= 100)*
      *     FINAL COMMIT IN CLOSE-ALL-FILES FOR REMAINING RECORDS.     *
      *     ROLLBACK ON CRITICAL ERRORS BEFORE STOP RUN.               *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2026/01/15                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT:  INDD (NEW.CUSTOMERS) - NEW CUSTOMER DATA (PS, 93 B)    *
      * OUTPUT: OUTDD (SUCCESS.LOG) - IMPORT RESULTS LOG (PS, V)       *
      ******************************************************************
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. DB2JOB19.                                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT INPUT-FILE ASSIGN TO INDD                             
             ORGANIZATION IS SEQUENTIAL                                 
             FILE STATUS IS INPUT-STATUS.                               
                                                                        
           SELECT OUTPUT-FILE ASSIGN TO OUTDD                           
             ORGANIZATION IS SEQUENTIAL                                 
             FILE STATUS IS OUTPUT-STATUS.                              
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD INPUT-FILE RECORDING MODE IS F.                               
       01 INPUT-REC.                                                    
          05 INP-ID PIC X(6).                                           
          05 INP-NAME PIC X(30).                                        
          05 INP-EMAIL PIC X(40).                                       
          05 INP-PHONE PIC X(10).                                       
          05 INP-LIMIT PIC 9(5)V99.                                     
                                                                        
       FD OUTPUT-FILE RECORDING MODE IS V.                              
       01 OUTPUT-REC PIC X(80).                                         
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * SQL COMMUNICATION AREA                                          
           EXEC SQL                                                     
             INCLUDE SQLCA                                              
           END-EXEC.                                                    
                                                                        
      * DB2 HOST VARIABLES                                              
       01  HV-CUST-ID      PIC X(6).                                    
       01  HV-CUST-NAME.                                                
           49 HV-CUST-NAME-LEN  PIC S9(4) COMP-5.                       
           49 HV-CUST-NAME-TEXT PIC X(30).                              
       01  HV-EMAIL.                                                    
           49 HV-EMAIL-LEN      PIC S9(4) COMP-5.                       
           49 HV-EMAIL-TEXT     PIC X(40).                              
       01  HV-PHONE        PIC X(10).                                   
       01  HV-CREDIT       PIC S9(7)V99 COMP-3.                         
                                                                        
      * FORMATTED SQLCODE FOR DISPLAY                                   
       77 WS-SQLCODE-DISP  PIC -Z(9)9.                                  
                                                                        
      * FILE STATUS VARIABLES                                           
       01 FILE-STATUSES.                                                
          05 INPUT-STATUS PIC X(2).                                     
          05 OUTPUT-STATUS PIC X(2).                                    
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
          05 WS-EOF PIC X(1) VALUE 'N'.                                 
             88 EOF VALUE 'Y'.                                          
                                                                        
      * STATISTICS COUNTERS                                             
       01 WS-COUNTERS.                                                  
          05 RECORDS-PROCESSED PIC 9(5) VALUE 0.                        
          05 RECORDS-INSERTED PIC 9(5) VALUE 0.                         
          05 RECORDS-ERRORS PIC 9(5) VALUE 0.                           
          05 COMMIT-COUNTER PIC 9(5) VALUE 0.                           
          05 COMMIT-BATCHES PIC 9(5) VALUE 0.                           
                                                                        
      * DISPLAY-FORMATTED COUNTERS                                      
       01 WS-DISP-COUNTERS.                                             
          05 RECORDS-PROCESSED-DISP PIC Z(4)9.                          
          05 RECORDS-INSERTED-DISP PIC Z(4)9.                           
          05 RECORDS-ERRORS-DISP PIC Z(4)9.                             
          05 COMMIT-COUNTER-DISP PIC Z(4)9.                             
          05 COMMIT-BATCHES-DISP PIC Z(4)9.                             
                                                                        
      * VALIDATION WORKING VARIABLES                                    
       01 VALIDATION-VARS.                                              
          05 WS-PHONE-LEN PIC 9(2) VALUE 0.                             
          05 WS-EMAIL-COUNT PIC 9(2) VALUE 0.                           
          05 WS-NAME-LEN PIC 9(2) VALUE 0.                              
          05 WS-EMAIL-LEN PIC 9(2) VALUE 0.                             
                                                                        
      * OUTPUT MESSAGE BUFFER                                           
       01 WS-OUT-MSG PIC X(100).                                        
                                                                        
      **********************************************                    
      * OPENS FILES, PROCESSES ALL RECORDS, CLOSES,                     
      * RUNS FINAL COMMIT, THEN DISPLAYS SUMMARY.                       
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-PARA.                                                       
           PERFORM OPEN-ALL-FILES.                                      
           PERFORM PROCESS-ALL-RECORDS.                                 
           PERFORM CLOSE-ALL-FILES.                                     
           PERFORM DISPLAY-SUMMARY.                                     
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * OPENS CUSTOMER-FILE (INPUT) AND LOG-FILE (OUTPUT).              
      * STOPS ON ANY NON-ZERO STATUS.                                   
      **********************************************                    
       OPEN-ALL-FILES.                                                  
           OPEN INPUT INPUT-FILE.                                       
           IF INPUT-STATUS NOT = '00'                                   
              DISPLAY 'ERROR OPENING INPUT-FILE: ' INPUT-STATUS         
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT OUTPUT-FILE.                                     
           IF OUTPUT-STATUS NOT = '00'                                  
              DISPLAY 'ERROR OPENING OUTPUT FILE: ' OUTPUT-STATUS       
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * READS CUSTOMER-FILE UNTIL EOF.                                  
      * FOR EACH RECORD: VALIDATES ID FIRST,                            
      * THEN EMAIL, THEN PHONE. FIRST FAILURE                           
      * LOGS ERROR AND SKIPS TO NEXT RECORD.                            
      **********************************************                    
       PROCESS-ALL-RECORDS.                                             
           PERFORM UNTIL EOF                                            
               READ INPUT-FILE                                          
                  AT END                                                
                      SET EOF TO TRUE                                   
                   NOT AT END                                           
                      ADD 1 TO RECORDS-PROCESSED                        
                      IF INP-ID = SPACES                                
                          PERFORM REPORT-ID-ERROR                       
                      ELSE                                              
                          PERFORM VALIDATE-EMAIL                        
                      END-IF                                            
               END-READ                                                 
           END-PERFORM.                                                 
                                                                        
      **********************************************                    
      * VALIDATION FAILED: CUST-ID IS SPACES.                           
      * WRITES ERROR LINE TO LOG-FILE.                                  
      **********************************************                    
       REPORT-ID-ERROR.                                                 
           ADD 1 TO RECORDS-ERRORS.                                     
           MOVE SPACES TO OUTPUT-REC.                                   
           MOVE SPACES TO WS-OUT-MSG.                                   
           MOVE 'VALIDATION ERROR: ID IS NULL' TO WS-OUT-MSG.           
           STRING INP-ID DELIMITED BY SIZE                              
                  ' ' DELIMITED BY SIZE                                 
                  WS-OUT-MSG DELIMITED BY SIZE                          
                  INTO OUTPUT-REC                                       
           END-STRING.                                                  
           WRITE OUTPUT-REC.                                            
           IF OUTPUT-STATUS NOT = '00'                                  
              DISPLAY 'ERROR WRITING OUTPUT FILE IN REPORT-ID-ERROR: '  
                       OUTPUT-STATUS                                    
              DISPLAY 'CUSTOMER ID: ' INP-ID                            
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * COUNTS '@' IN CUST-EMAIL.                                       
      * EXACTLY ONE '@': PROCEED TO VALIDATE-PHONE.                     
      * OTHER COUNT: LOG EMAIL ERROR.                                   
      **********************************************                    
       VALIDATE-EMAIL.                                                  
           MOVE 0 TO WS-EMAIL-COUNT.                                    
           INSPECT INP-EMAIL TALLYING WS-EMAIL-COUNT FOR ALL "@".       
           IF WS-EMAIL-COUNT = 1                                        
              PERFORM VALIDATE-PHONE                                    
           ELSE                                                         
              PERFORM REPORT-EMAIL-ERROR                                
           END-IF.                                                      
                                                                        
      **********************************************                    
      * VALIDATION FAILED: INVALID EMAIL FORMAT.                        
      * WRITES ERROR LINE TO LOG-FILE.                                  
      **********************************************                    
       REPORT-EMAIL-ERROR.                                              
           ADD 1 TO RECORDS-ERRORS.                                     
           MOVE SPACES TO OUTPUT-REC.                                   
           MOVE SPACES TO WS-OUT-MSG.                                   
           MOVE 'VALIDATION ERROR: INVALID EMAIL' TO WS-OUT-MSG.        
           STRING INP-ID DELIMITED BY SIZE                              
                  ' ' DELIMITED BY SIZE                                 
                  WS-OUT-MSG DELIMITED BY SIZE                          
                  INTO OUTPUT-REC                                       
           END-STRING.                                                  
           WRITE OUTPUT-REC.                                            
           IF OUTPUT-STATUS NOT = '00'                                  
              DISPLAY 'ERROR WRITING OUT FILE IN REPORT-EMAIL-ERROR: '  
                       OUTPUT-STATUS                                    
              DISPLAY 'CUSTOMER ID: ' INP-ID                            
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * TRIMS CUST-PHONE AND CHECKS LENGTH = 10                         
      * AND ALL DIGITS (IS NUMERIC).                                    
      * PASS: CALL INSERT-CUSTOMER.                                     
      * FAIL: LOG PHONE ERROR.                                          
      **********************************************                    
       VALIDATE-PHONE.                                                  
           COMPUTE WS-PHONE-LEN =                                       
                   FUNCTION LENGTH(FUNCTION TRIM(INP-PHONE)).           
           IF WS-PHONE-LEN NOT = 10 OR INP-PHONE IS NOT NUMERIC         
              PERFORM REPORT-PHONE-ERROR                                
           ELSE                                                         
              PERFORM INSERT-CUSTOMER                                   
           END-IF.                                                      
                                                                        
      **********************************************                    
      * VALIDATION FAILED: INVALID PHONE NUMBER.                        
      * WRITES ERROR LINE TO LOG-FILE.                                  
      **********************************************                    
       REPORT-PHONE-ERROR.                                              
           ADD 1 TO RECORDS-ERRORS.                                     
           MOVE SPACES TO OUTPUT-REC.                                   
           MOVE SPACES TO WS-OUT-MSG.                                   
           MOVE 'VALIDATION ERROR: INVALID PHONE' TO WS-OUT-MSG.        
           STRING INP-ID DELIMITED BY SIZE                              
                  ' ' DELIMITED BY SIZE                                 
                  WS-OUT-MSG DELIMITED BY SIZE                          
                  INTO OUTPUT-REC                                       
           END-STRING.                                                  
           WRITE OUTPUT-REC.                                            
           IF OUTPUT-STATUS NOT = '00'                                  
              DISPLAY 'ERROR WRITING OUT FILE IN REPORT-PHONE-ERROR: '  
                       OUTPUT-STATUS                                    
              DISPLAY 'CUSTOMER ID: ' INP-ID                            
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * MOVES CUSTOMER-REC FIELDS TO HOST VARIABLES.                    
      * COMPUTES VARCHAR LENGTHS FOR NAME AND EMAIL                     
      * BY COUNTING TRAILING SPACES VIA REVERSE/INSPECT.                
      * EXECUTES DB2 INSERT THEN EVALUATES SQLCODE.                     
      **********************************************                    
       INSERT-CUSTOMER.                                                 
           MOVE INP-ID TO HV-CUST-ID.                                   
           MOVE INP-PHONE TO HV-PHONE.                                  
           MOVE INP-LIMIT TO HV-CREDIT.                                 
                                                                        
           MOVE SPACES TO HV-CUST-NAME-TEXT.                            
           MOVE INP-NAME TO HV-CUST-NAME-TEXT.                          
           MOVE 0 TO WS-NAME-LEN.                                       
           INSPECT FUNCTION REVERSE(HV-CUST-NAME-TEXT)                  
                   TALLYING WS-NAME-LEN FOR LEADING SPACES.             
           COMPUTE HV-CUST-NAME-LEN = 30 - WS-NAME-LEN.                 
                                                                        
           MOVE SPACES TO HV-EMAIL-TEXT.                                
           MOVE INP-EMAIL TO HV-EMAIL-TEXT.                             
           MOVE 0 TO WS-EMAIL-LEN.                                      
           INSPECT FUNCTION REVERSE(HV-EMAIL-TEXT)                      
                   TALLYING WS-EMAIL-LEN FOR LEADING SPACES.            
           COMPUTE HV-EMAIL-LEN = 40 - WS-EMAIL-LEN.                    
                                                                        
           EXEC SQL                                                     
             INSERT INTO TB_CUSTOMERS                                   
             (CUST_ID,CUST_NAME,EMAIL,PHONE,CREDIT_LIMIT)               
             VALUES                                                     
             (:HV-CUST-ID,                                              
              :HV-CUST-NAME,                                            
              :HV-EMAIL,                                                
              :HV-PHONE,                                                
              :HV-CREDIT)                                               
           END-EXEC.                                                    
                                                                        
           EVALUATE SQLCODE                                             
               WHEN 0                                                   
                 PERFORM REPORT-SUCCESS                                 
                 ADD 1 TO COMMIT-COUNTER                                
                 IF COMMIT-COUNTER >= 100                               
                    EXEC SQL                                            
                      COMMIT WORK                                       
                    END-EXEC                                            
                    IF SQLCODE NOT = 0                                  
                       MOVE SQLCODE TO WS-SQLCODE-DISP                  
                       DISPLAY 'BATCH COMMIT ERROR: ' WS-SQLCODE-DISP   
                       EXEC SQL                                         
                         ROLLBACK WORK                                  
                       END-EXEC                                         
                       STOP RUN                                         
                    END-IF                                              
                    ADD 1 TO COMMIT-BATCHES                             
                    MOVE 0 TO COMMIT-COUNTER                            
                 END-IF                                                 
               WHEN -803                                                
                 PERFORM REPORT-DUPLICATE-KEY                           
               WHEN OTHER                                               
                 PERFORM REPORT-DB2-ERROR                               
               END-EVALUATE.                                            
                                                                        
      **********************************************                    
      * INSERT SUCCEEDED (SQLCODE = 0).                                 
      * WRITES OK LINE TO LOG-FILE.                                     
      **********************************************                    
       REPORT-SUCCESS.                                                  
           ADD 1 TO RECORDS-INSERTED.                                   
           MOVE SPACES TO OUTPUT-REC.                                   
           MOVE SPACES TO WS-OUT-MSG.                                   
           MOVE 'INSERTED OK' TO WS-OUT-MSG.                            
           STRING INP-ID DELIMITED BY SIZE                              
                  ' ' DELIMITED BY SIZE                                 
                  WS-OUT-MSG DELIMITED BY SIZE                          
                  INTO OUTPUT-REC                                       
           END-STRING.                                                  
           WRITE OUTPUT-REC.                                            
           IF OUTPUT-STATUS NOT = '00'                                  
              DISPLAY 'ERROR WRITING OUTPUT FILE IN REPORT-SUCCESS: '   
                       OUTPUT-STATUS                                    
              DISPLAY 'CUSTOMER ID: ' INP-ID                            
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * INSERT FAILED WITH SQLCODE -803.                                
      * DUPLICATE PRIMARY KEY - WRITES ERROR LINE.                      
      **********************************************                    
       REPORT-DUPLICATE-KEY.                                            
           ADD 1 TO RECORDS-ERRORS.                                     
           MOVE SPACES TO OUTPUT-REC.                                   
           MOVE SPACES TO WS-OUT-MSG.                                   
           MOVE 'DB2 ERROR: DUPLICATE PRIMARY KEY' TO WS-OUT-MSG.       
           STRING INP-ID DELIMITED BY SIZE                              
                  ' ' DELIMITED BY SIZE                                 
                  WS-OUT-MSG DELIMITED BY SIZE                          
                  INTO OUTPUT-REC                                       
           END-STRING.                                                  
           WRITE OUTPUT-REC.                                            
           IF OUTPUT-STATUS NOT = '00'                                  
              DISPLAY 'ERROR WRITING OUT FILE IN REPORT-DUPLICATE-KEY: '
                       OUTPUT-STATUS                                    
              DISPLAY 'CUSTOMER ID: ' INP-ID                            
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * INSERT FAILED WITH UNEXPECTED SQLCODE.                          
      * CRITICAL CODES (< -900, -911, -913):                            
      * ROLLBACK AND STOP RUN IMMEDIATELY.                              
      * NON-CRITICAL: WRITE ERROR LINE TO LOG-FILE.                     
      **********************************************                    
       REPORT-DB2-ERROR.                                                
           ADD 1 TO RECORDS-ERRORS.                                     
           IF SQLCODE < -900                                            
              MOVE SQLCODE TO WS-SQLCODE-DISP                           
              DISPLAY 'CRITICAL DB2 ERROR SQLCODE: ' WS-SQLCODE-DISP    
              DISPLAY 'ROLLING BACK ALL UNCOMMITTED CHANGES'            
              DISPLAY 'CUSTOMER ID: ' INP-ID                            
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           MOVE SPACES TO OUTPUT-REC.                                   
           MOVE SPACES TO WS-OUT-MSG.                                   
           MOVE SQLCODE TO WS-SQLCODE-DISP.                             
           STRING INP-ID DELIMITED BY SIZE                              
                  ' DB2 ERROR SQLCODE: ' DELIMITED BY SIZE              
                  WS-SQLCODE-DISP DELIMITED BY SIZE                     
                  INTO OUTPUT-REC                                       
           END-STRING.                                                  
           WRITE OUTPUT-REC.                                            
           IF OUTPUT-STATUS NOT = '00'                                  
              DISPLAY 'ERROR WRITING OUTPUT FILE IN REPORT-DB2-ERROR: ' 
                       OUTPUT-STATUS                                    
              DISPLAY 'CUSTOMER ID: ' INP-ID                            
              EXEC SQL                                                  
                ROLLBACK WORK                                           
              END-EXEC                                                  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * COMMITS REMAINING UNCOMMITTED INSERTS BEFORE CLOSING.           
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
                                                                        
           CLOSE INPUT-FILE.                                            
           IF INPUT-STATUS NOT = '00'                                   
              DISPLAY 'WARNING: ERROR CLOSING INPUT FILE: '             
                       INPUT-STATUS                                     
           END-IF.                                                      
                                                                        
           CLOSE OUTPUT-FILE.                                           
           IF OUTPUT-STATUS NOT = '00'                                  
              DISPLAY 'WARNING: ERROR CLOSING OUTPUT FILE: '            
                       OUTPUT-STATUS                                    
           END-IF.                                                      
                                                                        
      **********************************************                    
      * DISPLAY SUMMARY STATISTICS TO SYSOUT                            
      **********************************************                    
       DISPLAY-SUMMARY.                                                 
           MOVE RECORDS-PROCESSED TO RECORDS-PROCESSED-DISP.            
           MOVE RECORDS-INSERTED TO RECORDS-INSERTED-DISP.              
           MOVE RECORDS-ERRORS TO RECORDS-ERRORS-DISP.                  
           MOVE COMMIT-BATCHES TO COMMIT-BATCHES-DISP.                  
                                                                        
           DISPLAY '========================================'.          
           DISPLAY 'CUSTOMER IMPORT SUMMARY'.                           
           DISPLAY '========================================'.          
           DISPLAY 'RECORDS PROCESSED:   ' RECORDS-PROCESSED-DISP.      
           DISPLAY 'RECORDS INSERTED:    ' RECORDS-INSERTED-DISP.       
           DISPLAY 'RECORDS ERRORS:      ' RECORDS-ERRORS-DISP.         
           DISPLAY 'COMMIT BATCHES:      ' COMMIT-BATCHES-DISP.         
           DISPLAY '========================================'.          
