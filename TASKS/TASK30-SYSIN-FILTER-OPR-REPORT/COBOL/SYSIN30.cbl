      ******************************************************************
      * SYSIN-DRIVEN FILTERED OPERATION REPORT                         *
      *                                                                *
      * PURPOSE:                                                       *
      * READS FILTER PARAMETERS FROM SYSIN (DATE RANGE, MIN AMOUNT,    *
      * OPERATION TYPE), SCANS VSAM KSDS OPERATION LOG SEQUENTIALLY,   *
      * WRITES MATCHING RECORDS TO PS FILTERED REPORT FILE.            *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   PHASE 1 - READ AND PARSE SYSIN PARAMETERS:                   *
      *     FROM-DATE= -> WS-FROM-DATE (DEFAULT: 00000000).            *
      *     TO-DATE=   -> WS-TO-DATE   (DEFAULT: 99999999).            *
      *     MIN-AMOUNT= -> WS-MIN-AMOUNT-NUM (DEFAULT: 0).             *
      *     OPR-TYPE=  -> WS-OPR-TYPE  (DEFAULT: '*' = ALL TYPES).     *
      *     UNKNOWN KEY -> SILENTLY IGNORED (CONTINUE).                *
      *     NO '=' IN LINE -> LINE SKIPPED (EXIT PARAGRAPH).           *
      *   PHASE 2 - SEQUENTIAL SCAN OF KSDS OPERATION LOG:             *
      *     OPR-DATE < WS-FROM-DATE          -> SKIP RECORD.           *
      *     OPR-DATE > WS-TO-DATE            -> SKIP RECORD.           *
      *     OPR-AMOUNT < WS-MIN-AMOUNT-NUM   -> SKIP RECORD.           *
      *     WS-OPR-TYPE != '*' AND TYPE MISMATCH -> SKIP RECORD.       *
      *     ALL CHECKS PASSED -> WRITE TO FILTERED REPORT.             *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2026/02/11                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT:  SYSIN   (SYSIN)            - JCL FILTER PARAMETERS     *
      *         OPRDD   (OPR.LOG.KSDS)     - VSAM KSDS OPERATION LOG   *
      * OUTPUT: REPDD   (FILTERED.REPORT)  - PS FILTERED REPORT FILE   *
      ******************************************************************
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. SYSIN30.                                             
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT SYSIN-FILE ASSIGN TO SYSIN                            
             FILE STATUS IS SYSIN-STATUS.                               
                                                                        
           SELECT OPR-LOG-FILE ASSIGN TO OPRDD                          
             ORGANIZATION IS INDEXED                                    
             ACCESS MODE IS SEQUENTIAL                                  
             RECORD KEY IS OPR-KEY                                      
             FILE STATUS IS OPR-LOG-STATUS.                             
                                                                        
           SELECT FILTERED-REPORT-FILE ASSIGN TO REPDD                  
             ORGANIZATION IS SEQUENTIAL                                 
             FILE STATUS IS FILTERED-REPORT-STATUS.                     
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD SYSIN-FILE RECORDING MODE IS F.                               
       01 SYSIN-REC PIC X(80).                                          
                                                                        
       FD OPR-LOG-FILE.                                                 
       01 OPR-LOG-REC.                                                  
          05 OPR-KEY.                                                   
             10 OPR-ACCT-ID PIC X(6).                                   
             10 OPR-DATE PIC X(8).                                      
             10 OPR-ID PIC X(6).                                        
          05 OPR-TYPE PIC X(1).                                         
          05 OPR-AMOUNT PIC 9(7)V99.                                    
          05 FILLER PIC X(50).                                          
                                                                        
       FD FILTERED-REPORT-FILE RECORDING MODE IS F.                     
       01 FILTERED-REP-REC PIC X(36).                                   
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
          05 WS-EOF-SYSIN PIC X(1) VALUE 'N'.                           
             88 EOF-SYSIN VALUE 'Y'.                                    
          05 WS-EOF-OPR PIC X(1) VALUE 'N'.                             
             88 EOF-OPR VALUE 'Y'.                                      
                                                                        
      * FILE STATUS VARIABLES                                           
       01 FILE-STATUSES.                                                
          05 SYSIN-STATUS PIC X(2).                                     
          05 OPR-LOG-STATUS PIC X(2).                                   
          05 FILTERED-REPORT-STATUS PIC X(2).                           
                                                                        
      * SYSIN FILTER PARAMETERS (WITH DEFAULTS)                         
       01 WS-SYSIN-PARAMS.                                              
          05 WS-FROM-DATE PIC X(8) VALUE '00000000'.                    
          05 WS-TO-DATE PIC X(8) VALUE '99999999'.                      
          05 WS-OPR-TYPE PIC X(1) VALUE '*'.                            
          05 WS-MIN-AMOUNT PIC X(9) VALUE '000000000'.                  
                                                                        
      * NUMERIC FORM OF MIN-AMOUNT FOR COMPARISON                       
       01 WS-MIN-AMOUNT-NUM PIC 9(7)V99.                                
                                                                        
      * OUTPUT REPORT LINE BUFFER                                       
       01 WS-REPORT-LINE.                                               
          05 WS-REP-ACCT-ID PIC X(6).                                   
          05 FILLER PIC X(1).                                           
          05 WS-REP-ORD-DATE PIC X(8).                                  
          05 FILLER PIC X(1).                                           
          05 WS-REP-ORD-TYPE PIC X(1).                                  
          05 FILLER PIC X(1).                                           
          05 WS-REP-AMOUNT PIC Z(6)9.99.                                
          05 FILLER PIC X(1).                                           
          05 WS-REP-OPR-ID PIC X(6).                                    
                                                                        
      * SYSIN PARSE WORK VARIABLES                                      
       01 WS-POS-EQUAL PIC 9(2) VALUE 0.                                
       01 WS-KEY PIC X(10).                                             
       01 WS-VALUE PIC X(70).                                           
                                                                        
      **********************************************                    
      * OPEN -> READ SYSIN -> READ OPR-LOG -> CLOSE                     
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
           PERFORM OPEN-ALL-FILES.                                      
           PERFORM READ-SYSIN-DATA.                                     
           PERFORM READ-OPR-LOG.                                        
           PERFORM CLOSE-ALL-FILES.                                     
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * READS SYSIN LINE BY LINE UNTIL EOF.                             
      * PER LINE: CALLS PARSE-SYSIN-LINE TO EXTRACT                     
      * FILTER PARAMETERS INTO WS-SYSIN-PARAMS.                         
      * STOPS ON ANY NON-ZERO READ STATUS.                              
      **********************************************                    
       READ-SYSIN-DATA.                                                 
           PERFORM UNTIL EOF-SYSIN                                      
              READ SYSIN-FILE                                           
                AT END                                                  
                   SET EOF-SYSIN TO TRUE                                
                NOT AT END                                              
                   IF SYSIN-STATUS = '00'                               
                      PERFORM PARSE-SYSIN-LINE                          
                   ELSE                                                 
                      DISPLAY 'ERROR READING SYSIN FILE: ' SYSIN-STATUS 
                      STOP RUN                                          
                   END-IF                                               
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
      **********************************************                    
      * FINDS '=' IN SYSIN-REC VIA INSPECT TALLYING.                    
      * NO '=' FOUND -> EXIT PARAGRAPH (LINE IGNORED).                  
      * SPLITS LINE INTO WS-KEY (BEFORE =)                              
      * AND WS-VALUE (AFTER =).                                         
      * EVALUATES WS-KEY:                                               
      *   FROM-DATE  -> WS-FROM-DATE  (IF NOT SPACES).                  
      *   TO-DATE    -> WS-TO-DATE    (IF NOT SPACES).                  
      *   MIN-AMOUNT -> WS-MIN-AMOUNT AND WS-MIN-AMOUNT-NUM.            
      *   OPR-TYPE   -> WS-OPR-TYPE   (IF NOT SPACES).                  
      *   OTHER      -> CONTINUE (SILENTLY IGNORED).                    
      **********************************************                    
       PARSE-SYSIN-LINE.                                                
           MOVE 0 TO WS-POS-EQUAL.                                      
           INSPECT SYSIN-REC TALLYING WS-POS-EQUAL                      
               FOR CHARACTERS BEFORE '='.                               
                                                                        
           IF WS-POS-EQUAL = LENGTH OF SYSIN-REC                        
              EXIT PARAGRAPH                                            
           END-IF.                                                      
                                                                        
           IF WS-POS-EQUAL + 1 >= LENGTH OF SYSIN-REC                   
              EXIT PARAGRAPH                                            
           END-IF.                                                      
                                                                        
           MOVE SPACES TO WS-KEY WS-VALUE.                              
           MOVE SYSIN-REC(1:WS-POS-EQUAL) TO WS-KEY.                    
           MOVE SYSIN-REC(WS-POS-EQUAL + 2:                             
                LENGTH OF SYSIN-REC - (WS-POS-EQUAL + 1)) TO WS-VALUE   
                                                                        
           EVALUATE TRUE                                                
               WHEN WS-KEY = 'FROM-DATE'                                
                 IF WS-VALUE(1:8) NOT = SPACES                          
                    MOVE WS-VALUE(1:8) TO WS-FROM-DATE                  
                 END-IF                                                 
               WHEN WS-KEY = 'TO-DATE'                                  
                 IF WS-VALUE(1:8) NOT = SPACES                          
                    MOVE WS-VALUE(1:8) TO WS-TO-DATE                    
                 END-IF                                                 
               WHEN WS-KEY = 'MIN-AMOUNT'                               
                 IF WS-VALUE(1:9) NOT = SPACES                          
                    COMPUTE WS-MIN-AMOUNT-NUM =                         
                        FUNCTION NUMVAL(WS-VALUE(1:9))                  
                 END-IF                                                 
               WHEN WS-KEY = 'OPR-TYPE'                                 
                 IF WS-VALUE(1:1) NOT = SPACES                          
                    MOVE WS-VALUE(1:1) TO WS-OPR-TYPE                   
                 END-IF                                                 
               WHEN OTHER                                               
                 CONTINUE                                               
           END-EVALUATE.                                                
                                                                        
      **********************************************                    
      * READS OPR-LOG-FILE (KSDS) SEQUENTIALLY UNTIL EOF.               
      * PER RECORD: CALLS CHECK-WITH-PARAMS TO APPLY                    
      * ALL FOUR FILTERS AND WRITE MATCHING RECORDS.                    
      * STOPS ON ANY NON-ZERO READ STATUS.        
      **********************************************                    
       READ-OPR-LOG.                                                    
           PERFORM UNTIL EOF-OPR                                        
              READ OPR-LOG-FILE                                         
                AT END                                                  
                   SET EOF-OPR TO TRUE                                  
                NOT AT END                                              
                   IF OPR-LOG-STATUS = '00'                             
                      PERFORM CHECK-WITH-PARAMS                         
                   ELSE                                                 
                      DISPLAY 'ERROR READING OPR-LOG FILE: '            
                               OPR-LOG-STATUS                           
                      STOP RUN                                          
                   END-IF                                               
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
      **********************************************                    
      * APPLIES FOUR FILTERS IN SEQUENCE (EARLY EXIT):                  
      *   1. OPR-DATE < WS-FROM-DATE    -> EXIT PARAGRAPH.              
      *   2. OPR-DATE > WS-TO-DATE      -> EXIT PARAGRAPH.              
      *   3. OPR-AMOUNT < WS-MIN-AMOUNT-NUM -> EXIT PARAGRAPH.          
      *   4. TYPE MISMATCH (NOT '*')    -> EXIT PARAGRAPH.              
      * ALL PASSED: FILLS WS-REPORT-LINE AND WRITES                     
      * TO FILTERED-REPORT-FILE. STOPS ON WRITE ERROR.    
      **********************************************                    
       CHECK-WITH-PARAMS.                                               
           IF OPR-DATE < WS-FROM-DATE                                   
              EXIT PARAGRAPH                                            
           END-IF.                                                      
                                                                        
           IF OPR-DATE > WS-TO-DATE                                     
              EXIT PARAGRAPH                                            
           END-IF.                                                      
                                                                        
           IF OPR-AMOUNT < WS-MIN-AMOUNT-NUM                            
              EXIT PARAGRAPH                                            
           END-IF.                                                      
                                                                        
           IF WS-OPR-TYPE NOT = '*' AND OPR-TYPE NOT = WS-OPR-TYPE      
              EXIT PARAGRAPH                                            
           END-IF.                                                      
                                                                        
           MOVE SPACES TO WS-REPORT-LINE.                               
           MOVE OPR-ACCT-ID TO WS-REP-ACCT-ID.                          
           MOVE OPR-DATE TO WS-REP-ORD-DATE.                            
           MOVE OPR-TYPE TO WS-REP-ORD-TYPE.                            
           MOVE OPR-AMOUNT TO WS-REP-AMOUNT.                            
           MOVE OPR-ID TO WS-REP-OPR-ID.                                
           WRITE FILTERED-REP-REC FROM WS-REPORT-LINE.         
           IF FILTERED-REPORT-STATUS NOT = '00'                         
              DISPLAY 'ERROR WRITING FILTERED-REPORT FILE: '            
                       FILTERED-REPORT-STATUS                           
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * OPEN ALL FILES AND CHECK STATUS                                 
      **********************************************                    
       OPEN-ALL-FILES.                                                  
           OPEN INPUT SYSIN-FILE.                                       
           IF SYSIN-STATUS NOT = '00'                                   
              DISPLAY 'ERROR OPENING SYSIN: ' SYSIN-STATUS              
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN INPUT OPR-LOG-FILE.                                     
           IF OPR-LOG-STATUS NOT = '00'                                 
              DISPLAY 'ERROR OPENING OPR-LOG FILE: ' OPR-LOG-STATUS     
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT FILTERED-REPORT-FILE.                            
           IF FILTERED-REPORT-STATUS NOT = '00'                         
                DISPLAY 'ERROR OPENING FILTERED-REPORT FILE: '      
                         FILTERED-REPORT-STATUS                         
                STOP RUN                                                
           END-IF.                                                      
                                                                        
      **********************************************                    
      * CLOSE ALL FILES AND CHECK STATUS.                               
      **********************************************                    
       CLOSE-ALL-FILES.                                                 
           CLOSE SYSIN-FILE.                                            
           IF SYSIN-STATUS NOT = '00'                                   
              DISPLAY 'WARNING: ERROR CLOSING SYSIN: ' SYSIN-STATUS     
           END-IF.                                                      
                                                                        
           CLOSE OPR-LOG-FILE.                                          
           IF OPR-LOG-STATUS NOT = '00'                                 
              DISPLAY 'WARNING: ERROR CLOSING OPR-LOG FILE: '           
                       OPR-LOG-STATUS                                   
           END-IF.                                                      
                                                                        
           CLOSE FILTERED-REPORT-FILE.                                  
           IF FILTERED-REPORT-STATUS NOT = '00'                         
              DISPLAY 'WARNING: ERROR CLOSING FILTERED-REPORT FILE: '   
                       FILTERED-REPORT-STATUS                           
           END-IF.                                                      
