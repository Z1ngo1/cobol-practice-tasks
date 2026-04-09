      ******************************************************************
      * VSAM ALTERNATE INDEX SEARCH - LIBRARY BOOK FINDER              *
      *                                                                *
      * PURPOSE:                                                       *
      * SEARCHES LIBRARY CATALOG USING ALTERNATE INDEX (AIX) TO FIND   *
      * ALL BOOKS BY SPECIFIED AUTHORS.                                *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   PHASE 1 - READ: READ AUTHOR NAME FROM SEARCH-FILE.           *
      *   PHASE 2 - SEARCH: FOR EACH AUTHOR REQUEST:                   *
      *     START VSAM-FILE KEY = VSAM-AUTHOR (ALTERNATE KEY / AIX).   *
      *     INVALID KEY: AUTHOR NOT FOUND - WRITE NOT-FOUND-LINE.      *
      *     VALID KEY:   READ NEXT RECORDS WHILE VSAM-AUTHOR MATCHES.  *
      *                  WHEN AUTHOR CHANGES: STOP BROWSING.           *
      *   PHASE 3 - WRITE: WRITE BOOK DETAILS TO RESULT-FILE.          *
      * NOTE ON START:                                                 *
      *   START POSITIONS ON ALTERNATE KEY (VSAM-AUTHOR).              *
      *   SUBSEQUENT READ NEXT RETURNS RECORDS IN AIX ORDER.           *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2026/01/04                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT:  VSAMDD (LIBRARY.MASTER.PATH) - VSAM KSDS WITH AIX      *
      * INPUT:  SRCHDD (SEARCH.REQ) - AUTHOR SEARCH REQUESTS (PS, 80 B)*
      * OUTPUT: RSLTDD (RESULT.RPT) - SEARCH RESULTS REPORT (PS, V)    *
      ******************************************************************
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. VSAM18.                                              
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT VSAM-FILE ASSIGN TO VSAMDD                            
              ORGANIZATION IS INDEXED                                   
              ACCESS MODE IS DYNAMIC                                    
              RECORD KEY IS VSAM-ISBN                                   
              ALTERNATE RECORD KEY IS VSAM-AUTHOR WITH DUPLICATES       
              FILE STATUS IS VSAM-STATUS.                               
                                                                        
           SELECT SEARCH-FILE ASSIGN TO SRCHDD                          
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS SEARCH-STATUS.                             
                                                                        
           SELECT RESULT-FILE ASSIGN TO RSLTDD                          
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS RESULT-STATUS.                             
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD VSAM-FILE.                                                    
       01 VSAM-REC.                                                     
          05 VSAM-ISBN PIC X(10).                                       
          05 VSAM-AUTHOR PIC X(20).                                     
          05 VSAM-TITLE PIC X(30).                                      
          05 VSAM-YEAR PIC X(4).                                        
          05 FILLER PIC X(16).                                          
                                                                        
       FD SEARCH-FILE RECORDING MODE IS F.                              
       01 SEARCH-REC.                                                   
          05 SEARCH-AUTHOR PIC X(20).                                   
          05 FILLER PIC X(60).                                          
                                                                        
       FD RESULT-FILE RECORDING MODE IS V.                              
       01 RESULT-REC PIC X(80).                                         
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * FILE STATUS VARIABLES                                           
       01 FILE-STATUSES.                                                
          05 VSAM-STATUS PIC X(2).                                      
          05 SEARCH-STATUS PIC X(2).                                    
          05 RESULT-STATUS PIC X(2).                                    
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
          05 WS-EOF PIC X(1) VALUE 'N'.                                 
             88 EOF VALUE 'Y'.                                          
          05 WS-EOF-AUTHOR PIC X(1) VALUE 'N'.                          
             88 EOF-AUTHOR VALUE 'Y'.                                   
             88 NOT-EOF-AUTHOR VALUE 'N'.                               
                                                                        
      * STATISTICS COUNTERS                                             
       01 WS-COUNTERS.                                                  
          05 SEARCHES-PROCESSED PIC 9(5) VALUE 0.                       
          05 AUTHORS-FOUND PIC 9(5) VALUE 0.                            
          05 AUTHORS-NOT-FOUND PIC 9(5) VALUE 0.                        
          05 BOOKS-FOUND PIC 9(5) VALUE 0.                              
          05 READ-COUNTER PIC 9(5) VALUE 0.                             
                                                                        
      * DISPLAY-FORMATTED COUNTERS                                      
       01 WS-DISP-COUNTERS.                                             
          05 SEARCHES-PROCESSED-DISP PIC Z(4)9.                         
          05 AUTHORS-FOUND-DISP PIC Z(4)9.                              
          05 AUTHORS-NOT-FOUND-DISP PIC Z(4)9.                          
          05 BOOKS-FOUND-DISP PIC Z(4)9.                                
          05 READ-COUNTER-DISP PIC Z(4)9.                               
                                                                        
      * REPORT HEADER LINE                                              
       01 HEADER-LINE.                                                  
          05 FILLER PIC X(12) VALUE 'SEARCH FOR: '.                     
          05 HL-AUTHOR PIC X(20).                                       
                                                                        
      * REPORT DETAIL FOUND LINE                                        
       01 DETAIL-LINE.                                                  
          05 FILLER PIC X(5) VALUE SPACES.                              
          05 FILLER PIC X(7) VALUE 'FOUND: '.                           
          05 DL-TITLE PIC X(30).                                        
          05 FILLER PIC X(2) VALUE ' ('.                                
          05 DL-YEAR PIC X(4).                                          
          05 FILLER PIC X(1) VALUE ')'.                                 
                                                                        
      * REPORT DETAIL NOT FOUND LINE                                    
       01 NOT-FOUND-LINE.                                               
          05 FILLER PIC X(5) VALUE SPACES.                              
          05 FILLER PIC X(20) VALUE 'NOT FOUND'.                        
                                                                        
      * REPORT SEPARATOR LINE                                           
       01 SEPARATOR-LINE.                                               
          05 FILLER PIC X(40) VALUE ALL '-'.                            
                                                                        
      **********************************************                    
      * OPENS FILES, READS FIRST SEARCH REQUEST,                        
      * PROCESSES ALL AUTHORS, CLOSES AND REPORTS.                      
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
           PERFORM OPEN-ALL-FILES.                                      
           PERFORM READ-SEARCH-AUTHOR.                                  
           PERFORM PROCESS-ALL-SEARCHES.                                
           PERFORM CLOSE-ALL-FILES.                                     
           PERFORM DISPLAY-SUMMARY.                                     
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * OPEN ALL FILES AND CHECK STATUS                                 
      **********************************************                    
       OPEN-ALL-FILES.                                                  
           OPEN INPUT VSAM-FILE.                                        
           IF VSAM-STATUS NOT = '00'                                    
              DISPLAY 'ERROR OPENING VSAM FILE: ' VSAM-STATUS           
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN INPUT SEARCH-FILE.                                      
           IF SEARCH-STATUS NOT = '00'                                  
              DISPLAY 'ERROR OPENING SEARCH FILE: ' SEARCH-STATUS       
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT RESULT-FILE.                                     
           IF RESULT-STATUS NOT = '00'                                  
              DISPLAY 'ERROR OPENING RESULT FILE: ' RESULT-STATUS       
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * READS ONE SEARCH REQUEST FROM SEARCH-FILE.                      
      * CALLED BEFORE LOOP AND AT END OF EACH PASS.                     
      * SETS EOF FLAG WHEN NO MORE REQUESTS REMAIN.                     
      **********************************************                    
       READ-SEARCH-AUTHOR.                                              
           READ SEARCH-FILE                                             
             AT END                                                     
                SET EOF TO TRUE                                         
             NOT AT END                                                 
                IF SEARCH-STATUS = '00'                                 
                   ADD 1 TO READ-COUNTER                                
                ELSE                                                    
                   DISPLAY 'ERROR READING SEARCH FILE: ' SEARCH-STATUS  
                   STOP RUN                                             
                END-IF                                                  
           END-READ.                                                    
                                                                        
      **********************************************                    
      * LOOPS OVER ALL SEARCH REQUESTS.                                 
      * BLANK AUTHOR LINES ARE SKIPPED SILENTLY.                        
      * CALLS SEARCH-AUTHOR-BOOKS FOR EACH VALID                        
      * REQUEST, THEN READS NEXT SEARCH RECORD.                         
      **********************************************                    
       PROCESS-ALL-SEARCHES.                                            
           PERFORM UNTIL EOF                                            
               IF SEARCH-AUTHOR NOT = SPACES                            
                  MOVE SEARCH-AUTHOR TO VSAM-AUTHOR                     
                  ADD 1 TO SEARCHES-PROCESSED                           
                  PERFORM SEARCH-AUTHOR-BOOKS                           
               END-IF                                                   
                                                                        
               PERFORM READ-SEARCH-AUTHOR                               
           END-PERFORM.                                                 
                                                                        
      **********************************************                    
      * WRITES HEADER FOR CURRENT AUTHOR.                               
      * STARTS VSAM ON ALTERNATE KEY (VSAM-AUTHOR).                     
      * INVALID KEY: WRITES NOT-FOUND-LINE.                             
      * VALID KEY: CALLS READ-MATCHING-BOOKS.                           
      * ALWAYS WRITES SEPARATOR AT END.                                 
      **********************************************                    
       SEARCH-AUTHOR-BOOKS.                                             
           MOVE SEARCH-AUTHOR TO HL-AUTHOR.                             
           MOVE ALL SPACES TO RESULT-REC.                               
           WRITE RESULT-REC FROM HEADER-LINE.                           
           IF RESULT-STATUS NOT = '00'                                  
              DISPLAY 'ERROR WRITING HEADER: ' RESULT-STATUS            
              DISPLAY 'AUTHOR: ' VSAM-AUTHOR                            
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           START VSAM-FILE KEY IS EQUAL TO VSAM-AUTHOR                  
               INVALID KEY                                              
                   ADD 1 TO AUTHORS-NOT-FOUND                           
                   MOVE ALL SPACES TO RESULT-REC                        
                   WRITE RESULT-REC FROM NOT-FOUND-LINE                 
                   IF RESULT-STATUS NOT = '00'                          
                      DISPLAY 'ERROR WRITING NOT FOUND LINE: '          
                               RESULT-STATUS                            
                      DISPLAY 'AUTHOR: ' SEARCH-AUTHOR                  
                      STOP RUN                                          
                   END-IF                                               
               NOT INVALID KEY                                          
                   ADD 1 TO AUTHORS-FOUND                               
                   PERFORM READ-MATCHING-BOOKS                          
           END-START.                                                   
                                                                        
           MOVE ALL SPACES TO RESULT-REC.                               
           WRITE RESULT-REC FROM SEPARATOR-LINE.                        
           IF RESULT-STATUS NOT = '00'                                  
              DISPLAY 'ERROR WRITING SEPARATOR: ' RESULT-STATUS         
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * READS NEXT RECORDS FROM VSAM IN AIX ORDER.                      
      * STOPS WHEN VSAM-AUTHOR NO LONGER MATCHES                        
      * SEARCH-AUTHOR OR WHEN END OF FILE.                              
      * EACH MATCHING BOOK IS WRITTEN TO RESULT-FILE                    
      **********************************************                    
       READ-MATCHING-BOOKS.                                             
           SET NOT-EOF-AUTHOR TO TRUE.                                  
           PERFORM UNTIL EOF-AUTHOR                                     
              READ VSAM-FILE NEXT RECORD                                
                AT END                                                  
                   SET EOF-AUTHOR TO TRUE                               
                NOT AT END                                              
                   IF VSAM-AUTHOR NOT = SEARCH-AUTHOR                   
                      SET EOF-AUTHOR TO TRUE                            
                   ELSE                                                 
                      ADD 1 TO BOOKS-FOUND                              
                      MOVE ALL SPACES TO RESULT-REC                     
                      MOVE VSAM-TITLE TO DL-TITLE                       
                      MOVE VSAM-YEAR  TO DL-YEAR                        
                      WRITE RESULT-REC FROM DETAIL-LINE                 
                      IF RESULT-STATUS NOT = '00'                       
                         DISPLAY 'ERROR WRITING DETAIL LINE: '          
                                  RESULT-STATUS                         
                         DISPLAY 'BOOK: ' VSAM-TITLE                    
                         STOP RUN                                       
                      END-IF                                            
                   END-IF                                               
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
      **********************************************                    
      * CLOSE ALL FILES AND CHECK STATUS                                
      **********************************************                    
       CLOSE-ALL-FILES.                                                 
           CLOSE VSAM-FILE.                                             
           IF VSAM-STATUS NOT = '00'                                    
              DISPLAY 'WARNING: ERROR CLOSING VSAM FILE: ' VSAM-STATUS  
           END-IF.                                                      
                                                                        
           CLOSE SEARCH-FILE.                                           
           IF SEARCH-STATUS NOT = '00'                                  
              DISPLAY 'WARNING: ERROR CLOSING SEARCH FILE: '            
                       SEARCH-STATUS                                    
           END-IF.                                                      
                                                                        
           CLOSE RESULT-FILE.                                           
           IF RESULT-STATUS NOT = '00'                                  
              DISPLAY 'WARNING: ERROR CLOSING RESULT FILE: '            
                       RESULT-STATUS                                    
           END-IF.                                                      
                                                                        
      **********************************************                    
      * DISPLAY SUMMARY STATISTICS TO SYSOUT                            
      **********************************************                    
       DISPLAY-SUMMARY.                                                 
           MOVE READ-COUNTER TO READ-COUNTER-DISP.                      
           MOVE SEARCHES-PROCESSED TO SEARCHES-PROCESSED-DISP.          
           MOVE AUTHORS-FOUND TO AUTHORS-FOUND-DISP.                    
           MOVE AUTHORS-NOT-FOUND TO AUTHORS-NOT-FOUND-DISP.            
           MOVE BOOKS-FOUND TO BOOKS-FOUND-DISP.                        
                                                                        
           DISPLAY '========================================'.          
           DISPLAY 'LIBRARY SEARCH SUMMARY'.                            
           DISPLAY '========================================'.          
           DISPLAY 'SEARCHES READ:         ' READ-COUNTER-DISP.         
           DISPLAY 'SEARCHES PROCESSED:    ' SEARCHES-PROCESSED-DISP.   
           DISPLAY 'AUTHORS FOUND:         ' AUTHORS-FOUND-DISP.        
           DISPLAY 'AUTHORS NOT FOUND:     ' AUTHORS-NOT-FOUND-DISP.    
           DISPLAY 'BOOKS FOUND (TOTAL):   ' BOOKS-FOUND-DISP.          
           DISPLAY '========================================'.          
