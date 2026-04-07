      ******************************************************************
      * VSAM DUPLICATE DETECTION SYSTEM                                *
      *                                                                *
      * PURPOSE:                                                       *
      * READS CLIENT MASTER FILE (KSDS), SORTS BY NAME AND BIRTHDATE,  *
      * DETECTS DUPLICATE GROUPS (SAME NAME + BIRTHDATE),              *
      * AND GENERATES DUPLICATE REPORT.                                *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   SORT CLIENT RECORDS BY NAME (ASC), BIRTHDATE (ASC), ID (ASC) *
      *   GROUP CONSECUTIVE RECORDS WITH SAME NAME + BIRTHDATE:        *
      *     GROUP SIZE > 1 -> WRITE ALL RECORDS IN GROUP TO REPORT     *
      *     GROUP SIZE = 1 -> SKIP (NO DUPLICATE)                      *
      *   DISPLAY SUMMARY: TOTAL RECORDS, GROUPS, DUPLICATES           *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2025/12/25                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT:  VSAMDD (CLIENT.MASTER.FILE) - KSDS                     *
      * OUTPUT: REPDD (DUPLICATE.REPORT) - PS, 80 BYTES                *
      * WORK:   SRTDD (SORT WORK FILE)                                 *
      ******************************************************************
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. VSAMJOB9.                                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT CLIENT-MASTER-FILE ASSIGN TO VSAMDD                   
             ORGANIZATION IS INDEXED                                    
             ACCESS MODE IS SEQUENTIAL                                  
             RECORD KEY IS MAST-ID.                                    
                                                                        
           SELECT CLIENT-SORT-WORK ASSIGN TO SRTDD.                     
                                                                        
           SELECT DUPLICATE-REPORT-FILE ASSIGN TO REPDD                 
             ORGANIZATION IS SEQUENTIAL                                 
             FILE STATUS IS REP-STATUS.                                 
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD CLIENT-MASTER-FILE.                                           
       01 CLIENT-MASTER-REC.                                            
           05 MAST-ID PIC X(6).                                         
           05 MAST-NAME PIC X(30).                                      
           05 MAST-BIRTH PIC X(8).                                      
           05 MAST-PASSPORT PIC X(10).                                  
           05 MAST-CITY PIC X(20).                                      
                                                                        
       SD CLIENT-SORT-WORK.                                             
       01 SORTED-CLIENT-REC.                                            
           05 SRT-ID PIC X(6).                                          
           05 SRT-NAME PIC X(30).                                       
           05 SRT-BIRTH PIC X(8).                                       
           05 SRT-PASSPORT PIC X(10).                                   
           05 SRT-CITY PIC X(20).                                       
           05 FILLER PIC X(6).                                          
                                                                        
       FD DUPLICATE-REPORT-FILE RECORDING MODE IS F.                    
       01 REPORT-LINE.                                                  
           05 CL-ID PIC X(6).                                           
           05 FILLER PIC X(1).                                          
           05 CL-NAME PIC X(30).                                        
           05 FILLER PIC X(1).                                          
           05 CL-BIRTH PIC X(8).                                        
           05 FILLER PIC X(1).                                          
           05 CL-PASSPORT PIC X(10).                                    
           05 FILLER PIC X(23).                                         
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
           05 WS-EOF PIC X(1) VALUE 'N'.                                
              88 EOF VALUE 'Y'.                                         
                                                                        
      * FILE STATUS VARIABLE                                            
       01 FILE-STATUS.                                                  
           05 REP-STATUS PIC X(2).                                      
                                                                        
      * LOOP INDEX FOR GROUP TABLE PROCESSING                           
       01 WS-INDEX PIC 9(3).                                            
                                                                        
      * CURRENT GROUP KEY (NAME + BIRTHDATE)                            
       01 CURRENT-GROUP-KEY.                                            
           05 WS-CUR-NAME PIC X(30) VALUE SPACES.                       
           05 WS-CUR-BIRTH PIC X(8) VALUE SPACES.                       
                                                                        
      * BUFFER FOR STORING DUPLICATE GROUP (MAX 50 RECORDS)             
       01 DUPLICATE-GROUP-BUFFER.                                       
           05 WS-GROUP-COUNT PIC 9(3) VALUE 0.                          
           05 WS-GROUP-TABLE OCCURS 50 TIMES.                           
              10 G-ID PIC X(6).                                         
              10 G-NAME PIC X(30).                                      
              10 G-BIRTH PIC X(8).                                      
              10 G-PASSPORT PIC X(10).                                  
                                                                        
      * STATISTICS COUNTERS                                             
       01 WS-COUNTERS.                                                  
           05 TOTAL-DUPS PIC 9(5) VALUE 0.                              
           05 TOTAL-GROUPS PIC 9(5) VALUE 0.                            
           05 TOTAL-REC PIC 9(5) VALUE 0.                               
                                                                        
      * DISPLAY-FORMATTED COUNTERS                                      
       01 WS-DISP-COUNTERS.                                             
           05 TOTAL-DUPS-DISP PIC ZZZZ9.                                
           05 TOTAL-GROUPS-DISP PIC ZZZZ9.                              
           05 TOTAL-REC-DISP PIC ZZZZ9.                                 
                                                                        
      * FORMATTED REPORT LINE FOR OUTPUT                                
       01 WS-REPORT-LINE.                                               
          05 WS-CL-ID PIC X(6).                                         
          05 FILLER PIC X(1) VALUE SPACE.                               
          05 WS-CL-NAME PIC X(30).                                      
          05 FILLER PIC X(1) VALUE SPACE.                               
          05 WS-CL-BIRTH PIC X(8).                                      
          05 FILLER PIC X(1) VALUE SPACE.                               
          05 WS-CL-PASSPORT PIC X(10).                                  
          05 FILLER PIC X(23) VALUE SPACES.                             
                                                                        
      **********************************************                    
      * OPENS REPORT FILE,                                              
      * INVOKES SORT WITH OUTPUT PROCEDURE FOR DUPLICATE                
      * DETECTION, CLOSES FILE AND SHOWS SUMMARY.                       
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
           PERFORM OPEN-REPORT-FILE.                                    
           SORT CLIENT-SORT-WORK                                        
                ON ASCENDING KEY SRT-NAME                               
                ON ASCENDING KEY SRT-BIRTH                              
                ON ASCENDING KEY SRT-ID                                 
                USING CLIENT-MASTER-FILE                                
                OUTPUT PROCEDURE IS PRCSS-SORT-REC THROUGH PROCESS-EXIT.
                IF SORT-RETURN NOT = 0                                  
                   DISPLAY 'SORT FAILED, RETURN CODE: ' SORT-RETURN     
                   STOP RUN                                             
                END-IF.                                                 
           PERFORM CLOSE-REPORT-FILE.                                   
           PERFORM DISPLAY-SUMMARY-REPORT.                              
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * OPEN REPORT FILE AND CHECK STATUS                               
      **********************************************                    
       OPEN-REPORT-FILE.                                                
           OPEN OUTPUT DUPLICATE-REPORT-FILE.                           
           IF REP-STATUS NOT = '00'                                     
              DISPLAY 'ERROR OPENING REPORT FILE: ' REP-STATUS          
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * OUTPUT PROCEDURE FOR SORT. RETURNS RECORDS                      
      * ONE BY ONE, GROUPS BY NAME + BIRTHDATE,                         
      * CALLS WRITE-DUPLICATE-GROUP ON KEY CHANGE                       
      * AND AT END OF FILE.                                             
      **********************************************                    
       PRCSS-SORT-REC.                                                  
           RETURN CLIENT-SORT-WORK                                      
               AT END                                                   
                  SET EOF TO TRUE                                       
           END-RETURN.                                                  
                                                                        
           IF EOF                                                       
              GO TO PROCESS-EXIT                                        
           END-IF.                                                      
                                                                        
           ADD 1 TO TOTAL-REC.                                          
           MOVE SRT-NAME  TO WS-CUR-NAME.                               
           MOVE SRT-BIRTH TO WS-CUR-BIRTH.                              
           PERFORM ADD-TO-GROUP-BUFFER.                                 
                                                                        
      * LOOP THROUGH REMAINING SORTED RECORDS.                          
           PERFORM UNTIL EOF                                            
              RETURN CLIENT-SORT-WORK                                   
                  AT END                                                
                     SET EOF TO TRUE                                    
              END-RETURN                                                
                                                                        
              IF NOT EOF                                                
                 ADD 1 TO TOTAL-REC                                     
                                                                        
                 IF SRT-NAME = WS-CUR-NAME AND                          
                    SRT-BIRTH = WS-CUR-BIRTH                            
                    PERFORM ADD-TO-GROUP-BUFFER                         
                 ELSE                                                   
                    PERFORM WRITE-DUPLICATE-GROUP                       
                    MOVE SRT-NAME  TO WS-CUR-NAME                       
                    MOVE SRT-BIRTH TO WS-CUR-BIRTH                      
                    MOVE 0 TO WS-GROUP-COUNT                            
                    PERFORM ADD-TO-GROUP-BUFFER                         
                 END-IF                                                 
              END-IF                                                    
           END-PERFORM.                                                 
                                                                        
      * FLUSH LAST GROUP AFTER EOF.                                     
           PERFORM WRITE-DUPLICATE-GROUP.                               
                                                                        
           GO TO PROCESS-EXIT.                                          
                                                                        
      **********************************************                    
      * ADDS CURRENT SORTED RECORD TO GROUP BUFFER.                     
      * LOGS WARNING IF BUFFER LIMIT (50) EXCEEDED.                     
      **********************************************                    
       ADD-TO-GROUP-BUFFER.                                             
           IF WS-GROUP-COUNT < 50                                       
              ADD 1 TO WS-GROUP-COUNT                                   
              MOVE SRT-ID TO G-ID(WS-GROUP-COUNT)                       
              MOVE SRT-NAME TO G-NAME(WS-GROUP-COUNT)                   
              MOVE SRT-BIRTH TO G-BIRTH(WS-GROUP-COUNT)                 
              MOVE SRT-PASSPORT TO G-PASSPORT(WS-GROUP-COUNT)           
           ELSE                                                         
              DISPLAY 'WARNING: GROUP BUFFER OVERFLOW AT RECORD: '      
                       TOTAL-REC                                        
           END-IF.                                                      
                                                                        
      **********************************************                    
      * WRITES ALL RECORDS IN BUFFER TO REPORT IF                       
      * GROUP SIZE > 1. INCREMENTS DUPLICATE AND GROUP COUNTERS.        
      **********************************************                    
       WRITE-DUPLICATE-GROUP.                                           
           IF WS-GROUP-COUNT > 1                                        
              ADD 1 TO TOTAL-GROUPS                                     
              PERFORM VARYING WS-INDEX FROM 1 BY 1                      
                 UNTIL WS-INDEX > WS-GROUP-COUNT                        
                 INITIALIZE WS-REPORT-LINE                              
                 MOVE G-ID(WS-INDEX) TO WS-CL-ID                        
                 MOVE G-NAME(WS-INDEX) TO WS-CL-NAME                    
                 MOVE G-BIRTH(WS-INDEX) TO WS-CL-BIRTH                  
                 MOVE G-PASSPORT(WS-INDEX) TO WS-CL-PASSPORT            
                 MOVE WS-REPORT-LINE TO REPORT-LINE                     
                 WRITE REPORT-LINE                                      
                 IF REP-STATUS NOT = '00'                               
                    DISPLAY 'ERROR WRITING REPORT LINE: ' REP-STATUS    
                    STOP RUN                                            
                 END-IF                                                 
                 ADD 1 TO TOTAL-DUPS                                    
              END-PERFORM                                               
           END-IF.                                                      
                                                                        
      **********************************************                    
      * EXIT POINT FOR OUTPUT PROCEDURE                                 
      **********************************************                    
       PROCESS-EXIT.                                                    
           EXIT.                                                        
                                                                        
      **********************************************                    
      * CLOSE REPORT FILE AND CHECK STATUS                              
      **********************************************                    
       CLOSE-REPORT-FILE.                                               
           CLOSE DUPLICATE-REPORT-FILE.                                 
           IF REP-STATUS NOT = '00'                                     
              DISPLAY 'WARNING: ERROR CLOSING REPORT FILE: ' REP-STATUS 
           END-IF.                                                      
                                                                        
      **********************************************                    
      * DISPLAY SUMMARY STATISTICS TO SYSOUT                            
      **********************************************                    
       DISPLAY-SUMMARY-REPORT.                                          
           MOVE TOTAL-DUPS TO TOTAL-DUPS-DISP.                          
           MOVE TOTAL-GROUPS TO TOTAL-GROUPS-DISP.                      
           MOVE TOTAL-REC TO TOTAL-REC-DISP.                            
                                                                        
           DISPLAY '========================================'.          
           DISPLAY 'DUPLICATE REPORT SUMMARY'.                          
           DISPLAY '========================================'.          
           DISPLAY 'TOTAL RECORDS PROCESSED:  ' TOTAL-REC-DISP.         
           DISPLAY 'GROUPS WITH DUPLICATES:   ' TOTAL-GROUPS-DISP.      
           DISPLAY 'SUSPICIOUS RECORDS FOUND: ' TOTAL-DUPS-DISP.        
           DISPLAY '========================================'.          
