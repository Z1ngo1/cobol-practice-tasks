      ******************************************************************
      * CLIENT DATABASE CLEANUP AND ARCHIVING SYSTEM                   *
      *                                                                *
      * PURPOSE:                                                       *
      * READS CLIENT MASTER FILE (VSAM KSDS), IDENTIFIES INACTIVE      *
      * CLIENTS BASED ON CUTOFF DATE, ARCHIVES THEM TO PS FILE,        *
      * AND DELETES FROM VSAM TO CLEAN DISK SPACE.                     *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   READ CUTOFF DATE FROM PARAM FILE                             *
      *   SEQUENTIAL SCAN VSAM VIA START + READ NEXT:                  *
      *     CLIENT-LAST-DATE <= CUTOFF -> WRITE TO ARCHIVE, DELETE     *
      *    CLIENT-LAST-DATE >  CUTOFF -> KEEP, INCREMENT COUNTER       *
      *   DISPLAY FINAL STATISTICS ON CLOSE                            *
      *                                                                *
      * AUTHOR:        STANISLAV                                       *
      * DATE:          2025/12/11                                      *
      *                                                                *
      * INPUT:  INDD  (PARAM.FILE) - CUTOFF DATE (PS, 80 BYTES)        *
      * I-O:    CLTDD (CLIENT.MASTER) - CLIENT MASTER (KSDS)           *
      * OUTPUT: OUTDD (ARCHIVE.OLD) - ARCHIVED CLIENTS (PS, 80 BYTES)  *
      ******************************************************************
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. VSAMPS6.                                             
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT CLIENT-FILE ASSIGN TO CLTDD                           
               ORGANIZATION IS INDEXED                                  
               ACCESS MODE IS DYNAMIC                                   
               RECORD KEY IS CLIENT-ID                                  
               FILE STATUS IS CLIENT-STATUS.                            
                                                                        
           SELECT PARAM-FILE ASSIGN TO INDD                             
               ORGANIZATION IS SEQUENTIAL                               
               FILE STATUS IS PARAM-STATUS.                             
                                                                        
           SELECT ARCH-FILE ASSIGN TO OUTDD                             
               ORGANIZATION IS SEQUENTIAL                               
               FILE STATUS IS ARCH-STATUS.                              
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD CLIENT-FILE.                                                  
       01 CLIENT-REC.                                                   
          05 CLIENT-ID PIC X(6).                                        
          05 CLIENT-NAME PIC X(20).                                     
          05 CLIENT-LAST-DATE PIC 9(8).                                 
                                                                        
       FD PARAM-FILE RECORDING MODE IS F.                               
       01 PARAM-REC.                                                    
          05 PARAM-DATE PIC X(8).                                       
          05 FILLER PIC X(72).                                          
                                                                        
       FD ARCH-FILE RECORDING MODE IS F.                                
       01 ARCH-REC.                                                     
          05 ARCH-ID PIC X(6).                                          
          05 ARCH-NAME PIC X(20).                                       
          05 ARCH-DATE PIC 9(8).                                        
          05 FILLER PIC X(46).                                          
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * CUTOFF DATE PARAMETER (YYYYMMDD)                                
       01 WS-CUTOFF-DATE PIC 9(8).                                      
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
          05 WS-EOF PIC X(1) VALUE 'N'.                                 
             88 EOF VALUE 'Y'.                                          
                                                                        
      * FILE STATUS CODES                                               
       01 CLIENT-STATUS PIC X(2).                                       
       01 PARAM-STATUS PIC X(2).                                        
       01 ARCH-STATUS PIC X(2).                                         
                                                                        
      * COUNTERS FOR FINAL REPORT                                       
       01 WS-COUNTERS.                                                  
          05 REC-READ PIC 9(5) VALUE 0.                                 
          05 REC-DELETE PIC 9(5) VALUE 0.                               
          05 REC-KEPT PIC 9(5) VALUE 0.                                 
                                                                        
      * DISPLAY FORMATTING VARIABLES                                    
       01 WS-DISP-COUNTERS.                                             
          05 REC-READ-DISP PIC Z(4)9.                                   
          05 REC-DELETE-DISP PIC Z(4)9.                                 
          05 REC-KEPT-DISP PIC Z(4)9.                                   
                                                                        
      **********************************************                    
      * READS CUTOFF DATE, PROCESSES ALL VSAM RECORDS,                  
      * CLOSES FILES AND DISPLAYS FINAL STATISTICS.                     
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
                                                                        
           PERFORM OPEN-FILES.                                          
           PERFORM READ-CUTOFF-DATE.                                    
           PERFORM PROCESS-ALL-RECORDS.                                 
           PERFORM CLOSE-FILES.                                         
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * OPEN ALL FILES AND CHECKS STATUS                                
      **********************************************                    
       OPEN-FILES.                                                      
           OPEN I-O CLIENT-FILE.                                        
           IF CLIENT-STATUS NOT = '00'                                  
              DISPLAY 'ERROR OPENING CLIENT FILE: ' CLIENT-STATUS       
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN INPUT PARAM-FILE.                                       
           IF PARAM-STATUS NOT = '00'                                   
              DISPLAY 'ERROR OPENING PRM: ' PARAM-STATUS                
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT ARCH-FILE.                                       
           IF ARCH-STATUS NOT = '00'                                    
              DISPLAY 'ERROR OPENING ARCH: ' ARCH-STATUS                
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * READS SINGLE RECORD FROM PARAM FILE INTO WS-CUTOFF-DATE.        
      * CLOSES FILE AFTER READ SINCE IT IS NO LONGER NEEDED.            
      **********************************************                    
       READ-CUTOFF-DATE.                                                
           READ PARAM-FILE                                              
             AT END                                                     
                SET EOF TO TRUE                                         
                DISPLAY 'ERROR: PARAM FILE IS EMPTY OR UNREADABLE'      
                STOP RUN                                                
             NOT AT END                                                 
                IF PARAM-STATUS NOT = '00'                              
                   DISPLAY 'ERROR READING PARAM FILE: ' PARAM-STATUS    
                   STOP RUN                                             
                END-IF                                                  
                MOVE PARAM-DATE TO WS-CUTOFF-DATE                       
                DISPLAY 'DATE IS: ' WS-CUTOFF-DATE                      
           END-READ                                                     
           CLOSE PARAM-FILE.                                            
           IF PARAM-STATUS NOT = '00'                                   
              DISPLAY 'WARNING: ERROR CLOSING PARAM FILE: ' PARAM-STATUS
           END-IF.                                                      
                                                                        
      **********************************************                    
      * POSITIONS VSAM TO FIRST RECORD VIA START,                       
      * THEN READS SEQUENTIALLY UNTIL EOF.                              
      * CALLS CHECK-CLIENT-DATE FOR EACH RECORD.                        
      **********************************************                    
       PROCESS-ALL-RECORDS.                                             
           MOVE LOW-VALUES TO CLIENT-ID.                                
                                                                        
           START CLIENT-FILE KEY IS NOT LESS THAN CLIENT-ID             
               INVALID KEY                                              
                   DISPLAY 'VSAM FILE IS EMPTY'                         
               NOT INVALID KEY                                          
                   CONTINUE                                             
           END-START.                                                   
                                                                        
           IF CLIENT-STATUS NOT = '00' AND CLIENT-STATUS NOT = '23'     
              DISPLAY 'START FAILED STATUS: ' CLIENT-STATUS             
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      * READ ALL RECORDS SEQUENTIALLY AND PROCESS EACH.                 
           PERFORM UNTIL EOF                                            
               READ CLIENT-FILE NEXT RECORD                             
                   AT END                                               
                      SET EOF TO TRUE                                   
                   NOT AT END                                           
                      IF CLIENT-STATUS NOT = '00'                       
                         DISPLAY 'ERROR READING VSAM: ' CLIENT-STATUS   
                         DISPLAY 'RECORDS READ SO FAR: ' REC-READ       
                         STOP RUN                                       
                      END-IF                                            
                      ADD 1 TO REC-READ                                 
                      PERFORM CHECK-CLIENT-DATE                         
               END-READ                                                 
           END-PERFORM.                                                 
                                                                        
      **********************************************                    
      * COMPARES CLIENT DATE AGAINST CUTOFF.                            
      * INACTIVE CLIENTS ROUTED TO ARCHIVE-AND-DELETE,                  
      * ACTIVE CLIENTS INCREMENT KEPT COUNTER.                          
      **********************************************                    
       CHECK-CLIENT-DATE.                                               
           IF CLIENT-LAST-DATE <= WS-CUTOFF-DATE                        
              PERFORM ARCHIVE-AND-DELETE-RECORD                         
           ELSE                                                         
              ADD 1 TO REC-KEPT                                         
           END-IF.                                                      
                                                                        
      **********************************************                    
      * WRITES CURRENT CLIENT-REC TO ARCHIVE FILE,                      
      * THEN DELETES RECORD FROM VSAM.                                  
      **********************************************                    
       ARCHIVE-AND-DELETE-RECORD.                                       
           WRITE ARCH-REC FROM CLIENT-REC.                              
           IF ARCH-STATUS NOT = '00'                                    
              DISPLAY 'ERROR WRITING TO ARCHIVE: ' ARCH-STATUS          
              DISPLAY 'RECORD ID: ' CLIENT-ID                           
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           DELETE CLIENT-FILE                                           
              INVALID KEY                                               
                DISPLAY 'ERROR DELETING: ' CLIENT-ID                    
              NOT INVALID KEY                                           
                ADD 1 TO REC-DELETE                                     
                DISPLAY 'ARCH AND DELETE: ' CLIENT-ID                   
           END-DELETE.                                                  
                                                                        
           IF CLIENT-STATUS NOT = '00'                                  
              DISPLAY 'DELETE FAILED WITH STATUS: ' CLIENT-STATUS       
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * CLOSE FILES AND DISPLAY STATISTICS                              
      **********************************************                    
       CLOSE-FILES.                                                     
           CLOSE CLIENT-FILE.                                           
           IF CLIENT-STATUS NOT = '00'                                  
              DISPLAY 'WARNING: ERROR CLOSING VSAM: ' CLIENT-STATUS     
           END-IF.                                                      
                                                                        
           CLOSE ARCH-FILE.                                             
           IF ARCH-STATUS NOT = '00'                                    
              DISPLAY 'WARNING: ERROR CLOSING ARCHIVE: ' ARCH-STATUS    
           END-IF.                                                      
                                                                        
           MOVE REC-READ TO REC-READ-DISP.                              
           MOVE REC-DELETE TO REC-DELETE-DISP.                          
           MOVE REC-KEPT TO REC-KEPT-DISP.                              
                                                                        
           DISPLAY '----------------------------------------'.          
           DISPLAY 'STATISTIC REPORT:'.                                 
           DISPLAY 'RECORDS READ:   ' REC-READ-DISP.                    
           DISPLAY 'RECORDS DELETE: ' REC-DELETE-DISP.                  
           DISPLAY 'RECORDS KEPT:   ' REC-KEPT-DISP.                    
           DISPLAY '----------------------------------------'.          
