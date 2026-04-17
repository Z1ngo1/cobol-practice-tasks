      ******************************************************************
      * PS TO VSAM CUSTOMER IMPORT                                     *
      *                                                                *
      * PURPOSE:                                                       *
      * READS CUSTOMER RECORDS FROM PS INPUT FILE AND LOADS THEM       *
      * INTO VSAM KSDS MASTER FILE. USES SHARED COPYBOOK TASK32        *
      * FOR CUSTOMER RECORD LAYOUT IN BOTH FD AND WORKING-STORAGE.     *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   PHASE 1 - SEQUENTIAL READ OF PS INPUT FILE:                  *
      *     READ CUST-IN-FILE RECORD BY RECORD.                        *
      *     ON READ ERROR -> DISPLAY STATUS AND STOP.                  *
      *   PHASE 2 - WRITE TO VSAM KSDS MASTER FILE:                    *
      *     MOVE CUST-IN-REC TO CUST-MASTER-REC.                       *
      *     WRITE CUST-MASTER-REC.                                     *
      *     ON WRITE ERROR -> DISPLAY STATUS + CUST-ID AND STOP.       *
      *     ON SUCCESS -> ADD 1 TO WS-LOAD-COUNT.                      *
      *   PHASE 3 - FINAL STATISTICS:                                  *
      *     DISPLAY TOTAL LOADED RECORDS COUNT.                        *
      *                                                                *
      * COPYBOOK: TASK32 - CUSTOMER RECORD LAYOUT (CUST-RECORD)        *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2026/02/12                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT:  INDD    (CUST.IN.PS)       - PS CUSTOMER INPUT FILE    *
      * OUTPUT: MASTDD  (CUST.MASTER.VSAM) - VSAM KSDS MASTER FILE     *
      ******************************************************************
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. COP1LB32.                                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT CUST-IN-FILE ASSIGN TO INDD                           
              ORGANIZATION IS SEQUENTIAL                                
              FILE STATUS IS CUST-IN-STATUS.                            
                                                                        
           SELECT CUST-MASTER-FILE ASSIGN TO MASTDD                     
              ORGANIZATION IS INDEXED                                   
              ACCESS MODE IS SEQUENTIAL                                 
              RECORD KEY IS CUST-ID OF CUST-MASTER-REC                  
              FILE STATUS IS CUST-MASTER-STATUS.                        
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD CUST-IN-FILE RECORDING MODE IS F.                             
       01 CUST-IN-REC.                                                  
           COPY TASK32.                                                 
                                                                        
       FD CUST-MASTER-FILE.                                             
       01 CUST-MASTER-REC.                                              
           COPY TASK32.                                                 
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * FILE STATUS VARIABLES                                           
       01 WS-FILE-STATUSES.                                             
          05 CUST-IN-STATUS PIC X(2).                                   
          05 CUST-MASTER-STATUS PIC X(2).                               
                                                                        
      * CONTROL FLAGS                                                   
       01 WS-FLAGS.                                                     
          05 WS-EOF PIC X(1) VALUE 'N'.                                 
             88 EOF VALUE 'Y'.                                          
                                                                        
      * LOAD STATISTICS                                                 
       01 WS-LOAD-COUNT PIC 9(5) VALUE 0.                               
       01 WS-DISP-LOAD-COUNT PIC Z(4)9.                                 
                                                                        
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
           OPEN INPUT CUST-IN-FILE.                                     
           IF CUST-IN-STATUS NOT = '00'                                 
              DISPLAY 'ERROR OPENING CUST-IN FILE: ' CUST-IN-STATUS     
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT CUST-MASTER-FILE.                                
           IF CUST-MASTER-STATUS NOT = '00'                             
              DISPLAY 'ERROR OPENING CUST-MASTER FILE: '                
                       CUST-MASTER-STATUS                               
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           PERFORM UNTIL EOF                                            
              READ CUST-IN-FILE                                         
                AT END                                                  
                   SET EOF TO TRUE                                      
                NOT AT END                                              
                   IF CUST-IN-STATUS = '00'                             
                      MOVE CUST-IN-REC TO CUST-MASTER-REC               
                      WRITE CUST-MASTER-REC                             
                      IF CUST-MASTER-STATUS NOT = '00'                  
                         DISPLAY 'ERROR WRITING CUST-MASTER FILE: '     
                                  CUST-MASTER-STATUS                    
                         DISPLAY 'CUST-ID: ' CUST-ID OF CUST-MASTER-REC 
                         STOP RUN                                       
                      END-IF                                            
                      ADD 1 TO WS-LOAD-COUNT                            
                   ELSE                                                 
                      DISPLAY 'ERROR READING CUST-IN FILE: '            
                               CUST-IN-STATUS                           
                      STOP RUN                                          
                   END-IF                                               
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
           CLOSE CUST-IN-FILE.                                          
           IF CUST-IN-STATUS NOT = '00'                                 
              DISPLAY 'WARNING: ERROR CLOSING CUST-IN FILE: '           
                       CUST-IN-STATUS                                   
           END-IF.                                                      
                                                                        
           CLOSE CUST-MASTER-FILE.                                      
           IF CUST-MASTER-STATUS NOT = '00'                             
              DISPLAY 'WARNING: ERROR CLOSING CUST-MASTER FILE: '       
                       CUST-MASTER-STATUS                               
           END-IF.                                                      
                                                                        
           MOVE WS-LOAD-COUNT TO WS-DISP-LOAD-COUNT.                    

           DISPLAY 'TOTAL LOADED: ' FUNCTION TRIM(WS-DISP-LOAD-COUNT).  
                                                                        
           STOP RUN.                                                    
