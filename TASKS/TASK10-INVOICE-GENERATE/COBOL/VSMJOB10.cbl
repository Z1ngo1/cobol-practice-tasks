      ******************************************************************
      * INVOICE GENERATION SYSTEM (ENRICHMENT)                         *
      *                                                                *
      * PURPOSE:                                                       *
      * READS DAILY ORDERS FILE, ENRICHES WITH PRODUCT DATA FROM       *
      * VSAM MASTER FILE, CALCULATES TOTAL COSTS, AND GENERATES        *
      * INVOICE FILE WITH DETAILED LINE ITEMS.                         *
      *                                                                *
      * BUSINESS LOGIC:                                                *
      *   1. READ ORDER RECORD (ORDER-ID, PRODUCT-ID, QUANTITY)        *
      *   2. LOOKUP PRODUCT IN VSAM BY PRODUCT-ID (RANDOM ACCESS)      *
      *   3. IF FOUND: CALCULATE TOTAL-COST = QUANTITY * UNIT-PRICE    *
      *   4. WRITE ENRICHED INVOICE RECORD WITH PRODUCT-NAME AND COST  *
      *   5. IF NOT FOUND (FILE STATUS 23): LOG ERROR, SKIP INVOICE    *
      *                                                                *
      * AUTHOR: STANISLAV                                              *
      * DATE: 2025/12/27                                               *
      *                                                                *
      * FILES:                                                         *
      * INPUT: VSAMDD (PROD.MASTER) - PRODUCT MASTER FILE (VSAM KSDS)  *
      * INPUT: ORDD (ORDERS.DAILY) - DAILY ORDERS FILE (PS, 80 BYTES)  *
      * OUTPUT:  OUTDD (INVOICE.FILE) - INVOICE OUTPUT FILE (PS, 80 B) *
      ******************************************************************
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. VSMJOB10.                                            
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT PRODUCT-MASTER-FILE ASSIGN TO VSAMDD                  
             ORGANIZATION IS INDEXED                                    
             ACCESS MODE IS RANDOM                                      
             RECORD KEY IS PRODUCT-ID                                   
             FILE STATUS IS VSAM-STATUS.                                
                                                                        
           SELECT DAILY-ORDERS-FILE ASSIGN TO ORDD                      
             ORGANIZATION IS SEQUENTIAL                                 
             FILE STATUS IS ORDERS-STATUS.                              
                                                                        
           SELECT INVOICE-OUTPUT-FILE ASSIGN TO OUTDD                   
             ORGANIZATION IS SEQUENTIAL                                 
             FILE STATUS IS OUT-STATUS.                                 
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD PRODUCT-MASTER-FILE.                                          
       01 VSAM-REC.                                                     
          05 PRODUCT-ID PIC X(5).                                       
          05 PRODUCT-NAME PIC X(20).                                    
          05 UNIT-PRICE PIC 9(5)V99.                                    
                                                                        
       FD DAILY-ORDERS-FILE RECORDING MODE IS F.                        
       01 ORDERS-REC.                                                   
          05 ORDER-ID PIC X(5).                                         
          05 ORDER-PRODUCT-ID PIC X(5).                                 
          05 ORDER-QUANTITY PIC 9(3).                                   
          05 FILLER PIC X(67).                                          
                                                                        
       FD INVOICE-OUTPUT-FILE RECORDING MODE IS F.                      
       01 OUT-REC.                                                      
          05 INVOICE-ORDER-ID PIC X(5).                                 
          05 FILLER PIC X(1).                                           
          05 INVOICE-PRODUCT-NAME PIC X(20).                            
          05 FILLER PIC X(1).                                           
          05 INVOICE-QUANTITY PIC 9(3).                                 
          05 FILLER PIC X(1).                                           
          05 INVOICE-TOTAL-COST PIC Z(6).99.                            
          05 FILLER PIC X(40).                                          
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      * FILE STATUS VARIABLES                                           
       01 FILE-STATUSES.                                                
           05 VSAM-STATUS PIC X(2).                                     
           05 ORDERS-STATUS PIC X(2).                                   
           05 OUT-STATUS PIC X(2).                                      
                                                                        
      * CONTROL FLAGS                                                   
       01 FLAGS.                                                        
           05 WS-EOF PIC X(1) VALUE 'N'.                                
              88 EOF VALUE 'Y'.                                         
                                                                        
      * STATISTICS COUNTERS                                             
       01 WS-COUNTERS.                                                  
          05 TOTAL-ORDERS PIC 9(5) VALUE 0.                             
          05 TOTAL-INVOICES PIC 9(5) VALUE 0.                           
          05 TOTAL-ERRORS PIC 9(5) VALUE 0.                             
                                                                        
      * DISPLAY-FORMATTED COUNTERS                                      
       01 WS-DISP-COUNTERS.                                             
          05 TOTAL-ORDERS-DISP PIC Z(5).                                
          05 TOTAL-INVOICES-DISP PIC Z(5).                              
          05 TOTAL-ERRORS-DISP PIC Z(5).                                
                                                                        
      * CALCULATED TOTAL COST                                           
       01 CALC-TOTAL-COST PIC 9(6)V99 COMP-3.                           
                                                                        
      **********************************************                    
      * MAIN FLOW: OPEN -> PROCESS -> CLOSE -> REPORT                   
      **********************************************                    
       PROCEDURE DIVISION.                                              
       MAIN-LOGIC.                                                      
           PERFORM OPEN-ALL-FILES.                                      
           PERFORM PROCESS-ORDERS.                                      
           PERFORM CLOSE-ALL-FILES.                                     
           PERFORM DISPLAY-SUMMARY.                                     
           STOP RUN.                                                    
                                                                        
      **********************************************                    
      * OPEN ALL FILES AND CHECK STATUS                                 
      **********************************************                    
       OPEN-ALL-FILES.                                                  
           OPEN INPUT PRODUCT-MASTER-FILE.                              
           IF VSAM-STATUS NOT = '00'                                    
              DISPLAY 'ERROR OPENING PRODUCT MASTER FILE: ' VSAM-STATUS 
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN INPUT DAILY-ORDERS-FILE.                                
           IF ORDERS-STATUS NOT = '00'                                  
              DISPLAY 'ERROR OPENING DAILY ORDERS FILE: ' ORDERS-STATUS 
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
           OPEN OUTPUT INVOICE-OUTPUT-FILE.                             
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR OPENING INVOICE OUTPUT FILE: ' OUT-STATUS  
              STOP RUN                                                  
           END-IF.                                                      
                                                                        
      **********************************************                    
      * READ ORDERS AND PROCESS EACH RECORD                             
      **********************************************                    
       PROCESS-ORDERS.                                                  
           PERFORM UNTIL EOF                                            
              READ DAILY-ORDERS-FILE                                    
                AT END                                                  
                   SET EOF TO TRUE                                      
                NOT AT END                                              
                   IF ORDERS-STATUS = '00'                              
                      ADD 1 TO TOTAL-ORDERS                             
                      PERFORM PROCESS-ORDER                             
                   ELSE                                                 
                      DISPLAY 'ERROR READING DAILY ORDERS FILE: '       
                               ORDERS-STATUS                            
                      STOP RUN                                          
                   END-IF                                               
              END-READ                                                  
           END-PERFORM.                                                 
                                                                        
      **********************************************                    
      * PROCESS SINGLE ORDER: LOOKUP AND ENRICH                         
      **********************************************                    
       PROCESS-ORDER.                                                   
           MOVE SPACES TO OUT-REC.                                      
           MOVE ORDER-PRODUCT-ID TO PRODUCT-ID.                         
           READ PRODUCT-MASTER-FILE.                                    
           EVALUATE TRUE                                                
               WHEN VSAM-STATUS = '00'                                  
                    PERFORM WRITE-INVOICE-LINE                          
               WHEN VSAM-STATUS = '23'                                  
                    DISPLAY 'ORDER ' ORDER-ID ': PRODUCT '              
                            ORDER-PRODUCT-ID ' NOT FOUND.'              
                    ADD 1 TO TOTAL-ERRORS                               
               WHEN OTHER                                               
                    DISPLAY 'VSAM READ ERROR: ' VSAM-STATUS             
                    ADD 1 TO TOTAL-ERRORS                               
                    STOP RUN                                            
           END-EVALUATE.                                                
                                                                        
      **********************************************                    
      * CALCULATE TOTAL AND WRITE INVOICE LINE                          
      **********************************************                    
       WRITE-INVOICE-LINE.                                              
           COMPUTE CALC-TOTAL-COST = UNIT-PRICE * ORDER-QUANTITY.       
                                                                        
           MOVE ORDER-ID TO INVOICE-ORDER-ID                            
           MOVE PRODUCT-NAME TO INVOICE-PRODUCT-NAME                    
           MOVE ORDER-QUANTITY TO INVOICE-QUANTITY                      
           MOVE CALC-TOTAL-COST TO INVOICE-TOTAL-COST                   
           WRITE OUT-REC.                                               
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'ERROR WRITING INVOICE: ' OUT-STATUS              
              DISPLAY 'ORDER ID: ' ORDER-ID                             
              ADD 1 TO TOTAL-ERRORS                                     
              STOP RUN                                                  
           ELSE                                                         
              ADD 1 TO TOTAL-INVOICES                                   
           END-IF.                                                      
                                                                        
      **********************************************                    
      * CLOSE ALL FILES AND CHECK STATUS                                
      **********************************************                    
       CLOSE-ALL-FILES.                                                 
           CLOSE PRODUCT-MASTER-FILE.                                   
           IF VSAM-STATUS NOT = '00'                                    
              DISPLAY 'WARNING: ERROR CLOSING VSAM FILE: ' VSAM-STATUS  
           END-IF.                                                      
                                                                        
           CLOSE DAILY-ORDERS-FILE.                                     
           IF ORDERS-STATUS NOT = '00'                                  
              DISPLAY 'WARNING: ERROR CLOSING ORDERS FILE: '            
                       ORDERS-STATUS                                    
           END-IF.                                                      
                                                                        
           CLOSE INVOICE-OUTPUT-FILE.                                   
           IF OUT-STATUS NOT = '00'                                     
              DISPLAY 'WARNING: ERROR CLOSING INVOICE FILE: ' OUT-STATUS
           END-IF.                                                      
                                                                        
      **********************************************                    
      * DISPLAY SUMMARY STATISTICS TO SYSOUT                            
      **********************************************                    
       DISPLAY-SUMMARY.                                                 
           MOVE TOTAL-ORDERS TO TOTAL-ORDERS-DISP.                      
           MOVE TOTAL-INVOICES TO TOTAL-INVOICES-DISP.                  
           MOVE TOTAL-ERRORS TO TOTAL-ERRORS-DISP.                      
                                                                        
           DISPLAY '========================================'.          
           DISPLAY 'INVOICE GENERATION SUMMARY'.                        
           DISPLAY '========================================'.          
           DISPLAY 'TOTAL ORDERS PROCESSED: ' TOTAL-ORDERS-DISP.        
           DISPLAY 'TOTAL INVOICES CREATED: ' TOTAL-INVOICES-DISP.      
           DISPLAY 'TOTAL ERRORS:           ' TOTAL-ERRORS-DISP.        
           DISPLAY '========================================'.          
