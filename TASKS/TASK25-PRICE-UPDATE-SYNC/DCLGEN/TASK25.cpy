      ******************************************************************
      * DCLGEN TABLE(TB_PRICE_HISTORY)                                 *
      *        LIBRARY(Z73460.DCLGEN.COBOL(TASK25))                    *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE TB_PRICE_HISTORY TABLE                      
           ( PROD_ID                        CHAR(5) NOT NULL,           
             OLD_PRICE                      DECIMAL(7, 2),              
             NEW_PRICE                      DECIMAL(7, 2),              
             CHANGE_DATE                    DATE NOT NULL               
           ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TB_PRICE_HISTORY                   *
      ******************************************************************
       01  DCLTB-PRICE-HISTORY.                                         
           10 PROD-ID              PIC X(5).                            
           10 OLD-PRICE            PIC S9(5)V9(2) USAGE COMP-3.         
           10 NEW-PRICE            PIC S9(5)V9(2) USAGE COMP-3.         
           10 CHANGE-DATE          PIC X(10).                           
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
