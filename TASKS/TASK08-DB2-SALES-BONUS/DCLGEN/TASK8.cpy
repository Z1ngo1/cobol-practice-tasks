      ******************************************************************
      * DCLGEN TABLE(TB_SALES_BONUS)                                   *
      *        LIBRARY(Z73460.DCLGEN.COBOL(TASK8))                     *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE TB_SALES_BONUS TABLE
           ( EMP_ID                         CHAR(6) NOT NULL,
             EMP_NAME                       VARCHAR(30),
             REGION_CODE                    CHAR(2),
             YEAR_SALES                     DECIMAL(11, 2),
             BONUS_AMT                      DECIMAL(9, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TB_SALES_BONUS                     *
      ******************************************************************
       01  DCLTB-SALES-BONUS.
           10 EMP-ID               PIC X(6).
           10 EMP-NAME.
              49 EMP-NAME-LEN      PIC S9(4) USAGE COMP-5.
              49 EMP-NAME-TEXT     PIC X(30).
           10 REGION-CODE          PIC X(2).
           10 YEAR-SALES           PIC S9(9)V9(2) USAGE COMP-3.
           10 BONUS-AMT            PIC S9(7)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
