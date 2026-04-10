//CBLDBDG  JOB (123),'DB2COB',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*=====================================================================
//*STEP 1: DELETE OLD OUTPUT DATASETS IF IT EXISTS
//*=====================================================================
//DELREP   EXEC PGM=IEFBR14                                             
//DELDD1   DD  DSN=Z73460.TASK21.ORDERS.FILE,                           
//             SPACE=(TRK,(1,0),RLSE),                                  
//             DISP=(MOD,DELETE,DELETE)                                 
//DELDD2   DD  DSN=Z73460.TASK21.ORDER.LOG,                             
//             SPACE=(TRK,(1,0),RLSE),                                  
//             DISP=(MOD,DELETE,DELETE)                                 
//*=====================================================================
//* STEP 2: CREATE AND INSERT DATA INTO INPUT FILE
//*=====================================================================
//STEPINS  EXEC PGM=IEBGENER                                            
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD DUMMY                                                     
//SYSUT1   DD *                                                         
001000202501100010050                                                   
001001202501100020030                                                   
001002202501100099910                                                   
001003202501100010020                                                   
      202501100010010                                                   
001004202513000010050                                                   
001005202501100030000                                                   
001000202501100010015                                                   
001006202501100010010                                                   
001002202501100099910                                                   
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK21.ORDERS.FILE,                            
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=80)                          
//*=====================================================================
//* STEP 3: COBOL COMPILATION WITH DB2 PRECOMPILE
//*=====================================================================
//COMPIL   EXEC DB2CBL,MBR=DB2JOB21
//COBOL.SYSIN  DD DSN=Z73460.COB.PRAC(DB2JOB21),DISP=SHR
//COBOL.SYSLIB DD DSN=Z73460.DCLGEN,DISP=SHR
//*=====================================================================
//* STEP 4: PROGRAM EXECUTION UNDER DB2 CONTROL
//*=====================================================================
//RUNPROG  EXEC PGM=IKJEFT01,COND=(4,LT)
//STEPLIB  DD DSN=DSND10.SDSNLOAD,DISP=SHR
//         DD DSN=Z73460.LOAD,DISP=SHR
//INDD     DD DSN=Z73460.TASK21.ORDERS.FILE,DISP=SHR
//OUTDD    DD DSN=Z73460.TASK21.ORDER.LOG,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(2,2),RLSE),
//            DCB=(DSORG=PS,RECFM=VB,LRECL=84,BLKSIZE=800)
//*=====================================================================
//* STEP 5: DB2 EXECUTION CONTROL - RUN PROGRAM UNDER DBDG SUBSYSTEM
//*=====================================================================
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSTSIN  DD *
  DSN SYSTEM(DBDG)
  RUN PROGRAM(DB2JOB21) PLAN(Z73460) -
      LIB('Z73460.LOAD')
  END
/*
