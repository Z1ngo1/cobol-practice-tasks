//CBLDBDG  JOB (123),'DB2COB',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),        
//             NOTIFY=&SYSUID                                           
//*=====================================================================
//*STEP 1: DELETE OLD OUTPUT DATASETS IF IT EXISTS                      
//*=====================================================================
//DELREP   EXEC PGM=IEFBR14                                             
//DELDD    DD  DSN=Z73460.TASK8.REPORT.FILE,                            
//             SPACE=(TRK,(1,0),RLSE),                            
//             DISP=(MOD,DELETE,DELETE)                                 
//*=====================================================================
//* STEP 2: COBOL COMPILATION WITH DB2 PRECOMPILE                       
//*=====================================================================
//COMPIL   EXEC DB2CBL,MBR=DB2TASK8                                     
//COBOL.SYSIN  DD DSN=Z73460.COB.PRAC(DB2TASK8),DISP=SHR                
//COBOL.SYSLIB DD DSN=Z73460.DCLGEN,DISP=SHR                            
//*=====================================================================
//* STEP 3: PROGRAM EXECUTION UNDER DB2 CONTROL                         
//*=====================================================================
//RUNPROG  EXEC PGM=IKJEFT01,COND=(4,LT)                                
//STEPLIB  DD DSN=DSND10.SDSNLOAD,DISP=SHR                              
//         DD DSN=Z73460.LOAD,DISP=SHR                                  
//OUTDD    DD DSN=Z73460.TASK8.REPORT.FILE,                             
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2)),                                        
//            DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)              
//*=====================================================================
//* STEP 4: DB2 EXECUTION CONTROL - RUN PROGRAM UNDER DBDG SUBSYSTEM    
//*=====================================================================
//SYSTSPRT DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSTSIN  DD *                                                         
  DSN SYSTEM(DBDG)                                                      
  RUN PROGRAM(DB2TASK8) PLAN(Z73460) -                                  
      LIB('Z73460.LOAD')                                                
  END                                                                   
/*                                                     
