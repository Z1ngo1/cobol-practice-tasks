//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,             
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID                            
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB                                  
//*=====================================================================
//* DELETE IF EXISTS                                                    
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD    DD DSN=Z73460.TASK14.PAYROLL.TXT,                            
//            SPACE=(TRK,(1,0),RLSE),                              
//            DISP=(MOD,DELETE,DELETE)                                  
//*=====================================================================
//* COMPILE AND RUN PROGRAM                                             
//*=====================================================================
//STEP010  EXEC MYCOMPGO,MEMBER=PSTASK14                                
//RUN.TAXDD DD DSN=Z73460.TASK14.TAX.RATES,DISP=SHR                     
//RUN.EMPDD DD DSN=Z73460.TASK14.EMP.SALARY,DISP=SHR                    
//RUN.OUTDD DD DSN=Z73460.TASK14.PAYROLL.TXT,                           
//             DISP=(NEW,CATLG,DELETE),                                 
//             SPACE=(TRK,(2,2),RLSE),                                  
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80)                                
