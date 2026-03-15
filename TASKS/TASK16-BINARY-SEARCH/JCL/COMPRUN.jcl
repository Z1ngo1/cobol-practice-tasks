//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,             
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID                            
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB                                  
//*=====================================================================
//* DELETE IF EXISTS                                                    
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD    DD DSN=Z73460.TASK16.INVOICE.TXT,                            
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//*=====================================================================
//* COMPILE AND RUN PROGRAM                                             
//*=====================================================================
//STEP010  EXEC MYCOMPGO,MEMBER=PSTASK16                                
//RUN.PARTDD DD DSN=Z73460.TASK16.PARTS.CATALOG,DISP=SHR                
//RUN.ORDRDD DD DSN=Z73460.TASK16.ORDERS.FILE,DISP=SHR                  
//RUN.INVODD DD DSN=Z73460.TASK16.INVOICE.TXT,                          
//             DISP=(NEW,CATLG,DELETE),                                 
//             SPACE=(TRK,(2,2),RLSE),                                  
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80)                                   
