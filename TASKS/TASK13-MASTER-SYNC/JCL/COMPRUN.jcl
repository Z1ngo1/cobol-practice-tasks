//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,             
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID                            
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB                                  
//*=====================================================================
//* DELETE IF EXISTS                                                    
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD1   DD DSN=Z73460.TASK13.NEW.MASTER,                             
//            SPACE=(TRK,(1,0),RLSE),                                
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD2   DD DSN=Z73460.TASK13.ERROR.REPORT,                           
//            SPACE=(TRK,(1,0),RLSE),                                 
//            DISP=(MOD,DELETE,DELETE)                                  
//*=====================================================================
//* COMPILE AND RUN PROGRAM                                             
//*=====================================================================
//STEP010  EXEC MYCOMPGO,MEMBER=PSTASK13                                
//RUN.OLDDD DD DSN=Z73460.TASK13.OLD.MASTER,DISP=SHR                    
//RUN.TRNSDD DD DSN=Z73460.TASK13.TRANS.FILE,DISP=SHR                   
//RUN.NEWDD DD DSN=Z73460.TASK13.NEW.MASTER,                            
//             DISP=(NEW,CATLG,DELETE),                                 
//             SPACE=(TRK,(2,2),RLSE),                                  
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80)                         
//RUN.REPDD DD DSN=Z73460.TASK13.ERROR.REPORT,                          
//             DISP=(NEW,CATLG,DELETE),          
//             SPACE=(TRK,(2,2),RLSE),                
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80)                                 
