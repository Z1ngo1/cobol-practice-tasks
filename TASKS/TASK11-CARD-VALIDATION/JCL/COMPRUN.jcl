//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB
//*=====================================================================
//* DELETE IF EXISTS
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                    
//REPDD1   DD DSN=Z73460.TASK11.APPROVED.FILE, 
//            SPACE=(TRK,(1,0),RLSE),   
//            DISP=(MOD,DELETE,DELETE)         
//REPDD2   DD DSN=Z73460.TASK11.DECLINED.FILE, 
//            SPACE=(TRK,(1,0),RLSE),        
//            DISP=(MOD,DELETE,DELETE)         
//*=====================================================================
//* COMPILE AND RUN PROGRAM
//*=====================================================================
//STEP010  EXEC MYCOMPGO,MEMBER=VSMJOB11                   
//RUN.VSAMDD DD DSN=Z73460.TASK11.CARD.MASTER.VSAM,DISP=SHR
//RUN.TRNSDD DD DSN=Z73460.TASK11.TRANS.DAILY,DISP=SHR     
//RUN.APRVDD DD DSN=Z73460.TASK11.APPROVED.FILE,           
//             DISP=(NEW,CATLG,DELETE),                    
//             SPACE=(TRK,(2,2),RLSE),                     
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80)            
//RUN.DECLDD DD DSN=Z73460.TASK11.DECLINED.FILE,           
//             DISP=(NEW,CATLG,DELETE),                    
//             SPACE=(TRK,(2,2),RLSE),                     
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80)            
