//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB
//*=====================================================================
//* DELETE IF EXISTS
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                 
//REPDD    DD DSN=Z73460.TASK6.ARCHIVE.OLD, 
//            SPACE=(TRK,(1,0),RLSE),    
//            DISP=(MOD,DELETE,DELETE)      
//*=====================================================================
//* COMPILE AND RUN PROGRAM    
//*=====================================================================
//STEP010  EXEC MYCOMPGO,MEMBER=VSAMPS6                       
//RUN.INDD DD DSN=Z73460.TASK6.PARAM.FILE,DISP=SHR            
//RUN.CLTDD DD DSN=Z73460.TASK6.CLIENT.MASTER.VSAM,DISP=SHR   
//RUN.OUTDD DD DSN=Z73460.TASK6.ARCHIVE.OLD,                  
//             DISP=(NEW,CATLG,DELETE),                       
//             SPACE=(TRK,(2,2),RLSE),                        
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80)               
