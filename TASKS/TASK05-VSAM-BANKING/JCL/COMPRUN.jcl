//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB
//*=====================================================================
//* DELETE IF EXISTS
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14
//REPDD    DD DSN=Z73460.TASK5.REPORT,
//            SPACE=(TRK,(1,0),RLSE),
//            DISP=(MOD,DELETE,DELETE)
//*=====================================================================
//* COMPILE AND RUN PROGRAM    
//*=====================================================================
//STEP010  EXEC MYCOMPGO,MEMBER=VSAMJOB5
//RUN.INDD DD DSN=Z73460.TASK5.TRANS.FILE,DISP=SHR
//RUN.EMPDD DD DSN=Z73460.TASK5.ACCT.MASTER.VSAM,DISP=SHR
//RUN.REPDD DD DSN=Z73460.TASK5.REPORT,
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(2,2),RLSE),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80)
