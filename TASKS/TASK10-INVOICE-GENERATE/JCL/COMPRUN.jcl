//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB
//*=====================================================================
//* DELETE IF EXISTS
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14
//REPDD    DD DSN=Z73460.TASK10.INVOICE.FILE,
//            SPACE=(TRK,(1,0),RLSE),
//            DISP=(MOD,DELETE,DELETE)
//*=====================================================================
//* COMPILE AND RUN PROGRAM
//*=====================================================================
//STEP010  EXEC MYCOMPGO,MEMBER=VSMJOB10
//RUN.VSAMDD DD DSN=Z73460.TASK10.PROD.MASTER.VSAM,DISP=SHR
//RUN.ORDD DD DSN=Z73460.TASK10.ORDERS.DAILY,DISP=SHR
//RUN.OUTDD DD DSN=Z73460.TASK10.INVOICE.FILE,
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(2,2),RLSE),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80)
