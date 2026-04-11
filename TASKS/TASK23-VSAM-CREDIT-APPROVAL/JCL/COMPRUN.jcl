//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB
//*=====================================================================
//* DELETE IF EXISTS
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14
//REPDD1   DD DSN=Z73460.TASK23.LOAN.REQUESTS,
//            SPACE=(TRK,(1,0),RLSE),
//            DISP=(MOD,DELETE,DELETE)
//REPDD2   DD DSN=Z73460.TASK23.APPROVAL.LOG,
//            SPACE=(TRK,(1,0),RLSE),
//            DISP=(MOD,DELETE,DELETE)
//*=====================================================================
//* INSERT DATA TO INPUT DATA
//*=====================================================================
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
1000010001000001000
1000020002000001000
1000030009990001000
1000040009991000000
1000050004003000000
1000060003000010000
1000070002000990000
/*
//SYSUT2   DD DSN=Z73460.TASK23.LOAN.REQUESTS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(2,2),RLSE),
//            DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)
//*=====================================================================
//* COMPILE SUBPROGRAM
//*=====================================================================
//STEP013  EXEC MYCOMPGO,MEMBER=SUB1JB23,
//             COND.RUN=(0,EQ)
//*=====================================================================
//* COMPILE AND RUN PROGRAM
//*=====================================================================
//STEP015  EXEC MYCOMPGO,MEMBER=JOBSUB23
//RUN.VSAMDD DD DSN=Z73460.TASK23.CREDIT.VSAM,DISP=SHR
//RUN.INDD  DD DSN=Z73460.TASK23.LOAN.REQUESTS,DISP=SHR
//RUN.OUTDD DD DSN=Z73460.TASK23.APPROVAL.LOG,
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(1,1),RLSE),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)
