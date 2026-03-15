//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,             
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID                            
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB                                  
//*=====================================================================
//* DELETE IF EXISTS                                                    
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD    DD DSN=Z73460.TASK18.RESULT.RPT,                             
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//*=====================================================================
//* COMPILE AND RUN PROGRAM                                             
//*=====================================================================
//STEP010  EXEC MYCOMPGO,MEMBER=VSAM18                                  
//RUN.VSAMDD DD DSN=Z73460.TASK18.LIBRARY.MASTER.VSAM,DISP=SHR          
//RUN.VSAMDD1 DD DSN=Z73460.TASK18.LIBRARY.MASTER.VSAM.PATH,DISP=SHR    
//RUN.SRCHDD DD DSN=Z73460.TASK18.SEARCH.REQ,DISP=SHR                   
//RUN.RSLTDD DD DSN=Z73460.TASK18.RESULT.RPT,                           
//           DISP=(NEW,CATLG,DELETE),                                   
//           SPACE=(TRK,(2,2),RLSE),                                    
//           DCB=(DSORG=PS,RECFM=VB,LRECL=84)                           
