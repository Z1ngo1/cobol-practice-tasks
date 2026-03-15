//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,             
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID                            
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB                                  
//*=====================================================================
//* DELETE IF EXISTS                                                    
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD    DD DSN=Z73460.TASK17.HONOR.ROLL,                             
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//*=====================================================================
//* COMPILE AND RUN PROGRAM                                             
//*=====================================================================
//STEP010  EXEC MYCOMPGO,MEMBER=PSTASK17                                
//RUN.EXDD  DD DSN=Z73460.TASK17.EXAM.RAW,DISP=SHR                      
//RUN.SRTDD DD DSN=Z73460.TASK17.WORK.SORT,                             
//           DISP=(NEW,DELETE,DELETE),                                  
//           SPACE=(TRK,(2,2),RLSE),                                    
//           DCB=(DSORG=PS,RECFM=FB,LRECL=31)                           
//RUN.HNRDD DD DSN=Z73460.TASK17.HONOR.ROLL,                            
//           DISP=(NEW,CATLG,DELETE),                                   
//           SPACE=(TRK,(2,2),RLSE),                                    
//           DCB=(DSORG=PS,RECFM=FB,LRECL=31)                              
