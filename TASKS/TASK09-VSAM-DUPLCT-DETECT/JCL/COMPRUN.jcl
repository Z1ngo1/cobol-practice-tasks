//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,             
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID                            
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB                                  
//*=====================================================================
//* DELETE IF EXISTS                                                    
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD    DD DSN=Z73460.TASK9.DUPLCT.REPORT,                           
//            SPACE=(TRK,(1,0),RLSE),                           
//            DISP=(MOD,DELETE,DELETE)                                  
//*=====================================================================
//* COMPILE AND RUN PROGRAM                                        
//*=====================================================================
//STEP010  EXEC MYCOMPGO,MEMBER=VSAMJOB9                                
//RUN.VSAMDD DD DSN=Z73460.TASK9.CLIENT.MAST.VSAM,DISP=SHR              
//RUN.SRTDD DD DSN=Z73460.TASK9.SORTWORK,                               
//             DISP=(NEW,DELETE,DELETE),                                
//             SPACE=(TRK,(2,2))                                        
//RUN.REPDD DD DSN=Z73460.TASK9.DUPLCT.REPORT,                          
//             DISP=(NEW,CATLG,DELETE),                                 
//             SPACE=(TRK,(2,2),RLSE),                                  
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80)               
