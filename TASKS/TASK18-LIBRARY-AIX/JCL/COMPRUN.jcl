//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB
//*=====================================================================
//* DELETE IF EXISTS
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD1   DD DSN=Z73460.TASK18.SEARCH.REQ,                             
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD2   DD DSN=Z73460.TASK18.RESULT.RPT,                             
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
KING STEPHEN                                                            
ROWLING J.K.                                                            
PUSHKIN A.S.                                                            
ASIMOV ISAAC                                                            
TOLKIEN J.R.R.                                                          
HEMINGWAY ERNEST                                                        
CHRISTIE AGATHA                                                         
                                                                        
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK18.SEARCH.REQ,                             
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=80)                           
//*=====================================================================
//* COMPILE AND RUN PROGRAM
//*=====================================================================
//STEP020  EXEC MYCOMPGO,MEMBER=VSAM18                                  
//RUN.VSAMDD DD DSN=Z73460.TASK18.LIBRARY.MASTER.VSAM,DISP=SHR          
//RUN.VSAMDD1 DD DSN=Z73460.TASK18.LIBRARY.MASTER.VSAM.PATH,DISP=SHR    
//RUN.SRCHDD DD DSN=Z73460.TASK18.SEARCH.REQ,DISP=SHR                   
//RUN.RSLTDD DD DSN=Z73460.TASK18.RESULT.RPT,                           
//           DISP=(NEW,CATLG,DELETE),                                   
//           SPACE=(TRK,(1,1),RLSE),                                    
//           DCB=(DSORG=PS,RECFM=VB,LRECL=84)                            
