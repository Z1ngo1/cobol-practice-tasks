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
//REPDD2   DD DSN=Z73460.TASK6.PARAM.FILE, 
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
20231231                                                                
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK6.PARAM.FILE,                              
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)              
//*=====================================================================
//* COMPILE AND RUN PROGRAM    
//*=====================================================================
//STEP015  EXEC MYCOMPGO,MEMBER=VSAMPS6                       
//RUN.INDD DD DSN=Z73460.TASK6.PARAM.FILE,DISP=SHR            
//RUN.CLTDD DD DSN=Z73460.TASK6.CLIENT.MASTER.VSAM,DISP=SHR   
//RUN.OUTDD DD DSN=Z73460.TASK6.ARCHIVE.OLD,                  
//             DISP=(NEW,CATLG,DELETE),                       
//             SPACE=(TRK,(2,2),RLSE),                        
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80)        
