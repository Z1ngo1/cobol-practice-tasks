//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB
//*=====================================================================
//* DELETE IF EXISTS
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD1   DD DSN=Z73460.TASK15.COMM.TIERS,                             
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD2   DD DSN=Z73460.TASK15.SALES.WEEKLY,                           
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD3   DD DSN=Z73460.TASK15.PAYOUT.RPT,                             
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                   
//*=====================================================================
//* INSERT DATA TO INPUT DATA                                           
//*=====================================================================
//STEP010  EXEC PGM=IEBGENER                                            
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  GENERATE MAXFLDS=1                                                    
  RECORD FIELD=(9,1,,1)                                                 
//SYSUT1   DD *                                                         
001000020                                                               
005000050                                                               
010000100                                                               
050000125                                                               
999999150                                                               
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK15.COMM.TIERS,                             
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=9)                            
//*=====================================================================
//* INSERT DATA TO SECOND INPUT DATA                                    
//*=====================================================================
//STEP015  EXEC PGM=IEBGENER                                            
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  GENERATE MAXFLDS=1                                                    
  RECORD FIELD=(13,1,,1)                                                
//SYSUT1   DD *                                                         
0010000050000                                                           
0020000050000                                                           
0030000100000                                                           
0040000150000                                                           
0050000500000                                                           
0060001000000                                                           
0070001500000                                                           
0080005000000                                                           
0090010000000                                                           
0100050000000                                                           
0110099999900                                                           
0120099999999                                                           
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK15.SALES.WEEKLY,                           
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=13)                           
//*=====================================================================
//* COMPILE AND RUN PROGRAM
//*=====================================================================
//STEP020  EXEC MYCOMPGO,MEMBER=PSTASK15                                
//RUN.COMMDD DD DSN=Z73460.TASK15.COMM.TIERS,DISP=SHR                   
//RUN.SALDD DD DSN=Z73460.TASK15.SALES.WEEKLY,DISP=SHR                  
//RUN.OUTDD DD DSN=Z73460.TASK15.PAYOUT.RPT,                            
//             DISP=(NEW,CATLG,DELETE),                                 
//             SPACE=(TRK,(1,1),RLSE),                                  
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)              
