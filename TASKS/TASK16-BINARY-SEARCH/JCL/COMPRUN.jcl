//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB
//*=====================================================================
//* DELETE IF EXISTS
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD1   DD DSN=Z73460.TASK16.PARTS.CATALOG,                          
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD2   DD DSN=Z73460.TASK16.ORDERS.FILE,                            
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD3   DD DSN=Z73460.TASK16.INVOICE.TXT,                            
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
  RECORD FIELD=(10,1,,1)                                                
//SYSUT1   DD *                                                         
0010005000                                                              
0025010000                                                              
0030002000                                                              
0050007500                                                              
0075015000                                                              
0100020000                                                              
0150030000                                                              
0200045000                                                              
0500075000                                                              
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK16.PARTS.CATALOG,                          
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=10)                           
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
1000100100010                                                           
1000200250005                                                           
1000300300020                                                           
1000400500001                                                           
1001199999100                                                           
1001200075050                                                           
1001399888003                                                           
1001402000015                                                           
1001500001025                                                           
1002005000200                                                           
1002100200099                                                           
1002200010003                                                           
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK16.ORDERS.FILE,                            
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=13)                           
//*=====================================================================
//* COMPILE AND RUN PROGRAM
//*=====================================================================
//STEP020  EXEC MYCOMPGO,MEMBER=PSTASK16                                
//RUN.PARTDD DD DSN=Z73460.TASK16.PARTS.CATALOG,DISP=SHR                
//RUN.ORDRDD DD DSN=Z73460.TASK16.ORDERS.FILE,DISP=SHR                  
//RUN.INVODD DD DSN=Z73460.TASK16.INVOICE.TXT,                          
//             DISP=(NEW,CATLG,DELETE),                                 
//             SPACE=(TRK,(1,1),RLSE),                                  
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)              
