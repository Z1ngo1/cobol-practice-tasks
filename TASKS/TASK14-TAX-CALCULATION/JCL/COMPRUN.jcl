//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB
//*=====================================================================
//* DELETE IF EXISTS
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD1   DD DSN=Z73460.TASK14.EMP.SALARY,                             
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD2   DD DSN=Z73460.TASK14.PAYROLL.TXT,                            
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD3   DD DSN=Z73460.TASK14.TAX.RATES,                              
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
NY085                                                                   
CA100                                                                   
TX000                                                                   
FL050                                                                   
IL095                                                                   
MA110                                                                   
WA090                                                                   
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK14.TAX.RATES,                              
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)              
//*=====================================================================
//* INSERT DATA TO SECOND INPUT DATA                                    
//*=====================================================================
//STEP015  EXEC PGM=IEBGENER                                            
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD DUMMY                                                     
//SYSUT1   DD *                                                         
00100JOHN DOE            NY0500000                                      
00200JANE SMITH          CA0600000                                      
00300BOB MARLEY          TX0450000                                      
00400TOM JONES           FL0800000                                      
00500ALICE COOPER        IL1000000                                      
00600MARY WILLIAMS       MA0300000                                      
00700CHRIS BROWN         WA0750000                                      
00800UNKNOWN GUY         XX0400000                                      
00900MYSTERY WOMAN       ZZ0500000                                      
01000PETER PARKER        NY1200000                                      
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK14.EMP.SALARY,                             
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)              
//*=====================================================================
//* COMPILE AND RUN PROGRAM
//*=====================================================================
//STEP020  EXEC MYCOMPGO,MEMBER=PSTASK14                                
//RUN.TAXDD DD DSN=Z73460.TASK14.TAX.RATES,DISP=SHR                     
//RUN.EMPDD DD DSN=Z73460.TASK14.EMP.SALARY,DISP=SHR                    
//RUN.OUTDD DD DSN=Z73460.TASK14.PAYROLL.TXT,                           
//             DISP=(NEW,CATLG,DELETE),                                 
//             SPACE=(TRK,(1,1),RLSE),                                  
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)                                  
