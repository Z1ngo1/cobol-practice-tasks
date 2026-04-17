//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,             
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID                            
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB                                  
//*=====================================================================
//* DELETE IF EXISTS                                                    
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD1   DD DSN=Z73460.TASK30.FILTERED.REPORT,                        
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//*=====================================================================
//* INSERT DATA TO KSDS                                                 
//*=====================================================================
//STEP010  EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//INDD     DD *                                                         
00010020260405OP0001D000100000                                          
00010020260420OP0002D000100000                                          
00020020260412OP0003D000010000                                          
00030020260413OP0004C000100000                                          
00040020260414OP0005D000200000                                          
00050020260415OP0006D000500000                                          
00060020260416OP0007D000050000                                          
/*                                                                      
//OUTDD    DD DSN=Z73460.TASK30.OPR.LOG.KSDS,DISP=SHR                   
//SYSIN    DD *                                                         
  REPRO INFILE(INDD) OUTFILE(OUTDD)                                     
/*                                                                      
//*=====================================================================
//* COMPILE AND RUN PROGRAM                                             
//*=====================================================================
//STEP015  EXEC MYCOMPGO,MEMBER=SYSIN30                                 
//RUN.SYSIN DD *                                                        
FROM-DATE=20260410                                                      
TO-DATE=20260416                                                        
MIN-AMOUNT=500                                                          
OPR-TYPE=D                                                              
COMMENT LINE WITHOUT EQUALS SIGN                                        
UNKNOWN-KEY=IGNORED                                                     
//RUN.OPRDD DD DSN=Z73460.TASK30.OPR.LOG.KSDS,DISP=SHR                  
//RUN.REPDD DD DSN=Z73460.TASK30.FILTERED.REPORT,                       
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(1,1),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=36)                          
