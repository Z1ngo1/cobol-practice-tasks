//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,             
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID                            
//SETLIB   JCLLIB ORDER=YOUR.PROCLIB                                 
//*=====================================================================
//* DELETE IF EXISTS                                                    
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14    
//REPDD    DD DSN=YOUR.FILE.NAME,                             
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//*=====================================================================
//* COMPILE AND RUN PROGRAM                                             
//*=====================================================================
//STEP010  EXEC MYCOMPGO,MEMBER=YOUR.MEMBER.NAME                                  
//RUN.YOURDD DD DSN=YOUR.DATASET,DISP=SHR          
//RUN.YOURDD DD DSN=YOUR.DATASET,DISP=SHR    
//RUN.YOURDD DD DSN=YOUR.DATASET,DISP=SHR                   
//RUN.YOURDD DD DSN=YOUR.DATASET,                           
//           DISP=(NEW,CATLG,DELETE),                                   
//           SPACE=(TRK,(2,2),RLSE),                                    
//           DCB=(DSORG=PS,RECFM=VB,LRECL=84)     
//**********************************************************************
//* Depending on the length and format of your reading data set 
//* you want to set, change the DCB parameters!!! 
//* change all words from 'YOUR' to your data, 
//* namely DD NAMES and DATA SETS NAMES
//**********************************************************************
