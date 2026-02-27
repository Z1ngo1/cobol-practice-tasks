//DATA2PS  JOB (123),'TEST',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),         
//             NOTIFY=&SYSUID                                          
//STEP1    EXEC PGM=IEFBR14                                            
//DELDD    DD  DSN=YOUR.DATA.SET,                                     
//             SPACE=(TRK,(1,0),RLSE),                                 
//             DISP=(MOD,DELETE,DELETE)                                
//STEP2    EXEC PGM=IEBGENER                                           
//SYSPRINT DD SYSOUT=*                                                 
//SYSOUT   DD SYSOUT=*                                                 
//SYSIN    DD *                                                        
  GENERATE MAXFLDS=1                                                   
  RECORD FIELD=(32,1,,1)                                               
//SYSUT1   DD *                                                        
1234567890QWERTYUIOPCVASDFGHJKLZ                                       
/*                                                                     
//SYSUT2   DD DSN=YOUR.DATA.SET,                                      
//            DISP=(NEW,CATLG,DELETE),                                 
//            SPACE=(TRK,(1,1),RLSE),                                  
//            DCB=(DSORG=PS,RECFM=FB,LRECL=32)                                                                          
//**********************************************************************
//* Depending on the size and format of the dataset you want to create,
//* change the DCB settings!!! 
//* Change all the words from 'YOUR' to your data
//* Change the data in SYSUT1 and RECORD FIELD in SYSIN
//**********************************************************************
