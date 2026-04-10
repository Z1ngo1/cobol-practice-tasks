//CBLDBDG  JOB (123),'DB2COB',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),        
//             NOTIFY=&SYSUID                                           
//*=====================================================================
//*STEP 1: DELETE OLD OUTPUT DATASETS IF IT EXISTS                      
//*=====================================================================
//DELREP   EXEC PGM=IEFBR14                                             
//DELDD1   DD  DSN=Z73460.TASK20.EMP.UPDATE,                            
//             SPACE=(TRK,(1,0),RLSE),                                  
//             DISP=(MOD,DELETE,DELETE)                                 
//DELDD2   DD  DSN=Z73460.TASK20.SYNC.LOG,                              
//             SPACE=(TRK,(1,0),RLSE),                                  
//             DISP=(MOD,DELETE,DELETE)                                 
//*=====================================================================
//* STEP 2: INSERT DATA TO INPUT DATA                                   
//*=====================================================================
//STEPINS  EXEC PGM=IEBGENER                                            
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  GENERATE MAXFLDS=1                                                    
  RECORD FIELD=(44,1,,1)                                                
//SYSUT1   DD *                                                         
00100JOHN DOE            IT 050000019950101A                            
00200JANE SMITH          HR 045000020000515A                            
00300BOB MARLEY          SAL060000019700206I                            
     EMPTY ID EMPLOYEE   IT 040000020200101A                            
00400ALICE WONDERLAND    FIN055000019980315A                            
00500                    IT 035000020210101A                            
00600CHARLIE BROWN       IT 048000019991220A                            
00700DAVID BECKHAM       SALXXXXXXX20050701A                            
00800EMMA WATSON         HR 000000020010610A                            
00900FRANK SINATRA       IT 062000019151212A                            
00100JOHN DOE UPDATED    SAL075000019950101A                            
01000GRACE KELLY         HR 049000019561112A                            
01100HENRY FORD          IT XXXXXXX20200101A                            
01200IRENE CURIE         FIN051000019960615A                            
00200JANE SMITH PROMO    HR 065000020000515A                 
01300JACK LONDON         IT 053000018760116X                            
01400KATE WINSLET        SAL058000019751005A                            
00300BOB MARLEY ACTIVE   IT 085000019700206A                            
01500LARRY PAGE          IT 092000019980907A                            
01600MARY SHELLEY        FIN047000017970830A                            
01700MICHAEL JACKSON     HR 067000019580829I                            
01800ELVIS PRESLEY       SAL071000019350108I                            
01900WHITNEY HOUSTON     HR 063000019630809I                            
02000PRINCE ROGERS       IT 069000019580607I                            
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK20.EMP.UPDATE,                             
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=44)                          
//*=====================================================================
//* STEP 3: COBOL COMPILATION WITH DB2 PRECOMPILE                       
//*=====================================================================
//COMPIL   EXEC DB2CBL,MBR=DB2JOB20                                     
//COBOL.SYSIN  DD DSN=Z73460.COB.PRAC(DB2JOB20),DISP=SHR                
//COBOL.SYSLIB DD DSN=Z73460.DCLGEN,DISP=SHR                            
//*=====================================================================
//* STEP 4: PROGRAM EXECUTION UNDER DB2 CONTROL                         
//*=====================================================================
//RUNPROG  EXEC PGM=IKJEFT01,COND=(4,LT)                                
//STEPLIB  DD DSN=DSND10.SDSNLOAD,DISP=SHR                              
//         DD DSN=Z73460.LOAD,DISP=SHR                                  
//INDD     DD DSN=Z73460.TASK20.EMP.UPDATE,DISP=SHR                     
//OUTDD    DD DSN=Z73460.TASK20.SYNC.LOG,                               
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2)),                                        
//            DCB=(DSORG=PS,RECFM=VB,LRECL=84,BLKSIZE=800)              
//*=====================================================================
//* STEP 5: DB2 EXECUTION CONTROL - RUN PROGRAM UNDER DBDG SUBSYSTEM    
//*=====================================================================
//SYSTSPRT DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//SYSTSIN  DD *                                                         
  DSN SYSTEM(DBDG)                                                      
  RUN PROGRAM(DB2JOB20) PLAN(Z73460) -                                  
      LIB('Z73460.LOAD')                                                
  END                                                                   
/*                                                                      
