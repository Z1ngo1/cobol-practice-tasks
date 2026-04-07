//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB
//*=====================================================================
//* DELETE IF EXISTS
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD1   DD DSN=Z73460.TASK13.NEW.MASTER,                             
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD2   DD DSN=Z73460.TASK13.ERROR.REPORT,                           
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD3   DD DSN=Z73460.TASK13.OLD.MASTER,                             
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD4   DD DSN=Z73460.TASK13.TRANS.FILE,                             
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
00100JOHN DOE            0010000                                        
00200JANE SMITH          0050050                                        
00300BOB MARLEY          0000000                                        
00400TOM JONES           0025000                                        
00500ALICE COOPER        0099999                                        
00600MARY WILLIAMS       0012500                                        
00800CHRIS BROWN         0003000                                        
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK13.OLD.MASTER,                             
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
00050ANEW CUSTOMER        0050000                                       
00100U                    0005000                                       
00100U                    0002500                                       
00200A                    0000000                                       
00250AJACK ROBINSON       0020000                                       
00300D                    0000000                                       
00350USALLY FIELDS        0001000                                       
00400U                    0015000                                       
00500D                    0000000                                       
00600U                    0007500                                       
00650ANEW CLIENT TWO      0010000                                       
00700DMIKE DAVIS          0000000                                       
00800U                    0001000                                       
00800U                    0002000                                       
00900ALATE CUSTOMER       0015000                
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK13.TRANS.FILE,                             
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)              
//*=====================================================================
//* COMPILE AND RUN PROGRAM
//*=====================================================================
//STEP020  EXEC MYCOMPGO,MEMBER=PSTASK13                                
//RUN.OLDDD DD DSN=Z73460.TASK13.OLD.MASTER,DISP=SHR                    
//RUN.TRNSDD DD DSN=Z73460.TASK13.TRANS.FILE,DISP=SHR                   
//RUN.NEWDD DD DSN=Z73460.TASK13.NEW.MASTER,                            
//             DISP=(NEW,CATLG,DELETE),                                 
//             SPACE=(TRK,(1,1),RLSE),                                  
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)             
//RUN.REPDD DD DSN=Z73460.TASK13.ERROR.REPORT,                          
//             DISP=(NEW,CATLG,DELETE),                                 
//             SPACE=(TRK,(1,1),RLSE),                                  
//             DCB=(*.NEWDD)                                             
