//DEFAIX   JOB (123),'IDCAMS',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),       
//             NOTIFY=&SYSUID                                          
//*                                                                                        
//*==================================================================  
//* STEP10: DEFINE AIX USING IDCAMS
//*==================================================================    
//STEP10  EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                 
//SYSOUT   DD SYSOUT=*                                                 
//SYSIN    DD *                                                        
  DEFINE AIX                               -                           
     (NAME(Z73460.TASK18.LIBRARY.MASTER.VSAM.AIX) -                    
     RELATE (Z73460.TASK18.LIBRARY.MASTER.VSAM) -                      
     CISZ(4096)                            -                           
     KEYS(20 10)                            -                          
     NONUNIQUEKEY                          -                           
     UPGRADE                               -                           
     RECORDSIZE(64,64)                     -                           
     TRACKS(2,1)                            -                          
     FREESPACE(10,20))                                                 
/*                                                                     
