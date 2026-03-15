//DEFPATH  JOB (123),'IDCAMS',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),        
//             NOTIFY=&SYSUID                                           
//*                                                                                                
//*==================================================================  
//* STEP10: DEFINE PATH USING IDCAMS
//*==================================================================       
//STEP10   EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  DEFINE PATH ( -                                                       
     NAME (Z73460.TASK18.LIBRARY.MASTER.VSAM.PATH) -                    
     PATHENTRY(Z73460.TASK18.LIBRARY.MASTER.VSAM.AIX))-                 
     )                                                                  
/*                                                                      
