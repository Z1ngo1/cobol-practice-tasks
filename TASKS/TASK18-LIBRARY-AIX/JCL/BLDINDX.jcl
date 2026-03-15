//DEFPATH  JOB (123),'IDCAMS',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),        
//             NOTIFY=&SYSUID                                           
//*                                                                                                               
//*================================================================== 
//* STEP10: BUILD INDEX USING BLDINDEX COMMAND
//*==================================================================      
//STEP10   EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  BLDINDEX -                                                            
   INDATASET(Z73460.TASK18.LIBRARY.MASTER.VSAM) -                       
   OUTDATASET(Z73460.TASK18.LIBRARY.MASTER.VSAM.AIX)                    
/*                                                                      
