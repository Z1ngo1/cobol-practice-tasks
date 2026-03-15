//DEFKSDS  JOB (123),'IDCAMS',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),        
//             NOTIFY=&SYSUID                                           
//*                         
//*=====================================================================
//* STEP 1: DELETE OLD VSAM CLUSTER (IF EXISTS)
//*=====================================================================
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE Z73460.TASK18.LIBRARY.MASTER.VSAM PURGE CLUSTER
//*
  SET MAXCC=0
/*                                            
//*==================================================================          
//* STEP 2: DEFINE VSAM KSDS CLUSTER                                            
//*==================================================================          
//STEP10   EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  DEFINE CLUSTER (NAME(Z73460.TASK18.LIBRARY.MASTER.VSAM) -             
           RECORDSIZE(64,64)               -                            
           TRACKS(15)                      -                            
           KEYS(10 0)                       -                           
           CISZ(4096)                      -                            
           FREESPACE(10,20)                -                            
           INDEXED)                                                     
/*                                                                      
