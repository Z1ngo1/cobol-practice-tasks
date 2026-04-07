//DEFKSDS  JOB (123),'IDCAMS',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),        
//             NOTIFY=&SYSUID                                           
//*                                                
//*==================================================================          
//* STEP 1: DEFINE VSAM KSDS CLUSTER                                            
//*==================================================================          
//STEP10   EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  DEFINE CLUSTER (NAME(YOUR.CLUSTER.NAME) -             
           RECORDSIZE(80,80)               -                            
           TRACKS(2)                      -                            
           KEYS(5 0)                       -                           
           CISZ(4096)                      -                            
           FREESPACE(10,20)                -                            
           INDEXED)                                                     
/*                                                                      
//**********************************************************************
//* Depending on the size and format of the dataset you want to create,
//* change the following settings:
//*
//*   RECORDSIZE - set (average maximum) record length in bytes
//*   TRACKS     - set primary space allocation
//*   KEYS       - set (length offset) of the key field
//*   CISZ       - set Control Interval size (must be 512-32768)
//*   FREESPACE  - set (CI CA) free space percentages
//*
//* Change all the words from 'YOUR' to your actual dataset name.
//**********************************************************************
