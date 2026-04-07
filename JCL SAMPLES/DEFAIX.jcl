//DEFAIX   JOB (123),'IDCAMS',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),       
//             NOTIFY=&SYSUID                                          
//*                                                
//*==================================================================  
//* STEP10: DEFINE ALTERNATE INDEX (AIX) FOR KSDS CLUSTER
//*==================================================================    
//STEP10   EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                 
//SYSOUT   DD SYSOUT=*                                                 
//SYSIN    DD *                                                        
  DEFINE AIX                                        -                           
     (NAME(YOUR.CLUSTER.NAME.AIX)                   -                    
     RELATE(YOUR.CLUSTER.NAME)                       -                      
     CISZ(4096)                                     -                           
     KEYS(20 10)                                    -                          
     NONUNIQUEKEY                                   -                           
     UPGRADE                                        -                           
     RECORDSIZE(80,80)                              -                           
     TRACKS(2,1)                                    -                          
     FREESPACE(10,20))                                                 
/*
//**********************************************************************
//* AIX (Alternate Index) allows access to a KSDS cluster by a field
//* other than the primary key (e.g. by last name, department, etc.)
//*
//*   NAME     - name of the AIX dataset itself
//*   RELATE   - must point to the BASE KSDS cluster
//*   CISZ     - Control Interval size (match base cluster if possible)
//*   KEYS     - (length offset) of the ALTERNATE key field in record
//*   NONUNIQUEKEY - multiple records can share the same alternate key
//*                - use UNIQUEKEY if alternate key must be unique
//*   UPGRADE - AIX is automatically updated when base cluster changes
//*         - use NOUPGRADE if manual rebuild via BLDINDEX is preferred
//*   RECORDSIZE - (average maximum) must be >= base cluster RECORDSIZE
//*   TRACKS      - primary and secondary space allocation
//*   FREESPACE   - (CI CA) free space percentages
//*
//* Order of steps to use AIX:
//*   1. DEFKSDS  - define base KSDS cluster
//*   2. DEFAIX   - define Alternate Index (this JCL)
//*   3. DEFPATH  - define PATH to connect AIX to base cluster
//*   4. BLDINDEX - populate the AIX with data from base cluster
//*
//* Change all words 'YOUR.CLUSTER.NAME' to your actual dataset name.
//**********************************************************************
