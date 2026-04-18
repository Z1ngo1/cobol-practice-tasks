//DEFGDG   JOB (123),'MYJOB',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),         
//            NOTIFY=&SYSUID                                            
//**************************************************************        
//* JCL TO CREATE GDG                                             
//**************************************************************        
//STEP1    EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  DEFINE GDG(NAME(YOUR.GDG.NAME) -                          
         LIMIT(5) -                                                     
         NOEMPTY -                                                      
         SCRATCH                                                        
/*               
//**********************************************************************
//* GDG base is a catalog entry only - no physical dataset is created.
//* Each generation (GDS) is allocated separately as G0001V00, etc.
//*
//*   NAME    - fully qualified GDG base name in the catalog
//*   LIMIT   - max number of generations kept (1-255)
//*             oldest is removed when new one exceeds the limit
//*   NOEMPTY - only the oldest generation is removed when limit hit
//*             use EMPTY to remove ALL generations at once
//*   SCRATCH - physically deletes removed generation from the volume
//*             use NOSCRATCH to only uncatalog it (keep on disk)
//*
//* Change all words 'YOUR' to your actual GDG base name.
//**********************************************************************
