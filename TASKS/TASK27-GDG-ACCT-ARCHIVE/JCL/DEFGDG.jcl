//DEFGDG   JOB (123),'MYJOB',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),         
//            NOTIFY=&SYSUID                                            
//**************************************************************        
//* JCL TO CREATE UNMATCH GDG                                             
//**************************************************************        
//STEP1    EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  DEFINE GDG(NAME(Z73460.TASK27.UNMATCH.GDG) -                          
         LIMIT(5) -                                                     
         NOEMPTY -                                                      
         SCRATCH                                                        
/*               
//**************************************************************        
//* JCL TO CREATE ACCT.ACTIVE GDG                                             
//**************************************************************        
//STEP1    EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  DEFINE GDG(NAME(Z73460.TASK27.ACCT.ACTIVE.GDG) -                          
         LIMIT(5) -                                                     
         NOEMPTY -                                                      
         SCRATCH                                                        
/*                                                    
//**************************************************************        
//* JCL TO CREATE ARCHIVE GDG                                             
//**************************************************************        
//STEP1    EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  DEFINE GDG(NAME(Z73460.TASK27.ARCHIVE.OLD.GDG) -                          
         LIMIT(5) -                                                     
         NOEMPTY -                                                      
         SCRATCH                                                        
/*                                 
