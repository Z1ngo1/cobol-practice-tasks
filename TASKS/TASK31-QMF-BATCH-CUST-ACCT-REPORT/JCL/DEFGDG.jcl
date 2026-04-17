//DEFGDG   JOB (123),'MYJOB',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),         
//            NOTIFY=&SYSUID                                            
//**************************************************************        
//* JCL TO CREATE UNMATCH GDG                                             
//**************************************************************        
//STEP1    EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  DEFINE GDG(NAME(Z73460.TASK31.QMF.GDG) -                          
         LIMIT(10) -                                                     
         NOEMPTY -                                                      
         SCRATCH                                                        
/*               
