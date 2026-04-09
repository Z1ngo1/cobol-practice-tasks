//COMPRUN  JOB (123),'COMP AND RUN JCL',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//SETLIB   JCLLIB ORDER=Z73460.PROCLIB
//*=====================================================================
//* DELETE IF EXISTS
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//REPDD1   DD DSN=Z73460.TASK17.EXAM.RAW,                               
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                  
//REPDD2   DD DSN=Z73460.TASK17.HONOR.ROLL,                             
//            SPACE=(TRK,(1,0),RLSE),                                   
//            DISP=(MOD,DELETE,DELETE)                                   
//*=====================================================================
//* INSERT DATA TO INPUT DATA                                           
//*=====================================================================
//STEP010  EXEC PGM=IEBGENER                                            
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
  GENERATE MAXFLDS=1                                                    
  RECORD FIELD=(31,1,,1)                                                
//SYSUT1   DD *                                                         
00100IVAN IVANOV         09A045                                         
00200PETR PETROV         10B090                                         
00300MARIA SIDOROVA      09A088                                         
00400ALEX SMIRNOV        10B095                                         
00500JOHN DOE            09A060                                         
00600ANNA PETROVA        09B025                                         
00700SERGEY KOZLOV       10A100                                         
00800ELENA VOLKOVA       09B078                                         
00900DMITRY SOKOLOV      09A050                                         
01000OLGA NOVIKOVA       10B049                                         
01100VIKTOR LEBEDEV      10A085                                         
01200IRINA MOROZOVA      09B092                                         
01300ANDREY VASILEV      11A070                                         
01400NATALIA FEDOROVA    11A088                                         
01500PAVEL MIKHAILOV     09A000                                         
01600SVETLANA KUZNETSOVA 10A055                                         
01700NIKOLAY POPOV       11A095                                         
01800TATIANA SEMENOVA    09B050                                         
01900MAXIM VOLKOV        10B100                                         
02000YULIA ORLOVA        11A045                                         
/*                                                                      
//SYSUT2   DD DSN=Z73460.TASK17.EXAM.RAW,                               
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(2,2),RLSE),                                   
//            DCB=(DSORG=PS,RECFM=FB,LRECL=31)                           
//*=====================================================================
//* COMPILE AND RUN PROGRAM
//*=====================================================================
//STEP020  EXEC MYCOMPGO,MEMBER=PSTASK17                                
//RUN.EXDD DD DSN=Z73460.TASK17.EXAM.RAW,DISP=SHR                       
//RUN.SRTDD DD DSN=Z73460.TASK17.WORK.SORT,                             
//             DISP=(NEW,DELETE,DELETE),                                
//             SPACE=(TRK,(1,1),RLSE),                                  
//             DCB=(DSORG=PS,RECFM=FB,LRECL=31)                         
//RUN.HNRDD DD DSN=Z73460.TASK17.HONOR.ROLL,                            
//             DISP=(NEW,CATLG,DELETE),                                 
//             SPACE=(TRK,(1,1),RLSE),                                  
//             DCB=(DSORG=PS,RECFM=FB,LRECL=31)                          
