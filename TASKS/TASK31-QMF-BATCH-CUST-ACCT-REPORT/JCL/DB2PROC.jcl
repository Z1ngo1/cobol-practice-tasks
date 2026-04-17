//DB2PROC  JOB (123),'DB2 PROC',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),      
//             NOTIFY=&SYSUID                                           
//*=====================================================================
//* TASK31 - QMF BATCH REPORT: ACTIVE CUSTOMERS AND ACCOUNT BALANCES    
//*=====================================================================
//*=====================================================================
//* DELETE WORK FILES FROM PREVIOUS RUN (IF EXIST)                      
//*=====================================================================
//STEP005  EXEC PGM=IEFBR14                                             
//DEL1     DD DSN=Z73460.TASK31.QUERY1,                                 
//         SPACE=(CYL,(1,0),RLSE),DISP=(MOD,DELETE,DELETE)              
//DEL2     DD DSN=Z73460.TASK31.QUERY2,                                 
//         SPACE=(CYL,(1,0),RLSE),DISP=(MOD,DELETE,DELETE)              
//DEL3     DD DSN=Z73460.TASK31.SORTED.REPORT,                          
//         SPACE=(CYL,(1,0),RLSE),DISP=(MOD,DELETE,DELETE)              
//*=====================================================================
//* RUN QMF BATCH PROC (TASK31P)                                        
//* M=B - MODE=BATCH: RUN QMF IN BATCH                                  
//* S=DBDG - DB2 SUBSYSTEM NAME TO CONNECT                              
//* P=QMFD10 - QMF PLAN NAME (PRECOMPILED DB2 APPLICATION PLAN)         
//* I=TASK31P - INITIAL QMF PROC TO EXECUTE ON STARTUP                  
//*=====================================================================
//STEP010  EXEC PGM=DSQQMFE,PARM='M=B,S=DBDG,P=QMFD10,I=TASK31P'        
//STEPLIB  DD DSN=QMFD10.SDSQLOAD,DISP=SHR                              
//         DD DSN=DSND10.DBDG.SDSNEXIT,DISP=SHR                         
//         DD DSN=DSND10.SDSNLOAD,DISP=SHR                              
//SYSTSPRT DD SYSOUT=*                                                  
//SYSPRINT DD SYSOUT=*                                                  
//DSQDBUG  DD SYSOUT=*                                                  
//ADMGGMAP DD DSN=QMFD10.SDSQMAPE,DISP=SHR                              
//*=====================================================================
//* WRITE SEPARATOR HEADER FOR QUERY 1 REPORT AS VB FILE.               
//* SORT COPY WITH FTOV (FIXED TO VARIABLE CONVERSION).                 
//*=====================================================================
//STEP015  EXEC PGM=SORT                                                
//SYSOUT   DD SYSOUT=*                                                  
//VBOUTP   DD DSN=Z73460.TASK31.QMF.GDG(+1),                            
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(5,5),RLSE),                                   
//            DCB=(LRECL=100,RECFM=VB)                                  
//SORTIN   DD *                                                         
                    ***FIRST TEST - TABLE***                            
//SYSIN    DD *                                                         
   OPTION COPY                                                          
   OUTFIL FNAMES=VBOUTP,FTOV                                            
/*                                                                      
//*=====================================================================
//* WRITE SEPARATOR HEADER FOR QUERY 2 REPORT AS VB FILE.               
//* SORT COPY WITH FTOV (FIXED TO VARIABLE CONVERSION).                 
//*=====================================================================
//STEP020  EXEC PGM=SORT                                                
//SYSOUT   DD SYSOUT=*                                                  
//VBOUTP   DD DSN=Z73460.TASK31.QMF.GDG(+2),                            
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(5,5),RLSE),                                   
//            DCB=(LRECL=100,RECFM=VB)                                  
//SORTIN   DD *                                                         
                     ***SECOND TEST - TABLE***                          
//SYSIN    DD *                                                         
   OPTION COPY                                                          
   OUTFIL FNAMES=VBOUTP,FTOV                                            
/*                                                                      
//*=====================================================================
//* MERGE ALL PARTS INTO FINAL SORTED REPORT                            
//* VTOF (VARIABLE TO FIXED), REMOVECC (STRIP CARRIAGE CTRL).           
//* ADDS REPORT HEADER WITH DATE AND TIME.                              
//*=====================================================================
//STEP025  EXEC PGM=SORT                                                
//SYSOUT   DD SYSOUT=*                                                  
//SORTIN   DD DSN=Z73460.TASK31.QMF.GDG(+1),DISP=SHR                    
//         DD DSN=Z73460.TASK31.QUERY1,DISP=SHR                         
//         DD DSN=Z73460.TASK31.QMF.GDG(+2),DISP=SHR                    
//         DD DSN=Z73460.TASK31.QUERY2,DISP=SHR                         
//SORTOUT  DD DSN=Z73460.TASK31.SORTED.REPORT,                          
//            DISP=(,CATLG,DELETE),                                     
//            SPACE=(CYL,(5,5))                                         
//SYSIN    DD *                                                         
       SORT FIELDS=COPY                                                 
       OUTFIL VTOF,OUTREC(5,71,80:X),REMOVECC,                          
   HEADER1=(20:C'TEST TABLES DATED: ',                                  
       40:DATE=(MD4/),'@',TIME),                                        
   HEADER2=(X)                                                          
/*                                                                      
