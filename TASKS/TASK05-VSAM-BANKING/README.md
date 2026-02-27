# TASK05 - VSAM Banking Transaction Processor

Banking transaction processing system using VSAM KSDS random update operations.

## Business Scenario

A bank needs to process daily transactions (deposits and withdrawals) against customer account master file. The system must:
- Update account balances based on transaction type
- Validate account existence before processing
- Check for sufficient funds on withdrawals
- Generate error report for failed transactions
- Handle numeric overflow conditions

## Files

### Input Files

#### 1. ACCT.MASTER (VSAM KSDS) - Account Master File

**Access Mode:** 
- INPUT-OUTPUT (Random)
**Organization:**
- INDEXED (KSDS)
**Key:**
- ACCT-NUM (5 characters, positions 1-5)

**Record Layout:**
- 01  ACCT-RECORD.
  - 05  ACCT-NUM        PIC X(5).
  - 05  ACCT-NAME       PIC X(20).
  - 05  ACCT-BALANCE    PIC 9(5)V99.

**Sample Data:** [DATA/ACCT-MASTER-BEFORE](DATA/ACCT-MASTER-BEFORE)

#### 2. TRANS-FILE (PS) - Transaction File

- **Access Mode:** INPUT (Sequential)
- **Organization:** SEQUENTIAL
- **Record Format:** Fixed (RECFM=FB, LRECL=13)

**Record Layout:**
- 01  TRANS-RECORD.
  - 05  TRANS-ACCT      PIC X(5).
  - 05  TRANS-TYPE      PIC X(1).
  - 05  TRANS-AMOUNT    PIC 9(5)V99.

**Sample Data:** [DATA/TRANS-FILE-INPUT](DATA/TRANS-FILE-INPUT)

### Output Files

#### 3. ERROR-REPORT (PS) - Error Report

- **Access Mode:** OUTPUT (Sequential)
- **Organization:** SEQUENTIAL
- **Record Format:** Variable (RECFM=VB, LRECL=84)

**Error Types:**
- ACCOUNT NOT FOUND - Account number doesn't exist in master file
- INSUFFICIENT FUNDS - Withdrawal amount exceeds current balance
- OVERFLOW - Balance exceeds maximum limit (999999.99)

**Expected Output:** [DATA/ERROR-REPORT-OUTPUT](DATA/ERROR-REPORT-OUTPUT)

#### 4. ACCT.MASTER (VSAM KSDS) - Updated Master File

Updated balances after successful transactions.

**Expected Final State:** [DATA/ACCT-MASTER-AFTER](DATA/ACCT-MASTER-AFTER)

### Error Handling

**FILE STATUS Codes:**
- 00 - Successful operation
- 23 - Record not found (invalid account number)
- Other codes - I/O errors (logged to SYSOUT)  

## Program Flow

1. **Initialization**
   - Opens ACCT.MASTER (VSAM KSDS) in I-O mode for random access
   - Opens TRANS-FILE (PS) for sequential input
   - Opens ERROR-REPORT (PS) for sequential output
   - Initializes counters for statistics tracking

2. **Transaction Processing Loop**
   - Reads transaction records sequentially from TRANS-FILE
   - For each transaction:
     - Performs random READ from VSAM using TRANS-ACCT as key
     - If account found (FILE STATUS '00'):
       - **Deposit (TRANS-TYPE = 'D')**: 
         - Adds TRANS-AMOUNT to ACCT-BALANCE
         - Checks for numeric overflow (balance > 999999.99)
         - Rewrites updated record to VSAM
       - **Withdrawal (TRANS-TYPE = 'W')**: 
         - Validates sufficient funds (ACCT-BALANCE >= TRANS-AMOUNT)
         - Subtracts TRANS-AMOUNT from ACCT-BALANCE
         - Rewrites updated record to VSAM
         - Writes error if insufficient funds
     - If account not found (FILE STATUS '23'):
       - Writes error message to ERROR-REPORT with account number

3. **Termination**
   - Closes all files (VSAM, TRANS-FILE, ERROR-REPORT)
   - Displays processing statistics:
     - Total transactions processed
     - Successful updates
     - Total errors (account not found + insufficient funds + overflow)
   - Validates FILE STATUS after every file operation

## JCL Jobs

### 1. [DEFKSDS.jcl](JCL/DEFKSDS.jcl) - Define VSAM Cluster

Defines KSDS cluster for account master file.

**Key Parameters:**
- RECORDSIZE(32,32) - Fixed 32-byte records
- KEYS(5 0) - 5-byte key starting at position 0
- TRACKS(15) - Initial allocation
- INDEXED - KSDS organization

### 2. [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Execute

Standard compile-link-go JCL using MYCOMPGO procedure.

**DD Statements:**
- VSAM-DD - ACCT.MASTER (VSAM KSDS)
- TRANS-DD - TRANS-FILE (PS)
- ERROR-DD - ERROR-REPORT (PS)

## How to Run

### Step 1: Define VSAM Cluster

**Submit:** [JCL/DEFKSDS.jcl](JCL/DEFKSDS.jcl)  
**Verify:** Check for MAXCC=0 in job output

### Step 2: Load Master Data into VSAM

**Option A: Use File Manager**
1. Navigate to VSAM file in ISPF 
2. Open with File Manager (FM)
3. Insert records manually from [DATA/ACCT-MASTER-BEFORE](DATA/ACCT-MASTER-BEFORE)

**Option B: Use REPRO with inline data**

**Important:** Inline DD * data is padded to 80 bytes. You must either:
- Define VSAM with RECORDSIZE(80,80) to match inline format and add FILLER PIC X(48) to FD ACCT-RECORD in COBOL program

**⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.

```JCL
//********************************************
//* STEP 1: DEFINE VSAM CLUSTER              
//********************************************
//STEP10   EXEC PGM=IDCAMS                             
//SYSPRINT DD SYSOUT=*                                 
//SYSOUT   DD SYSOUT=*                                 
//SYSIN    DD *                                        
  DEFINE CLUSTER (NAME(YOUR.VSAM.CLUSTER) -
           RECORDSIZE(80 80)               -           
           TRACKS(1 1)                     -           
           KEYS(20 0)                      -          
           CISZ(4096)                      -           
           FREESPACE(10 20)                -           
           INDEXED)                                    
/*
//********************************************
//* STEP 2: LOAD DATA INTO VSAM USING REPRO  
//********************************************
//STEP20   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//INFILE   DD *
YOUR DATA HERE
/*
//SYSIN    DD *
  REPRO INFILE(INFILE) -
        OUTDATASET(YOUR.VSAM.CLUSTER)
/*
```

- OR Create temporary PS file with exact record length (32 bytes), load data there first, then REPRO to VSAM. Example in [JCL SAMPLES/DATAVSAM.jcl](../../JCL%20SAMPLES/DATAVSAM.jcl) uses SORT utility (can also be done with ICETOOL, IEBGENER)

### Step 3: Prepare Transaction File

**Option A: Manual upload**

Upload [DATA/TRANS-FILE-INPUT](DATA/TRANS-FILE-INPUT) to PS dataset manually via ISPF

**Option B: Create via JCL**

Create PS file with LRECL=80 and insert inline data using IEBGENER:

**⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.

```JCL
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
YOUR DATA HERE
/*
//SYSUT2   DD DSN=YOUR.DATA.SET,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(RECFM=F,LRECL=80,BLKSIZE=80)
```

**Alternative:** Allocate PS file and insert exact length of your file transaction data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 4: Execute Program

**Submit:** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**Check:** SYSOUT for statistics and FILE STATUS messages  
**Review:** [ERROR-REPORT-OUTPUT](DATA/ERROR-REPORT-OUTPUT) for failed transactions

### Step 5: Verify Results

Compare updated VSAM file content with expected results in [DATA/ACCT-MASTER-AFTER](DATA/ACCT-MASTER-AFTER)

## Common Issues

### Issue 1: FILE STATUS '92' or '93'

**Cause:** VSAM file not defined or damaged
**Solution:** Re-run DEFKSDS.jcl

### Issue 2: FILE STATUS '23' for all transactions

**Cause:** Master file is empty or wrong key field
**Solution:** Verify data loaded correctly, check key offset

### Issue 3: Abend S0C7 (Data Exception)

**Cause:** Non-numeric data in ACCT-BALANCE or TRANS-AMOUNT
**Solution:** Verify input data format (no spaces in numeric fields)

### Issue 4: Wrong final balances

**Cause:** Transaction file processed out of order
**Solution:** Verify transaction sequence in input file

## Notes

- Program uses random access mode for VSAM (not sequential)
- Each transaction is independent - one failure doesn't affect others
- VSAM file remains open throughout processing for performance
- Error report uses variable-length records to accommodate long messages
- Tested on IBM z/OS with Enterprise COBOL
