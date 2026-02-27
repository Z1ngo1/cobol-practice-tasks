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

**Sample Data:** See DATA/ACCT-MASTER-BEFORE

#### 2. TRANS-FILE (PS) - Transaction File

- **Access Mode:** INPUT (Sequential)
- **Organization:** SEQUENTIAL
- **Record Format:** Fixed (RECFM=FB, LRECL=13)

**Record Layout:**
- 01  TRANS-RECORD.
  - 05  TRANS-ACCT      PIC X(5).
  - 05  TRANS-TYPE      PIC X(1).
  - 05  TRANS-AMOUNT    PIC 9(5)V99.

**Sample Data:** See DATA/TRANS-FILE-INPUT

### Output Files

#### 3. ERROR-REPORT (PS) - Error Report

- **Access Mode:** OUTPUT (Sequential)
- **Organization:** SEQUENTIAL
- **Record Format:** Variable (RECFM=VB, LRECL=84)

**Error Types:**
- ACCOUNT NOT FOUND - Account number doesn't exist in master file
- INSUFFICIENT FUNDS - Withdrawal amount exceeds current balance
- OVERFLOW - Balance exceeds maximum limit (999999.99)

**Expected Output:** See DATA/ERROR-REPORT-OUTPUT

#### 4. ACCT.MASTER (VSAM KSDS) - Updated Master File

Updated balances after successful transactions.

**Expected Final State:** See DATA/ACCT-MASTER-AFTER

## Data Format Examples

### Transaction Record Format

**Raw data:** 10001D0050000

**Breakdown:**
- 10001 - Account number (5 chars)
- D - Transaction type (1 char): D=Deposit, W=Withdraw
- 0050000 - Amount (7 digits): 0050000 = 500.00

### Account Master Record Format

**Raw data:** 10001IVAN IVANOV         0100000

**Breakdown:**
- 10001 - Account number (5 chars)
- IVAN IVANOV          - Customer name (20 chars, padded with spaces)
- 0100000 - Balance (7 digits): 0100000 = 1000.00

## Program Logic

### Main Processing Flow

**1. Initialization**
- Open VSAM file in I-O mode (INPUT-OUTPUT)
- Open TRANS-FILE for input
- Open ERROR-REPORT for output
- Initialize counters

**2. Transaction Processing Loop**

```cobol
READ TRANS-FILE UNTIL END-OF-FILE
    READ ACCT-MASTER KEY IS TRANS-ACCT
    IF FOUND (FILE STATUS = '00')
        IF TRANS-TYPE = 'D'
            ADD TRANS-AMOUNT TO ACCT-BALANCE
            CHECK FOR OVERFLOW
            REWRITE ACCT-RECORD
        ELSE IF TRANS-TYPE = 'W'
            IF ACCT-BALANCE >= TRANS-AMOUNT
                SUBTRACT TRANS-AMOUNT FROM ACCT-BALANCE
                REWRITE ACCT-RECORD
            ELSE
                WRITE ERROR "INSUFFICIENT FUNDS"
            END-IF
        END-IF
    ELSE IF FILE STATUS = '23'
        WRITE ERROR "ACCOUNT NOT FOUND"
    END-IF
END-PERFORM
```

**3. Termination**
- Display statistics (total processed, errors)
- Close all files

### Error Handling

**FILE STATUS Codes:**
- 00 - Successful operation
- 23 - Record not found (invalid account number)
- Other codes - I/O errors (logged to SYSOUT)

**Business Rule Validations:**
- Account existence check before processing
- Sufficient funds check for withdrawals
- Numeric overflow detection (balance > 999999.99)

## JCL Jobs

### 1. DEFKSDS.jcl - Define VSAM Cluster

Defines KSDS cluster for account master file.

**Key Parameters:**
- RECORDSIZE(32,32) - Fixed 32-byte records
- KEYS(5 0) - 5-byte key starting at position 0
- TRACKS(15) - Initial allocation
- INDEXED - KSDS organization

### 2. COMPRUN.jcl - Compile and Execute

Standard compile-link-go JCL using MYCOMPGO procedure.

## How to Run

### Step 1: Define VSAM Cluster

**Submit:** JCL/DEFKSDS.jcl
**Verify:** Check for MAXCC=0 in job output

### Step 2: Load Master Data into VSAM

**Option A : Use File Manager**
1. Navigate to VSAM file in ISPF 
2. Open with File Manager (FM)
3. Insert records manually from DATA/ACCT-MASTER-BEFORE

**Option B: Use REPRO with inline data**

**Important:** Inline DD * data is padded to 80 bytes. You must either:
- Define VSAM with RECORDSIZE(80,80) to match inline format, OR
- Create temporary PS file with exact record length (32 bytes), load data there first, then REPRO to VSAM

See JCL-SAMPLES folder for REPRO example JCL.

### Step 3: Prepare Transaction File

**Option A : Manual upload**

Upload DATA/TRANS-FILE-INPUT to PS dataset manually via ISPF 

**Option B: Create via JCL**

Allocate PS file and insert transaction data using IEBGENER or inline DD.

### Step 4: Execute Program

**Submit:** JCL/COMPRUN.jcl
**Check:** SYSOUT for statistics and FILE STATUS messages
**Review:** ERROR-REPORT-OUTPUT for failed transactions

### Step 5: Verify Results

Compare updated VSAM file content with expected results in DATA/ACCT-MASTER-AFTER

## Key Concepts Demonstrated

- VSAM Random Access - Direct read by key
- READ statement - With KEY IS clause
- REWRITE statement - Update existing record
- FILE STATUS checking - Validate every I/O operation
- Transaction validation - Business rules enforcement
- Error handling - Graceful failure with logging
- Numeric overflow - Detection and prevention

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

## Program Output (SYSOUT)

Expected execution log - see OUTPUT/SYSOUT.txt for full output example.

## Notes

- Program uses random access mode for VSAM (not sequential)
- Each transaction is independent - one failure doesn't affect others
- VSAM file remains open throughout processing for performance
- Error report uses variable-length records to accommodate long messages
- Tested on IBM z/OS with Enterprise COBOL
