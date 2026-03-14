# TASK05 - VSAM Banking Transaction Processor

Banking transaction processing system using VSAM KSDS random update operations.

## Business Scenario

A bank needs to process daily transactions (deposits and withdrawals) against customer account master file. The system must:
- Update account balances based on transaction type
- Validate account existence before processing
- Check for sufficient funds on withdrawals
- Generate error report for failed transactions

## Files

### Input Files

#### 1. ACCT.MASTER (VSAM KSDS) - Account Master File

**Access Mode:** INPUT-OUTPUT (Random)
**Organization:** INDEXED (KSDS)
**Key:** ACCT-NUM (5 characters, positions 1-5)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| ACCT-ID | X(5) | 5 | Account ID (Primary Key) |
| ACCT-NAME | X(20) | 20 | Account holder name |
| ACCT-BAL | 9(5)V99 | 7 | Current balance (implied decimal) |

**Sample Data:** [DATA/ACCT.MASTER.BEFORE](DATA/ACCT.MASTER.BEFORE)

#### 2. TRANS-FILE (PS) - Transaction File

- **Access Mode:** INPUT (Sequential)
- **Organization:** SEQUENTIAL
- **Record Format:** Fixed (RECFM=FB, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| TRANS-ACCT-ID | X(5) | 5 | Account ID to look up in VSAM |
| TRANS-TYPE | X(1) | 1 | Transaction type: D=Deposit, W=Withdrawal |
| TRANS-AMOUNT | 9(5)V99 | 7 | Transaction amount (implied decimal) |
| FILLER | X(67) | 67 | Unused |

**Sample Data:** [DATA/TRANS.FILE.INPUT](DATA/TRANS.FILE.INPUT)

### Output Files

#### 3. ERROR-REPORT (PS) - Error Report

- **Access Mode:** OUTPUT (Sequential)
- **Organization:** SEQUENTIAL
- **Record Format:** FIXED (RECFM=FB, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| REP-MSG-CONST | X(13) | 13 | Constant: 'TRANS ERROR: ' |
| REP-ID | X(5) | 5 | Account ID from failed transaction |
| FILLER | X(1) | 1 | Space |
| REP-DESC | X(61) | 61 | Error description (ACCOUNT NOT FOUND / INSUFFICIENT FUNDS) |

**Error Types:**
- ACCOUNT NOT FOUND - Account number doesn't exist in master file
- INSUFFICIENT FUNDS - Withdrawal amount exceeds current balance

**Expected Output:** [DATA/ERROR.REPORT.OUTPUT](DATA/ERROR.REPORT.OUTPUT)

#### 4. ACCT.MASTER (VSAM KSDS) - Updated Master File

Updated balances after successful transactions.

**Expected Final State:** [DATA/ACCT.MASTER.AFTER](DATA/ACCT.MASTER.AFTER)

### Error Handling

**FILE STATUS Codes:**
- 00 - Successful operation
- 23 - Record not found (invalid account number)
- Other codes - I/O errors (logged to SYSOUT)

### Transaction Processing Logic

| TRANS-TYPE | Condition | Action |
|---|---|---|
| D (Deposit) | Account found | ACCT-BAL = ACCT-BAL + TRANS-AMOUNT → REWRITE |
| W (Withdrawal) | ACCT-BAL >= TRANS-AMOUNT | ACCT-BAL = ACCT-BAL − TRANS-AMOUNT → REWRITE |
| W (Withdrawal) | ACCT-BAL < TRANS-AMOUNT | Write INSUFFICIENT FUNDS to ERROR-REPORT |
| Any | FILE STATUS '23' | Write ACCOUNT NOT FOUND to ERROR-REPORT |

**Examples:**
- ACCT 10001, D, 500.00 → balance +500.00 (REWRITE)
- ACCT 10002, W, 200.00, balance 1000.00 → balance 800.00 (REWRITE)
- ACCT 10003, W, 9999.00, balance 500.00 → INSUFFICIENT FUNDS (error)
- ACCT 99999, D, 100.00 → ACCOUNT NOT FOUND (error)

## Program Flow

1. **Initialization**
   - Opens ACCT.MASTER in I-O mode for random access, TRANS-FILE for sequential input, ERROR-REPORT for output
   - Validates FILE STATUS '00' on each OPEN

2. **Transaction Processing Loop**
   - Reads TRANS-FILE sequentially until EOF
   - For each record: performs random READ on VSAM using TRANS-ACCT-ID as key
   - FILE STATUS '00' → applies deposit or withdrawal logic, REWRITEs updated record
   - FILE STATUS '23' → writes ACCOUNT NOT FOUND to ERROR-REPORT, continues to next record
   - Withdrawal with insufficient funds → writes INSUFFICIENT FUNDS to ERROR-REPORT, skips REWRITE

3. **Termination**
   - Closes all three files; validates FILE STATUS after each CLOSE
   - STOP RUN

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
- EMPDD - ACCT.MASTER (VSAM KSDS)
- INDD - TRANS-FILE (PS)
- REPDD - ERROR-REPORT (PS)

**PROC reference:** [JCLPROC/MYCOMPGO.jcl](../../JCLPROC/MYCOMPGO.jcl)

## How to Run

### Step 1: Define VSAM Cluster

**Submit:** [JCL/DEFKSDS.jcl](JCL/DEFKSDS.jcl)  
**Verify:** Check for MAXCC=0 in job output

### Step 2: Load Master Data into VSAM

**Use File Manager**
1. Navigate to VSAM file in ISPF 
2. Open with File Manager (FM)
3. Insert records manually from [DATA/ACCT.MASTER.BEFORE](DATA/ACCT.MASTER.BEFORE)

**Alternative:**
1. Define VSAM with RECORDSIZE(80,80) to match inline format
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Create temporary PS file with exact record length (32 bytes), load data there first, then REPRO to VSAM.
- Example in [JCL SAMPLES/DATAVSAM.jcl](../../JCL%20SAMPLES/DATAVSAM.jcl) uses SORT utility (can also be done with ICETOOL, IEBGENER)

### Step 3: Prepare Transaction File

**Manual upload**

Upload [DATA/TRANS.FILE.INPUT](DATA/TRANS.FILE.INPUT) to PS dataset manually via ISPF with exact record length (32 bytes)

**Alternative:**
1. Create PS file with LRECL=80 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length of your file transaction data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 4: Execute Program

**Submit:** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**Check:** SYSOUT for FILE STATUS messages  
**Review:** [ERROR.REPORT.OUTPUT](DATA/ERROR.REPORT.OUTPUT) for failed transactions

**Alternative:**
If you prefer to compile and run separately, use these jobs:  
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)

### Step 5: Verify Results

Compare updated VSAM file content with expected results in [DATA/ACCT.MASTER.AFTER](DATA/ACCT.MASTER.AFTER)

## Common Issues

### Issue 1: FILE STATUS '23' for all transactions

**Cause:** Master file is empty or KEYS offset is wrong in DEFKSDS  
**Solution:** Verify data loaded correctly; confirm KEYS(5 0) — key starts at byte 0

### Issue 2: Abend S0C7 (Data Exception)

**Cause:** Non-numeric data in ACCT-BAL or TRANS-AMOUNT (spaces, wrong offset)  
**Solution:** Check input data — no spaces in numeric fields; verify record layout offsets match FD

### Issue 3: FILE STATUS '92' on OPEN

**Cause:** VSAM cluster not defined or previously damaged  
**Solution:** Re-run DEFKSDS.jcl; check IDCAMS DEFINE output for errors

## Notes

- VSAM file stays open throughout the entire job — do not open/close per transaction
- Each transaction is fully independent: one error does not stop processing of subsequent records
- Tested on IBM z/OS with Enterprise COBOL
