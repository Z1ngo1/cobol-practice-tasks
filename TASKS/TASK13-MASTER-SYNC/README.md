# TASK13 - Match-Merge Algorithm: Master File Update

Master file update system that applies daily transactions to an old master file to produce a new master file. Demonstrates the classic Match-Merge (Balance Line) algorithm using two pre-sorted sequential PS files processed in parallel.

## Business Scenario

Company runs daily batch updates to its customer master file. Three types of transactions are applied: adding new customers, updating balances, and deleting records. The system must:
- Read OLD.MASTER and TRANS.FILE simultaneously, both pre-sorted by customer ID
- Compare keys and route processing based on relative position of each ID
- Apply transactions: ADD new record, UPDATE existing balance, DELETE record from output
- Log all invalid transactions to ERROR.REPORT (add-duplicate, update/delete-nonexistent)
- Produce a clean NEW.MASTER with all changes applied
- Display processing summary to SYSOUT

## Match-Merge Logic

Both input files are read in parallel and exhausted using HIGH-VALUES as sentinel:

```
TRANS-ID > MASTER-ID  → no transaction for this master record
                         copy master to new master as-is
                         read next master record

TRANS-ID < MASTER-ID  → transaction has no matching master record
                         if 'A': write as new record (ADD)
                         if 'U' or 'D': log error (no master to update/delete)
                         read next transaction

TRANS-ID = MASTER-ID  → transaction matches master record
                         'U': ADD TRNS-AMOUNT TO WS-CUR-BAL (multiple U accumulate)
                         'D': set WS-DEL-FLAG = 'Y' (suppressed from output)
                         'A': log error (duplicate — master already exists)
                         read next transaction
```

**Loop termination:** Continue until BOTH WS-OLD-ID = HIGH-VALUES AND WS-TRNS-ID = HIGH-VALUES

## Files

### Input Files

#### 1. OLD.MASTER (PS) - Current Customer Master File

**Access Mode:** INPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  
**Sort Requirement:** Pre-sorted ascending by OLD-ID (primary key)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| OLD-ID | X(5) | 5 | Customer ID (sort key) |
| OLD-NAME | X(20) | 20 | Customer name |
| OLD-BAL | 9(5)V99 | 7 | Current balance (implied decimal) |
| FILLER | X(48) | 48 | Unused |

**Sample Data:** [DATA/OLD.MASTER](DATA/OLD.MASTER)

#### 2. TRANS.FILE (PS) - Daily Transactions File

**Access Mode:** INPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  
**Sort Requirement:** Pre-sorted ascending by TRNS-ID (same key as master)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| TRNS-ID | X(5) | 5 | Customer ID (sort key, matches OLD-ID) |
| TRNS-ACT | X(1) | 1 | Transaction type: A=Add, U=Update, D=Delete |
| TRNS-DATA | X(20) | 20 | Customer name (used for A transactions) |
| TRNS-AMOUNT | 9(5)V99 | 7 | Amount (balance for A; delta for U; ignored for D) |
| FILLER | X(47) | 47 | Unused |

**Sample Data:** [DATA/TRANS.FILE](DATA/TRANS.FILE)

### Output Files

#### 3. NEW.MASTER (PS) - Updated Customer Master File

**Access Mode:** OUTPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed Block (RECFM=FB, LRECL=80)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| NEW-ID | X(5) | 5 | Customer ID |
| NEW-NAME | X(20) | 20 | Customer name |
| NEW-BAL | 9(5)V99 | 7 | Updated balance (implied decimal) |
| FILLER | X(48) | 48 | Unused |

**Expected Output:** [DATA/NEW.MASTER](DATA/NEW.MASTER)

#### 4. ERROR.REPORT (PS) - Error Log File

**Access Mode:** OUTPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed Block (RECFM=FB, LRECL=80)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| REP-ID | X(5) | 5 | Customer ID from failed transaction |
| REP-ACT | X(1) | 1 | Transaction type: A=Add, U=Update, D=Delete |
| REP-NAME | X(20) | 20 | Customer name from transaction |
| REP-BAL | 9(5)V99 | 7 | Amount from transaction (implied decimal) |
| FILLER | X(47) | 47 | Unused |

**Expected Output:** [DATA/ERROR.REPORT](DATA/ERROR.REPORT)

### Error Handling

**Error Conditions (logged to ERROR.REPORT):**
| Condition | Trans Type | Action |
|---|---|---|
| ADD but customer already exists | A with TRANS-ID = MASTER-ID | Log to ERROR.REPORT |
| UPDATE but customer not found | U with TRANS-ID < MASTER-ID | Log to ERROR.REPORT |
| DELETE but customer not found | D with TRANS-ID < MASTER-ID | Log to ERROR.REPORT |

**FILE STATUS Codes (all four files):**
- 00 - Successful operation
- 35 - File not found on OPEN (program displays status and STOP RUN)
- Other codes - I/O errors on open, read, or write (program displays status and STOP RUN)

CLOSE errors treated as warnings (display only, no STOP RUN).

## Program Flow

1. **Initialization**
   - Opens all four files; validates FILE STATUS '00' on each
   - Primes the loop: reads first OLD.MASTER record into WS-OLD-ID and WS-CUR-REC; reads first TRANS.FILE record into WS-TRNS-ID

2. **Main Merge Loop**
   - Repeats until BOTH WS-OLD-ID = HIGH-VALUES AND WS-TRNS-ID = HIGH-VALUES
   - **TRNS-ID > OLD-ID:** writes current master to NEW.MASTER (if WS-DEL-FLAG = 'N'), resets flag, reads next master
   - **TRNS-ID < OLD-ID:** if 'A' → writes new record to NEW.MASTER; if 'U' or 'D' → logs error; reads next transaction
   - **TRNS-ID = OLD-ID:** applies transaction (U: ADD to WS-CUR-BAL; D: set WS-DEL-FLAG; A: log error as duplicate); reads next transaction
   - AT END of either file: MOVE HIGH-VALUES to that file's ID to drain the other file

3. **Termination**
   - Closes all four files (non-zero status on CLOSE is warning only)
   - Displays summary to SYSOUT: OLD-MASTER-READ / TRANS-READ / RECORDS-ADDED / RECORDS-UPDATED / RECORDS-DELETED / ERRORS-LOGGED
   - STOP RUN

## JCL Jobs

### [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure.

- OLDDD: OLD.MASTER (current master input)
- TRNSDD: TRANS.FILE (daily transactions input)
- NEWDD: NEW.MASTER (updated master output)
- REPDD: ERROR.REPORT (error log output)

**PROC reference:** [JCLPROC/MYCOMPGO.jcl](../../JCLPROC/MYCOMPGO.jcl)

## How to Run

### Step 1: Allocate and Load Old Master File

- Upload [DATA/OLD.MASTER](DATA/OLD.MASTER)

**Alternative:**
1. Create PS file with LRECL=80 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 2: Allocate and Load Transactions File

- Upload [DATA/TRANS.FILE](DATA/TRANS.FILE)

**Alternative:**
1. Create PS file with LRECL=80 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 3: Execute Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution summary  
**Review** [DATA/NEW.MASTER](DATA/NEW.MASTER) for expected new master output  
**Review** [DATA/ERROR.REPORT](DATA/ERROR.REPORT) for expected error log

**Alternative:**
If you prefer to compile and run separately, use these jobs:
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)

### Step 4: Verify Results

- **New master records:** 9 (7 old − 2 deleted + 4 added)
- **Errors logged:** 3 (00200-A duplicate, 00350-U not found, 00700-D not found)
- **Updated balances:** 00100 → 175.00 (100.00 + 50.00 + 25.00), 00400 → 400.00, 00600 → 200.00, 00800 → 60.00
- **Deleted records:** 00300 (BOB MARLEY), 00500 (ALICE COOPER) — not in new master

## Common Issues

### Issue 1: Output Files Not Allocated

**Cause:** IEFBR14 delete step failed for NEW.MASTER or ERROR.REPORT, or DSN mismatch
**Solution:** Verify delete step RC=0 for both output files; confirm NEWDD and REPDD DD statements present in COMPRUN.jcl

### Issue 2: Files Not Sorted

**Cause:** Input files not sorted — match-merge produces wrong results silently
**Solution:** Verify OLD.MASTER sorted ascending by bytes 1–5 (OLD-ID) and TRANS.FILE sorted ascending by bytes 1–5 (TRNS-ID); use DFSORT SORT FIELDS=(1,5,CH,A) on both input files if needed

### Issue 3: Abend S0C7 (Data Exception)

**Cause:** Non-numeric OLD-BAL or TRNS-AMOUNT in input files
**Solution:** Verify OLD-BAL is 7 numeric digits at offset 25; confirm TRNS-AMOUNT is 7 numeric digits at offset 26 (5+1+20=26)

## Program Output (SYSOUT)

Expected execution log — see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for full output

## Notes

- Both input files MUST be pre-sorted by the same key (customer ID) — no internal sorting
- HIGH-VALUES sentinel strategy: when either file reaches EOF, its ID is set to X'FFFFFF...' (highest possible value), ensuring the other file continues to drain correctly
- Multiple U transactions for the same customer ID all accumulate into WS-CUR-BAL before the master record is written
- Error records written to ERROR.REPORT preserve full transaction layout for audit trail
- Tested on IBM z/OS with Enterprise COBOL
