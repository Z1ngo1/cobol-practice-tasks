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

## Match-Merge Key Comparison Logic

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
                         apply U/D/A logic
                         read next transaction
```

**Loop termination:** Continue until BOTH WS-OLD-ID = HIGH-VALUES AND WS-TRNS-ID = HIGH-VALUES

## Files

### Input Files

#### 1. OLD.MASTER (PS) - Current Customer Master File

**Access Mode:**
- INPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

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

**Access Mode:**
- INPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

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

**Access Mode:**
- OUTPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed Block (RECFM=FB, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| NEW-ID | X(5) | 5 | Customer ID |
| NEW-NAME | X(20) | 20 | Customer name |
| NEW-BAL | 9(5)V99 | 7 | Updated balance (implied decimal) |
| FILLER | X(48) | 48 | Unused |

**Expected Output:** [DATA/NEW.MASTER](DATA/NEW.MASTER)

#### 4. ERROR.REPORT (PS) - Error Log File

**Access Mode:**
- OUTPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed Block (RECFM=FB, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| REP-ID | X(5) | 5 | Customer ID from failed transaction |
| REP-ACT | X(1) | 1 | Transaction type: A=Add, U=Update, D=Delete |
| REP-NAME | X(20) | 20 | Customer name from transaction |
| REP-BAL | 9(5)V99 | 7 | Amount from transaction (implied decimal) |
| FILLER | X(47) | 47 | Unused |

**Expected Output:** [DATA/ERROR.REPORT](DATA/ERROR.REPORT)

### Transaction Type Logic

**'A' (ADD) — TRANS-ID < MASTER-ID (no matching master):**
- Write new customer record to NEW.MASTER directly from transaction
- NEW-ID = TRNS-ID, NEW-NAME = TRNS-DATA, NEW-BAL = TRNS-AMOUNT
- Increment RECORDS-ADDED
- If TRANS-ID = MASTER-ID and master exists → ADD is an error (duplicate), log to ERROR.REPORT

**'U' (UPDATE) — TRANS-ID = MASTER-ID (match found):**
- Add TRNS-AMOUNT to WS-CUR-BAL (accumulate in working storage)
- Multiple UPDATE transactions for same ID are all applied before writing
- Increment RECORDS-UPDATED per transaction
- If no matching master (TRANS-ID < MASTER-ID) → error, log to ERROR.REPORT

**'D' (DELETE) — TRANS-ID = MASTER-ID (match found):**
- Set WS-DEL-FLAG = 'Y'
- Record is suppressed in output (not written to NEW.MASTER)
- Increment RECORDS-DELETED
- If no matching master (TRANS-ID < MASTER-ID) → error, log to ERROR.REPORT

### Error Conditions

| Condition | Trans Type | Action |
|---|---|---|
| ADD but customer already exists | A with TRANS-ID = MASTER-ID | Log to ERROR.REPORT |
| UPDATE but customer not found | U with TRANS-ID < MASTER-ID | Log to ERROR.REPORT |
| DELETE but customer not found | D with TRANS-ID < MASTER-ID | Log to ERROR.REPORT |

### File Status Variables

**Four independent FILE STATUS variables:**
- OLD-MASTER-STATUS — tracks OLD-MASTER-FILE reads
- TRANS-STATUS — tracks TRANSACTIONS-FILE reads
- NEW-MASTER-STATUS — tracks NEW-MASTER-FILE writes
- ERROR-REPORT-STATUS — tracks ERROR-REPORT-FILE writes

CLOSE errors are treated as warnings (display only, no STOP RUN).

## Program Flow

1. **Open Files: OPEN-ALL-FILES**
   - Opens all four files, validates each FILE STATUS = '00'

2. **Prime the Loop:**
   - PERFORM READ-OLD-MASTER (loads first old master record into WS-OLD-ID and WS-CUR-REC)
   - PERFORM READ-TRANSACTION (loads first transaction into WS-TRNS-ID)

3. **Main Loop: PROCESS-MERGE-LOGIC**
   - PERFORM UNTIL WS-OLD-ID = HIGH-VALUES AND WS-TRNS-ID = HIGH-VALUES
   - Each iteration calls PROCESS-MERGE-LOGIC once

4. **Merge Decision: PROCESS-MERGE-LOGIC**
   - EVALUATE TRUE:
     - **TRNS-ID > OLD-ID:** perform WRITE-NEW-MASTER-REC, then READ-OLD-MASTER
     - **TRNS-ID < OLD-ID:** perform PROCESS-UNMATCHED, then READ-TRANSACTION
     - **TRNS-ID = OLD-ID:** perform APPLY-TRANSACTION, then READ-TRANSACTION

5. **Write Master: WRITE-NEW-MASTER-REC**
   - Only writes if WS-DEL-FLAG = 'N' AND WS-OLD-ID ≠ HIGH-VALUES
   - Copies WS-CUR-ID, WS-CUR-NAME, WS-CUR-BAL to NEW-MASTER-REC
   - Resets WS-DEL-FLAG = 'N' after every call

6. **Unmatched Transaction: PROCESS-UNMATCHED**
   - Guard: only processes if WS-TRNS-ID ≠ HIGH-VALUES
   - If TRNS-ACT = 'A': write new record to NEW.MASTER, increment RECORDS-ADDED
   - If TRNS-ACT = 'U' or 'D': perform LOG-ERROR-TRANSACTION

7. **Apply Transaction: APPLY-TRANSACTION**
   - 'U': ADD TRNS-AMOUNT TO WS-CUR-BAL, increment RECORDS-UPDATED
   - 'D': MOVE 'Y' TO WS-DEL-FLAG, increment RECORDS-DELETED
   - 'A': perform LOG-ERROR-TRANSACTION (duplicate add = error)

8. **Read Old Master: READ-OLD-MASTER**
   - AT END: MOVE HIGH-VALUES TO WS-OLD-ID (sentinel)
   - NOT AT END: load OLD-ID into WS-OLD-ID, copy record to WS-CUR-REC, increment OLD-MASTER-READ

9. **Read Transaction: READ-TRANSACTION**
   - AT END: MOVE HIGH-VALUES TO WS-TRNS-ID (sentinel)
   - NOT AT END: load TRNS-ID into WS-TRNS-ID, increment TRANS-READ

10. **Close + Summary: CLOSE-ALL-FILES + DISPLAY-SUMMARY**
    - Close all four files (warnings only on error)
    - Display full summary to SYSOUT

## JCL Jobs

### [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure.

- OLDDD: OLD.MASTER (current master input)
- TRNSDD: TRANS.FILE (daily transactions input)
- NEWDD: NEW.MASTER (updated master output)
- REPDD: ERROR.REPORT (error log output)

## How to Run

### Step 1: Allocate and Load Old Master File

- Upload [DATA/OLD.MASTER](DATA/OLD.MASTER)

### Step 2: Allocate and Load Transactions File

- Upload [DATA/TRANS.FILE](DATA/TRANS.FILE)

### Step 3: Execute Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution summary  
**Review** [DATA/NEW.MASTER](DATA/NEW.MASTER) for expected new master output  
**Review** [DATA/ERROR.REPORT](DATA/ERROR.REPORT) for expected error log

### Step 4: Verify Results

- **New master records:** 9 (7 old − 2 deleted + 4 added)
- **Errors logged:** 3 (00200-A duplicate, 00350-U not found, 00700-D not found)
- **Updated balances:** 00100 → 175.00 (100.00 + 50.00 + 25.00), 00400 → 400.00, 00600 → 200.00, 00800 → 60.00
- **Deleted records:** 00300 (BOB MARLEY), 00500 (ALICE COOPER) — not in new master

## Common Issues

### Issue 1: Infinite Loop

**Cause:** HIGH-VALUES sentinel not set on AT END, or loop condition checks only one file
**Solution:** Verify READ-OLD-MASTER and READ-TRANSACTION both MOVE HIGH-VALUES to their respective WS-xxx-ID on AT END
Confirm loop condition: UNTIL WS-OLD-ID = HIGH-VALUES AND WS-TRNS-ID = HIGH-VALUES (both must be exhausted)

### Issue 2: Records Duplicated or Missing in New Master

**Cause:** WRITE-NEW-MASTER-REC called at wrong point in merge — writing before all transactions for that ID are applied
**Solution:** For TRNS-ID = MASTER-ID: only READ-TRANSACTION after APPLY-TRANSACTION, do NOT yet call WRITE-NEW-MASTER-REC
Master is only written when TRNS-ID > MASTER-ID (next master record is needed)

### Issue 3: WS-DEL-FLAG Not Reset

**Cause:** Delete flag remains 'Y' after deleted record, suppresses next valid record
**Solution:** Verify MOVE 'N' TO WS-DEL-FLAG is at the END of WRITE-NEW-MASTER-REC (executes whether record was written or skipped)

### Issue 4: Multiple Updates Not Accumulated

**Cause:** WS-CUR-BAL overwritten instead of incremented between multiple U transactions for same ID
**Solution:** Verify APPLY-TRANSACTION uses ADD TRNS-AMOUNT TO WS-CUR-BAL (not MOVE)
When TRANS-ID = MASTER-ID and multiple updates exist: READ-TRANSACTION loops back, same master record in WS-CUR-REC accumulates each delta

### Issue 5: Error Count Wrong

**Cause:** LOG-ERROR-TRANSACTION called for wrong conditions or missed
**Solution:** Verify three error cases: (1) A when TRNS-ID = MASTER-ID, (2) U when TRNS-ID < MASTER-ID, (3) D when TRNS-ID < MASTER-ID
Expected for test data: ERRORS-LOGGED = 3

### Issue 6: Files Not Sorted

**Cause:** Input files not sorted — match-merge produces wrong results silently
**Solution:** Verify OLD.MASTER sorted ascending by bytes 1–5 (OLD-ID) and TRANS.FILE sorted ascending by bytes 1–5 (TRNS-ID)
Use DFSORT SORT FIELDS=(1,5,CH,A) on both input files before running PSTASK13

### Issue 7: Abend S0C7 (Data Exception)

**Cause:** Non-numeric OLD-BAL or TRNS-AMOUNT in input files
**Solution:** Verify OLD-MASTER-INPUT OLD-BAL is 7 numeric digits at offset 25
Check TRANS-FILE-INPUT TRNS-AMOUNT is 7 numeric digits at offset 26 (after 5-char ID + 1-char ACT + 20-char DATA)

## Program Output (SYSOUT)

Expected execution log — see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for full output

## Notes

- Both input files MUST be pre-sorted by the same key (customer ID) — no internal sorting
- HIGH-VALUES sentinel strategy: when either file reaches EOF, its ID is set to X'FFFFFF...' (highest possible value), ensuring the other file continues to drain correctly
- WS-CUR-REC working storage record accumulates all UPDATE deltas before being written — supports multiple U transactions per customer ID
- WS-DEL-FLAG controls write suppression — reset unconditionally after each WRITE-NEW-MASTER-REC call
- Error records written to ERROR.REPORT preserve full transaction layout for audit trail
- Four independent FILE STATUS variables for precise error identification
- No VSAM, no DB2 — pure sequential PS-to-PS batch processing (classic mainframe batch pattern)
- Tested on IBM z/OS with Enterprise COBOL
