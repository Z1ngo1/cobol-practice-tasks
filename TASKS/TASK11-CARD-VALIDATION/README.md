# TASK11 - VSAM Credit Card Transaction Validation

Credit card transaction validation system that reads daily transactions from a PS file, validates each card against a VSAM KSDS master file, and splits results into approved and declined output files with decline reason codes.

## Business Scenario

Company processes daily credit card transactions and must validate each transaction before authorizing. The system must:
- Read daily transactions file (PS) with TRANS-ID, CARD-NUM, AMOUNT
- Look up each CARD-NUM in VSAM KSDS card master file (random access)
- Apply three validation checks in priority order:
  - **CHECK 1:** Card exists in master file (FILE STATUS '23' = NOT FOUND)
  - **CHECK 2:** Card status = 'A' (Active), not 'B' (Blocked)
  - **CHECK 3:** Card expiry date >= current system date (MMYY format)
- If all checks pass: write to APPROVED file
- If any check fails: write to DECLINED file with reason code (NOT FOUND / BLOCKED / EXPIRED)
- Generate detailed summary: total transactions, approved, declined by reason

## Files

### Input Files

#### 1. CARD.MASTER.VSAM (VSAM KSDS) - Card Master File

**Access Mode:** RANDOM (direct lookup by CARD-NUMBER key)  
**Organization:** INDEXED (KSDS)  
**KEY:** CARD-NUMBER (PIC 9(16))  
**Record Format:** Fixed, LRECL=41  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| CARD-NUMBER | 9(16) | 16 | Card number (Primary Key) |
| CARD-OWNER-NAME | X(20) | 20 | Cardholder full name |
| CARD-EXPIRY-DATE | X(4) | 4 | Expiry date in MMYY format |
| CARD-STATUS | X(1) | 1 | Card status: A=Active, B=Blocked |

**Sample Data:** [DATA/CARD.MASTER.VSAM](DATA/CARD.MASTER.VSAM)

#### 2. TRANS.DAILY (PS) - Daily Transactions File

**Access Mode:** INPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| TRANSACTION-ID | X(5) | 5 | Transaction ID |
| TRANSACTION-CARD-NUM | 9(16) | 16 | Card number to look up in VSAM |
| TRANSACTION-AMOUNT | 9(5)V99 | 7 | Transaction amount (implied decimal) |
| FILLER | X(52) | 52 | Unused |

**Sample Data:** [DATA/TRANS.DAILY](DATA/TRANS.DAILY)

### Output Files

#### 3. APPROVED.FILE (PS) - Approved Transactions

**Access Mode:** OUTPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| APPROVED-TRANS-ID | X(5) | 5 | Transaction ID |
| FILLER | X(1) | 1 | Space |
| APPROVED-CARD-NUM | 9(16) | 16 | Card number |
| FILLER | X(1) | 1 | Space |
| APPROVED-AMOUNT | $$$$9.99 | 8 | Formatted amount with $ sign |
| FILLER | X(49) | 49 | Unused |

**Expected Output:** [DATA/APPROVED.FILE](DATA/APPROVED.FILE)

#### 4. DECLINED.FILE (PS) - Declined Transactions

**Access Mode:** OUTPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| DECLINED-TRANS-ID | X(5) | 5 | Transaction ID |
| FILLER | X(1) | 1 | Space |
| DECLINED-CARD-NUM | 9(16) | 16 | Card number |
| FILLER | X(1) | 1 | Space |
| DECLINED-AMOUNT | $$$$9.99 | 8 | Formatted amount with $ sign |
| FILLER | X(1) | 1 | Space |
| DECLINE-REASON | X(10) | 10 | Reason: NOT FOUND / BLOCKED / EXPIRED |
| FILLER | X(38) | 38 | Unused |

**Expected Output:** [DATA/DECLINED.FILE](DATA/DECLINED.FILE)

### Error Handling

**FILE STATUS Codes (VSAM - CARD-MASTER-FILE):**
- 00 - Card found successfully
- 23 - Card not found (declined as NOT FOUND, processing continues)
- 92/93 - VSAM not defined or damaged (program stops)
- Other - Critical VSAM error (program displays CARD-NUM and TRANS-ID, stops)

**FILE STATUS Codes (TRANS, APPROVED, DECLINED Files):**
- 00 - Successful operation
- Other codes - I/O errors (program displays error and stops)

### Validation Logic

**Validation Priority (first failing check wins):**

| Priority | Check | Condition | Reason Code |
|---|---|---|---|
| 1 | Card exists | VSAM FILE STATUS = '23' | NOT FOUND |
| 2 | Card active | CARD-STATUS = 'B' | BLOCKED |
| 3 | Not expired | CARD-EXPIRY-DATE < current date | EXPIRED |
| — | All passed | — | APPROVED |

**Expiry Comparison (MMYY format):**
- WS-CARD-YY < WS-CUR-YY → EXPIRED
- WS-CARD-YY = WS-CUR-YY AND WS-CARD-MM < WS-CUR-MM → EXPIRED
- Otherwise → not expired

**Current Date:** obtained via `ACCEPT WS-CUR-DATE-GROUP FROM DATE YYYYMMDD`; YY = positions 3:2 of year, MM = positions 5:2

**Examples (run date: 2026/02/23):**
- **TRANS 00001**, card 1111222233334444, expiry 1227, status A → APPROVED
- **TRANS 00002**, card 2222333344445555, expiry 0325, status A → EXPIRED (2025 < 2026)
- **TRANS 00004**, card 4444555566667777, expiry 0126, status B → BLOCKED
- **TRANS 00006**, card 9999888877776666, not in VSAM → NOT FOUND
- **TRANS 00008**, card 8888999900001111, expiry 1026, status A → APPROVED (month 10 >= 02)

## Program Flow

1. **Initialization**
   - ACCEPT WS-CUR-DATE-GROUP FROM DATE YYYYMMDD; extracts WS-CUR-YY and WS-CUR-MM
   - Displays current date, compare year, compare month to SYSOUT
   - Opens all four files (VSAM RANDOM, TRANS INPUT, APPROVED OUTPUT, DECLINED OUTPUT); validates FILE STATUS '00' on each

2. **Main Processing Loop**
   - Reads DAILY-TRANS-FILE sequentially until EOF; increments TOTAL-TRANSACTIONS per record
   - For each transaction: MOVEs TRANSACTION-CARD-NUM to CARD-NUMBER, executes random READ on VSAM
   - FILE STATUS '23' → writes to DECLINED with reason NOT FOUND
   - FILE STATUS '00' → checks CARD-STATUS: 'B' → BLOCKED; else checks expiry: expired → EXPIRED; else → APPROVED
   - Other VSAM status → displays TRANS-ID and CARD-NUM, STOP RUN

3. **Termination**
   - Closes all four files with status validation
   - Displays summary to SYSOUT: TOTAL / APPROVED / DECLINED / NOT FOUND / BLOCKED / EXPIRED
   - STOP RUN

## JCL Jobs

### 1. [DEFKSDS.jcl](JCL/DEFKSDS.jcl) - Define VSAM KSDS Cluster

Defines KSDS cluster for card master file.

**Key Parameters:**
- RECORDSIZE(41,41) - Fixed 41-byte records
- KEYS(16,0) - 16-byte key starting at position 0
- INDEXED - KSDS organization

### 2. [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure.

- VSAMDD: CARD.MASTER.VSAM (VSAM KSDS)
- TRNSDD: TRANS.DAILY (daily transactions input)
- APRVDD: APPROVED.FILE (approved output)
- DECLDD: DECLINED.FILE (declined output)

**PROC reference:** [JCLPROC/MYCOMPGO.jcl](../../JCLPROC/MYCOMPGO.jcl)

## How to Run

### Step 1: Define VSAM Cluster

**Submit** [JCL/DEFKSDS.jcl](JCL/DEFKSDS.jcl)
**Verify:** IDCAMS completes RC=0, cluster defined

### Step 2: Load Card Master Data

**Use File Manager**
1. Navigate to VSAM file in ISPF 
2. Open with File Manager (FM)
3. Insert records manually from [DATA/CARD.MASTER.VSAM](DATA/CARD.MASTER.VSAM) 

**Alternative:**  
1. Define VSAM with RECORDSIZE(80,80) to match inline format
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Create temporary PS file with exact record length (41 bytes), load data there first, then REPRO to VSAM.
- Example in [JCL SAMPLES/DATAVSAM.jcl](../../JCL%20SAMPLES/DATAVSAM.jcl) uses SORT utility (can also be done with ICETOOL, IEBGENER)

### Step 3: Allocate Daily Transactions File

- Upload [DATA/TRANS.DAILY](DATA/TRANS.DAILY) to PS dataset manually via ISPF

### Step 4: Execute Validation Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution summary and date display  
**Review** [DATA/APPROVED.FILE](DATA/APPROVED.FILE) — should contain 2 approved transactions  
**Review** [DATA/DECLINED.FILE](DATA/DECLINED.FILE) — should contain 7 declined transactions

**Alternative:**
If you prefer to compile and run separately, use these jobs:
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)

### Step 5: Verify Results

- **Approved count:** 2 records (TRANS 00001 and 00008)
- **Declined count:** 7 records (3 EXPIRED, 2 BLOCKED, 2 NOT FOUND)
- **Verify decline reasons:** check DECLINED-FILE columns 33–42 for reason codes
- **Confirm summary:** TOTAL=9, APPROVED=2, DECLINED=7, NOT FOUND=2, BLOCKED=2, EXPIRED=3

## Common Issues

### Issue 1: FILE STATUS '92' or '93' on VSAM Open

**Cause:** VSAM KSDS not defined or cluster definition missing  
**Solution:** Re-run JCL/DEFKSDS.jcl, verify IDCAMS RC=0

### Issue 2: FILE STATUS '23' for All Transactions

**Cause:** VSAM card master file is empty or wrong key field offset  
**Solution:** Verify data loaded via REPRO, check KEYS(16,0) matches CARD-NUMBER PIC 9(16) at offset 0
Confirm LISTCAT shows non-zero REC-TOTAL for the cluster

### Issue 3: All Active Cards Showing as EXPIRED

**Cause:** System date comparison logic error — YY extraction from YYYYMMDD incorrect  
**Solution:** Verify MOVE WS-CUR-YYYY(3:2) TO WS-CUR-YY extracts positions 3 and 4 of the 4-digit year
Check SYSOUT displays correct COMPARE YEAR and COMPARE MONTH values at startup

### Issue 4: Abend S0C7 (Data Exception)

**Cause:** Non-numeric data in TRANSACTION-AMOUNT, TRANSACTION-CARD-NUM, or CARD-EXPIRY-DATE fields  
**Solution:** Verify TRANS-DAILY-INPUT TRANSACTION-AMOUNT is 7 digits at offset 21 (no spaces)
Check CARD-MASTER-VSAM-INPUT expiry field is exactly 4 numeric digits in MMYY format

### Issue 5: Output or Input Files Not Allocated

**Cause:** APPROVED.FILE, DECLINED.FILE not deleted from prior run, or TRANS.DAILY not cataloged  
**Solution:** Verify IEFBR14 delete step RC=0 for both output files; confirm APRVDD, DECLDD, TRNSDD DD statements present in COMPRUN.jcl; check TRANS-STATUS after OPEN (should be '00', not '35')

## Program Output (SYSOUT)

Expected execution log — see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for full output

## Notes

- Current system date obtained via ACCEPT FROM DATE YYYYMMDD — no hardcoded dates
- Expiry date stored in MMYY format (e.g. 1227 = December 2027) — positions 1:2 = MM, 3:2 = YY
- Validation priority: NOT FOUND → BLOCKED → EXPIRED → APPROVED (first failing check wins)
- FILE STATUS '23' is a soft error — card not found is a valid business outcome, not a fatal error
- Tested on IBM z/OS with Enterprise COBOL
