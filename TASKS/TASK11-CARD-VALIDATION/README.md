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

**Access Mode:**
- RANDOM (direct lookup by CARD-NUMBER key)

**Organization:**
- INDEXED (KSDS)

**KEY:** 
- CARD-NUMBER (PIC 9(16))

**Record Format:**
- Fixed, LRECL=41

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| CARD-NUMBER | 9(16) | 16 | Card number (Primary Key) |
| CARD-OWNER-NAME | X(20) | 20 | Cardholder full name |
| CARD-EXPIRY-DATE | X(4) | 4 | Expiry date in MMYY format |
| CARD-STATUS | X(1) | 1 | Card status: A=Active, B=Blocked |

**Sample Data:** [DATA/CARD.MASTER.VSAM](DATA/CARD.MASTER.VSAM)

#### 2. TRANS.DAILY (PS) - Daily Transactions File

**Access Mode:**
- INPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

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

**Access Mode:**
- OUTPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

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

**Access Mode:**
- OUTPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

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

### Validation Logic

**Validation Priority (checks applied in order):**

1. **NOT FOUND** — VSAM FILE STATUS = '23'
   - Card number does not exist in CARD.MASTER.VSAM
   - Action: write to DECLINED with reason 'NOT FOUND', increment TOTAL-NOT-FOUND

2. **BLOCKED** — CARD-STATUS = 'B'
   - Card exists but is blocked by issuer
   - Action: write to DECLINED with reason 'BLOCKED', increment TOTAL-BLOCKED

3. **EXPIRED** — CARD-EXPIRY-DATE < current system date
   - Expiry check: CARD-EXPIRY-DATE format is MMYY (e.g. 1227 = December 2027)
   - Compare WS-CARD-YY < WS-CUR-YY → EXPIRED
   - Compare WS-CARD-YY = WS-CUR-YY AND WS-CARD-MM < WS-CUR-MM → EXPIRED
   - Action: write to DECLINED with reason 'EXPIRED', increment TOTAL-EXPIRED

4. **APPROVED** — All checks passed
   - Card found, status = 'A', expiry date >= current date
   - Action: write to APPROVED, increment TOTAL-APPROVED

**Current Date Handling:**
- System date obtained via ACCEPT WS-CUR-DATE-GROUP FROM DATE YYYYMMDD
- Only YY (last 2 digits of year) and MM used for comparison
- SYSOUT displays current date, compare year, and compare month at startup

**Examples (run date: 2026/02/23):**
- TRANS 00001, card 1111222233334444, expiry 1227 (Dec 2027), status A → APPROVED
- TRANS 00002, card 2222333344445555, expiry 0325 (Mar 2025), status A → EXPIRED (2025 < 2026)
- TRANS 00004, card 4444555566667777, expiry 0126, status B → BLOCKED
- TRANS 00006, card 9999888877776666, not in VSAM → NOT FOUND
- TRANS 00008, card 8888999900001111, expiry 1026 (Oct 2026), status A → APPROVED (month 10 >= 02)

### Error Handling

**FILE STATUS Codes (VSAM - CARD-MASTER-FILE):**
- 00 - Card found successfully
- 23 - Card not found (declined as NOT FOUND, processing continues)
- 92/93 - VSAM not defined or damaged (program stops)
- Other - Critical VSAM error (program displays CARD-NUM and TRANS-ID, stops)

**FILE STATUS Codes (TRANS, APPROVED, DECLINED Files):**
- 00 - Successful operation
- Other codes - I/O errors (program displays error and stops)

## Program Flow

1. **Initialization: INIT-PROCESS**
   - ACCEPT WS-CUR-DATE-GROUP FROM DATE YYYYMMDD
   - Extract WS-CUR-YY (positions 3:2 of year) and WS-CUR-MM
   - Display current date, compare year, compare month to SYSOUT

2. **Open Files: OPEN-ALL-FILES**
   - Opens CARD-MASTER-FILE (VSAM KSDS, INPUT, RANDOM), validates VSAM-STATUS = '00'
   - Opens DAILY-TRANS-FILE (PS, INPUT), validates TRANS-STATUS = '00'
   - Opens APPROVED-TRANS-FILE (PS, OUTPUT), validates APRV-STATUS = '00'
   - Opens DECLINED-TRANS-FILE (PS, OUTPUT), validates DECL-STATUS = '00'

3. **Main Loop: PROCESS-TRANS**
   - Loop PERFORM UNTIL EOF:
     - MOVE SPACES to APPROVED-REC and DECLINED-REC (clear output buffers)
     - READ DAILY-TRANS-FILE AT END SET EOF TO TRUE
     - If TRANS-STATUS = '00': increment TOTAL-TRANSACTIONS, perform PROCESS-TRANSACTION
     - If TRANS-STATUS ≠ '00': display error, STOP RUN

4. **Single Transaction: PROCESS-TRANSACTION**
   - MOVE TRANSACTION-CARD-NUM TO CARD-NUMBER (set VSAM key)
   - READ CARD-MASTER-FILE (random lookup)
   - If VSAM-STATUS = '23': move 'NOT FOUND' to WS-REASON, perform WRITE-DECLINED-TRANS
   - If VSAM-STATUS = '00': perform VALIDATE-STATUS
   - If other VSAM-STATUS: display critical error with TRANS-ID and CARD-NUM, STOP RUN

5. **Status Validation: VALIDATE-STATUS**
   - If CARD-STATUS = 'B': move 'BLOCKED' to WS-REASON, perform WRITE-DECLINED-TRANS
   - Else: perform VALIDATE-EXPIRY

6. **Expiry Validation: VALIDATE-EXPIRY**
   - Extract WS-CARD-MM (positions 1:2) and WS-CARD-YY (positions 3:2) from CARD-EXPIRY-DATE
   - If WS-CARD-YY < WS-CUR-YY: move 'EXPIRED' to WS-REASON, perform WRITE-DECLINED-TRANS
   - Else if WS-CARD-YY = WS-CUR-YY AND WS-CARD-MM < WS-CUR-MM: EXPIRED
   - Else: perform WRITE-APPROVED-TRANS

7. **Write Approved: WRITE-APPROVED-TRANS**
   - Move TRANS-ID, CARD-NUM, AMOUNT to APPROVED-REC fields
   - WRITE APPROVED-REC, validate APRV-STATUS = '00'
   - Increment TOTAL-APPROVED

8. **Write Declined: WRITE-DECLINED-TRANS**
   - Move TRANS-ID, CARD-NUM, AMOUNT, WS-REASON to DECLINED-REC fields
   - WRITE DECLINED-REC, validate DECL-STATUS = '00'
   - Increment TOTAL-DECLINED
   - Increment specific counter: TOTAL-NOT-FOUND, TOTAL-BLOCKED, or TOTAL-EXPIRED based on WS-REASON

9. **Termination: CLOSE-ALL-FILES + DISPLAY-SUMMARY**
   - Close all four files with status validation
   - Display summary banner to SYSOUT with all counters

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

- Upload [DATA/TRANS.DAILY](DATA/TRANS.DAILY)

**Alternative:**
1. Create PS file with LRECL=80 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length of your file transaction data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 4: Execute Validation Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution summary and date display  
**Review** [DATA/APPROVED.FILE](DATA/APPROVED.FILE) — should contain 2 approved transactions  
**Review** [DATA/DECLINED.FILE](DATA/DECLINED.FILE) — should contain 7 declined transactions  

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

### Issue 5: Output Files Not Allocated (APRVDD or DECLDD Missing)

**Cause:** REPDD1/REPDD2 delete step failed or output DSN conflict from previous run  
**Solution:** Verify STEP005 (IEFBR14) completed RC=0 for both APPROVED.FILE and DECLINED.FILE
Check both APRVDD and DECLDD DD statements present in STEP010

### Issue 6: Transactions File Not Found (TRNSDD)

**Cause:** Z73460.TASK11.TRANS.DAILY not cataloged or wrong DSN  
**Solution:** Verify PS dataset exists: LISTCAT ENTRY(Z73460.TASK11.TRANS.DAILY)
Check TRANS-STATUS after OPEN — should be '00', not '35' (file not found)

### Issue 7: Wrong Decline Counts in Summary

**Cause:** Specific reason counters (TOTAL-NOT-FOUND, TOTAL-BLOCKED, TOTAL-EXPIRED) not incremented  
**Solution:** Verify WRITE-DECLINED-TRANS paragraph increments the correct sub-counter based on WS-REASON value after TOTAL-DECLINED increment
Expected: NOT FOUND=2, BLOCKED=2, EXPIRED=3

## Program Output (SYSOUT)

Expected execution log — see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for full output

## Notes

- Program uses VSAM KSDS in RANDOM access mode — each transaction triggers a direct READ by key
- Four independent FILE STATUS variables track VSAM, transactions, approved, and declined files
- Current system date obtained via ACCEPT FROM DATE YYYYMMDD — no hardcoded dates
- Expiry date stored in MMYY format (e.g. 1227 = December 2027) — positions 1:2 = MM, 3:2 = YY
- Only last 2 digits of year (YY) used for expiry comparison — sufficient for near-future cards
- Validation priority: NOT FOUND → BLOCKED → EXPIRED → APPROVED (first failing check wins)
- FILE STATUS '23' is a soft error — card not found is a valid business outcome, not a fatal error
- Both output files (APPROVED and DECLINED) deleted and recreated fresh each run via IEFBR14
- TRANSACTION-AMOUNT uses PIC 9(5)V99 (implied decimal) — output formatted with $$$$ edit picture
- Tested on IBM z/OS with Enterprise COBOL
