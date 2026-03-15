# TASK15 - Tiered Commission Calculator: In-Memory Table Lookup

Weekly sales commission calculator that loads commission tier brackets into an in-memory table, then assigns each employee to their correct tier using a threshold-based scan. Demonstrates the tiered lookup pattern (OCCURS + INDEXED BY) where the matching criterion is the first tier whose limit is greater than or equal to the employee's sales amount.

## Business Scenario

Company pays weekly sales commissions on a tiered bracket system — lower sales amounts earn lower percentage commissions, and higher sales amounts earn higher percentages. Tiers are loaded once from a reference file into memory before processing begins. The system must:
- Load COMM.TIERS file into an in-memory table (max 20 tiers) sorted ascending by LIMIT
- For each employee: scan the tier table and apply the first tier where LIMIT >= SALARY
- If no tier matches (salary exceeds all tier limits): log error, skip record (no commission paid)
- Write payroll output: employee ID, sales amount, commission percentage, calculated commission
- Display processing summary to SYSOUT

## Difference vs. TASK14 (Exact Match vs. Tiered Threshold)

| Algorithm | TASK14 Table Lookup | TASK15 Tiered Lookup |
|---|---|---|
| Match condition | `WS-REGION = EMP-REGION` (exact) | `WS-LIMIT >= SAL-AMT` (first threshold) |
| Table type | Code → Rate mapping | Bracket → Rate mapping |
| No match result | Default rate (20%) | Error logged, no output record |
| Table sorted? | Not required | MUST be sorted ascending by LIMIT |

## Files

### Input Files

#### 1. COMM.TIERS (PS) - Commission Tier Reference File

**Access Mode:**
- INPUT (Sequential, loaded once at startup)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

**Sort Requirement:** MUST be sorted ascending by COMM-LIMIT — scan finds the FIRST tier where LIMIT >= salary

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| COMM-LIMIT | 9(6) | 6 | Upper salary boundary for this tier |
| COMM-PCT | V999 | 3 | Commission rate as implied decimal |

**Sample Data:** [DATA/COMM-TIERS-INPUT](DATA/COMM-TIERS-INPUT)

**Tier Table (loaded into memory):**
```
Tier 1: LIMIT=001000, PCT=0.020 (2.0%)  → salary ≤   $10.00
Tier 2: LIMIT=005000, PCT=0.050 (5.0%)  → salary ≤   $50.00
Tier 3: LIMIT=010000, PCT=0.100 (10.0%) → salary ≤  $100.00
Tier 4: LIMIT=050000, PCT=0.125 (12.5%) → salary ≤  $500.00
Tier 5: LIMIT=999999, PCT=0.150 (15.0%) → salary ≤ $9999.99
```

**Max entries:** 20 (TIER-ENTRY OCCURS 20 TIMES)

#### 2. SALES.WEEKLY (PS) - Weekly Employee Sales File

**Access Mode:**
- INPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| SAL-ID | 9(5) | 5 | Employee ID |
| SAL-AMT | 9(6)V99 | 8 | Weekly sales amount (implied decimal) |

**Sample Data:** [DATA/SALES-WEEKLY-INPUT](DATA/SALES-WEEKLY-INPUT)

### Output Files

#### 3. PAYOUT.RPT (PS) - Commission Payout Report

**Access Mode:**
- OUTPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed Block (RECFM=FB, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| OUT-ID | 9(5) | 5 | Employee ID |
| FILLER | X(1) | 1 | Space separator |
| OUT-SAL-AMT | 9(6)V99 | 8 | Sales amount (from input) |
| FILLER | X(1) | 1 | Space separator |
| OUT-PCT | V999 | 3 | Commission percentage applied |
| FILLER | X(1) | 1 | Space separator |
| OUT-RES | 9(6)V99 | 8 | Calculated commission amount |
| FILLER | X(53) | 53 | Unused |

**Expected Output:** [DATA/PAYOUT-REPORT-EXPECTED](DATA/PAYOUT-REPORT-EXPECTED)

### In-Memory Table Structure

```cobol
01 TIER-TABLE.
   05 TIER-ENTRY OCCURS 20 TIMES INDEXED BY IDX.
      10 WS-LIMIT  PIC 9(6).
      10 WS-PCT    PIC V999.
```

- Loaded in LOAD-COMMISSION-TABLE: ADD 1 TO TIERS-LOADED, SET IDX TO TIERS-LOADED, MOVE COMM-LIMIT/COMM-PCT to table entries
- Searched in CALCULATE-COMMISSION: PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TIERS-LOADED OR WS-FOUND = 'Y'
- Match condition: `IF WS-LIMIT(IDX) >= SAL-AMT` (first-fit threshold, not exact match)

### Tier Matching Logic

```
For each employee salary:
  Scan TIER-TABLE from IDX=1 to TIERS-LOADED:
    IF WS-LIMIT(IDX) >= SAL-AMT:
      → MATCH: apply WS-PCT(IDX), write output, stop scan
  If scan exhausted without match (salary > all limits):
    → NO MATCH: ADD 1 to NO-TIER-MATCH, skip (no output record written)
```

**Critical requirement:** COMM.TIERS MUST be sorted ascending by LIMIT. An unsorted table produces wrong tier assignments without any error message.

### File Status Variables

**Three independent FILE STATUS variables:**
- COMMISSION-STATUS — tracks COMMISSION-TIERS-FILE reads
- SALARY-STATUS — tracks EMPLOYEE-SALARY-FILE reads
- OUTPUT-STATUS — tracks COMMISSION-OUTPUT-FILE writes

CLOSE errors treated as warnings (display only, no STOP RUN).

## Program Flow

1. **Open Files: OPEN-ALL-FILES**
   - Opens all three files, validates each FILE STATUS = '00'

2. **Load Table: LOAD-COMMISSION-TABLE**
   - PERFORM UNTIL WS-COMM-EOF = 'Y':
     - READ COMMISSION-TIERS-FILE
     - AT END: MOVE 'Y' TO WS-COMM-EOF
     - NOT AT END: ADD 1 TO TIERS-LOADED, SET IDX TO TIERS-LOADED, load WS-LIMIT(IDX) and WS-PCT(IDX)
   - Result: TIER-TABLE populated with 5 entries, TIERS-LOADED = 5

3. **Process Salaries: PROCESS-SALARIES**
   - PERFORM UNTIL WS-SAL-EOF = 'Y':
     - READ EMPLOYEE-SALARY-FILE
     - AT END: MOVE 'Y' TO WS-SAL-EOF
     - NOT AT END: ADD 1 TO EMPLOYEES-PROCESSED, PERFORM CALCULATE-COMMISSION

4. **Calculate Commission: CALCULATE-COMMISSION**
   - MOVE 'N' TO WS-FOUND
   - PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TIERS-LOADED OR WS-FOUND = 'Y':
     - IF WS-LIMIT(IDX) >= SAL-AMT:
       - MOVE 'Y' TO WS-FOUND
       - MOVE SPACES TO COMMISSION-REC
       - COMPUTE OUT-RES = SAL-AMT * WS-PCT(IDX)
       - Fill OUT-ID, OUT-SAL-AMT, OUT-PCT
       - WRITE COMMISSION-REC
       - ADD 1 TO TIER-MATCH-COUNT, RECORDS-WRITTEN
   - IF WS-FOUND = 'N': ADD 1 TO NO-TIER-MATCH (no record written)

5. **Close + Summary: CLOSE-ALL-FILES + DISPLAY-SUMMARY**
   - Close all three files (warnings only on error)
   - Display full summary to SYSOUT

## JCL Jobs

### [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

**Step 1:** Delete old PAYOUT.RPT (IEFBR14)
**Step 2:** Compile and run PSTASK15 using MYCOMPGO proc (MEMBER=PSTASK15):
- COMMDD: Z73460.TASK15.COMM.TIERS (DISP=SHR) — commission tier reference
- SALDD: Z73460.TASK15.SALES.WEEKLY (DISP=SHR) — weekly sales amounts
- OUTDD: Z73460.TASK15.PAYOUT.RPT (NEW,CATLG,DELETE, FB, LRECL=80) — payout report output

> **Note:** `MYCOMPGO` PROC is shared across all tasks.
> See [/PROCLIB/MYCOMPGO.jcl](/PROCLIB/MYCOMPGO.jcl) for full definition.

## How to Run

### Step 1: Allocate and Load Commission Tiers File

Ensure Z73460.TASK15.COMM.TIERS is cataloged and contains [DATA/COMM-TIERS-INPUT](DATA/COMM-TIERS-INPUT)
**Verify:** 5 tiers, sorted ascending by LIMIT: 001000, 005000, 010000, 050000, 999999

### Step 2: Allocate and Load Sales File

Ensure Z73460.TASK15.SALES.WEEKLY is cataloged and contains [DATA/SALES-WEEKLY-INPUT](DATA/SALES-WEEKLY-INPUT)
**Verify:** 12 records; record 01200 (salary=99999999) exceeds all tier limits — triggers NO TIER MATCH

### Step 3: Execute Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution summary
**Review** [DATA/PAYOUT-REPORT-EXPECTED](DATA/PAYOUT-REPORT-EXPECTED) for expected output

### Step 4: Verify Results

- **Tiers loaded:** 5
- **Employees processed:** 12
- **Records written:** 11 (employee 01200 exceeds all tiers — no output)
- **Tier matched:** 11
- **No tier match:** 1 (employee 01200, salary=99999999 > tier 5 limit of 999999)

## Calculation Trace

| EMP-ID | Sales Amount | Tier Matched | PCT | Commission |
|---|---|---|---|---|
| 00100 | 500.00 | Tier 1 (≤1000) | 2.0% | 10.00 |
| 00200 | 500.00 | Tier 1 (≤1000) | 2.0% | 10.00 |
| 00300 | 1000.00 | Tier 1 (≤1000) | 2.0% | 20.00 |
| 00400 | 1500.00 | Tier 2 (≤5000) | 5.0% | 75.00 |
| 00500 | 5000.00 | Tier 2 (≤5000) | 5.0% | 250.00 |
| 00600 | 10000.00 | Tier 3 (≤10000) | 10.0% | 1000.00 |
| 00700 | 15000.00 | Tier 4 (≤50000) | 12.5% | 1875.00 |
| 00800 | 50000.00 | Tier 4 (≤50000) | 12.5% | 6250.00 |
| 00900 | 100000.00 | Tier 5 (≤999999) | 15.0% | 15000.00 |
| 01000 | 500000.00 | Tier 5 (≤999999) | 15.0% | 75000.00 |
| 01100 | 999999.00 | Tier 5 (≤999999) | 15.0% | 149999.85 |
| 01200 | 999999.99 | **NO MATCH** | — | — (not written) |

## Common Issues

### Issue 1: Wrong Tier Applied (Too High or Too Low)

**Cause:** COMM.TIERS not sorted ascending by LIMIT — scan finds wrong first-fit tier
**Solution:** Verify DATA/COMM-TIERS-INPUT rows are in ascending LIMIT order: 001000 → 005000 → 010000 → 050000 → 999999

### Issue 2: Record 01200 Written with Zero Commission

**Cause:** CALCULATE-COMMISSION writes COMMISSION-REC before WS-FOUND check, or WS-FOUND logic is incorrect
**Solution:** Verify WRITE is INSIDE the IF WS-LIMIT(IDX) >= SAL-AMT block only
If WS-FOUND = 'N' after the loop: NO write should occur, only ADD 1 TO NO-TIER-MATCH

### Issue 3: Boundary Employee Gets Wrong Tier

**Cause:** `>=` vs `>` comparison — employee with salary exactly at tier limit should match that tier
**Solution:** Condition is IF WS-LIMIT(IDX) >= SAL-AMT (greater than OR EQUAL) — salary=1000.00 must match Tier 1 (LIMIT=001000), salary=5000.00 must match Tier 2 (LIMIT=005000)

### Issue 4: All Employees Get Tier 5 (Highest Rate)

**Cause:** PERFORM VARYING loop not stopping on first match — WS-FOUND not terminating loop correctly
**Solution:** Verify loop condition includes OR WS-FOUND = 'Y' — UNTIL IDX > TIERS-LOADED OR WS-FOUND = 'Y'
Without WS-FOUND check, loop overwrites result with each subsequent matching tier

### Issue 5: WS-FOUND Not Reset Between Employees

**Cause:** WS-FOUND = 'Y' from previous employee carries over — all subsequent employees skip lookup
**Solution:** Verify MOVE 'N' TO WS-FOUND is at the START of CALCULATE-COMMISSION before PERFORM VARYING

### Issue 6: Table Overflow Abend

**Cause:** COMM.TIERS has more than 20 records — SET IDX exceeds OCCURS 20 TIMES
**Solution:** Limit COMM.TIERS to 20 records or increase OCCURS size and recompile; add boundary check before SET IDX

### Issue 7: Abend S0C7

**Cause 1:** COMM-LIMIT non-numeric (COMM.TIERS bytes 1–6 not all digits)
**Cause 2:** SAL-AMT non-numeric (SALES.WEEKLY bytes 6–13 not all digits)
**Solution:** Verify both input files use strictly numeric data in amount fields with no embedded spaces or alphabetic characters

## Program Output (SYSOUT)

See [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution log

**Normal Execution (5 tiers, 12 employees):**
```
========================================
COMMISSION CALCULATION SUMMARY
========================================
COMMISSION TIERS LOADED:     5
EMPLOYEES PROCESSED:        12
RECORDS WRITTEN:            11
TIER MATCHED:               11
NO TIER MATCH:               1
========================================
```

## Notes

- TIER-TABLE uses OCCURS 20 TIMES INDEXED BY IDX — same pattern as TASK14 but lookup is threshold-based, not exact-match
- Table MUST be sorted ascending by LIMIT — the algorithm finds the FIRST tier where limit satisfies the condition
- Employee 01200 (salary=99999999) intentionally exceeds all 5 tier limits to test the no-match error path
- No default rate applied on no-match (unlike TASK14) — record is simply skipped with counter increment
- MOVE SPACES TO COMMISSION-REC before each write prevents dirty FILLER data
- No VSAM, no DB2 — pure sequential PS-to-PS batch with in-memory tiered reference table
- Compare with TASK14: exact region lookup vs. TASK15: tiered threshold lookup — two core COBOL table patterns
- Tested on IBM z/OS with Enterprise COBOL
