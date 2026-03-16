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

## Two-Phase Design

```
PHASE 1 - INITIALIZATION (run once):
  OPEN COMM.TIERS → load all records into TIER-TABLE → CLOSE COMM.TIERS

PHASE 2 - PROCESSING (per employee):
  READ SALES.WEEKLY
  SCAN TIER-TABLE: find first IDX where WS-LIMIT(IDX) >= SAL-AMT
  IF FOUND:     COMPUTE OUT-RES = SAL-AMT * WS-PCT(IDX), WRITE record
  IF NOT FOUND: ADD 1 TO NO-TIER-MATCH, skip
```

**Critical requirement:** COMM.TIERS MUST be sorted ascending by LIMIT. Unsorted table produces wrong tier assignments without any error message.

**Performance benefit:** Loading tiers once eliminates re-reading the reference file for every employee record.

## Files

### Input Files

#### 1. COMM.TIERS (PS) - Commission Tier Reference File

**Access Mode:** INPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  
**Sort Requirement:** MUST be sorted ascending by COMM-LIMIT — scan finds the FIRST tier where LIMIT >= salary  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| COMM-LIMIT | 9(6) | 6 | Upper salary boundary for this tier |
| COMM-PCT | V999 | 3 | Commission rate as implied decimal |

**Sample Data:** [DATA/COMM.TIERS](DATA/COMM.TIERS)

#### 2. SALES.WEEKLY (PS) - Weekly Employee Sales File

**Access Mode:** INPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| SAL-ID | 9(5) | 5 | Employee ID |
| SAL-AMT | 9(6)V99 | 8 | Weekly sales amount (implied decimal) |

**Sample Data:** [DATA/SALES.WEEKLY](DATA/SALES.WEEKLY)

### Output Files

#### 3. PAYOUT.RPT (PS) - Commission Payout Report

**Access Mode:** OUTPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed Block (RECFM=FB, LRECL=80)  

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

**Expected Output:** [DATA/PAYOUT.RPT](DATA/PAYOUT.RPT)

### Error Handling

**FILE STATUS Codes (all three files):**
- 00 - Successful operation
- Other codes - I/O errors on open, read, or write (program displays status and STOP RUN)

CLOSE errors treated as warnings (display only, no STOP RUN)

## Program Flow

1. **Initialization**
   - Opens all three files; validates FILE STATUS '00' on each
   - Loads COMM.TIERS into TIER-TABLE: reads sequentially, stores WS-LIMIT(IDX) and WS-PCT(IDX) per entry, increments TIERS-LOADED; closes COMM.TIERS after load

2. **Main Processing Loop**
   - Reads SALES.WEEKLY sequentially until EOF; increments EMPLOYEES-PROCESSED per record
   - For each employee: MOVE 'N' TO WS-FOUND, scans TIER-TABLE VARYING IDX FROM 1 BY 1 UNTIL IDX > TIERS-LOADED OR WS-FOUND = 'Y'
   - Match (WS-LIMIT(IDX) >= SAL-AMT) → COMPUTE OUT-RES = SAL-AMT × WS-PCT(IDX); writes PAYOUT record; increments TIER-MATCH-COUNT and RECORDS-WRITTEN
   - No match → increments NO-TIER-MATCH; no record written

3. **Termination**
   - Closes remaining files (non-zero status on CLOSE is warning only)
   - Displays summary to SYSOUT: TIERS LOADED / EMPLOYEES PROCESSED / RECORDS WRITTEN / TIER MATCHED / NO TIER MATCH
   - STOP RUN

## JCL Jobs

### [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure.

- COMMDD: COMM.TIERS (commission tier reference)
- SALDD: SALES.WEEKLY (weekly sales amounts)
- OUTDD: PAYOUT.RPT (payout report output)

**PROC reference:** [JCLPROC/MYCOMPGO.jcl](../../JCLPROC/MYCOMPGO.jcl)

## How to Run

### Step 1: Allocate and Load Commission Tiers File

- Upload [DATA/COMM.TIERS](DATA/COMM.TIERS) to PS dataset manually via ISPF

**Alternative:**
1. Create PS file with LRECL=80 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 2: Allocate and Load Sales File

- Upload [DATA/SALES.WEEKLY](DATA/SALES.WEEKLY) to PS dataset manually via ISPF

**Alternative:**
1. Create PS file with LRECL=80 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 3: Execute Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution summary  
**Review** [DATA/PAYOUT.RPT](DATA/PAYOUT.REPORT) for expected output  

**Alternative:**
If you prefer to compile and run separately, use these jobs:
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)

### Step 4: Verify Results

- **Tiers loaded:** 5
- **Employees processed:** 12
- **Records written:** 11 (employee 01200 exceeds all tiers — no output)
- **Tier matched:** 11
- **No tier match:** 1 (employee 01200, salary=99999999 > tier 5 limit of 999999)

## Common Issues

### Issue 1: Wrong Tier Applied (Too High or Too Low)

**Cause:** COMM.TIERS not sorted ascending by LIMIT — scan finds wrong first-fit tier  
**Solution:** Verify DATA/COMM-TIERS-INPUT rows are in ascending LIMIT order: 001000 → 005000 → 010000 → 050000 → 999999

### Issue 2: Table Overflow Abend

**Cause:** COMM.TIERS has more than 20 records — SET IDX exceeds OCCURS 20 TIMES  
**Solution:** Limit COMM.TIERS to 20 records or increase OCCURS size and recompile; add boundary check before SET IDX

### Issue 3: Abend S0C7

**Cause:** Non-numeric COMM-LIMIT in COMM.TIERS (bytes 1–6) or non-numeric SAL-AMT in SALES.WEEKLY (bytes 6–13)
**Solution:** Verify both input files use strictly numeric data in amount fields with no embedded spaces or alphabetic characters

## Program Output (SYSOUT)

See [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution log

## Notes

- TIER-TABLE uses OCCURS 20 TIMES INDEXED BY IDX — same pattern as TASK14 but lookup is threshold-based, not exact-match
- Table MUST be sorted ascending by LIMIT — the algorithm finds the FIRST tier where limit satisfies the condition
- No default rate applied on no-match (unlike TASK14) — record is simply skipped with counter increment
- Tested on IBM z/OS with Enterprise COBOL
