# TASK14 - Table Lookup Algorithm: Tax Calculation with In-Memory Table

Employee payroll tax calculator that loads region-based tax rates into an in-memory table, then processes employee salary records with per-employee lookups. Demonstrates the classic COBOL OCCURS + INDEXED BY table technique for efficient in-memory reference data processing.

## Business Scenario

Company pays employees across multiple states with different tax rates. Loading rates from a flat file into memory before processing eliminates repeated I/O during the main loop. The system must:
- Load TAX.RATES file into an in-memory table (max 50 entries) during initialization
- For each employee: look up their state code in the table and apply the matching rate
- Apply a default rate of 20% for employees whose state code is not in the table
- Write a payroll output record with employee ID, region code, and calculated tax amount
- Display processing summary to SYSOUT

## Two-Phase Design

```
PHASE 1 - INITIALIZATION (run once):
  OPEN TAX.RATES
  Load all records into TAX-TABLE (OCCURS 50 TIMES INDEXED BY IDX)
  CLOSE TAX.RATES

PHASE 2 - PROCESSING (per employee):
  READ EMP.SALARY
  PERFORM LOOKUP-TAX-RATE (linear scan of TAX-TABLE)
  IF FOUND:  COMPUTE OUT-TAX = EMP-SALARY * WS-RATE(IDX)
  IF NOT FOUND: COMPUTE OUT-TAX = EMP-SALARY * DEF-TAX-RATE (0.200)
  WRITE PAYROLL-REC
```

**Performance benefit:** With 10,000+ employees, loading 7 rates once vs. re-reading the tax file 10,000 times eliminates massive I/O overhead.

## Files

### Input Files

#### 1. TAX.RATES (PS) - Region Tax Rates Reference File

**Access Mode:** INPUT (Sequential, loaded once at startup)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| TAX-REGION-CODE | X(2) | 2 | State/region code (lookup key) |
| RATE | V999 | 3 | Tax rate as implied decimal (e.g. 085 = 8.5%) |
| FILLER | X(75) | 75 | Unused |

**Sample Data:** [DATA/TAX.RATES](DATA/TAX.RATES)

#### 2. EMP.SALARY (PS) - Employee Salary File

**Access Mode:** INPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| EMP-ID | X(5) | 5 | Employee ID |
| EMP-NAME | X(20) | 20 | Employee name |
| EMP-REGION-CODE | X(2) | 2 | State/region code (lookup key against TAX-TABLE) |
| EMP-SALARY | 9(5)V99 | 7 | Annual salary (implied decimal) |
| FILLER | X(46) | 46 | Unused |

**Sample Data:** [DATA/EMP.SALARY](DATA/EMP.SALARY)

### Output Files

#### 3. PAYROLL.TXT (PS) - Calculated Payroll Tax File

**Access Mode:** OUTPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed Block (RECFM=FB, LRECL=80)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| OUT-ID | X(5) | 5 | Employee ID |
| OUT-REGION | X(2) | 2 | Region code applied (actual or 'XX' for default) |
| OUT-TAX | 9(5)V99 | 7 | Calculated tax amount |
| FILLER | X(66) | 66 | Unused |

**Expected Output:** [DATA/PAYROLL.TXT](DATA/PAYROLL.TXT)

### Error Handling

**FILE STATUS Codes (TAX.RATES, EMP.SALARY, PAYROLL.TXT):**
- 00 - Successful operation
- 35 - File not found on OPEN (program displays status and STOP RUN)
- Other codes - I/O errors on open, read, or write (program displays status and STOP RUN)

CLOSE errors treated as warnings (display only, no STOP RUN).

## Program Flow

1. **Initialization**
   - Opens all three files; validates FILE STATUS '00' on each
   - Loads TAX.RATES into TAX-TABLE: reads sequentially, stores WS-REGION(IDX) and WS-RATE(IDX) per entry, increments TAX-RATES-LOADED; closes TAX.RATES after load

2. **Main Processing Loop**
   - Reads EMP.SALARY sequentially until EOF; increments EMPLOYEES-PROCESSED per record
   - For each employee: scans TAX-TABLE VARYING IDX FROM 1 BY 1 until match or end of table
   - Match found → COMPUTE OUT-TAX = EMP-SALARY × WS-RATE(IDX); MOVE actual region code to OUT-REGION
   - Not found → COMPUTE OUT-TAX = EMP-SALARY × 0.200; MOVE 'XX' to OUT-REGION
   - Writes PAYROLL-REC; validates PAYROLL-STATUS = '00'; increments TAXES-CALCULATED

3. **Termination**
   - Closes remaining files (non-zero status on CLOSE is warning only)
   - Displays summary to SYSOUT: TAX RATES LOADED / EMPLOYEES PROCESSED / TAXES CALCULATED / RATE FOUND / DEFAULT RATE USED
   - STOP RUN
  
## JCL Jobs

### [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure.

- TAXDD: TAX.RATES (tax rates reference)
- EMPDD: EMP.SALARY (employee salaries)
- OUTDD: PAYROLL.TXT (payroll output)

**PROC reference:** [JCLPROC/MYCOMPGO.jcl](../../JCLPROC/MYCOMPGO.jcl)

## How to Run

### Step 1: Allocate and Load Tax Rates File

- Upload [DATA/TAX.RATES](DATA/TAX.RATES) to PS dataset manually via ISPF

### Step 2: Allocate and Load Employee Salary File

- Upload [DATA/EMP.SALARY](DATA/EMP.SALARY) to PS dataset manually via ISPF

### Step 3: Execute Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution summary
**Review** [DATA/PAYROLL.TXT](DATA/PAYROLL.TXT) for expected output

**Alternative:**
If you prefer to compile and run separately, use these jobs:
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)

### Step 4: Verify Results

- **Tax rates loaded:** 7
- **Employees processed:** 10
- **Payroll records written:** 10
- **Rate found:** 8 (NY×2, CA, TX, FL, IL, MA, WA)
- **Default rate used:** 2 (XX and ZZ — unknown region codes → 20% default)

## Common Issues

### Issue 1: Table Overflow Abend

**Cause:** TAX.RATES file contains more than 50 records — SET IDX TO TAX-RATES-LOADED exceeds OCCURS 50 TIMES
**Solution:** Limit TAX.RATES to 50 records or increase OCCURS size and recompile

### Issue 2: Wrong Tax Calculated

**Cause:** RATE field PIC V999 interpreted incorrectly — implied decimal means 085 = 0.085, not 85
**Solution:** Verify RATE field is exactly 3 numeric digits at offset 2 (bytes 3–5); example: NY085 → WS-RATE = 0.085; SALARY 5000.00 × 0.085 = 425.00

### Issue 3: Abend S0C7 (Data Exception)

**Cause:** Non-numeric RATE in TAX.RATES or non-numeric EMP-SALARY in EMP.SALARY
**Solution:** Verify bytes 3–5 of each TAX.RATES record are strictly numeric; confirm EMP-SALARY is 7 numeric digits at offset 27 (5+20+2=27)

## Program Output (SYSOUT)

See [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution log

## Notes

- TAX-TABLE declared with OCCURS 50 TIMES INDEXED BY IDX — index (not subscript) for SET/PERFORM VARYING compatibility
- Linear scan used for lookup (7 entries) — for large tables (100+ entries) SEARCH ALL (binary search) with ASCENDING KEY would be more efficient
- TAX.RATES loaded once in PHASE 1 and then closed — file is not re-read during employee processing
- Tested on IBM z/OS with Enterprise COBOL
