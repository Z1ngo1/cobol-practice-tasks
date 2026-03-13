# TASK14 - Table Lookup Algorithm: Tax Calculation with In-Memory Table

Employee payroll tax calculator that loads region-based tax rates into an in-memory table, then processes employee salary records with O(1)-overhead lookups per employee. Demonstrates the classic COBOL OCCURS + INDEXED BY table technique for efficient in-memory reference data processing.

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

**Access Mode:**
- INPUT (Sequential, loaded once at startup)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| TAX-REGION-CODE | X(2) | 2 | State/region code (lookup key) |
| RATE | V999 | 3 | Tax rate as implied decimal (e.g. 085 = 8.5%) |
| FILLER | X(75) | 75 | Unused |

**Sample Data:** [DATA/TAX.RATES](DATA/TAX.RATES)

#### 2. EMP.SALARY (PS) - Employee Salary File

**Access Mode:**
- INPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

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

**Access Mode:**
- OUTPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed Block (RECFM=FB, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| OUT-ID | X(5) | 5 | Employee ID |
| OUT-REGION | X(2) | 2 | Region code applied (actual or 'XX' for default) |
| OUT-TAX | 9(5)V99 | 7 | Calculated tax amount |
| FILLER | X(66) | 66 | Unused |

**Expected Output:** [DATA/PAYROLL.TXT](DATA/PAYROLL.TXT)

### File Status Variables

**Three independent FILE STATUS variables:**
- TAX-RATES-STATUS — tracks TAX-RATES-FILE reads
- EMPLOYEE-STATUS — tracks EMPLOYEE-SALARY-FILE reads
- PAYROLL-STATUS — tracks PAYROLL-OUTPUT-FILE writes

CLOSE errors treated as warnings (display only, no STOP RUN).

## Program Flow

1. **Open Files: OPEN-ALL-FILES**
   - Opens all three files, validates each FILE STATUS = '00'

2. **Load Table: LOAD-TAX-TABLE**
   - PERFORM UNTIL TAX-EOF:
     - READ TAX-RATES-FILE
     - AT END: MOVE 'Y' TO WS-TAX-EOF
     - NOT AT END: ADD 1 TO TAX-RATES-LOADED, SET IDX TO TAX-RATES-LOADED, load WS-REGION(IDX) and WS-RATE(IDX)
   - Result: TAX-TABLE populated with 7 entries, TAX-RATES-LOADED = 7

3. **Process Employees: PROCESS-EMPLOYEES**
   - PERFORM UNTIL EMP-EOF:
     - READ EMPLOYEE-SALARY-FILE
     - AT END: MOVE 'Y' TO WS-EMP-EOF
     - NOT AT END: ADD 1 TO EMPLOYEES-PROCESSED, MOVE SPACES TO PAYROLL-REC, PERFORM LOOKUP-TAX-RATE

4. **Lookup Rate: LOOKUP-TAX-RATE**
   - MOVE 'N' TO WS-FOUND
   - PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TAX-RATES-LOADED OR WS-FOUND = 'Y'
   - On match: MOVE 'Y' TO WS-FOUND, COMPUTE OUT-TAX, MOVE EMP-ID/WS-REGION to output fields, ADD 1 TO RATE-FOUND-COUNT

5. **Apply Default: APPLY-DEFAULT-RATE (if WS-FOUND = 'N')**
   - COMPUTE OUT-TAX = EMP-SALARY * DEF-TAX-RATE (0.200)
   - MOVE EMP-ID TO OUT-ID
   - MOVE 'XX' TO OUT-REGION
   - ADD 1 TO DEFAULT-RATE-COUNT

6. **Write + Count:**
   - WRITE PAYROLL-REC (both found and default paths)
   - Validate PAYROLL-STATUS = '00'
   - ADD 1 TO TAXES-CALCULATED

7. **Close + Summary: CLOSE-ALL-FILES + DISPLAY-SUMMARY**
   - Close all three files (warnings only on error)
   - Display full summary to SYSOUT

## JCL Jobs

### [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure.

- TAXDD: TAX.RATES (tax rates reference)
- EMPDD: EMP.SALARY (employee salaries)
- OUTDD: PAYROLL.TXT (payroll output)

## How to Run

### Step 1: Allocate and Load Tax Rates File

- Upload [DATA/TAX.RATES](DATA/TAX.RATES)

### Step 2: Allocate and Load Employee Salary File

Ensure Z73460.TASK14.EMP.SALARY is cataloged and contains [DATA/EMP.SALARY](DATA/EMP.SALARY)
**Verify:** 10 records; records 00800, 00900 use unknown state codes (ZZ, XX-like) to trigger default rate

### Step 3: Execute Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution summary
**Review** [DATA/PAYROLL.TXT](DATA/PAYROLL.TXT) for expected output

### Step 4: Verify Results

- **Tax rates loaded:** 7
- **Employees processed:** 10
- **Payroll records written:** 10
- **Rate found:** 8 (NY×2, CA, TX, FL, IL, MA, WA)
- **Default rate used:** 2 (XX and ZZ — unknown region codes → 20% default)

## Common Issues

### Issue 1: All Employees Get Default Rate

**Cause:** TAX-TABLE not loaded before PROCESS-EMPLOYEES; LOAD-TAX-TABLE not performed or TAX-RATES-LOADED = 0
**Solution:** Verify MAIN-LOGIC order: OPEN-ALL-FILES → LOAD-TAX-TABLE → PROCESS-EMPLOYEES
Verify TAX.RATES DSN exists and has records; check SYSOUT for TAX RATES LOADED count

### Issue 2: Table Overflow Abend

**Cause:** TAX.RATES file contains more than 50 records — SET IDX TO TAX-RATES-LOADED exceeds OCCURS 50 TIMES
**Solution:** Limit TAX.RATES to 50 records or increase OCCURS size and recompile

### Issue 3: Wrong Tax Calculated

**Cause:** RATE field PIC V999 interpreted incorrectly — implied decimal means 085 = 0.085, not 85
**Solution:** Verify TAX-RATES-INPUT RATE field is exactly 3 numeric digits at offset 2 (bytes 3–5)
Example: NY085 → WS-RATE = 0.085; SALARY 5000.00 × 0.085 = 425.00

### Issue 4: Default Region Shows Actual Code Instead of 'XX'

**Cause:** APPLY-DEFAULT-RATE not executing; WS-FOUND remains 'Y' from previous employee (not reset)
**Solution:** Verify MOVE 'N' TO WS-FOUND at the START of LOOKUP-TAX-RATE (before PERFORM VARYING)
WS-FOUND must be reset for each employee before table scan begins

### Issue 5: Abend S0C7 (Data Exception)

**Cause 1:** RATE field in TAX.RATES contains non-numeric characters  
**Solution:** Verify bytes 3–5 of each TAX.RATES record are strictly numeric digits  

**Cause 2:** EMP-SALARY field in EMP.SALARY is non-numeric  
**Solution:** Verify bytes 28–34 of employee records are 7 numeric digits (implied decimal, no decimal point in data)  

## Program Output (SYSOUT)

See [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution log

## Notes

- TAX-TABLE declared with OCCURS 50 TIMES INDEXED BY IDX — index (not subscript) for SET/PERFORM VARYING compatibility
- Linear scan used for lookup (7 entries) — for large tables (100+ entries) SEARCH ALL (binary search) with ASCENDING KEY would be more efficient
- DEF-TAX-RATE PIC V999 VALUE .200 — 20% default applied when region not in table, output region forced to 'XX' regardless of employee's actual code
- TAX.RATES loaded once in PHASE 1 and then closed — file is not re-read during employee processing
- PS-to-PS batch with in-memory reference table
- Tested on IBM z/OS with Enterprise COBOL
