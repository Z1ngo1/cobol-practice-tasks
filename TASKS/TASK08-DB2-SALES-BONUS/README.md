# TASK08 - DB2 Sales Bonus Calculator

Sales bonus calculation system using DB2 cursor operations with region-based increases and high-performer bonuses.

## Business Scenario

Company performs annual sales bonus calculation based on region and sales performance. The system must: 
- Read employee sales bonus data from DB2 table TB_SALES_BONUS
- Apply bonus increases by region (EU +12%, NE +10%, AS +8%, SW +5%)
- Apply additional 5% bonus for high performers (YEAR_SALES >= 150000)
- Apply maximum bonus cap at 20000
- Update DB2 table with new bonus amounts using positioned cursor updates
- Generate detailed bonus change report with status indicators
- Commit every 50 records for optimal performance and rollback safety

## Files

### Input Files

#### 1. TB_SALES_BONUS (DB2 Table) - Sales Bonus Master

**Table Structure:**
| Column | DB2 Type | COBOL PIC | Description |
|---|---|---|---|
| EMP_ID | CHAR(6) NOT NULL | PIC X(6) | Employee ID (Primary Key) |
| EMP_NAME | VARCHAR(30) | PIC X(30) + LEN PIC S9(4) COMP-5 | Employee full name |
| REGION_CODE | CHAR(2) | PIC X(2) | Region code (EU / NE / AS / SW) |
| YEAR_SALES | DECIMAL(11,2) | PIC S9(9)V9(2) COMP-3 | Annual sales amount |
| BONUS_AMT | DECIMAL(9,2) | PIC S9(7)V9(2) COMP-3 | Current bonus amount |

**Sample Data:** [DATA/TB.SALES.BONUS.BEFORE](DATA/TB.SALES.BONUS.BEFORE)

**DCLGEN:** [DCLGEN/TASK8.cpy](DCLGEN/TASK8.cpy)

### Output Files

#### 2. BONUS-REPORT (PS) - Bonus Change Report

**Access Mode:** OUTPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  

**Report Line Layout:**
| Field | PIC | Description |
|---|---|---|
| EMP-ID | X(6) | Employee ID |
| REGION-CODE | X(2) | Region code |
| OLD_BONUS | ZZZZ9.99 | Bonus before calculation |
| NEW_BONUS | ZZZZ9.99 | Bonus after calculation |
| STATUS | X(7) | Result: HIGHSAL / CAP / LOW / OK |

**Header:** `EMPID  REGION OLD_BONUS NEW_BONUS STATUS`  
**Footer:** `TOTAL: nnn ROWS UPDATED`

**Expected Output:** [DATA/BONUS.REPORT](DATA/BONUS.REPORT)

#### 3. TB_SALES_BONUS (DB2 Table) - Updated Employee Data

Same table as input, updated in place with new bonus values

**Expected Final State:** [DATA/TB.SALES.BONUS.AFTER](DATA/TB.SALES.BONUS.AFTER)

### Error Handling

**SQLCODE Values:** 
- 0 - Successful operation
- +100 - No more rows (end of data)
- -501 - Cursor already closed (handled, not fatal)
- -805 - PLAN not found (BIND error)
- -204 - Table not found
- Other negative codes - SQL errors (program rolls back and stops)

**FILE STATUS Codes (Report File):** 
- 00 - Successful operation
- Other codes - I/O errors (program rolls back and stops)

### Bonus Calculation Logic

| REGION_CODE | Multiplier | Formula |
|---|---|---|
| EU | +12% | CALC_BONUS = BONUS_AMT × 1.12 |
| NE | +10% | CALC_BONUS = BONUS_AMT × 1.10 |
| AS | +8% | CALC_BONUS = BONUS_AMT × 1.08 |
| SW | +5% | CALC_BONUS = BONUS_AMT × 1.05 |

**High Sales Boost:** If YEAR_SALES >= 150000 → CALC_BONUS = CALC_BONUS × 1.05

**Status Classification** (evaluated in priority order):
- **CAP** — CALC_BONUS > 20000 → capped at 20000
- **LOW** — final bonus < 2000
- **HIGHSAL** — YEAR_SALES >= 150000 (boost applied, no cap)
- **OK** — standard increase

**Examples:**
- **EMP 000101**, EU, bonus 1500.00, sales 80000 → 1500 × 1.12 = 1680.00 (LOW)
- **EMP 000102**, EU, bonus 3000.00, sales 180000 → 3000 × 1.12 × 1.05 = 3528.00 (HIGHSAL)
- **EMP 000103**, NE, bonus 10000.00, sales 200000 → 10000 × 1.10 × 1.05 = 11550.00 (HIGHSAL)
- **EMP 000104**, AS, bonus 18000.00, sales 250000 → 18000 × 1.08 × 1.05 = 20412 → 20000.00 (CAP)
- **EMP 000106**, NE, bonus 5000.00, sales 120000 → 5000 × 1.10 = 5500.00 (OK)

### DB2 Objects

#### Cursor: CUR-BONUS

**Declaration:**
- DECLARE CUR-BONUS CURSOR WITH HOLD FOR SELECT * FROM TB_SALES_BONUS FOR UPDATE OF BONUS_AMT

**WITH HOLD:** Cursor remains open after COMMIT (essential for batch processing)

**FOR UPDATE OF BONUS_AMT:** Enables positioned UPDATE WHERE CURRENT OF

## Program Flow

1. **Initialization**
   - Opens BONUS-REPORT-FILE, writes header line
   - Initializes TOTAL-REC-UPDATED and COMMIT-COUNT counters
   - Opens CUR-BONUS cursor; validates SQLCODE = 0

2. **Fetch and Process Loop**
   - Fetches rows via CUR-BONUS until SQLCODE = +100 (EOF)
   - Saves OLD-BONUS, calculates base CALC-BONUS by REGION-CODE (EU ×1.12 / NE ×1.10 / AS ×1.08 / SW ×1.05)
   - If YEAR-SALES >= 150000: applies additional ×1.05 boost
   - Determines STATUS by priority: CAP (>20000) → LOW (<2000) → HIGHSAL → OK
   - Executes UPDATE WHERE CURRENT OF CUR-BONUS; validates SQLCODE = 0
   - Writes detail line to BONUS-REPORT; increments counters
   - Every 50 records: COMMIT WORK, display commit message, reset COMMIT-COUNT

3. **Termination**
   - Executes final COMMIT WORK; closes CUR-BONUS (SQLCODE -501 handled gracefully)
   - Writes footer with total count; closes BONUS-REPORT-FILE
   - Displays "BONUS UPDATE COMPLETED: {count}"; STOP RUN

## SQL Scripts

### 1. [CREATE.TABLE.sql](SQL/CREATE.TABLE.sql) - Create Sales Bonus Table

Creates TB_SALES_BONUS table with primary key on EMP_ID

### 2. [INSERT.DATA.sql](SQL/INSERT.DATA.sql) - Load Initial Data

Inserts 10 employee records with varying regions and sales volumes  
Coverage: EU=3, NE=3, AS=2, SW=2. High sales (>=150000): 5 employees

## JCL Jobs

### 1. [COBDB2CP.jcl](JCL/COBDB2CP.jcl) - DB2 Precompile, Compile, and Execute

**Step 1:** Delete old REPORT.FILE (IEFBR14)  
**Step 2:** DB2 Precompile, COBOL Compile, Link, and BIND PLAN (DB2CBL proc) - converts EXEC SQL to CALL statements  
**Step 3:** Allocate STEPLIB and OUTDD (IKJEFT01) - sets up DB2 load library and report dataset  
**Step 4:** Execute Program under DB2 control (DSN SYSTEM)

## How to Run

### Step 1: Create DB2 Table

**Execute** [SQL/CREATE.TABLE.sql](SQL/CREATE.TABLE.sql) via SPUFI or QMF

### Step 2: Load Initial Sales Bonus Data

**Execute** [SQL/INSERT.DATA.sql](SQL/INSERT.DATA.sql) via SPUFI or QMF  
**See** [DATA/TB.SALES.BONUS.BEFORE](DATA/TB.SALES.BONUS.BEFORE)

### Step 3: Execute Bonus Calculation Program

**Submit** [JCL/COBDB2CP.jcl](JCL/COBDB2CP.jcl)  
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt)  
**Review** [DATA/BONUS.REPORT](DATA/BONUS.REPORT) for detailed report

### Step 4: Verify Results

**Query updated table:**  

- SELECT EMP_ID, EMP_NAME, REGION_CODE, YEAR_SALES, BONUS_AMT FROM TB_SALES_BONUS ORDER BY EMP_ID  
- **Compare** Before [DATA/TB.SALES.BONUS.BEFORE](DATA/TB.SALES.BONUS.BEFORE) vs After [DATA/TB.SALES.BONUS.AFTER](DATA/TB.SALES.BONUS.AFTER)  
- **Verify calculations** EU × 1.12, NE × 1.10, AS × 1.08, SW × 1.05, high-sales × 1.05, cap at 20000  
- **Check report** Header, 10 detail lines, STATUS indicators (HIGHSAL/CAP/LOW/OK), Footer with total count

## Common Issues

### Issue 1: SQLCODE -805 (PLAN not found)

**Cause:** BIND PLAN not executed or PLAN name mismatch  
**Solution:** Verify BIND step CC 0000, check PLAN name in BIND and RUN steps.

### Issue 2: SQLCODE -204 (Table not found)

**Cause:** TB_SALES_BONUS not created or wrong schema/database  
**Solution:** Verify table exists (SELECT COUNT(*) FROM TB_SALES_BONUS), check current schema 
```sql
SELECT COUNT(*) FROM TB_SALES_BONUS
VALUES CURRENT SCHEMA
```

### Issue 3: VARCHAR Field Error

**Cause:** Incorrect handling of EMP-NAME VARCHAR field  
**Solution:** Use EMP-NAME-LEN (length) and EMP-NAME-TEXT (data) subfields as generated by DCLGEN  
Do not move directly to EMP-NAME group level

### Issue 4: S0C7 Data Exception

**Cause:** BONUS_AMT field alignment issue between DCLGEN and table  
**Solution:** Regenerate DCLGEN, verify BONUS_AMT declared as PIC S9(7)V9(2) USAGE COMP-3  
Ensure CALC-BONUS working storage variable uses same picture and usage

### Issue 5: Report File Not Allocated

**Cause:** OUTDD DD statement missing or incorrect DSN  
**Solution:** Verify RUNPROG step includes OUTDD DD statement, check TASK8.REPORT.FILE cataloged after execution, review SYSOUT for allocation errors

## Program Output (SYSOUT)

Expected execution log - see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for full output example

**Normal Execution (10 records):** 
- BONUS UPDATE COMPLETED: 00010

**Large Dataset (120 records):** 
- COMMIT AFTER 00050 RECORDS  
- COMMIT AFTER 00050 RECORDS  
- BONUS UPDATE COMPLETED: 00120

**Error Execution:** 
- UPDATE ERROR FOR EMPLOYEE: 000042  
- SQLCODE: -00000803  
- ROLLBACK ERROR: +00000000  
- ALL CHANGES ROLLED BACK DUE TO UPDATE ERROR

## Notes

- WITH HOLD cursor maintains position across COMMIT — without it cursor closes after each commit and batch processing fails
- Status priority: CAP overrides HIGHSAL, LOW applies if final bonus under threshold
- VARCHAR field EMP_NAME handled via length prefix (EMP-NAME-LEN) and text area (EMP-NAME-TEXT)
- Tested on IBM z/OS with DB2 for z/OS and Enterprise COBOL

