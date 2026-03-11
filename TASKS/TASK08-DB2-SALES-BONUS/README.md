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

**Sample Data:** [DATA/TB.SALES.BONUS.BEFORE](DATA/TB.SALES.BONUS.BEFORE)

**DCLGEN Structure:** [DCLGEN/TASK8.cpy](DCLGEN/TASK8.cpy)

### Output Files

#### 2. BONUS-REPORT (PS) - Bonus Change Report

**Access Mode:** 
- OUTPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

**Expected Output:** [DATA/BONUS.REPORT](DATA/BONUS.REPORT)

#### 3. TB_SALES_BONUS (DB2 Table) - Updated Employee Data

Same table as input, updated in place with new bonus values

**Expected Final State:** [DATA/TB.SALES.BONUS.AFTER](DATA/TB.SALES.BONUS.AFTER)

### Bonus Calculation Logic

**Region-Based Increases:** 
- EU Region: CALC_BONUS = CURRENT_BONUS × 1.12 (+12%)
- NE Region: CALC_BONUS = CURRENT_BONUS × 1.10 (+10%)
- AS Region: CALC_BONUS = CURRENT_BONUS × 1.08 (+8%)
- SW Region: CALC_BONUS = CURRENT_BONUS × 1.05 (+5%)

**High Sales Bonus:** 
- If YEAR_SALES >= 150000, apply additional 5%: CALC_BONUS = CALC_BONUS × 1.05

**Maximum Cap Rule:** 
- If CALC_BONUS > 20000, cap at 20000 and set STATUS = CAP

**Status Classification:**
- **HIGHSAL** - High sales (>= 150000), bonus boost applied
- **CAP** - Bonus capped at 20000
- **LOW** - Final bonus < 2000
- **OK** - Standard increase

**Examples:** 
- EMP 000101, EU, 1500.00, sales 80000 → 1500 × 1.12 = 1680.00 (LOW)
- EMP 000102, EU, 3000.00, sales 180000 → 3000 × 1.12 × 1.05 = 3528.00 (HIGHSAL)
- EMP 000103, NE, 10000.00, sales 200000 → 10000 × 1.10 × 1.05 = 11550.00 (HIGHSAL)
- EMP 000104, AS, 18000.00, sales 250000 → 18000 × 1.08 × 1.05 = 20412 → 20000.00 (CAP)
- EMP 000106, NE, 5000.00, sales 120000 → 5000 × 1.10 = 5500.00 (OK)

### DB2 Objects

#### Cursor: CUR-BONUS

**Declaration:** 
- DECLARE CUR-BONUS CURSOR WITH HOLD FOR SELECT * FROM TB_SALES_BONUS FOR UPDATE OF BONUS_AMT

**WITH HOLD:** 
- Cursor remains open after COMMIT (essential for batch processing)

**FOR UPDATE OF BONUS_AMT:** 
- Enables positioned UPDATE WHERE CURRENT OF

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

## Program Flow

1. **Initialization:**
- Opens BONUS-REPORT-FILE (PS) for output
- Validates OUT-STATUS = '00'
- Writes report header line
- Initializes counters (TOTAL-REC-UPDATED = 0, COMMIT-COUNT = 0)

2. **Open DB2 Cursor:**
- Executes OPEN CUR-BONUS
- Validates SQLCODE = 0

3. **Fetch and Process Loop**
- Performs FETCH NEXT until SQLCODE = 100 (EOF)
- For each employee record fetched:
  - Saves OLD-BONUS
  - Calculates base increase by REGION-CODE:
    - **EU**: CALC-BONUS = OLD-BONUS * 1.12
    - **NE**: CALC-BONUS = OLD-BONUS * 1.10
    - **AS**: CALC-BONUS = OLD-BONUS * 1.08
    - **SW**: CALC-BONUS = OLD-BONUS * 1.05
  - **IF YEAR-SALES >= 150000**:
    - Applies high-sales boost: CALC-BONUS = CALC-BONUS * 1.05
  - Determines STATUS:
    - **IF CALC-BONUS > 20000**: Caps at 20000, STATUS = 'CAP'
    - **ELSE IF CALC-BONUS < 2000**: STATUS = 'LOW'
    - **ELSE IF YEAR-SALES >= 150000**: STATUS = 'HIGHSAL'
    - **ELSE**: STATUS = 'OK'
  - Executes UPDATE TB_SALES_BONUS
  - Validates SQLCODE = 0 after UPDATE
  - Increments TOTAL-REC-UPDATED
  - Increments COMMIT-COUNT
  - Writes detail line to BONUS-REPORT
  - **IF COMMIT-COUNT >= 50**:
    - Executes COMMIT WORK
    - Displays "COMMIT AFTER {count} RECORDS"
    - Resets COMMIT-COUNT to 0

4. **Final Commit and Close**
- Executes COMMIT WORK
- Validates SQLCODE = 0
- Executes CLOSE CUR-BONUS
- Validates SQLCODE = 0 or -501

5. **Termination**
- Writes report footer: "TOTAL: nnn ROWS UPDATED"
- Closes BONUS-REPORT-FILE
- Validates OUT-STATUS = '00'
- Displays "BONUS UPDATE COMPLETED: {count}"
- Stops execution with proper return code

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
**Solution:** Verify BIND step CC 0000, check PLAN name in BIND and RUN steps match  
Verification: SELECT NAME FROM SYSIBM.SYSPLAN WHERE NAME = 'Z73460'

### Issue 2: SQLCODE -204 (Table not found)

**Cause:** TB_SALES_BONUS not created or wrong schema/database  
**Solution:** Verify table exists (SELECT COUNT(*) FROM TB_SALES_BONUS), check current schema (VALUES CURRENT SCHEMA), verify IN DATABASE clause matches your environment, check user privileges

### Issue 3: DCLGEN Mismatch

**Cause:** DCLGEN TASK8 copybook not found or structure mismatch  
**Solution:** Regenerate DCLGEN: DSN SYSTEM(DBDG) DCLGEN TABLE(TB_SALES_BONUS) LIBRARY(Z73460.DCLGEN) MEMBER(TASK8) LANGUAGE(COBOL) QUOTE  
Verify JCL COBOL.SYSLIB points to correct DCLGEN library

### Issue 4: VARCHAR Field Error

**Cause:** Incorrect handling of EMP-NAME VARCHAR field  
**Solution:** Use EMP-NAME-LEN (length) and EMP-NAME-TEXT (data) subfields as generated by DCLGEN  
Do not move directly to EMP-NAME group level

### Issue 5: S0C7 Data Exception

**Cause:** BONUS_AMT field alignment issue between DCLGEN and table  
**Solution:** Regenerate DCLGEN, verify BONUS_AMT declared as PIC S9(7)V9(2) USAGE COMP-3  
Ensure CALC-BONUS working storage variable uses same picture and usage

### Issue 6: Report File Not Allocated

**Cause:** OUTDD DD statement missing or incorrect DSN  
**Solution:** Verify RUNPROG step includes OUTDD DD statement, check TASK8.REPORT.FILE cataloged after execution, review SYSOUT for allocation errors

### Issue 7: Intermediate Commit Not Triggered

**Cause:** Dataset has fewer than 50 records  
**Solution:** This is normal for 10-record test dataset  
INTERMEDIATE COMMIT only occurs when COMMIT-COUNT reaches 50, final COMMIT always executes

### Issue 8: SQLCODE -501 (Cursor Already Closed)

**Cause:** Attempting to close already-closed cursor  
**Solution:** Program includes IF SQLCODE NOT = 0 AND SQLCODE NOT = -501 check  
This is normal if cursor closed by prior error, no action needed

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

- Program uses WITH HOLD cursor to maintain position across COMMIT operations
- UPDATE WHERE CURRENT OF performs positioned update on last fetched row
- Intermediate commits every 50 records balance performance and restart capability
- ROLLBACK on any error ensures data consistency (all-or-nothing within batch)
- Multi-factor calculation: base region increase, then high-sales boost, then cap check
- Status priority: CAP overrides HIGHSAL, LOW applies if final bonus under threshold
- BONUS_AMT and CALC_BONUS use COMP-3 (packed decimal) for DB2 DECIMAL column mapping
- Report uses edited numeric fields (ZZZZ9.99) for right-aligned decimal display
- VARCHAR field EMP_NAME handled via length prefix (EMP-NAME-LEN) and text area (EMP-NAME-TEXT)
- Tested on IBM z/OS with DB2 for z/OS and Enterprise COBOL

