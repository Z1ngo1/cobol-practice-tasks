# TASK07 - DB2 Employee Salary Indexing

Employee salary indexing system using DB2 cursor operations and batch updates with periodic commits.

## Business Scenario

Company performs annual salary indexing based on department policy. The system must: 
- Read employee salary data from DB2 table TB_EMP_SALARY
- Apply salary increases by department (IT +10%, SAL +5%, OTHER +3%)
- Apply maximum salary cap at 100000
- Update DB2 table with new salaries using positioned cursor updates
- Generate detailed salary change report
- Commit every 100 records for optimal performance and rollback safety

## Files

### Input Files

#### 1. TB_EMP_SALARY (DB2 Table) - Employee Salary Master

**Sample Data:** [DATA/TB.EMP.SALARY.BEFORE](DATA/TB.EMP.SALARY.BEFORE)

**DCLGEN Structure:** 

**Sample Data:** [DCLGEN/TASK7.cpy](DCLGEN/TASK7.cpy)

### Output Files

#### 2. SALARY-REPORT (PS) - Salary Change Report

**Access Mode:** 
- OUTPUT (Sequential)
**Organization:**
- SEQUENTIAL
**Record Format:**
- Fixed (RECFM=F, LRECL=80)

**Expected Output:** [DATA/SALARY.REPORT](DATA/SALARY.REPORT)

#### 3. TB_EMP_SALARY (DB2 Table) - Updated Employee Data

Same table as input, updated in place with new salary values

**Expected Final State:** [DATA/TB.EMP.SALARY.AFTER](DATA/TB.EMP.SALARY.AFTER)

### Salary Indexing Logic

**Department-Based Increases:** 
- IT Department: NEW_SALARY = CURRENT_SALARY × 1.10 (+10%)
- SAL Department: NEW_SALARY = CURRENT_SALARY × 1.05 (+5%)
- All Other Departments: NEW_SALARY = CURRENT_SALARY × 1.03 (+3%)

**Maximum Cap Rule:** 
- If NEW_SALARY > 100000, cap at 100000 and set STATUS = MAXCAP
- Otherwise, set STATUS = OK

**Examples:** 
- EMP 00010, IT, 50000.00 → 50000 × 1.10 = 55000.00 (OK)
- EMP 00040, IT, 95000.00 → 95000 × 1.10 = 104500 → 100000.00 (MAXCAP) 
- EMP 00030, SAL, 60000.00 → 60000 × 1.05 = 63000.00 (OK)
- EMP 00020, HR, 45000.00 → 45000 × 1.03 = 46350.00 (OK)

### DB2 Objects

#### Cursor: CUR-SALARY

**Declaration:** 
- DECLARE CUR-SALARY CURSOR WITH HOLD FOR SELECT * FROM TB_EMP_SALARY FOR UPDATE OF SALARY

**WITH HOLD:** 
- Cursor remains open after COMMIT (essential for batch processing)

**FOR UPDATE OF SALARY:** 
- Enables positioned UPDATE WHERE CURRENT OF

### Error Handling

**SQLCODE Values:** 
- 0 - Successful operation
- +100 - No more rows (end of data)
- -805 - PLAN not found (BIND error)
- -204 - Table not found
- Other negative codes - SQL errors (program rolls back and stops)

**FILE STATUS Codes (Report File):** 
- 00 - Successful operation
- Other codes - I/O errors (program rolls back and stops)

## Program Flow

1. **Initialization:**
- Opens SALARY-REPORT-FILE (PS) for output
- Validates OUT-STATUS = '00'
- Writes report header line
- Initializes counters (TOTAL-RECORDS-UPDATED = 0, COMMIT-COUNT = 0)

2. **Open DB2 Cursor:**
- Executes OPEN CUR-SALARY
- Validates SQLCODE = 0

3. **Fetch and Process Loop**
- Performs FETCH NEXT until SQLCODE = 100 (EOF)
- For each employee record fetched:
  - Saves OLD-SALARY
  - Calculates NEW-SALARY based on DEPT-CODE:
    - **IT**: NEW-SALARY = OLD-SALARY * 1.10
    - **SAL**: NEW-SALARY = OLD-SALARY * 1.05
    - **OTHER**: NEW-SALARY = OLD-SALARY * 1.03
  -  **IF NEW-SALARY > 100000**:
    -  Caps at 100000
    -  Sets STATUS = 'MAXCAP'
  - Executes UPDATE TB_EMP_SALARY
  - Validates SQLCODE = 0 after UPDATE
  - Increments TOTAL-RECORDS-UPDATED
  - Increments COMMIT-COUNT
  - Writes detail line to SALARY-REPORT
  - **IF COMMIT-COUNT >= 100**:
    - Executes COMMIT WORK
    - Displays "INTERMEDIATE COMMIT AT: {count}"
    - Resets COMMIT-COUNT to 0

4. **Final Commit and Close**
- Executes COMMIT WORK
- Validates SQLCODE = 0
- Executes CLOSE CUR-SALARY
- Validates SQLCODE = 0

5. **Termination**
- Writes report footer: "TOTAL: nnn RECORDS UPDATED"
- Closes SALARY-REPORT-FILE
- Validates OUT-STATUS = '00'
- Displays "SALARY INDEXING COMPLETED: {count}"
- STOP RUN
  
## SQL Scripts

### 1. [CREATE.TABLE.sql](SQL/CREATE.TABLE.sql) - Create Employee Table

Creates TB_EMP_SALARY table with primary key on EMP_ID

### 2. [INSERT.DATA.sql](SQL/INSERT.DATA.sql) - Load Initial Data

Inserts 10 employee records with varying departments and salaries 
Coverage: IT=4, SAL=3, HR=2, FIN=1

## JCL Jobs

### 1. [COBDB2CP.jcl](JCL/COBDB2CP.jcl) - DB2 Precompile, Compile, Bind, and Execute

**Step 1:** Delete old DBRM (IEFBR14)  
**Step 2:** DB2 Precompile, COBOL Compile, Link, and BIND PLAN - converts EXEC SQL to CALL statements  
**Step 3:** Allocate STEPLIB and OUTDD (IKJEFT01) - sets up DB2 load library and report dataset  
**Step 4:** Execute Program under DB2 control  

## How to Run

### Step 1: Create DB2 Table

**Execute** [SQL/CREATE.TABLE.sql](SQL/CREATE.TABLE.sql) via SPUFI or QMF

### Step 2: Load Initial Employee Data

**Execute** [SQL/INSERT.DATA.sql](SQL/INSERT.DATA.sql) via SPUFI or QMF  
**See** [DATA/TB.EMP.SALARY.BEFORE](DATA/TB.EMP.SALARY.BEFORE).

### Step 3: Execute Salary Indexing Program

**Submit** [JCL/COBDB2CP.jcl](JCL/COBDB2CP.jcl).   
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt)  
**Review** [DATA/SALARY.REPORT](DATA/SALARY.REPORT) for detailed report

### Step 4: Verify Results

**Query updated table:**  

- SELECT EMP_ID, NAME, DEPT_CODE, SALARY FROM TB_EMP_SALARY ORDER BY EMP_ID.  
- **Compare** Before [DATA/TB.EMP.SALARY.BEFORE](DATA/TB.EMP.SALARY.BEFORE) vs After [DATA/TB.EMP.SALARY.AFTER](DATA/TB.EMP.SALARY.AFTER).  
- **Verify calculations** IT × 1.10, SAL × 1.05, OTHER × 1.03, cap at 100000.   
- **Check report** Header, 10 detail lines, STATUS (OK or MAXCAP), Footer with total count.

## Common Issues

### Issue 1: SQLCODE -805 (PLAN not found)

**Cause:** BIND PLAN not executed or PLAN name mismatch.   
**Solution:** Verify BIND step CC 0000, check PLAN name in BIND and RUN steps match.   
Verification: SELECT NAME FROM SYSIBM.SYSPLAN WHERE NAME = 'DB2TASK7'.

### Issue 2: SQLCODE -204 (Table not found)

**Cause:** TB_EMP_SALARY not created or wrong schema.   
**Solution:** Verify table exists (SELECT COUNT(*) FROM TB_EMP_SALARY), check current schema (VALUES CURRENT SCHEMA), verify user privileges.

### Issue 3: S0C7 Data Exception

**Cause:** SALARY field alignment issue between DCLGEN and table.   
**Solution:** Regenerate DCLGEN using DSNHD utility, verify SALARY declared as PIC S9(7)V99 COMP-3.

### Issue 4: Report File Not Allocated

**Cause:** OUTDD DD statement missing or incorrect DSN.  
**Solution:** Verify COBDB2CP.jcl RUN step includes OUTDD DD statement, check DSN exists after execution, review SYSOUT for error messages.

### Issue 5: Intermediate Commit Not Triggered

**Cause:** Dataset has fewer than 100 records.   
**Solution:** This is normal for 10-record test dataset. 
INTERMEDIATE COMMIT only occurs when COMMIT-COUNT reaches 100, final COMMIT always executes.

## Program Output (SYSOUT)

Expected execution log - see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for full output example

**Normal Execution (10 records):** 
- SALARY INDEXING COMPLETED: 010

**Large Dataset (100 records):** 
- INTERMEDIATE COMMIT AT: 100. SALARY INDEXING COMPLETED: 100

## Notes

- Program uses WITH HOLD cursor to maintain position across COMMIT operations
- UPDATE WHERE CURRENT OF performs positioned update on last fetched row
- Intermediate commits every 100 records balance performance and restart capability
- ROLLBACK on any error ensures data consistency
- SALARY field uses COMP-3 (packed decimal) for DB2 DECIMAL column mapping
- Report uses edited numeric fields (Z(7).99) for right-aligned decimal display
- Tested on IBM z/OS with DB2 for z/OS and Enterprise COBOL
