# TASK20 - DB2 Upsert: Employee Sync (UPDATE or INSERT)

Employee data synchronization system that implements UPSERT logic: for each input record, a `SELECT` check determines whether the employee exists — if yes, `UPDATE` all fields; if no, `INSERT` new row. Extends TASK19 with bidirectional DB2 DML, old-vs-new salary comparison, date format conversion, and four-field validation. Batch commit every 50 successful operations.

## Business Scenario

HR department receives a daily employee sync file from an external HR system. Records may be new employees or updates to existing ones — the system must handle both without prior knowledge of which is which. The program must:
- Read employee records from EMP.UPDATE sequential file (PS, FB, LRECL=44)
- Validate each record: ID not empty, NAME not empty, SALARY > 0 and numeric, STATUS = 'A' or 'I'
- For valid records: SELECT from TB_EMPLOYEES to check existence
- If found (SQLCODE=0): UPDATE all fields, log salary change details
- If not found (SQLCODE=100): INSERT new row, log success
- COMMIT every 50 successful operations; ROLLBACK on critical errors
- Output full audit log to SYNC.LOG

## Validation Rules

| # | Field | Rule | Error Message |
|---|---|---|---|
| 1 | EMP_ID | Must not be SPACES | `ID VALIDATION ERROR: ID IS EMPTY` |
| 2 | EMP_NAME | Must not be SPACES | `VALIDATION ERROR: NAME IS EMPTY (ID=xxxxx)` |
| 3 | SALARY | Must be NUMERIC, > 0, not ZERO | `SALARY VALIDATION ERROR: NON-NUMERIC / ZERO VALUE` |
| 4 | STATUS | Must be `'A'` or `'I'` | `STATUS VALIDATION ERROR` |

All four validations run regardless — `WS-ERROR-FIND` flag accumulates failures. If any validation sets `ERROR-FIND`, PROCESS-EMPLOYEE is skipped and ADD 1 TO RECORDS-ERRORS is performed once.

## DB2 Table

```sql
CREATE TABLE TB_EMPLOYEES (
  EMP_ID    CHAR(5)       NOT NULL,
  EMP_NAME  VARCHAR(30),
  DEPT      CHAR(3),
  SALARY    DECIMAL(9,2),
  HIRE_DATE DATE,
  STATUS    CHAR(1),
  PRIMARY KEY (EMP_ID)
) IN DATABASE Z73460;
```

See [SQL/CREATE.TABLE.sql](SQL/CREATE.TABLE.sql) for full DDL.

## SQLCODE Handling

| SQLCODE | Meaning | Action |
|---|---|---|
| 0 | INSERT or UPDATE successful | log success, increment COMMIT-COUNTER |
| 100 | Employee not found (SELECT) | PERFORM INSERT-EMPLOYEE |
| -911 | Deadlock/timeout (rollback by DB2) | ROLLBACK + STOP RUN |
| -913 | Deadlock/timeout (no rollback) | ROLLBACK + STOP RUN |
| < -900 | Critical system error | ROLLBACK + STOP RUN |
| Other | Non-critical DB2 error | log error, count as error, continue |

## Files

### Input File

#### EMP.UPDATE (PS) — Employee Sync Records

**Organization:** SEQUENTIAL  
**Record Format:** Fixed Block (RECFM=FB, LRECL=44)

**Record Layout:**
| Field | PIC | Length | Offset | Description |
|---|---|---|---|---|
| INP-ID | X(5) | 5 | 0 | Employee ID (primary key) |
| INP-NAME | X(20) | 20 | 5 | Employee full name |
| INP-DEPT | X(3) | 3 | 25 | Department code (IT/HR/FIN/SAL) |
| INP-SALARY | S9(5)V99 | 7 | 28 | Salary (signed packed, implied decimal) |
| INP-HIRE-DATE | 9(8) | 8 | 35 | Hire date YYYYMMDD |
| INP-STATUS | X(1) | 1 | 43 | Employee status: A=Active, I=Inactive |

**Sample Data (24 records):** [DATA/EMP.UPDATE](DATA/EMP.UPDATE)

### Output Files

#### SYNC.LOG (PS) — Audit Log

**Organization:** SEQUENTIAL  
**Record Format:** Variable Block (RECFM=VB, LRECL=84)

**Log line format:** `{EMP_ID} {MESSAGE}`

**Message types:**
- `INSERT SUCCESS` — new employee inserted
- `UPDATE(SALARY CHANGE FROM {old} TO {new})` — updated, salary changed
- `UPDATED (NO SALARY CHANGE: {value})` — updated, no salary change
- `ID VALIDATION ERROR: ID IS EMPTY` — blank ID
- `VALIDATION ERROR: NAME IS EMPTY (ID=xxxxx)` — blank name
- `SALARY VALIDATION ERROR: NON-NUMERIC` — non-numeric salary field
- `SALARY VALIDATION ERROR: ZERO VALUE` — salary = 0
- `STATUS VALIDATION ERROR` — status not A or I

**Expected output:** [DATA/SYNC.LOG](DATA/SYNC.LOG)

#### TB_EMPLOYEES (DB2 Table) — Synced Employee Data

**Expected final state (15 rows):** [DATA/TB_EMPLOYEES.AFTER](DATA/TB_EMPLOYEES.AFTER)

### Error Handling

**FILE STATUS Codes (INP-FILE and OUT-FILE):**
- 00 - Successful operation
- Other codes - I/O errors on open, read, or write (program displays status and STOP RUN)

CLOSE errors treated as warnings (display only, no STOP RUN).

## Program Flow

1. **Initialization**
   - Opens INP-FILE and OUT-FILE; validates FILE STATUS '00' on each

2. **Main Processing Loop**
   - Reads EMP.UPDATE sequentially until EOF; increments RECORDS-PROCESSED per record
   - Runs all 4 validations: VALIDATE-ID → VALIDATE-NAME → VALIDATE-SALARY → VALIDATE-STATUS
   - `WS-ERROR-FIND` flag set on any failure; all errors logged; single ADD 1 TO RECORDS-ERRORS if any failed
   - If no errors: PERFORM PROCESS-EMPLOYEE → SELECT to check existence → UPDATE or INSERT
   - After each successful operation: ADD 1 TO COMMIT-COUNTER; if COMMIT-COUNTER ≥ 50 → COMMIT WORK, ADD 1 TO COMMIT-BATCHES, reset counter

3. **Termination**
   - Final COMMIT if COMMIT-COUNTER > 0; ROLLBACK + STOP RUN if final COMMIT fails
   - Closes both files (non-zero status on CLOSE is warning only)
   - Displays summary to SYSOUT: RECORDS PROCESSED / INSERTED / UPDATED / ERRORS / COMMIT BATCHES / UNCOMMITTED RECORDS
   - STOP RUN

## SQL Scripts

### 1. [CREATE.TABLE.sql](SQL/CREATE.TABLE.sql) - Create Employees Table

Creates TB_EMPLOYEES table with primary key on EMP_ID

### 2. [DATA/TB_EMPLOYEES.AFTER](DATA/TB_EMPLOYEES.AFTER) - Verify Synced Data

Queries all rows from TB_EMPLOYEES after sync run

## JCL Jobs

### 1. [COBDB2CP.jcl](JCL/COBDB2CP.jcl) - DB2 Precompile, Compile, and Execute

**Step 1:** Delete old SYNC.LOG (IEFBR14)  
**Step 2:** DB2 Precompile, COBOL Compile, Link, and BIND PLAN (DB2CBL proc) — converts EXEC SQL to CALL statements  
**Step 3:** Allocate STEPLIB and OUTDD (IKJEFT01) — sets up DB2 load library and sync log dataset  
**Step 4:** Execute Program under DB2 control (DSN SYSTEM)

## How to Run

### Step 1: Create DB2 Table

**Execute** [SQL/CREATE.TABLE.sql](SQL/CREATE.TABLE.sql) via SPUFI or QMF

### Step 2: Load Input Data

**Allocate** and load `EMP.UPDATE` with [DATA/EMP.UPDATE](DATA/EMP.UPDATE)

### Step 3: Execute Sync Program

**Submit** [JCL/COBDB2CP.jcl](JCL/COBDB2CP.jcl)  
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt)  
**Review** [DATA/SYNC.LOG](DATA/SYNC.LOG) for expected audit log

### Step 4: Verify Results

**Query synced table:**

- `SELECT EMP_ID, EMP_NAME, DEPT, SALARY, HIRE_DATE, STATUS FROM TB_EMPLOYEES ORDER BY EMP_ID`
- **Expected:** 15 rows; 00100/00200/00300 show updated salary values
- **Check SYNC.LOG** — 15 INSERT SUCCESS, 3 UPDATE messages, 6 validation errors
- **Compare** [DATA/TB_EMPLOYEES.AFTER](DATA/TB_EMPLOYEES.AFTER) vs actual query result

> **To re-run cleanly**, truncate TB_EMPLOYEES first:
> ```sql
> DELETE FROM TB_EMPLOYEES;
> COMMIT;
> ```

## Common Issues

### Issue 1: SQLCODE -805 — Package or Plan Not Found

**Cause:** DB2CBL proc BIND step failed or was skipped  
**Solution:** Verify BIND step completed RC=0; check SYSTSPRT output for bind errors

### Issue 2: IKJEFT01 Abend — DB2 Subsystem Not Available

**Cause:** `DSN SYSTEM(DBDG)` cannot attach to DB2 subsystem DBDG  
**Solution:** Verify DB2 subsystem DBDG is active; confirm subsystem name matches your z/OS installation

### Issue 3: SQLCODE -818 — Timestamp Mismatch

**Cause:** Program recompiled but not rebound — precompile timestamp in load module does not match DBRM  
**Solution:** DB2CBL proc includes BIND step automatically — verify it ran successfully after recompile

### Issue 4: SYNC.LOG LRECL Mismatch

**Cause:** OUTDD allocated with LRECL=80 instead of LRECL=84 for VB format  
**Solution:** VB records need 4-byte RDW prefix; LRECL must be data length + 4 = 80 + 4 = 84

### Issue 5: S0C7 on INP-SALARY or HV-SALARY

**Cause:** INP-SALARY field contains non-numeric data — NUMERIC check in VALIDATE-SALARY must run before MOVE to host variable  
**Solution:** Verify VALIDATE-SALARY runs before PROCESS-EMPLOYEE; confirm PIC S9(5)V99 matches input layout at offset 28

### Issue 6: DATE Conversion Abend

**Cause:** INP-HIRE-DATE contains spaces or non-numeric data — STRING into HV-HIRE-DATE produces invalid ISO date  
**Solution:** Verify input HIRE_DATE field is exactly 8 numeric digits at offset 35; DB2 rejects malformed date with SQLCODE -180

## Program Output (SYSOUT)

See [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution log.

## Notes

- UPSERT pattern: SELECT first, then branch on SQLCODE — avoids duplicate key errors and unnecessary blind UPDATEs
- `SQLCODE=100` from SELECT means "not found" (same as AT END for cursors) — used here as the INSERT trigger
- Two sets of host variables needed: `HV-*` for new data, `HV-OLD-*` for SELECT result; mixing them would overwrite old values before comparison
- DATE host variable stored as `PIC X(10)` — DB2 accepts ISO string `YYYY-MM-DD` directly for DATE columns
- `WS-ERROR-FIND` flag approach: all 4 validations run, flag set on any failure — logs all errors per record, not just the first
- Batch size 50 (vs 100 in TASK19): for mixed INSERT/UPDATE workloads, smaller batches reduce rollback exposure
- DB2 precompiler converts `EXEC SQL` blocks into COBOL `CALL` statements before standard COBOL compile — this is why `DB2CBL` proc is needed instead of `MYCOMPGO`
- Tested on IBM z/OS with DB2 subsystem DBDG and Enterprise COBOL
