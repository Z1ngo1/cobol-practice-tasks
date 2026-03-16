# TASK20 - DB2 Upsert: Employee Sync (UPDATE or INSERT)

Employee data synchronization system that implements UPSERT logic: for each input record, a `SELECT` check determines whether the employee exists — if yes, `UPDATE` all fields; if no, `INSERT` new row. Extends TASK19 with bidirectional DB2 DML, old-vs-new salary comparison, date format conversion, and four-field validation. Batch commit every 50 successful operations.

## Business Scenario

HR department receives a daily employee sync file from an external HR system. Records may be new employees or updates to existing ones — the system must handle both without prior knowledge of which is which. The program must:
- Read employee records from EMP.UPDATE sequential file (PS, FB, LRECL=44)
- Validate each record: ID not empty, NAME not empty, SALARY > 0 and numeric, STATUS = 'A' or 'I'
- For valid records: SELECT from TB_EMPLOYEES to check existence
- If found (SQLCODE=0): UPDATE all fields, log salary change details if salary changed
- If not found (SQLCODE=100): INSERT new row, log success
- COMMIT every 50 successful operations; ROLLBACK on critical errors
- Output full audit log to SYNC.LOG

## Upsert Flow

```
For each valid input record:
  EXEC SQL SELECT EMP_NAME, SALARY
           FROM TB_EMPLOYEES
           WHERE EMP_ID = :HV-EMP-ID

  SQLCODE = 0   → employee EXISTS   → PERFORM UPDATE-EMPLOYEE
  SQLCODE = 100 → employee NOT FOUND → PERFORM INSERT-EMPLOYEE
  SQLCODE = other → log SELECT error, count as error, skip
```

## Validation Rules (All Four Must Pass)

| # | Field | Rule | Error Message |
|---|---|---|---|
| 1 | EMP_ID | Must not be SPACES | `ID VALIDATION ERROR: ID IS EMPTY` |
| 2 | EMP_NAME | Must not be SPACES | `VALIDATION ERROR: NAME IS EMPTY (ID=xxxxx)` |
| 3 | SALARY | Must be NUMERIC, > 0, not ZERO | `SALARY VALIDATION ERROR: NON-NUMERIC / NEGATIVE VALUE / ZERO VALUE` |
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

See [SQL/CREATE-TABLE.sql](SQL/CREATE-TABLE.sql) for full DDL.

## DB2 Host Variables

```cobol
* Current values (for INSERT/UPDATE)
01 HV-EMP-ID       PIC X(5).
01 HV-EMP-NAME.
   49 HV-EMP-NAME-LEN  PIC S9(4) COMP-5.
   49 HV-EMP-NAME-TEXT PIC X(20).
01 HV-EMP-DEPT     PIC X(3).
01 HV-SALARY       PIC S9(7)V99 COMP-3.
01 HV-HIRE-DATE    PIC X(10).
01 HV-STATUS       PIC X(1).

* Old values (fetched by SELECT, used for salary comparison)
01 HV-OLD-NAME.
   49 HV-OLD-NAME-LEN  PIC S9(4) COMP-5.
   49 HV-OLD-NAME-TEXT PIC X(20).
01 HV-OLD-SALARY   PIC S9(7)V99 COMP-3.
```

Two sets of host variables:
- `HV-*` — new values from input file, used for INSERT/UPDATE
- `HV-OLD-*` — old values fetched from DB2 by SELECT, used to detect salary change

## Date Format Conversion

Input file stores HIRE_DATE as `YYYYMMDD` (8-digit numeric). DB2 `DATE` column requires `YYYY-MM-DD` (10-char string). Conversion in `FORMAT-HIRE-DATE`:

```cobol
MOVE INP-HIRE-DATE(1:4) TO WS-YEAR
MOVE INP-HIRE-DATE(5:2) TO WS-MONTH
MOVE INP-HIRE-DATE(7:2) TO WS-DAY
STRING WS-YEAR '-' WS-MONTH '-' WS-DAY INTO HV-HIRE-DATE
```

- `HV-HIRE-DATE PIC X(10)` holds the formatted result
- DB2 accepts string literal in ISO date format `YYYY-MM-DD` for DATE columns

## Update Logic: Salary Change Detection

When UPDATE succeeds (SQLCODE=0):
- `HV-OLD-SALARY` (fetched by SELECT) is compared to `HV-SALARY` (new value from input)
- If different: log `UPDATE(SALARY CHANGE FROM {old} TO {new})`
- If same: log `UPDATED (NO SALARY CHANGE: {value})`

This gives a full audit trail showing exactly what changed.

## Batch Commit Strategy

```
After each successful INSERT or UPDATE:
  ADD 1 TO COMMIT-COUNTER

  IF COMMIT-COUNTER >= 50:
    EXEC SQL COMMIT WORK END-EXEC
    → IF SQLCODE ≠ 0: ROLLBACK + STOP RUN
    ADD 1 TO COMMIT-BATCHES
    MOVE 0 TO COMMIT-COUNTER

At CLOSE-ALL-FILES (final commit):
  IF COMMIT-COUNTER > 0:
    EXEC SQL COMMIT WORK END-EXEC
    ADD 1 TO COMMIT-BATCHES
```

Batch size is 50 (vs 100 in TASK19) — smaller batch = more frequent commits = less rollback exposure for update-heavy workloads.

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

**Sample Data (24 records):** [DATA/EMP-UPDATE-INPUT](DATA/EMP-UPDATE-INPUT)

**Intentional test cases:**
- Empty EMP_ID (2 records — spaces in ID field)
- Empty EMP_NAME (1 record — ID=00500, name = spaces)
- Non-numeric SALARY (`XXXXXXX` — 2 records: 00700, 01100)
- Zero SALARY (1 record — ID=00800)
- Invalid STATUS = `'X'` (1 record — ID=01300)
- Duplicate IDs that trigger UPDATE: 00100 (salary: 5000→7500), 00200 (salary: 4500→6500), 00300 (salary: 6000→8500, status: I→A)

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

**Expected output:** [DATA/SYNC-LOG-EXPECTED](DATA/SYNC-LOG-EXPECTED)

#### DB2 Table State After Run

**Expected query result (15 rows):** [DATA/DB2-TABLE-EXPECTED](DATA/DB2-TABLE-EXPECTED)

## Test Results Trace (24 records)

| # | ID | Validation | DB2 Action | Log Message |
|---|---|---|---|---|
| 1 | 00100 | ✓ | INSERT | INSERT SUCCESS |
| 2 | 00200 | ✓ | INSERT | INSERT SUCCESS |
| 3 | 00300 | ✓ | INSERT (STATUS=I) | INSERT SUCCESS |
| 4 | (blank) | ✗ ID | skip | ID VALIDATION ERROR: ID IS EMPTY |
| 5 | 00400 | ✓ | INSERT | INSERT SUCCESS |
| 6 | 00500 | ✗ NAME | skip | VALIDATION ERROR: NAME IS EMPTY (ID=00500) |
| 7 | 00600 | ✓ | INSERT | INSERT SUCCESS |
| 8 | 00700 | ✗ SALARY | skip | SALARY VALIDATION ERROR: NON-NUMERIC |
| 9 | 00800 | ✗ SALARY | skip | SALARY VALIDATION ERROR: ZERO VALUE |
| 10 | 00900 | ✓ | INSERT | INSERT SUCCESS |
| 11 | 00100 | ✓ | UPDATE | UPDATE(SALARY CHANGE FROM 5000.00 TO 7500.00) |
| 12 | 01000 | ✓ | INSERT | INSERT SUCCESS |
| 13 | 01100 | ✗ SALARY | skip | SALARY VALIDATION ERROR: NON-NUMERIC |
| 14 | 01200 | ✓ | INSERT | INSERT SUCCESS |
| 15 | 00200 | ✓ | UPDATE | UPDATE(SALARY CHANGE FROM 4500.00 TO 6500.00) |
| 16 | 01300 | ✗ STATUS | skip | STATUS VALIDATION ERROR |
| 17 | 01400 | ✓ | INSERT | INSERT SUCCESS |
| 18 | 00300 | ✓ | UPDATE | UPDATE(SALARY CHANGE FROM 6000.00 TO 8500.00) |
| 19 | 01500 | ✓ | INSERT | INSERT SUCCESS |
| 20 | 01600 | ✓ | INSERT | INSERT SUCCESS |
| 21 | 01700 | ✓ | INSERT | INSERT SUCCESS |
| 22 | 01800 | ✓ | INSERT | INSERT SUCCESS |
| 23 | 01900 | ✓ | INSERT | INSERT SUCCESS |
| 24 | 02000 | ✓ | INSERT | INSERT SUCCESS |

**Result: 15 inserted, 3 updated, 6 errors — 1 COMMIT batch (18 ops < 50 threshold)**

## TASK19 vs TASK20: DB2 Patterns

| Pattern | TASK19 | TASK20 |
|---|---|---|
| DML operations | INSERT only | INSERT + UPDATE + SELECT |
| Upsert logic | No (duplicate = error) | Yes (SELECT → decide) |
| Old value tracking | No | Yes (HV-OLD-NAME, HV-OLD-SALARY) |
| Date formatting | No | Yes (YYYYMMDD → YYYY-MM-DD) |
| Validation fields | 3 (ID, email, phone) | 4 (ID, name, salary, status) |
| Batch commit size | 100 | 50 |
| SQLCODE -803 | Handled (duplicate key) | Not needed (upsert prevents it) |
| VARCHAR columns | 2 (CUST_NAME, EMAIL) | 1 (EMP_NAME) |

## JCL Jobs

### 1. [COBDB2CP.jcl](JCL/COBDB2CP.jcl) - DB2 Precompile, Compile, and Execute

**Step 1:** Delete old SYNC.LOG (IEFBR14)  
**Step 2:** DB2 Precompile, COBOL Compile, Link, and BIND PLAN (DB2CBL proc) — converts EXEC SQL to CALL statements  
**Step 3:** Allocate STEPLIB and OUTDD (IKJEFT01) — sets up DB2 load library and sync log dataset  
**Step 4:** Execute Program under DB2 control (DSN SYSTEM)

## How to Run

### Option A: Compile + Run (Recommended)

**Prerequisites:**
1. TB_EMPLOYEES table must exist — run [SQL/CREATE-TABLE.sql](SQL/CREATE-TABLE.sql) in SPUFI
2. Table must be **empty** before first run (otherwise initial inserts will become updates)
3. Input dataset Z73460.TASK20.EMP.UPDATE allocated and loaded with [DATA/EMP-UPDATE-INPUT](DATA/EMP-UPDATE-INPUT)

Submit [JCL/COBDB2CP.jcl](JCL/COBDB2CP.jcl)

### Option B: Run Only (After Compile)

Ensure SYNC.LOG deleted, then submit RUNPROG step manually.

> **Note:** To re-run cleanly, truncate TB_EMPLOYEES first:
> ```sql
> DELETE FROM TB_EMPLOYEES;
> COMMIT;
> ```

### Verify Results

- Browse SYNC.LOG — compare with [DATA/SYNC-LOG-EXPECTED](DATA/SYNC-LOG-EXPECTED)
- Run SELECT in SPUFI — compare with [DATA/DB2-TABLE-EXPECTED](DATA/DB2-TABLE-EXPECTED)
- Check SYSOUT — compare with [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt)

**Expected: 15 rows in TB_EMPLOYEES, 3 with updated salary values**

## Program Output (SYSOUT)

See [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution log.

```
========================================
EMPLOYEE UPSERT SUMMARY
========================================
RECORDS PROCESSED:      24
RECORDS INSERTED:       15
RECORDS UPDATED:         3
RECORDS ERRORS:          6
COMMIT BATCHES:          1
UNCOMMITTED RECORDS:     0
========================================
```

## Notes

- UPSERT pattern: SELECT first, then branch on SQLCODE — avoids duplicate key errors and unnecessary blind UPDATEs
- `SQLCODE=100` from SELECT means "not found" (same as AT END for cursors) — used here as the INSERT trigger
- Two sets of host variables needed: `HV-*` for new data, `HV-OLD-*` for SELECT result; mixing them would overwrite old values before comparison
- DATE host variable stored as `PIC X(10)` (not DATE type in COBOL) — DB2 accepts ISO string `YYYY-MM-DD` directly
- `WS-ERROR-FIND` flag approach: all 4 validations run, flag is set on first failure — this logs all validation errors per record, not just the first one
- Batch size 50 (vs 100 in TASK19): for mixed INSERT/UPDATE workloads with potential conflicts, smaller batches reduce rollback exposure
- `COND=(4,LT)` on RUNPROG ensures execution only after successful compile
- No VSAM, no sort — pure DB2 DML with PS I/O
- Tested on IBM z/OS with DB2 subsystem DBDG and Enterprise COBOL
