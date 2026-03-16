# TASK19 - DB2 Bulk Insert: Customer Data Import with Validation

Batch customer import system that reads new customer records from a sequential file, applies three-level validation (ID presence, email format, phone format), and inserts valid records into DB2 table `TB_CUSTOMERS` using a batch commit strategy (COMMIT every 100 inserts). Demonstrates COBOL+DB2 precompile, VARCHAR host variables, SQLCA handling, and `IKJEFT01` execution model.

## Business Scenario

Bank operations team needs to bulk-load new customer records from an external flat file into the DB2 customer master table. Input data is unvalidated and may contain missing IDs, malformed emails, bad phone numbers, or duplicate primary keys. The system must:
- Read records from NEW.CUSTOMERS sequential file (PS, FB, LRECL=93)
- Validate each record before attempting DB2 INSERT
- Insert valid records into TB_CUSTOMERS with batch commit every 100 rows
- Log every record outcome (success or specific error) to SUCCESS.LOG
- ROLLBACK on critical DB2 errors (deadlock, timeout, critical SQLCODE)
- Display final import summary to SYSOUT

## Validation Rules

| Check | Field | Rule | Error Message |
|---|---|---|---|
| 1 | CUST_ID | Must not be SPACES | `VALIDATION ERROR: ID IS NULL` |
| 2 | EMAIL | Must contain exactly one `@` | `VALIDATION ERROR: INVALID EMAIL` |
| 3 | PHONE | Trimmed length must = 10 AND NUMERIC | `VALIDATION ERROR: INVALID PHONE` |

Validation is sequential: ID checked first, then EMAIL, then PHONE. If any check fails, record is skipped — no INSERT attempted.

## DB2 Table

```sql
CREATE TABLE TB_CUSTOMERS (
  CUST_ID      CHAR(6)       NOT NULL,
  CUST_NAME    VARCHAR(30),
  EMAIL        VARCHAR(40),
  PHONE        CHAR(10),
  CREDIT_LIMIT DECIMAL(9,2),
  PRIMARY KEY (CUST_ID)
) IN DATABASE Z73460;
```

See [SQL/CREATE.TABLE.sql](SQL/CREATE.TABLE.sql) for full DDL.

## SQLCODE Handling

| SQLCODE | Meaning | Action |
|---|---|---|
| 0 | Insert successful | REPORT-SUCCESS, increment COMMIT-COUNTER |
| -803 | Duplicate primary key | REPORT-DUPLICATE-KEY, count as error, continue |
| -911 | Deadlock/timeout (rollback by DB2) | ROLLBACK + STOP RUN |
| -913 | Deadlock/timeout (no rollback) | ROLLBACK + STOP RUN |
| < -900 | Critical system error | ROLLBACK + STOP RUN |
| Other | Non-critical DB2 error | REPORT-DB2-ERROR, count as error, continue |

## Files

### Input File

#### NEW.CUSTOMERS (PS) — New Customer Records

**Organization:** SEQUENTIAL  
**Record Format:** Fixed Block (RECFM=FB, LRECL=93)

**Record Layout:**
| Field | PIC | Length | Offset | Description |
|---|---|---|---|---|
| INP-ID | X(6) | 6 | 0 | Customer ID (primary key) |
| INP-NAME | X(30) | 30 | 6 | Customer full name |
| INP-EMAIL | X(40) | 40 | 36 | Email address |
| INP-PHONE | X(10) | 10 | 76 | Phone number (10 digits) |
| INP-LIMIT | 9(5)V99 | 7 | 86 | Credit limit (implied decimal) |

**Sample Data (20 records):** [DATA/NEW.CUSTOMER](DATA/NEW.CUSTOMER)

### Output Files

#### SUCCESS.LOG (PS) — Import Results Log

**Organization:** SEQUENTIAL  
**Record Format:** Variable Block (RECFM=VB, LRECL=84)

**Log line format:** `{CUST_ID} {MESSAGE}`

**Message types:**
- `INSERTED OK` — record successfully inserted
- `VALIDATION ERROR: ID IS NULL` — CUST_ID was blank
- `VALIDATION ERROR: INVALID EMAIL` — email had 0 or 2+ @ symbols
- `VALIDATION ERROR: INVALID PHONE` — phone not 10 numeric digits
- `DB2 ERROR: DUPLICATE PRIMARY KEY` — SQLCODE -803
- `DB2 ERROR: CHECK SQLCODE` — other non-critical DB2 error

**Expected output:** [DATA/SUCCESS.LOG](DATA/SUCCESS.LOG)

### Error Handling

**FILE STATUS Codes (INPUT-FILE and OUTPUT-FILE):**
- 00 - Successful operation
- Other codes - I/O errors on open, read, or write (program displays status and STOP RUN)

CLOSE errors treated as warnings (display only, no STOP RUN).

## Program Flow

1. **Initialization**
   - Opens INPUT-FILE and OUTPUT-FILE; validates FILE STATUS '00' on each

2. **Main Processing Loop**
   - Reads NEW.CUSTOMERS sequentially until EOF; increments RECORDS-PROCESSED per record
   - IF INP-ID = SPACES → REPORT-ID-ERROR, skip record
   - ELSE → VALIDATE-EMAIL (INSPECT TALLYING WS-EMAIL-COUNT FOR ALL "@"): if count ≠ 1 → REPORT-EMAIL-ERROR, skip
   - ELSE → VALIDATE-PHONE (FUNCTION LENGTH(FUNCTION TRIM(INP-PHONE)) = 10 AND NUMERIC): if fail → REPORT-PHONE-ERROR, skip
   - ELSE → INSERT-CUSTOMER: MOVE fields to host variables, calculate VARCHAR lengths, EXEC SQL INSERT; EVALUATE SQLCODE: 0 → REPORT-SUCCESS + batch COMMIT if COMMIT-COUNTER ≥ 100; -803 → REPORT-DUPLICATE-KEY; OTHER → REPORT-DB2-ERROR or ROLLBACK+STOP RUN if critical

3. **Termination**
   - Final COMMIT if COMMIT-COUNTER > 0; ROLLBACK + STOP RUN if final COMMIT fails
   - Closes both files (non-zero status on CLOSE is warning only)
   - Displays summary to SYSOUT: RECORDS PROCESSED / RECORDS INSERTED / RECORDS ERRORS / COMMIT BATCHES / UNCOMMITTED RECORDS
   - STOP RUN

## SQL Scripts

### 1. [CREATE.TABLE.sql](SQL/CREATE.TABLE.sql) - Create Customers Table

Creates TB_CUSTOMERS table with primary key on CUST_ID

### 2. [TB_CUSTOMERS.AFTER](DATA/TB_CUSTOMERS.AFTER) - Verify Inserted Data

Queries all rows from TB_CUSTOMERS after import run

## JCL Jobs

### 1. [JCL/COBDB2CP.jcl](JCL/COBDB2CP.jcl) - DB2 Precompile, Compile, and Execute

**Step 1:** Delete old SUCCESS.LOG (IEFBR14)  
**Step 2:** DB2 Precompile, COBOL Compile, Link, and BIND PLAN (DB2CBL proc) — converts EXEC SQL to CALL statements  
**Step 3:** Allocate STEPLIB and OUTDD (IKJEFT01) — sets up DB2 load library and results log dataset  
**Step 4:** Execute Program under DB2 control (DSN SYSTEM)

## How to Run

### Step 1: Create DB2 Table

**Execute** [SQL/CREATE.TABLE.sql](SQL/CREATE.TABLE.sql) via SPUFI or QMF

### Step 2: Load Input Data

**Allocate** and load `NEW.CUSTOMER` with [DATA/NEW.CUSTOMER](DATA/NEW.CUSTOMER)

### Step 3: Execute Import Program

**Submit** [JCL/COBDB2CP.jcl](JCL/COBDB2CP.jcl)  
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt)  
**Review** [DATA/SUCCESS.LOG](DATA/SUCCESS.LOG) for expected log output

### Step 4: Verify Results

**Query inserted rows:**

- `SELECT CUST_ID, CUST_NAME, EMAIL, PHONE, CREDIT_LIMIT FROM TB_CUSTOMERS ORDER BY CUST_ID`
- **Expected:** 10 rows (IDs: 000100, 000400, 000600, 000900, 001100, 001300, 001400, 001500, 001600, 001700)
- **Check SUCCESS.LOG** — 10 INSERTED OK, 10 errors (validation + duplicate)
- **Verify** [SQL/QUERY.OUTPUT](SQL/QUERY.OUTPUT.sql) via SPUFI matches [DATA/SUCCESS.LOG](DATA/SUCCESS.LOG)

## Common Issues

### Issue 1: SQLCODE -805 — Package or Plan Not Found

**Cause:** Program not bound to DB2 plan Z73460 — DB2CBL proc BIND step failed or was skipped  
**Solution:** Verify BIND step completed RC=0 in compile job; check SYSTSPRT output for bind errors

### Issue 2: IKJEFT01 Abend — DB2 Subsystem Not Available

**Cause:** `DSN SYSTEM(DBDG)` cannot attach to DB2 subsystem DBDG — subsystem down or wrong name  
**Solution:** Verify DB2 subsystem DBDG is active; confirm subsystem name matches your z/OS installation

### Issue 3: SQLCODE -818 — Timestamp Mismatch

**Cause:** Program recompiled but not rebound — precompile timestamp in load module does not match DBD  
**Solution:** Always rebind after recompile; DB2CBL proc includes BIND step automatically — verify it ran successfully

### Issue 4: SUCCESS.LOG LRECL Mismatch

**Cause:** OUTDD allocated with LRECL=80 instead of LRECL=84 for VB format  
**Solution:** VB records need 4-byte RDW prefix; LRECL must be data length + 4 = 80 + 4 = 84

### Issue 5: S0C7 on INP-LIMIT or HV-CREDIT

**Cause:** INP-LIMIT field in NEW.CUSTOMERS contains non-numeric data at bytes 87–93  
**Solution:** Verify NEW-CUSTOMER-INPUT credit limit field is exactly 7 numeric digits at offset 86; confirm PIC 9(5)V99 matches input layout

## Program Output (SYSOUT)

See [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution log.

## Notes

- DB2 precompiler converts `EXEC SQL` blocks into COBOL `CALL` statements before standard COBOL compile — this is why `DB2CBL` proc is needed instead of `MYCOMPGO`
- VARCHAR host variables require level-49 structure — level 49 is a special COBOL level used exclusively for VARCHAR mapping
- `FUNCTION REVERSE` + `INSPECT TALLYING FOR LEADING SPACES` pattern calculates effective string length without intrinsic LENGTH which counts all characters including trailing spaces
- SQLCODE -803 (duplicate key) is handled gracefully — logged and processing continues; only critical codes trigger ROLLBACK + STOP RUN
- Tested on IBM z/OS with DB2 subsystem DBDG and Enterprise COBOL
