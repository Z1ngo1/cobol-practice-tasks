# Task 29 — ESDS Operations Log + Master Reconciliation

## Overview
This task simulates a real-world banking audit scenario. A nightly batch program reconciles an online ESDS operations log against a VSAM KSDS master file and a DB2 database. The program validates transaction formats, verifies account existence, and ensures business logic consistency without updating the records.

---

## Data Sources

| Source | Name | Organization | Mode | Description |
|---|---|---|---|---|
| **ESDS Log** | `OPR.LOG.ESDS` | ESDS | INPUT | Daily operations journal (Technical Log) |
| **KSDS Master**| `ACCT.MASTER` | KSDS | INPUT | Fast-access account master file |
| **DB2 Table** | `TB_ACCOUNT_BAL`| DB2 | SELECT | Current balances in relational database |
| **Report** | `RECON.LOG` | PS | OUTPUT | Reconciliation discrepancy report |

---

## Record Layouts

### Input: `OPR.LOG.ESDS`
- **OPR-ACCT-ID**: Account Identifier.
- **OPR-TYPE**: Operation Type ('D' = Debit, 'C' = Credit).
- **OPR-AMT**: Transaction Amount (Must be > 0).

### VSAM KSDS: `ACCT.MASTER`
- **ACCT-MASTER-ID**: Key.
- **STATUS**: Account Status ('C' = Closed, 'A' = Active).

---

## Business Logic

### 1. Global Counters
The program tracks the following metrics in `WORKING-STORAGE`:
- `WS-TOTAL-READ`: Total records processed.
- `WS-OK-COUNT`: Successfully reconciled operations.
- `WS-ERR-COUNT`: Formatting or logic errors found.
- `WS-SKIP-COUNT`: Records skipped (e.g., closed accounts).

### 2. Validation Steps
For each operation in the ESDS log:
1. **Format Check**:
   - `OPR-TYPE` must be 'D' or 'C'.
   - `OPR-AMT` must be greater than 0.
   - *On failure*: Log to `RECON.LOG`, increment `WS-ERR-COUNT`.
2. **KSDS Master Lookup**:
   - Search for `OPR-ACCT-ID` in `ACCT.MASTER`.
   - *If File Status = '23'* (Not Found): Log error, increment `WS-ERR-COUNT`.
   - *If Status = 'C'* (Closed): Increment `WS-SKIP-COUNT` and move to next record.
3. **DB2 Balance Query**:
   - `SELECT BALANCE FROM TB_ACCOUNT_BAL WHERE ACCT_ID = :OPR-ACCT-ID`.
   - *If SQLCODE = 100* (Not Found): Log error, increment `WS-ERR-COUNT`.
   - *If SQLCODE < 0*: Handle DB2 technical error.
4. **Reconciliation Logic**:
   - **Debit ('D')**: Check if `DB2-BALANCE-BEFORE >= OPR-AMT`.
     - *If Balance < Amount*: Log "NEGATIVE BALANCE AFTER OPR" error.
   - **Credit ('C')**: Generally accepted.

---

## Program Flow
- **STEP-01**: Open all files (ESDS as sequential, KSDS as random).
- **STEP-02**: Read ESDS log sequentially until EOF.
- **PROCESS-ONE-OPR**: Execute the multi-stage validation (Format -> KSDS -> DB2 -> Logic).
- **STEP-03**: Close files and display summary counters in SYSOUT.

---

## Key Learning Objectives
- **Multi-Source Reconciliation**: Combining VSAM (ESDS, KSDS) and DB2 in a single COBOL program.
- **VSAM Random Access**: Using `READ ... KEY IS ...` for KSDS lookups.
- **DB2 Integration**: Executing singleton SELECTs and handling SQL return codes.
- **Error Logging**: Implementing a systematic way to track and report discrepancies in a production-like environment.
