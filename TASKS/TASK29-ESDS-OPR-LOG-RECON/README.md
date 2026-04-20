# Task 29 — ESDS Operations Log + Master Reconciliation

## Overview
This task simulates a rigorous audit process for a banking system. The program performs a multi-source reconciliation, ensuring that daily transactions logged in an **ESDS** file align correctly with the **VSAM KSDS** master file and the **DB2** relational database balances. This is a \"read-only\" audit program designed to identify and log discrepancies without modifying any data.

---

## Data Sources

| Source | Name | Organization | Mode | Description |
|---|---|---|---|---|
| **Operations Log** | `OPR.LOG.ESDS` | ESDS | INPUT | Daily technical log of all transactions |
| **Account Master** | `ACCT.MASTER` | KSDS | INPUT | Fast-access account status file |
| **Balance DB** | `TB_ACCOUNT_BAL`| DB2 | SELECT | System of record for current balances |
| **Audit Report** | `RECON.LOG` | PS | OUTPUT | Detailed log of reconciliation errors |

---

## Record Layouts

### Input: `OPR.LOG.ESDS` (100 bytes)
| Field | Picture | Offset | Description |
|---|---|---|---|
| `OPR-ACCT-ID` | `X(10)` | 1 | Account Number |
| `OPR-TYPE` | `X(1)` | 11 | Type ('D' = Debit, 'C' = Credit) |
| `OPR-AMT` | `9(7)V99` | 12 | Operation Amount |
| `FILLER` | `X(80)` | 21 | Padding |

### VSAM KSDS: `ACCT.MASTER` (50 bytes)
| Field | Picture | Offset | Description |
|---|---|---|---|
| `ACCT-MASTER-ID` | `X(10)` | 1 | Key: Account Number |
| `STATUS` | `X(1)` | 11 | 'A' = Active, 'C' = Closed |
| `FILLER` | `X(39)` | 12 | Padding |

### Output: `RECON.LOG` (133 bytes)
| Field | Picture | Offset | Description |
|---|---|---|---|
| `ERR-ACCT-ID` | `X(10)` | 1 | Account involved |
| `ERR-MSG` | `X(50)` | 12 | Description of the discrepancy |
| `ERR-TIMESTAMP`| `X(20)` | 63 | Time of detection |

---

## Business Logic

### 1. Global Counters (`WORKING-STORAGE`)
- `WS-TOTAL-READ`: Incremented for every record read from the ESDS log.
- `WS-OK-COUNT`: Incremented when all checks pass.
- `WS-ERR-COUNT`: Incremented for any validation, lookup, or logic failure.
- `WS-SKIP-COUNT`: Incremented for accounts marked as 'Closed' in KSDS.

### 2. Processing Steps (`PROCESS-ONE-OPR`)

#### Phase A: Basic Validation
- Check `OPR-TYPE`: Must be 'D' or 'C'.
- Check `OPR-AMT`: Must be greater than 0.
- **On Error**: Write \"INVALID TYPE/AMOUNT\" to `RECON.LOG`, `WS-ERR-COUNT++`, and exit paragraph.

#### Phase B: KSDS Master Verification
- Move `OPR-ACCT-ID` to `ACCT-MASTER-ID` and `READ ACCT-MASTER`.
- **File Status '23'**: Write \"ACCOUNT NOT IN KSDS\" to `RECON.LOG`, `WS-ERR-COUNT++`, and exit.
- **Status 'C' (Closed)**: Increment `WS-SKIP-COUNT` and exit (no error logged).

#### Phase C: DB2 Integrity Check
- Execute: `SELECT BALANCE FROM TB_ACCOUNT_BAL WHERE ACCT_ID = :OPR-ACCT-ID`.
- **SQLCODE 100** (Not Found): Write \"ACCOUNT NOT IN DB2\" to `RECON.LOG`, `WS-ERR-COUNT++`, and exit.
- **SQLCODE < 0**: Handle DB2 error (increment `WS-ERR-COUNT`, optional non-zero Return Code).

#### Phase D: Business Balance Logic
- **If Debit ('D')**:
  - If `DB2-BALANCE-BEFORE < OPR-AMT`: Write \"NEGATIVE BALANCE AFTER OPR\" to `RECON.LOG`, `WS-ERR-COUNT++`.
  - Else: `WS-OK-COUNT++`.
- **If Credit ('C')**:
  - Automatically considered `WS-OK-COUNT++` (assuming no upper limits).

---

## Program Flow
1. **INITIALIZATION**: Open ESDS (Sequential), KSDS (Random), and RECON.LOG.
2. **MAIN-LOOP**: Read `OPR.LOG.ESDS` until EOF.
3. **RECONCILE-PARAGRAPH**: Executes Phases A through D sequentially.
4. **TERMINATION**: 
   - Display summary counters in SYSOUT.
   - Close all files and DB2 connection.

---

## Key Learning Objectives
- **Enterprise Reconciliation**: Handling un-indexed ESDS logs against indexed KSDS and SQL DB2.
- **VSAM Status Handling**: Differentiating between successful reads, record-not-found ('23'), and other status codes.
- **SQL Exception Handling**: Using SQLCODE to drive business branch logic.
- **Audit Trails**: Creating a professional audit log (`RECON.LOG`) to track system integrity.

---

## How to Run
1. **Setup DB2**: Ensure `TB_ACCOUNT_BAL` is populated using `INSERT.DATA.sql`.
2. **Setup VSAM**: Run JCL to define and load the KSDS `ACCT.MASTER`.
3. **Execution**: Submit `ESDS29.jcl`.
4. **Results**: Check `SYSOUT` for totals and `RECON.LOG` for specific errors.

---

## Test Data & Expected Results

### Input: `OPR.LOG.ESDS`
```
1234567890 D 000050000 (Debit  $500.00)
0987654321 C 000100000 (Credit $1000.00)
1111111111 D 000020000 (Debit  $200.00 - Account Closed)
9999999999 D 000010000 (Debit  $100.00 - Not in KSDS)
```

### Context Data (KSDS/DB2)
- **Account 1234567890**: Status 'A', DB2 Balance = $600.00 -> **OK**
- **Account 0987654321**: Status 'A', DB2 Balance = $100.00 -> **OK**
- **Account 1111111111**: Status 'C' (Closed) -> **SKIP**
- **Account 9999999999**: Not in KSDS -> **ERROR**

### Expected `RECON.LOG` Entries
```
9999999999 ACCOUNT NOT IN KSDS               2026-04-21-02.00.00
```

### Expected SYSOUT Summary
```
TOTAL RECORDS READ: 0004
TOTAL RECONCILED OK: 0002
TOTAL ERRORS FOUND: 0001
TOTAL SKIPPED (CLSD): 0001
```
