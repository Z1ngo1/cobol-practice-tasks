# Task 28 — Transaction Journal (ESDS) + Client Report

## DB2 Tables
N/A (This task focuses on VSAM ESDS sequential processing).

---

## Files

| DD Name | File | Org | Mode | Description |
|---|---|---|---|---|
| `ACCT` | `ACCT.LIST` | PS | INPUT | List of client IDs to process |
| `AS-TRNS` | `TRANS.LOG.ESDS` | ESDS | INPUT | Sequential transaction log |
| `ACCTREP` | `ACCT.REPORT` | PS | OUTPUT | Final report with debit/credit totals and net |

### Input Record Layout — `ACCT.LIST` (80 bytes)

| Field | Picture | Offset | Description |
|---|---|---|---|
| `ACCT-LIST-ID` | `X(6)` | 1 | Client Identifier |
| `FILLER` | `X(74)` | 7 | Padding |

### Input Record Layout — `TRANS.LOG.ESDS` (100 bytes)

| Field | Picture | Offset | Description |
|---|---|---|---|
| `TRANS-ACCT-ID` | `X(6)` | 1 | Client Identifier |
| `TRANS-DATE` | `X(8)` | 7 | Transaction Date (YYYYMMDD) |
| `TRANS-TYPE` | `X(1)` | 15 | 'D' for Debit, 'C' for Credit |
| `TRANS-AMOUNT` | `9(7)V99` | 16 | Transaction Amount |
| `FILLER` | `X(56)` | 25 | Padding |

---

## Business Logic

Processes accounts from a PS master list. For each account, the program traverses the entire transaction history stored in an ESDS file.

1. **Phase 1 - Client List Processing**:
    - Read `ACCT.LIST` sequentially.
    - For each `ACCT-ID`:
        - Reset `WS-TOTAL-DEBIT` and `WS-TOTAL-CREDIT` to 0.
        - Start a full scan of the ESDS log.
2. **Phase 2 - ESDS Log Scan (Per Client)**:
    - **Open** `TRANS.LOG.ESDS` for sequential input.
    - Read records until EOF.
    - If `TRANS-ACCT-ID` matches current `ACCT-ID`:
        - If `TRANS-TYPE = 'D'` -> Accumulate in Debit total.
        - If `TRANS-TYPE = 'C'` -> Accumulate in Credit total.
    - **Close** `TRANS.LOG.ESDS` after scan completion to reset pointer for the next client.
3. **Phase 3 - Summary and Reporting**:
    - **Net Result** = Total Credit - Total Debit.
    - **Status Mapping**:
        - Both totals = 0 -> `NO TRANS`
        - Any total > 0 -> `OK`
    - Write result to `ACCT.REPORT`.

---

## Program Flow

1. **MAIN-LOGIC**: Controls the high-level flow (Open -> Read -> Close).
2. **READ-ACCT-LIST**: Outer loop reading the master list of client IDs.
3. **PROCESS-TRANS-LOG**: Inner loop managing the ESDS file lifecycle (Open, Scan, Close).
4. **COMPUTE-NET-STATUS**: Performs financial calculations and determines the line status.
5. **WRITE-ACCT-REPORT**: Formats and writes the summary line using the `STRING` statement.

---

## TEST DATA

### Input: `ACCT.LIST`
```
100001
100002
100003
```

### Input: `TRANS.LOG.ESDS`
```
100001 20260101 C 000150000 (Credit +1500.00)
100001 20260105 D 000050000 (Debit -500.00)
100002 20260110 C 000200000 (Credit +2000.00)
```

---

## Expected Output (`ACCT.REPORT`)

```
100001 500.00 1500.00 +1000.00 OK
100002 0.00 2000.00 +2000.00 OK
100003 0.00 0.00 +0.00 NO TRANS
```

---

## How to Run

1. **Define ESDS**: Submit `DEFESDS.jcl` to create the VSAM cluster.
2. **Prepare Data**: Load test records into `ACCT.LIST` and `TRANS.LOG.ESDS`.
3. **Execution**: Run the job `ESDS28`.
4. **Verification**: Verify the results in the `ACCTREP` output dataset.

---

## Key COBOL + VSAM Concepts Used

- **Entry-Sequenced Data Set (ESDS)**: Records are accessed strictly in the order they were written. No primary keys.
- **Nested File Operations**: Demonstrates that for specific queries on un-indexed files, multiple passes are required.
- **STRING Statement**: Used for building dynamic report strings from numeric and alphanumeric fields.
- **COMP-3 (Packed Decimal)**: Used for internal financial counters to save space and improve calculation speed.

---

## Notes
- This program uses an **O(N*M)** complexity approach (Clients * Transactions).
- The ESDS file is opened and closed for **each** client to reset the sequential read pointer to the beginning of the dataset.
