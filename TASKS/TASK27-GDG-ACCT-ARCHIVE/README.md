# Task 27 — Account Archive Batch (GDG + VSAM Lookup)

## Overview

Processes a daily account file (PS), identifies accounts inactive for more than 180 days by looking up their transaction history in a VSAM KSDS, and routes them to different Generation Data Group (GDG) versions. Active accounts, archived (inactive) accounts, and accounts missing from the history are separated into three distinct GDG outputs.

The core technique is **Version-based Archiving (GDG)**: utilizing Z/OS Generation Data Groups to maintain multiple historical versions of active, archived, and unmatched account files without manual renaming.

---

## Files

| DD Name | File | Org | Mode | Description |
|---|---|---|---|---|
| `INPSDD` | `ACCT.DATA` | PS | INPUT | Daily account master file |
| `VSAMDD` | `ACCT.HISTORY` | KSDS | INPUT | Transaction history (Last Activity Date) |
| `GDGDD1` | `ACCT.ACTIVE(+1)` | GDG | OUTPUT | Accounts with activity within 180 days |
| `GDGDD2` | `ACCT.ARCHIVE(+1)` | GDG | OUTPUT | Accounts inactive for > 180 days |
| `GDGDD3` | `ACCT.UNMATCH(+1)` | GDG | OUTPUT | Accounts not found in history VSAM |
| `REPPSDD` | `PROCESS.REP` | PS | OUTPUT | Batch processing summary report |

### Input Record Layout — `ACCT.DATA` (`INPSDD`), LRECL=58, RECFM=F

| Field | Picture | Offset | Description |
|---|---|---|---|
| `DATA-ACCT-ID` | `X(6)` | 1 | Account Identifier |
| `DATA-CUST-NAME` | `X(25)` | 7 | Customer Name |
| `DATA-LAST-ACTV-DATE`| `9(8)` | 32 | Last activity date on master (YYYYMMDD) |
| `DATA-BALANCE` | `9(7)V99` | 40 | Current Balance |
| `FILLER` | `X(10)` | 49 | Padding |

### History Record Layout — `ACCT.HISTORY` (`VSAMDD`), KSDS, Key=1-6

| Field | Picture | Offset | Description |
|---|---|---|---|
| `HIST-ACCT-ID` | `X(6)` | 1 | **Primary Key** — Account ID |
| `HIST-LAST-TRNS-DATE`| `9(8)` | 7 | Real last transaction date (YYYYMMDD) |
| `HIST-TRNS-COUNT` | `9(4)` | 15 | Total transaction count |

---

## Business Logic: Multi-Destination Routing

The program implements a routing pipeline based on temporal logic and referential integrity.

### Phase 1 — Initialize & Cutoff Calculation

The program accepts the current system date and calculates the **180-day cutoff date** using standard COBOL intrinsic functions:
- `FUNCTION INTEGER-OF-DATE`: Converts YYYYMMDD to integer days.
- `FUNCTION DATE-OF-INTEGER`: Converts back to YYYYMMDD after subtracting 180.

### Phase 2 — Random VSAM Lookup

For each record in the daily file, the program performs a random read of `ACCT.HISTORY` by `ACCT-ID`:

| Condition | Action |
|---|---|
| **Status 23 (Not Found)** | Increment `UNMATCH-COUNT` and `ERROR-COUNT`; call `WRITE-UNMATCHED` |
| **Found (Status 00)** | Proceed to Activity Check |
| **Other Status** | Log fatal error and `STOP RUN` |

### Phase 3 — Activity Routing

Compares the `HIST-LAST-TRNS-DATE` from VSAM against the calculated `WS-CUTOFF-DATE`:

- **Active**: If `Last Trans Date >= Cutoff Date` → Call `WRITE-ACTIVE` (routes to `GDGDD1`).
- **Archive**: If `Last Trans Date < Cutoff Date` → Call `WRITE-ARCHIVE` (routes to `GDGDD2`).

---

## Program Flow

1. `OPEN-ALL-FILES` — Open 1 Input PS, 1 Input VSAM, 3 Output GDGs, and 1 Output Report. Check File Status for all.
2. `INITIALIZE-DATA` — Accept `WS-CURR-DATE` and compute `WS-CUTOFF-DATE` (Today - 180).
3. `READ-ACCT-DATA` — Main loop until `ACCT.DATA` EOF:
   - 3.1. `READ ACCT-DATA-FILE` record.
   - 3.2. `CHECK-ACCT-HIST` — Perform random read on VSAM.
   - 3.3. **If Not Found** → `WRITE-UNMATCHED` (GDGDD3).
   - 3.4. **If Found** → Compare date; call `WRITE-ACTIVE` (GDGDD1) or `WRITE-ARCHIVE` (GDGDD2).
4. `WRITE-FINAL-REPORT` — Set `RETURN-CODE` based on error count; write formatted summary to `PROCESS.REP`.
5. `CLOSE-ALL-FILES` — Close all 6 files.
6. `STOP RUN`.

---

## Return Code Logic

The job return code (RC) is set at finalization:

| Condition | Final RC | Meaning |
|---|---|---|
| `ERROR-COUNT = 0` | **0** | All records routed to Active or Archive. |
| `ERROR-COUNT < 10` | **4** | Some records were unmatched (Warning). |
| `ERROR-COUNT >= 10`| **12** | Excessive unmatched records (Failure). |

---

## Test Data

Files are located in the [`DATA/`](DATA/) folder.

| File | Description |
|---|---|
| [`DATA/ACCT.DATA`](DATA/ACCT.DATA) | PS Input — Daily accounts to process. |
| [`DATA/UNMATCH.GDG.G0001V00`](DATA/UNMATCH.GDG.G0001V00) | Example of a GDG generation for unmatched accounts. |

---

## Expected SYSOUT (PROCESS.REP)

```text
PROCESSING DATE: 20260129
TOTAL RECORDS READ: 00025
ACTIVE ACCOUNTS:    00018
ARCHIVED ACCOUNTS:  00005
UNMATCHED ACCOUNTS: 00002
PROCESSING STATUS: COMPLETED WITH ERRORS
RETURN CODE: 4
```

---

## How to Run

1. **Define GDGs** — Submit [`JCL/DEFGDG.jcl`](JCL/DEFGDG.jcl) to define the base GDG entries on the mainframe.
2. **Upload VSAM** — Load the `ACCT.HISTORY` KSDS with transaction dates.
3. **Submit Job** — Submit the processing JCL to run `GDGJOB27`.
4. **Verify Generations** — Check catalog for `ACCT.ACTIVE.G0001V00`, etc.

---

## Key COBOL + GDG Concepts Used

- **Generation Data Groups (GDG)** — Automates historical file management using `(+1)` for new versions and `(0)` for current.
- **Intrinsic Date Functions** — `INTEGER-OF-DATE` and `DATE-OF-INTEGER` for robust leap-year-aware date arithmetic.
- **Random VSAM Read** — `READ ... INVALID KEY` pattern for cross-reference lookup.
- **Multi-Output Routing** — One input stream split into three output streams based on business rules.
- **Tiered RC Management** — Conditional `RETURN-CODE` assignment for JCL step control.

---

## Notes

- **Cutoff logic**: Inactive accounts are those with no transaction in the last 180 days. Accounts with no transaction history are considered a data error and routed to Unmatched.
- **GDG versions**: Each run of the job creates a new generation (+1) for each of the three output categories.
- Tested on IBM z/OS with Enterprise COBOL.
