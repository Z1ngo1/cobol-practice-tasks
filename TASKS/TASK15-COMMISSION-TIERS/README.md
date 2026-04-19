# Task 15 — Commission Tiers (Tiered Table Lookup / In-Memory Array)

## Overview

Reads a commission tiers reference file (`COMM.TIERS`) into an in-memory array (`TIER-TABLE`), then processes an employee salary file (`SALES.WEEKLY`) and writes a commission output (`PAYOUT.RPT`) with the calculated commission for each employee.
The core technique is a **Tiered Table Lookup**: instead of a simple key match, the program finds the first tier where `WS-LIMIT >= SAL-AMT` — the salary bracket determines the commission rate. The tier table is loaded once into memory before any salary record is processed.

---

## Files

| DD Name | File | Org | Mode | Description |
|---|---|---|---|---|
| `COMMDD` | `COMM.TIERS` | PS | INPUT | Commission tiers table — salary limit + commission rate; loaded into memory at startup |
| `SALDD` | `SALES.WEEKLY` | PS | INPUT | Employee salary records — ID and weekly sales amount |
| `OUTDD` | `PAYOUT.RPT` | PS | OUTPUT | Commission results — one line per employee with salary, rate, and commission amount |

### Input Record Layout — `COMM.TIERS` (`COMMDD`), LRECL=9, RECFM=F

| Field | Picture | Offset | Description |
|---|---|---|---|
| `COMM-LIMIT` | `9(6)` | 1 | Salary upper bound for this tier |
| `COMM-PCT` | `V999` | 7 | Commission rate — implied 3 decimal places (e.g. `020` = 0.020 = 2%) |

### Input Record Layout — `SALES.WEEKLY` (`SALDD`), LRECL=13, RECFM=F

| Field | Picture | Offset | Description |
|---|---|---|---|
| `SAL-ID` | `9(5)` | 1 | Employee ID |
| `SAL-AMT` | `9(6)V99` | 6 | Weekly sales amount — implied 2 decimal places |

### Output Record Layout — `PAYOUT.RPT` (`OUTDD`), LRECL=80, RECFM=F

| Field | Picture | Offset | Description |
|---|---|---|---|
| `OUT-ID` | `9(5)` | 1 | Employee ID |
| FILLER | `X(1)` | 6 | Separator |
| `OUT-SAL-AMT` | `9(6)V99` | 7 | Weekly sales amount |
| FILLER | `X(1)` | 15 | Separator |
| `OUT-PCT` | `V999` | 16 | Applied commission rate |
| FILLER | `X(1)` | 17 | Separator |
| `OUT-RES` | `9(6)V99` | 18 | Calculated commission amount |
| FILLER | `X(53)` | 26 | Padding to 80 bytes |

---

## Two-Phase Processing

### Phase 1 — Load Commission Table (Initialization)

```
OPEN COMM.TIERS
PERFORM UNTIL EOF
    READ COMM.TIERS
    ADD 1 TO TIERS-LOADED
    MOVE COMM-LIMIT TO WS-LIMIT(IDX)
    MOVE COMM-PCT   TO WS-PCT(IDX)
END-PERFORM
CLOSE COMM.TIERS
```

After this phase the entire tier table lives in `TIER-TABLE` in memory. `TIERS-LOADED` holds the number of loaded entries and is used as the upper bound for all subsequent lookups. Table size is bounded by `OCCURS 20 TIMES` — if `COMM.TIERS` has more than 20 records the program displays a warning and ignores the excess.

### Phase 2 — Process Salary File

```
OPEN SALES.WEEKLY, PAYOUT.RPT
PERFORM UNTIL EOF
    READ SALES.WEEKLY
    ADD 1 TO EMPLOYEES-PROCESSED
    PERFORM CALCULATE-COMMISSION
END-PERFORM
CLOSE SALES.WEEKLY, PAYOUT.RPT
```

---

## Tiered Lookup Logic

The lookup searches `TIER-TABLE` from index 1 to `TIERS-LOADED` looking for the **first tier where `WS-LIMIT >= SAL-AMT`**. This is the key difference from a simple key match — the salary bracket, not an exact key, selects the commission rate.

### Using `PERFORM VARYING`

```cobol
MOVE 'N' TO WS-FOUND
PERFORM VARYING IDX FROM 1 BY 1
        UNTIL IDX > TIERS-LOADED OR WS-FOUND = 'Y'
    IF WS-LIMIT(IDX) >= SAL-AMT
        MOVE 'Y' TO WS-FOUND
        COMPUTE OUT-RES = SAL-AMT * WS-PCT(IDX)
        MOVE SAL-ID     TO OUT-ID
        MOVE SAL-AMT    TO OUT-SAL-AMT
        MOVE WS-PCT(IDX) TO OUT-PCT
        WRITE COMMISSION-REC
        ADD 1 TO TIER-MATCH-COUNT
    END-IF
END-PERFORM
```

### No Tier Match

If no tier satisfies `WS-LIMIT >= SAL-AMT` (salary exceeds all defined limits), the record is **not written** to output and `NO-TIER-MATCH` counter is incremented. This means the `COMM.TIERS` file should always include a catch-all top tier (e.g. `999999` limit) to cover the highest possible salary.

---

## Commission Calculation

```
OUT-RES = SAL-AMT x WS-PCT(IDX)   (tier where WS-LIMIT >= SAL-AMT)
```

`COMPUTE` is used to avoid truncation on the implied decimal positions.

---

## Test Data

All input and expected output files are in the [`DATA/`](DATA/) folder.

| File | Description |
|---|---|
| [`DATA/COMM.TIERS`](DATA/COMM.TIERS) | 5 commission tier entries |
| [`DATA/SALES.WEEKLY`](DATA/SALES.WEEKLY) | 12 employee salary records |
| [`DATA/PAYOUT.RPT`](DATA/PAYOUT.RPT) | Expected commission output |

---

## Expected SYSOUT

Actual job output is stored in [`OUTPUT/SYSOUT.txt`](OUTPUT/SYSOUT.txt).

```
========================================
COMMISSION CALCULATION SUMMARY
========================================
COMMISSION TIERS LOADED:  5
EMPLOYEES PROCESSED:     12
RECORDS WRITTEN:         12
TIER MATCHED:            12
NO TIER MATCH:            0
========================================
```

---

## How to Run

1. Upload [`DATA/COMM.TIERS`](DATA/COMM.TIERS) and [`DATA/SALES.WEEKLY`](DATA/SALES.WEEKLY) to your mainframe datasets manually through option '3.4 and edit your dataset' or with pre-prepared data
2. Submit [`JCL/COMPRUN.jcl`](JCL/COMPRUN.jcl) with pre-prepared data

> **PROC reference:** `COMPRUN.jcl` uses the [`MYCOMPGO`](../../JCLPROC/MYCOMPGO.jcl) catalogued procedure for compilation and execution. Make sure `MYCOMPGO` is available in your system's `PROCLIB` before submitting.

---

## Key COBOL Concepts Used

- **`OCCURS` + `INDEXED BY`** — defines `TIER-TABLE` as a fixed-size array (`OCCURS 20 TIMES`) of limit/rate pairs loaded entirely into working-storage before any salary record is processed
- **Tiered (bracket) lookup** — unlike a simple key match, the search finds the first entry where `WS-LIMIT >= SAL-AMT`; the tiers must be stored in ascending order of `COMM-LIMIT` for the algorithm to work correctly
- **Two-phase design** — strict initialization phase (load `TIER-TABLE`, close `COMM.TIERS`) before opening `SALES.WEEKLY`; `COMM.TIERS` is read exactly once regardless of how many salary records exist
- **No-match handling** — when no tier covers the employee salary the record is silently skipped and `NO-TIER-MATCH` is incremented; ensure a catch-all top tier exists in `COMM.TIERS`

---

## Notes

- Tiers in `COMM.TIERS` must be sorted in **ascending order** of `COMM-LIMIT` — the first tier where the limit is greater than or equal to the salary amount is used; unsorted tiers will produce incorrect commission assignments
- The table size is bounded by the `OCCURS` limit in working-storage (`OCCURS 20 TIMES`) — if `COMM.TIERS` has more than 20 records the program displays a warning and skips the excess; increase the `OCCURS` value if a larger table is needed
- `COMM.TIERS` is closed after Phase 1 and never reopened — all lookups in Phase 2 are purely in-memory
- Tested on IBM z/OS with Enterprise COBOL
