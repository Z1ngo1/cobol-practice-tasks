# Task 14 — Tax Calculation (Table Lookup / In-Memory Array)

## Overview

Reads a small tax-rate reference file (`TAX.RATES`) into an in-memory array (`TAX-TABLE`), then processes an employee salary file (`EMP.SALARY`) and writes a payroll output (`PAYROLL.TXT`) with the calculated tax amount for each employee.
The core technique is the **Table Lookup** pattern: instead of reading two files simultaneously, the reference data is loaded into memory once and searched on every salary record — much simpler than a match-merge and appropriate when the lookup table is small (10–20 entries).

---

## Files

| DD Name | File | Org | Mode | Description |
|---|---|---|---|---|
| `TAXDD` | `TAX.RATES` | PS | INPUT | Tax rate table — region code + rate; loaded into memory at startup |
| `EMPDD` | `EMP.SALARY` | PS | INPUT | Employee salary records — ID, name, region code, gross salary |
| `OUTDD` | `PAYROLL.TXT` | PS | OUTPUT | Payroll results — one line per employee with tax amount and net pay |

### Input Record Layout — `TAX.RATES` (`TAXDD`), LRECL=80, RECFM=F

| Field | Picture | Offset | Description |
|---|---|---|---|
| `TR-REGION-CODE` | `X(3)` | 1 | Region code — lookup key |
| `TR-TAX-RATE` | `V999` | 4 | Tax rate — implied 3 decimal places (e.g. `200` = 0.200 = 20%) |
| FILLER | `X(74)` | 7 | Padding to 80 bytes |

### Input Record Layout — `EMP.SALARY` (`EMPDD`), LRECL=80, RECFM=F

| Field | Picture | Offset | Description |
|---|---|---|---|
| `EMP-ID` | `X(5)` | 1 | Employee ID |
| `EMP-NAME` | `X(20)` | 6 | Employee name |
| `REGION-CODE` | `X(3)` | 26 | Region code — matched against `TAX-TABLE` |
| `GROSS-SALARY` | `9(7)V99` | 29 | Gross salary — implied 2 decimal places |
| FILLER | `X(42)` | 37 | Padding to 80 bytes |

### Output Record Layout — `PAYROLL.TXT` (`OUTDD`), LRECL=80, RECFM=F

| Field | Picture | Offset | Description |
|---|---|---|---|
| `OUT-EMP-ID` | `X(5)` | 1 | Employee ID |
| `OUT-EMP-NAME` | `X(20)` | 6 | Employee name |
| `OUT-REGION` | `X(3)` | 26 | Region code used for lookup |
| `OUT-GROSS` | `9(7)V99` | 29 | Gross salary |
| `OUT-TAX-AMT` | `9(7)V99` | 37 | Calculated tax amount |
| `OUT-NET-PAY` | `9(7)V99` | 45 | Net pay = Gross − Tax |
| FILLER | `X(26)` | 53 | Padding to 80 bytes |

---

## Two-Phase Processing

### Phase 1 — Load Tax Table (Initialization)

```
OPEN TAX.RATES
PERFORM UNTIL EOF
    READ TAX.RATES
    ADD 1 TO WS-TABLE-COUNT
    MOVE TR-REGION-CODE TO TAX-TABLE entry
    MOVE TR-TAX-RATE    TO TAX-TABLE entry
END-PERFORM
CLOSE TAX.RATES
```

After this phase the entire rate table lives in `TAX-TABLE` in memory.
`WS-TABLE-COUNT` holds the number of loaded entries and is used as the upper bound for all subsequent searches.

### Phase 2 — Process Salary File

```
OPEN EMP.SALARY, PAYROLL.TXT
PERFORM UNTIL EOF
    READ EMP.SALARY
    PERFORM LOOKUP-TAX-RATE using REGION-CODE against TAX-TABLE
    If found   : COMPUTE TAX-AMOUNT = GROSS-SALARY * WS-RATE
    If not found: COMPUTE TAX-AMOUNT = GROSS-SALARY * WS-DEFAULT-RATE (0.200)
    WRITE to PAYROLL.TXT
END-PERFORM
CLOSE EMP.SALARY, PAYROLL.TXT
```

---

## Table Lookup Logic

The lookup searches `TAX-TABLE` from index 1 to `WS-TABLE-COUNT` comparing `REGION-CODE` against each entry.

### Using `PERFORM VARYING`

```cobol
MOVE 'N' TO WS-FOUND-FLAG
PERFORM VARYING WS-TABLE-COUNT FROM 1 BY 1
        UNTIL WS-TABLE-COUNT > WS-TABLE-COUNT OR WS-FOUND-FLAG = 'Y'
    IF TAX-TABLE region entry = REGION-CODE
        MOVE WS-RATE     TO WS-FOUND-RATE
        MOVE 'Y'         TO WS-FOUND-FLAG
    END-IF
END-PERFORM
```

### Using `SEARCH` (alternative)

```cobol
SEARCH TAX-TABLE
    AT END
        MOVE WS-DEFAULT-RATE TO WS-FOUND-RATE
    WHEN region entry = REGION-CODE
        MOVE WS-RATE TO WS-FOUND-RATE
END-SEARCH
```

> `SEARCH` requires the table index to be defined with `INDEXED BY` and reset to 1 before each call. `PERFORM VARYING` is simpler when the table is small and not sorted.

### Default Rate (Region Not Found)

If no matching `REGION-CODE` is found in `TAX-TABLE`, the program applies `WS-DEFAULT-RATE = 0.200` (20%).
The output record is written normally — no error is logged, but `OUT-REGION` in the output will show the unmatched code so it can be spotted during review.

---

## Tax Calculation

```
TAX-AMOUNT = GROSS-SALARY × WS-RATE  (or WS-DEFAULT-RATE if not found)
NET-PAY    = GROSS-SALARY − TAX-AMOUNT
```

`COMPUTE` with `ROUNDED` is used to avoid truncation on the implied decimal positions.

---

## Test Data

All input and expected output files are in the [`DATA/`](DATA/) folder.

| File | Description |
|---|---|
| [`DATA/TAX.RATES`](DATA/TAX.RATES) | 8 region tax rate entries |
| [`DATA/EMP.SALARY`](DATA/EMP.SALARY) | 12 employee salary records |
| [`DATA/PAYROLL.TXT`](DATA/PAYROLL.TXT) | Expected payroll output with tax and net pay |

---

## Run Statistics (SYSOUT)

```
========================================
TAX CALCULATION SUMMARY
========================================
TAX TABLE ENTRIES LOADED:    8
EMPLOYEES PROCESSED:        12
REGION FOUND IN TABLE:      10
DEFAULT RATE APPLIED:        2
========================================
```

---

## How to Run

1. Upload [`DATA/TAX.RATES`](DATA/TAX.RATES) and [`DATA/EMP.SALARY`](DATA/EMP.SALARY) to your mainframe datasets
2. Submit [`JCL/COMPRUN.jcl`](JCL/COMPRUN.jcl)

> **PROC reference:** `COMPRUN.jcl` uses the [`MYCOMP`](../../JCLPROC/MYCOMP.jcl) catalogued procedure for compilation and execution. Make sure `MYCOMP` is available in your system's `PROCLIB` before submitting.

---

## Key COBOL Concepts Used

- **`OCCURS` + `INDEXED BY`** — defines `TAX-TABLE` as a fixed-size array of region/rate pairs loaded entirely into working-storage before any salary record is processed
- **`SEARCH`** — sequential table scan that walks the `OCCURS` array from the current index position; stops at the first matching entry or falls through to `AT END` for the default rate
- **Two-phase design** — strict initialization phase (load `TAX-TABLE`, close `TAX.RATES`) before opening `EMP.SALARY`; `TAX.RATES` is read exactly once regardless of how many salary records exist
- **`WS-DEFAULT-RATE` fallback** — when no entry in `TAX-TABLE` matches `REGION-CODE` the program applies `WS-DEFAULT-RATE` (0.200) and continues normally without stopping or logging an error

---

## Notes

- Tax table loading does not check for duplicate region codes — if `TAX.RATES` contains duplicate entries for the same region, only the **first** occurrence will be matched during lookup; ensure the tax rates file has unique region codes per entry
- The table size is bounded by the `OCCURS` limit in working-storage (`OCCURS 20 TIMES`) — if `TAX.RATES` has more than 20 records the program will ABEND on the 21st read; increase the `OCCURS` value if a larger table is needed
- `TAX.RATES` is closed after Phase 1 and never reopened — all lookups in Phase 2 are purely in-memory
- Tested on IBM z/OS with Enterprise COBOL
