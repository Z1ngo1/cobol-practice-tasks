# TASK18 - VSAM ALTERNATE INDEX: LIBRARY BOOK FINDER

## Overview

This program searches a library catalog VSAM KSDS file using an Alternate Index (AIX) built on the author field. For each author name read from a search request file, the program positions on the AIX and browses all matching book records. Results are written to an output report file with a final summary.

## Files

| DD Name | Dataset | Type | Description |
|---------|---------|------|-------------|
| VSAMDD | LIBRARY.MASTER.VSAM | VSAM KSDS (AIX) | Library master file with alternate index |
| SRCHDD | SEARCH.REQ | PS, 80 B | Author search requests input file |
| RSLTDD | RESULT.RPT | PS, V | Search results report output file |

## Record Layouts

### VSAM-REC (VSAM-FILE) — 80 bytes

| Field | PIC | Offset | Description |
|-------|-----|--------|-------------|
| VSAM-ISBN | X(10) | 1-10 | Primary key (ISBN) |
| VSAM-AUTHOR | X(20) | 11-30 | Alternate key (AIX key) |
| VSAM-TITLE | X(30) | 31-60 | Book title |
| VSAM-YEAR | X(4) | 61-64 | Publication year |
| FILLER | X(16) | 65-80 | Unused |

### SEARCH-REC (SEARCH-FILE) — 80 bytes

| Field | PIC | Description |
|-------|-----|-------------|
| SEARCH-AUTHOR | X(20) | Author name to search |
| FILLER | X(60) | Unused |

### RESULT-REC (RESULT-FILE) — variable

| Field | PIC | Description |
|-------|-----|-------------|
| RESULT-LINE | X(80) | Output report line |

## Processing Logic

### Phase 1 — Read Search Requests
- Read author name from SEARCH-FILE sequentially
- Count each record read into WS-SEARCHES-READ
- Skip duplicate author names (WS-PREV-AUTHOR check)
- Count processed searches into WS-SEARCHES-PROCESSED

### Phase 2 — Search VSAM via Alternate Index
- START VSAM-FILE KEY = VSAM-AUTHOR (AIX positioning)
- **INVALID KEY**: Author not found — write NOT-FOUND-LINE, increment WS-AUTHORS-NOT-FOUND
- **VALID KEY**: Read next records while VSAM-AUTHOR matches search author
  - Write book details to RESULT-FILE
  - Increment WS-BOOKS-FOUND counter
  - When author changes: stop browsing
- Increment WS-AUTHORS-FOUND

### Phase 3 — Write Results
- Write each matched book record to RESULT-FILE
- After all searches complete, write SUMMARY-LINES to RESULT-FILE

## JCL Steps (ALLSTEPS.jcl)

| Step | Program | Description |
|------|---------|-------------|
| STEP005 | IDCAMS | Delete existing VSAM cluster and define new KSDS cluster |
| STEP010 | IDCAMS | Load book records into LIBRARY.MASTER.VSAM via REPRO |
| STEP015 | IDCAMS | Define Alternate Index (AIX) on VSAM-AUTHOR field |
| STEP020 | IDCAMS | Build the AIX path (BLDINDEX) |
| STEP025 | IDCAMS | Define PATH connecting AIX to base cluster |
| STEP030 | VSAM18 | Execute COBOL program to search by author |

## Key Concepts

- **VSAM KSDS with AIX**: Base cluster keyed by ISBN; alternate index keyed by VSAM-AUTHOR with DUPLICATES allowed
- **DYNAMIC Access Mode**: Enables both sequential (READ NEXT) and random (START) access in the same program
- **AIX Browsing**: START positions on first matching AIX record; READ NEXT returns subsequent records in AIX key order
- **Duplicate Key Handling**: Multiple books by the same author are all returned via READ NEXT until author changes
- **ALTERNATE RECORD KEY IS VSAM-AUTHOR WITH DUPLICATES**: Declared in FILE-CONTROL to allow non-unique AIX keys

## Output (SYSOUT)

```
======================================
LIBRARY SEARCH SUMMARY
======================================
SEARCHES READ:         8
SEARCHES PROCESSED:    7
AUTHORS FOUND:         5
AUTHORS NOT FOUND:     2
BOOKS FOUND (TOTAL):  14
======================================
```

## Notes

- COND=(04,LT) on each step ensures subsequent steps run only if previous step RC <= 4
- SET MAXCC=0 after DELETE prevents job failure if cluster did not previously exist
- The AIX is defined with UPGRADE attribute so it stays synchronized with the base cluster
- FREESPACE(10,20) and CISZ(4096) are tuning parameters for the VSAM cluster
- Records in DATA section of JCL are fixed 80-byte, matching RECORDSIZE(64,64) padded to block size
