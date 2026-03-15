# TASK18 - VSAM Alternate Index (AIX): Library Book Finder

Library catalog search system that uses a VSAM KSDS with an Alternate Index (AIX) to retrieve all books by a given author. Demonstrates the full VSAM AIX lifecycle: DEFINE CLUSTER → LOAD DATA → DEFINE AIX → DEFINE PATH → BLDINDEX, and COBOL browsing via `START` + `READ NEXT` on an alternate key with duplicates.

## Business Scenario

Library needs a book search system where users can look up all titles by a given author. Author names are not unique keys — one author can have multiple books (NONUNIQUEKEY AIX). The system must:
- Maintain LIBRARY.MASTER.VSAM (KSDS, primary key = ISBN-10)
- Build an Alternate Index on AUTHOR field (20 bytes, offset 10) to allow author-based searches
- For each author in SEARCH.REQ: use `START` to position on first matching AIX record, then `READ NEXT` in a loop while author still matches
- If author not found: write NOT FOUND line
- If author found: write all matching book titles with publication year
- Output to variable-length report file (RECFM=VB)

## VSAM AIX Architecture

```
LIBRARY.MASTER.VSAM (KSDS)        LIBRARY.MASTER.VSAM.AIX
Primary Key: ISBN (10 bytes)  ←→  Alternate Key: AUTHOR (20 bytes, offset 10)
                                   NONUNIQUEKEY: multiple ISBNs per author

LIBRARY.MASTER.VSAM.PATH
  → Points to AIX
  → Used as DD VSAMDD1 in JCL
  → Accessed in COBOL via ALTERNATE RECORD KEY clause
```

### Why PATH Is Needed

`DEFINE PATH` creates a logical connector between the AIX and the base cluster. Without a PATH, COBOL cannot use the AIX through the `ALTERNATE RECORD KEY` clause — the PATH is the bridge that VSAM uses to navigate from the alternate key back to the base data records.

## Setup: Four Required JCL Steps (Before Running COBOL)

The AIX requires four IDCAMS steps in sequence. Each is provided both as a standalone JCL and within [JCL/ALL-STEPS.jcl](JCL/ALL-STEPS.jcl).

| Step | JCL File | IDCAMS Command | Purpose |
|---|---|---|---|
| 1 | [IDCAMS-DEFINE-CLUSTER.jcl](JCL/IDCAMS-DEFINE-CLUSTER.jcl) | DEFINE CLUSTER | Create KSDS base cluster |
| 2 | [IDCAMS-DEFINE-AIX.jcl](JCL/IDCAMS-DEFINE-AIX.jcl) | DEFINE AIX | Define alternate index structure |
| 3 | [IDCAMS-DEFINE-PATH.jcl](JCL/IDCAMS-DEFINE-PATH.jcl) | DEFINE PATH | Link AIX to base cluster |
| 4 | [IDCAMS-BLDINDEX.jcl](JCL/IDCAMS-BLDINDEX.jcl) | BLDINDEX | Populate AIX from existing data |

> **Recommended:** Use [JCL/ALL-STEPS.jcl](JCL/ALL-STEPS.jcl) which runs all setup steps plus compile and run in one job with COND=(04,LT) chaining.

## Files

### VSAM File

#### 1. LIBRARY.MASTER.VSAM (KSDS) - Library Catalog

**Organization:** VSAM KSDS (Indexed)
**Access Mode in COBOL:** DYNAMIC (supports both random START and sequential READ NEXT)
**Primary Key:** VSAM-ISBN — `RECORD KEY IS VSAM-ISBN`
**Alternate Key:** VSAM-AUTHOR — `ALTERNATE RECORD KEY IS VSAM-AUTHOR WITH DUPLICATES`

**DEFINE CLUSTER Parameters:**
```
RECORDSIZE(64,64)   — fixed 64-byte records
KEYS(10 0)          — ISBN: 10 bytes at offset 0
CISZ(4096)          — 4K control interval
FREESPACE(10,20)    — 10% CI free, 20% CA free
INDEXED             — KSDS organization
```

**Record Layout:**
| Field | PIC | Length | Offset | Description |
|---|---|---|---|---|
| VSAM-ISBN | X(10) | 10 | 0 | Primary key — ISBN-10 number |
| VSAM-AUTHOR | X(20) | 20 | 10 | Alternate key — Author name |
| VSAM-TITLE | X(30) | 30 | 30 | Book title |
| VSAM-YEAR | X(4) | 4 | 60 | Publication year |

**Initial Data (14 books):** [DATA/LIBRARY-MASTER-INPUT](DATA/LIBRARY-MASTER-INPUT)

**Catalog Contents:**
```
ISBN       | Author               | Title                          | Year
0001112222 | KING STEPHEN         | IT                             | 1986
0001234567 | ASIMOV ISAAC         | FOUNDATION                     | 1951
0002345678 | CHRISTIE AGATHA      | MURDER ON ORIENT               | 1934
0003334444 | TOLKIEN J.R.R.       | THE HOBBIT                     | 1937
0004445555 | ROWLING J.K.         | HARRY POTTER 1                 | 1997
0005556666 | KING STEPHEN         | THE SHINING                    | 1977
0006667777 | ASIMOV ISAAC         | I ROBOT                        | 1950
0007778888 | ROWLING J.K.         | HARRY POTTER 2                 | 1998
0008889999 | TOLKIEN J.R.R.       | LORD OF RINGS 1                | 1954
0009990000 | KING STEPHEN         | MISERY                         | 1987
0010101010 | ROWLING J.K.         | HARRY POTTER 3                 | 1999
0011111111 | ASIMOV ISAAC         | CAVES OF STEEL                 | 1954
0013131313 | CHRISTIE AGATHA      | TEN LITTLE INDIANS             | 1939
0014141414 | TOLKIEN J.R.R.       | LORD OF RINGS 2                | 1954
```

#### 2. LIBRARY.MASTER.VSAM.AIX - Alternate Index

**DEFINE AIX Parameters:**
```
RELATE(Z73460.TASK18.LIBRARY.MASTER.VSAM)  — links to base cluster
KEYS(20 10)     — AUTHOR: 20 bytes at offset 10
NONUNIQUEKEY    — multiple ISBNs per author allowed
UPGRADE         — AIX updated automatically when base cluster changes
RECORDSIZE(64,64)
CISZ(4096)
TRACKS(2,1)
FREESPACE(10,20)
```

**UPGRADE keyword:** When new records are added to LIBRARY.MASTER.VSAM, the AIX is automatically updated because UPGRADE is specified. Without UPGRADE, BLDINDEX would need to be re-run manually after each data change.

#### 3. LIBRARY.MASTER.VSAM.PATH - AIX Path

**DEFINE PATH Parameters:**
```
NAME(Z73460.TASK18.LIBRARY.MASTER.VSAM.PATH)
PATHENTRY(Z73460.TASK18.LIBRARY.MASTER.VSAM.AIX)
```

**JCL usage:** Defined as DD VSAMDD1 alongside VSAMDD (base cluster) — both DDs are required when ALTERNATE RECORD KEY is used.

### PS Input File

#### 4. SEARCH.REQ (PS) - Author Search Requests

**Organization:** SEQUENTIAL
**Record Format:** Fixed (RECFM=FB, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| SEARCH-AUTHOR | X(20) | 20 | Author name to search |
| FILLER | X(60) | 60 | Unused padding to LRECL=80 |

**Sample Data:** [DATA/SEARCH-REQ-INPUT](DATA/SEARCH-REQ-INPUT)

**7 search requests:**
- KING STEPHEN → 3 books (found)
- ROWLING J.K. → 3 books (found)
- PUSHKIN A.S. → 0 books (NOT FOUND)
- ASIMOV ISAAC → 3 books (found)
- TOLKIEN J.R.R. → 3 books (found)
- HEMINGWAY ERNEST → 0 books (NOT FOUND)
- CHRISTIE AGATHA → 2 books (found)

### PS Output File

#### 5. RESULT.RPT (PS) - Search Results Report

**Organization:** SEQUENTIAL
**Record Format:** Variable Block (RECFM=VB, LRECL=84)

**Note:** RECFM=VB — variable-length output; the 4-byte RDW (Record Descriptor Word) is added by VSAM automatically; effective max data length = 80 bytes

**Report line types:**
- Header: `SEARCH FOR: {AUTHOR-NAME}` — written before each author search
- Found: `     FOUND: {TITLE} ({YEAR})` — written for each matching book
- Not found: `     NOT FOUND` — written when author has no books
- Separator: `----------------------------------------` — written after each author block

**Expected Output:** [DATA/RESULT-REPORT-EXPECTED](DATA/RESULT-REPORT-EXPECTED)

## COBOL SELECT Clause for AIX

```cobol
SELECT VSAM-FILE ASSIGN TO VSAMDD
   ORGANIZATION IS INDEXED
   ACCESS MODE IS DYNAMIC
   RECORD KEY IS VSAM-ISBN
   ALTERNATE RECORD KEY IS VSAM-AUTHOR WITH DUPLICATES
   FILE STATUS IS VSAM-STATUS.
```

- `ACCESS MODE IS DYNAMIC` — required to mix `START` (random) with `READ NEXT` (sequential)
- `ALTERNATE RECORD KEY IS VSAM-AUTHOR WITH DUPLICATES` — matches NONUNIQUEKEY AIX
- Both VSAMDD (base cluster) and VSAMDD1 (PATH) must be allocated in JCL

## START + READ NEXT Pattern (AIX Browse)

```cobol
MOVE SEARCH-AUTHOR TO VSAM-AUTHOR

START VSAM-FILE KEY IS EQUAL TO VSAM-AUTHOR
    INVALID KEY
       ← author not in AIX → write NOT FOUND
    NOT INVALID KEY
       ← positioned at first book by this author
       PERFORM UNTIL EOF-AUTHOR
           READ VSAM-FILE NEXT
               AT END: SET EOF-AUTHOR TO TRUE
               NOT AT END:
                   IF VSAM-AUTHOR = SEARCH-AUTHOR
                      → WRITE FOUND line
                   ELSE
                      SET EOF-AUTHOR TO TRUE  ← moved past author group
           END-READ
       END-PERFORM
END-START
```

- `START` positions the AIX cursor — does NOT read a record, only sets position
- First actual record is retrieved by the first `READ NEXT` inside the loop
- Loop ends when author changes (records are in AIX key order) or physical EOF

## Program Flow

1. **Open Files: OPEN-ALL-FILES**
   - Opens VSAM-FILE (INPUT), SEARCH-FILE (INPUT), RESULT-FILE (OUTPUT)
   - Validates FILE STATUS for each

2. **Prime Read: READ-SEARCH-AUTHOR**
   - Reads first author name from SEARCH.REQ before the main loop

3. **Main Loop: PROCESS-ALL-SEARCHES**
   - PERFORM UNTIL EOF:
     - IF SEARCH-AUTHOR NOT = SPACES: ADD 1 TO SEARCHES-PROCESSED, PERFORM SEARCH-AUTHOR-BOOKS
     - PERFORM READ-SEARCH-AUTHOR (advance to next)

4. **Author Search: SEARCH-AUTHOR-BOOKS**
   - MOVE SEARCH-AUTHOR TO VSAM-AUTHOR
   - WRITE HEADER-LINE (SEARCH FOR: name)
   - START VSAM-FILE KEY IS EQUAL TO VSAM-AUTHOR:
     - INVALID KEY: WRITE NOT-FOUND-LINE, ADD 1 TO AUTHORS-NOT-FOUND
     - NOT INVALID KEY: ADD 1 TO AUTHORS-FOUND, PERFORM BROWSE-AUTHOR-BOOKS
   - WRITE SEPARATOR-LINE

5. **Browse Loop: BROWSE-AUTHOR-BOOKS**
   - SET NOT-EOF-AUTHOR TO TRUE
   - PERFORM UNTIL EOF-AUTHOR:
     - READ VSAM-FILE NEXT:
       - AT END: SET EOF-AUTHOR TO TRUE
       - NOT AT END: IF VSAM-AUTHOR = SEARCH-AUTHOR: WRITE DETAIL-LINE (FOUND: title (year)), ADD 1 TO BOOKS-FOUND — ELSE: SET EOF-AUTHOR TO TRUE

6. **Close + Summary: CLOSE-ALL-FILES + DISPLAY-SUMMARY**

## JCL Jobs

### [ALL-STEPS.jcl](JCL/ALL-STEPS.jcl) — Full Setup + Compile + Run (Recommended)

**7-step master job with COND=(04,LT) chaining:**

| Step | Program | Action |
|---|---|---|
| STEP005 | IDCAMS | DELETE old cluster (PURGE) + SET MAXCC=0 + DEFINE new KSDS cluster |
| STEP010 | IDCAMS | REPRO inline data → LIBRARY.MASTER.VSAM |
| STEP015 | IDCAMS | DEFINE AIX (NONUNIQUEKEY, UPGRADE, KEYS(20 10)) |
| STEP020 | IDCAMS | DEFINE PATH (link AIX to base cluster) |
| STEP025 | IDCAMS | BLDINDEX (populate AIX from existing data) |
| STEP030+035 | IEFBR14 + IEBGENER | Delete old SEARCH.REQ + create new with inline data |
| STEP040+045 | IEFBR14 + IGYWCL | Delete old RESULT.RPT + compile VSAM18 |
| STEP050 | VSAM18 | Run search program |

> **COND=(04,LT):** Each step runs only if the previous step's return code is ≤ 4. This ensures the pipeline stops safely if DEFINE or BLDINDEX fails.

### [COMPRUN.jcl](JCL/COMPRUN.jcl) — Compile + Run Only (After Setup)

**Step 1:** Delete old RESULT.RPT (IEFBR14)
**Step 2:** Compile and run VSAM18 using MYCOMPGO proc (MEMBER=VSAM18):
- VSAMDD: Z73460.TASK18.LIBRARY.MASTER.VSAM (DISP=SHR) — base cluster
- VSAMDD1: Z73460.TASK18.LIBRARY.MASTER.VSAM.PATH (DISP=SHR) — PATH for AIX access
- SRCHDD: Z73460.TASK18.SEARCH.REQ (DISP=SHR) — search requests
- RSLTDD: Z73460.TASK18.RESULT.RPT (NEW,CATLG,DELETE, VB, LRECL=84) — report output

> **Note:** `MYCOMPGO` PROC is shared across all tasks.
> See [/PROCLIB/MYCOMPGO.jcl](/PROCLIB/MYCOMPGO.jcl) for full definition.

## How to Run

### Option A: Full Reset (Recommended First Time)

**Submit** [JCL/ALL-STEPS.jcl](JCL/ALL-STEPS.jcl) — runs complete setup + compile + run in one job

### Option B: Compile + Run Only (Setup Already Done)

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl) — assumes VSAM cluster and AIX already exist

### Manual Setup (Individual JCL Files)

If running steps individually, execute in this order:
1. [JCL/IDCAMS-DEFINE-CLUSTER.jcl](JCL/IDCAMS-DEFINE-CLUSTER.jcl) — define and load KSDS
2. [JCL/IDCAMS-DEFINE-AIX.jcl](JCL/IDCAMS-DEFINE-AIX.jcl) — define AIX structure
3. [JCL/IDCAMS-DEFINE-PATH.jcl](JCL/IDCAMS-DEFINE-PATH.jcl) — define PATH
4. [JCL/IDCAMS-BLDINDEX.jcl](JCL/IDCAMS-BLDINDEX.jcl) — build (populate) AIX
5. [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl) — compile and run COBOL program

### Step 5: Verify Results

- **Searches processed:** 7
- **Authors found:** 5 (KING, ROWLING, ASIMOV, TOLKIEN, CHRISTIE)
- **Authors not found:** 2 (PUSHKIN A.S., HEMINGWAY ERNEST)
- **Books found (total):** 14

## Search Result Trace

| Author | Books Found | Titles |
|---|---|---|
| KING STEPHEN | 3 | IT (1986), THE SHINING (1977), MISERY (1987) |
| ROWLING J.K. | 3 | HARRY POTTER 1–3 (1997–1999) |
| PUSHKIN A.S. | 0 | NOT FOUND |
| ASIMOV ISAAC | 3 | FOUNDATION (1951), I ROBOT (1950), CAVES OF STEEL (1954) |
| TOLKIEN J.R.R. | 3 | THE HOBBIT (1937), LORD OF RINGS 1 & 2 (1954) |
| HEMINGWAY ERNEST | 0 | NOT FOUND |
| CHRISTIE AGATHA | 2 | MURDER ON ORIENT (1934), TEN LITTLE INDIANS (1939) |

## Common Issues

### Issue 1: IEC070I — VSAMDD1 DD Missing

**Cause:** JCL does not have VSAMDD1 DD pointing to the PATH dataset
**Solution:** Both VSAMDD (base cluster) and VSAMDD1 (PATH) must be allocated in JCL when ALTERNATE RECORD KEY is used in COBOL SELECT

### Issue 2: START Returns INVALID KEY Even Though Author Exists

**Cause 1:** BLDINDEX not yet run — AIX structure defined but empty
**Cause 2:** SEARCH-AUTHOR padded differently than VSAM-AUTHOR — trailing spaces matter in X(20) comparison
**Solution:** Verify BLDINDEX step completed RC=0; verify SEARCH.REQ records have correct 20-byte author field matching VSAM data exactly

### Issue 3: READ NEXT Returns Records From Wrong Author

**Cause:** WS-EOF-AUTHOR not reset to 'N' before each new author's browse loop
**Solution:** Verify `SET NOT-EOF-AUTHOR TO TRUE` executes at the start of BROWSE-AUTHOR-BOOKS before the PERFORM UNTIL loop

### Issue 4: First Book of Each Author Skipped

**Cause:** `START` is being treated as a READ — programmer reads inside the START block instead of after it
**Solution:** `START` only positions the cursor — it does NOT return a record; the first `READ NEXT` inside the browse loop retrieves the first actual record

### Issue 5: ABEND — DEFINE AIX Run After BLDINDEX

**Cause:** Steps run out of order — BLDINDEX executed before DEFINE AIX or DEFINE PATH
**Solution:** Order is mandatory: DEFINE CLUSTER → LOAD DATA → DEFINE AIX → DEFINE PATH → BLDINDEX

### Issue 6: AIX Not Updated After New Records Added to Base Cluster

**Cause:** AIX defined without UPGRADE keyword — new base cluster records not reflected in AIX
**Solution:** Verify `UPGRADE` is present in DEFINE AIX; re-run BLDINDEX to rebuild AIX from current base cluster data

### Issue 7: RECFM=VB LRECL Mismatch

**Cause:** RSLTDD allocated with LRECL=80 instead of LRECL=84 for VB format
**Solution:** VB records need 4-byte RDW prefix; LRECL must be data length + 4 = 80 + 4 = 84

### Issue 8: Abend S0C4 or IEC020I on VSAM Open

**Cause:** DEFINE CLUSTER used RECORDSIZE(64,64) but COBOL FD record is 64 bytes — mismatch with ALL-STEPS which uses RECORDSIZE(80,80)
**Solution:** Verify RECORDSIZE in DEFINE CLUSTER matches the actual COBOL FD record length (64 bytes: 10+20+30+4)

## Program Output (SYSOUT)

See [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution log

**Normal Execution (7 searches, 5 found, 2 not found, 14 books):**
```
========================================
LIBRARY SEARCH SUMMARY
========================================
SEARCHES PROCESSED:      7
AUTHORS FOUND:           5
AUTHORS NOT FOUND:       2
BOOKS FOUND:            14
RECORDS READ:            7
========================================
```

## Notes

- `ACCESS MODE IS DYNAMIC` is required — without it, mixing START (random) and READ NEXT (sequential) is not allowed
- `WITH DUPLICATES` in ALTERNATE RECORD KEY must match `NONUNIQUEKEY` in DEFINE AIX — both must agree
- UPGRADE keyword in DEFINE AIX means the AIX is maintained automatically; without it, every batch load requires manual BLDINDEX re-run
- DEFINE PATH is not optional — even if AIX exists, COBOL cannot use it without a PATH
- BLDINDEX must run AFTER the base cluster is loaded with data — running it on empty cluster produces an empty AIX
- Browse loop termination: loop stops either on physical EOF (AT END) or when author changes (VSAM-AUTHOR ≠ SEARCH-AUTHOR) — both cases handled via EOF-AUTHOR flag
- RECFM=VB output chosen to demonstrate variable-length PS output — different from all previous PS tasks which used FB
- [ALL-STEPS.jcl](JCL/ALL-STEPS.jcl) is the most complete JCL file in this portfolio — shows full production-style pipeline with COND chaining, IEBGENER for data loading, and multi-step dependency management
- No DB2 — pure VSAM AIX with COBOL DYNAMIC access mode
- Tested on IBM z/OS with Enterprise COBOL
