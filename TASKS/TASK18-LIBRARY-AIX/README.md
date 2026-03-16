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

The AIX requires four IDCAMS steps in sequence. Each is provided both as a standalone JCL and within [JCL/ALLSTEPS.jcl](JCL/ALLSTEPS.jcl).

| Step | JCL File | IDCAMS Command | Purpose |
|---|---|---|---|
| 1 | [DEFKSDS.jcl](JCL/DEFKSDS.jcl) | DEFINE CLUSTER | Create KSDS base cluster |
| 2 | [DEFAIX.jcl](JCL/DEFAIX.jcl) | DEFINE AIX | Define alternate index structure |
| 3 | [DEFPATH.jcl](JCL/DEFPATH.jcl) | DEFINE PATH | Link AIX to base cluster |
| 4 | [BLDINDX.jcl](JCL/BLDINDX.jcl) | BLDINDEX | Populate AIX from existing data |

> **Recommended:** Use [JCL/ALLSTEPS.jcl](JCL/ALLSTEPS.jcl) which runs all setup steps plus compile and run in one job with COND=(04,LT) chaining.

## Files

### VSAM File

#### 1. LIBRARY.MASTER.VSAM (KSDS) - Library Catalog

**Organization:** VSAM KSDS (Indexed)  
**Access Mode** DYNAMIC   
**Primary Key:** VSAM-ISBN   
**Alternate Key:** VSAM-AUTHOR  

**Record Layout:**
| Field | PIC | Length | Offset | Description |
|---|---|---|---|---|
| VSAM-ISBN | X(10) | 10 | 0 | Primary key — ISBN-10 number |
| VSAM-AUTHOR | X(20) | 20 | 10 | Alternate key — Author name |
| VSAM-TITLE | X(30) | 30 | 30 | Book title |
| VSAM-YEAR | X(4) | 4 | 60 | Publication year |

**Initial Data (14 books):** [DATA/LIBRARY.MASTER.VSAM](DATA/LIBRARY.MASTER.VSAM)

#### 2. LIBRARY.MASTER.VSAM.AIX - Alternate Index

**KEYS(20 10):** AUTHOR field — 20 bytes at offset 10  
**NONUNIQUEKEY:** multiple ISBNs per author allowed  
**UPGRADE:** AIX updated automatically when base cluster changes — without UPGRADE, BLDINDEX must be re-run manually after every data change  

#### 3. LIBRARY.MASTER.VSAM.PATH - AIX Path

Links AIX to base cluster. Used as DD VSAMDD1 alongside VSAMDD (base cluster) — both DDs required in JCL when ALTERNATE RECORD KEY is used.

### PS Input File

#### 4. SEARCH.REQ (PS) - Author Search Requests

**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=FB, LRECL=80)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| SEARCH-AUTHOR | X(20) | 20 | Author name to search |

**Sample Data:** [DATA/SEARCH.REQ](DATA/SEARCH.REQ)

### PS Output File

#### 5. RESULT.RPT (PS) - Search Results Report

**Organization:** SEQUENTIAL  
**Record Format:** Variable Block (RECFM=VB, LRECL=84)  

**Note:** RECFM=VB — variable-length output; the 4-byte RDW (Record Descriptor Word) is added in jcl(LRECL=84) effective max data length = 80 bytes

**Report line types:**
- Header: `SEARCH FOR: {AUTHOR-NAME}` — written before each author search
- Found: `     FOUND: {TITLE} ({YEAR})` — written for each matching book
- Not found: `     NOT FOUND` — written when author has no books
- Separator: `----------------------------------------` — written after each author block

**Expected Output:** [DATA/RESULT.RPT](DATA/RESULT.RPT)

### Error Handling

**FILE STATUS Codes (all three files):**
- 00 - Successful operation
- 23 - Record not found (START INVALID KEY — author not in AIX)
- Other codes - I/O errors on open, read, or write (program displays status and STOP RUN)

CLOSE errors treated as warnings (display only, no STOP RUN).

## Program Flow

1. **Initialization**
   - Opens VSAM-FILE (INPUT), SEARCH-FILE (INPUT), RESULT-FILE (OUTPUT); validates FILE STATUS '00' on each
   - Prime read: reads first SEARCH-AUTHOR from SEARCH.REQ before main loop

2. **Main Processing Loop**
   - Reads SEARCH.REQ sequentially until EOF; increments SEARCHES-PROCESSED per non-blank author
   - For each author: MOVE SEARCH-AUTHOR TO VSAM-AUTHOR, WRITE HEADER-LINE
   - START VSAM-FILE KEY IS EQUAL TO VSAM-AUTHOR:
     - INVALID KEY → WRITE NOT-FOUND-LINE, increment AUTHORS-NOT-FOUND
     - NOT INVALID KEY → increment AUTHORS-FOUND, PERFORM READ-MATCHING-BOOKS
   - WRITE SEPARATOR-LINE after each author block

3. **Browse + Termination**
   - SET NOT-EOF-AUTHOR TO TRUE; PERFORM UNTIL EOF-AUTHOR
   - READ VSAM-FILE NEXT: AT END → SET EOF-AUTHOR; IF VSAM-AUTHOR ≠ SEARCH-AUTHOR → SET EOF-AUTHOR; ELSE → WRITE FOUND detail line, increment BOOKS-FOUND
   - Closes all files (non-zero status on CLOSE is warning only)
   - Displays summary to SYSOUT: SEARCHES READ / SEARCHES PROCESSED / AUTHORS FOUND / AUTHORS NOT FOUND / BOOKS FOUND
   - STOP RUN

## JCL Jobs

### [ALL-STEPS.jcl](JCL/ALL-STEPS.jcl) — Full Setup + Compile + Run (Recommended)

**8-step master job with COND=(04,LT) chaining:**

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
- VSAMDD: LIBRARY.MASTER.VSAM (base cluster)
- VSAMDD1: LIBRARY.MASTER.VSAM.PATH (PATH for AIX access)
- SRCHDD: SEARCH.REQ (search requests)
- RSLTDD: RESULT.RPT (report output)

## How to Run

### Option A: Full Reset (Recommended First Time)

**Submit** [JCL/ALLSTEPS.jcl](JCL/ALLSTEPS.jcl) — runs complete setup + compile + run in one job

### Option B: Step-by-Step Manual Setup

Submit individual JCL files in this exact order:

1. [JCL/DEFKSDS.jcl](JCL/DEFKSDS.jcl) — define and load KSDS
2. [JCL/DEFAIX.jcl](JCL/DEFAIX.jcl) — define AIX structure
3. [JCL/DEFPATH.jcl](JCL/DEFPATH.jcl) — define PATH
4. [JCL/BLDINDX.jcl](JCL/BLDINDX.jcl) — build (populate) AIX
5. [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl) — compile and run COBOL program

**Alternative:** If you prefer to compile and run separately, use these jobs instead of COMPRUN.jcl:
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)

### Verify Results

- **Searches processed:** 7
- **Authors found:** 5 (KING, ROWLING, ASIMOV, TOLKIEN, CHRISTIE)
- **Authors not found:** 2 (PUSHKIN A.S., HEMINGWAY ERNEST)
- **Books found (total):** 14

## Common Issues

### Issue 1: IEC070I — VSAMDD1 DD Missing

**Cause:** JCL does not have VSAMDD1 DD pointing to the PATH dataset  
**Solution:** Both VSAMDD (base cluster) and VSAMDD1 (PATH) must be allocated in JCL when ALTERNATE RECORD KEY is used in COBOL SELECT

### Issue 2: START Returns INVALID KEY Even Though Author Exists

**Cause 1:** BLDINDEX not yet run — AIX structure defined but empty  
**Cause 2:** SEARCH-AUTHOR padded differently than VSAM-AUTHOR — trailing spaces matter in X(20) comparison  
**Solution:** Verify BLDINDEX step completed RC=0; verify SEARCH.REQ records have correct 20-byte author field matching VSAM data exactly

### Issue 3: ABEND — Steps Run Out of Order

**Cause:** BLDINDEX executed before DEFINE AIX or DEFINE PATH  
**Solution:** Order is mandatory: DEFINE CLUSTER → LOAD DATA → DEFINE AIX → DEFINE PATH → BLDINDEX

### Issue 4: RECFM=VB LRECL Mismatch

**Cause:** RSLTDD allocated with LRECL=80 instead of LRECL=84  
**Solution:** VB records need 4-byte RDW prefix; LRECL must be data length + 4 = 80 + 4 = 84

### Issue 5: Abend S0C4 or IEC020I on VSAM Open

**Cause:** RECORDSIZE in DEFINE CLUSTER does not match COBOL FD record length  
**Solution:** Verify RECORDSIZE(64,64) in DEFINE CLUSTER matches actual record layout: 10+20+30+4 = 64 bytes

## Program Output (SYSOUT)

See [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution log

## Notes

- `ACCESS MODE IS DYNAMIC` is required — without it, mixing START (random) and READ NEXT (sequential) is not allowed
- `WITH DUPLICATES` in ALTERNATE RECORD KEY must match `NONUNIQUEKEY` in DEFINE AIX — both must agree
- DEFINE PATH is not optional — even if AIX exists, COBOL cannot use it without a PATH
- BLDINDEX must run AFTER the base cluster is loaded with data — running it on empty cluster produces an empty AIX
- RECFM=VB output chosen to demonstrate variable-length PS output — different from all previous PS tasks which used FB
- Tested on IBM z/OS with Enterprise COBOL
