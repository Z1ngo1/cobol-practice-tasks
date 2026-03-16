# TASK17 - Internal Sort with Filtering: Honor Roll Generator

Exam result processor that filters out failing students and sorts passing students into an honor roll using COBOL's native `SORT` verb with `INPUT PROCEDURE` and `OUTPUT PROCEDURE`. Demonstrates the full three-phase internal sort pattern: filter via `RELEASE`, sort by two keys (CLASS ascending, SCORE descending), and write via `RETURN`.

## Business Scenario

School administration needs an honor roll report from raw exam results. Raw data is unsorted and includes failing students (score < 50) that must be excluded. The system must:
- Read raw exam results from EXAM.RAW (unsorted, unfiltered)
- In the INPUT PROCEDURE: filter out failing students (STUD-SCORE < 50), RELEASE passing students to the sort work file
- Automatically sort passing students: primary key CLASS-ID ascending, secondary key SCORE descending (best student first within each class)
- In the OUTPUT PROCEDURE: RETURN sorted records and write them to HONOR.ROLL output file
- Display processing summary to SYSOUT

## Three-Phase Internal Sort Architecture

```
INPUT PROCEDURE          SORT ENGINE          OUTPUT PROCEDURE
(FILTER-INPUT-DATA)    (automatic, z/OS)    (WRITE-SORTED-REPORT)

  READ EXAM-FILE    →    SORT-FILE (SD)    →    RETURN SORT-FILE
  IF SCORE >= 50         sorted by:             WRITE HONOR-REC
    RELEASE SORT-REC     1. SORT-CLASS ASC
  ELSE: skip             2. SORT-SCORE DESC
```

## Files

### Input Files

#### 1. EXAM.RAW (PS) - Raw Exam Results File

**Access Mode:** INPUT (Sequential, opened inside FILTER-INPUT-DATA)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=31)  
**Sort Requirement:** None — input is unsorted  

**Record Layout:**
| Field | PIC | Length | Offset | Description |
|---|---|---|---|---|
| STUD-ID | X(5) | 5 | 1 | Student ID |
| STUD-NAME | X(20) | 20 | 6 | Student full name |
| STUD-CLASS | X(3) | 3 | 26 | Class identifier (e.g. 09A, 10B, 11A) |
| STUD-SCORE | 9(3) | 3 | 29 | Exam score (0–100) |

**Sample Data:** [DATA/EXAM.RAW](DATA/EXAM.RAW)

#### 2. SORT-FILE (SD) - Sort Work File

**Type:** SD (Sort Description) — not FD, not SELECT with FILE STATUS  
**Organization:** Temporary work file — automatically deleted after sort completes  
**Record Layout:** Same structure as EXAM-REC (SORT-ID, SORT-NAME, SORT-CLASS, SORT-SCORE)  
**JCL DD name:** SRTDD — allocated as PS FB LRECL=31, DISP=(NEW,DELETE,DELETE)  

> **Important:** SD files do NOT have FILE STATUS. Do NOT OPEN or CLOSE the sort file — managed exclusively by the SORT verb.

### Output Files

#### 3. HONOR.ROLL (PS) - Honor Roll Report

**Access Mode:** OUTPUT (Sequential, opened inside WRITE-SORTED-REPORT)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed Block (RECFM=FB, LRECL=31)    
**Record Layout:** Same layout as EXAM-REC — HONOR-REC mirrors input record structure  

**Expected Output:** [DATA/HONOR.ROLL](DATA/HONOR.ROLL)

### Error Handling

**FILE STATUS Codes (EXAM-FILE and HONOR-FILE only — SD file has no FILE STATUS):**
- 00 - Successful operation
- Other codes - I/O errors on open, read, or write (program displays status and STOP RUN)

CLOSE errors treated as warnings (display only, no STOP RUN).

## Program Flow

1. **Initialization + Filter (FILTER-INPUT-DATA)**
   - OPEN INPUT EXAM-FILE; validates FILE STATUS '00'
   - Reads EXAM.RAW sequentially; increments RECORDS-READ per record
   - IF STUD-SCORE >= 50: MOVE fields to SORT-REC, RELEASE SORT-REC, increment RECORDS-PASSED
   - ELSE: increment RECORDS-FILTERED (record discarded)
   - CLOSE EXAM-FILE after last record

2. **Sort (automatic)**
   - COBOL runtime sorts all RELEASEd records: SORT-CLASS ASC, then SORT-SCORE DESC
   - No programmer code — SORT verb manages SORT-FILE entirely

3. **Write + Termination (WRITE-SORTED-REPORT)**
   - OPEN OUTPUT HONOR-FILE; validates FILE STATUS '00'
   - RETURNs sorted records one by one; MOVEs SORT-REC fields to HONOR-REC; WRITEs HONOR-REC
   - Increments RECORDS-WRITTEN per write; validates HONOR-STATUS = '00'
   - CLOSE HONOR-FILE after last record
   - Displays summary to SYSOUT: RECORDS-READ / RECORDS-FILTERED / RECORDS-PASSED / RECORDS-WRITTEN
   - STOP RUN

## JCL Jobs

### [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure.

- EXDD: EXAM.RAW (raw exam input)
- SRTDD: WORK.SORT (temporary sort work file)
- HNRDD: HONOR.ROLL (honor roll output)

**PROC reference:** [JCLPROC/MYCOMPGO.jcl](../../JCLPROC/MYCOMPGO.jcl)

## How to Run

### Step 1: Allocate and Load Exam File

Upload [DATA/EXAM.RAW](DATA/EXAM.RAW) to PS dataset manually via ISPF

**Alternative:**
1. Create PS file with LRECL=31 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 2: Allocate Sort Work File

- Sort work file (SRTDD) allocated as DISP=(NEW,DELETE,DELETE) in JCL — temporary, deleted automatically after each run

### Step 3: Execute Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution summary  
**Review** [DATA/HONOR.ROLL](DATA/HONOR.ROLL) for expected output  

**Alternative:**
If you prefer to compile and run separately, use these jobs:
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)

### Step 4: Verify Results

- **Records read:** 20
- **Records filtered (score < 50):** 5
- **Records passed (score >= 50):** 15
- **Records written:** 15
- **Sort order:** classes 09A → 09B → 10A → 10B → 11A, within each class highest score first

## Common Issues

### Issue 1: Abend S013 or S002 — LRECL Mismatch

**Cause:** SRTDD or HNRDD allocated with wrong LRECL — record is 31 bytes (5+20+3+3) but DD has LRECL=80  
**Solution:** Verify both SRTDD and HNRDD have `DCB=(DSORG=PS,RECFM=FB,LRECL=31)` in JCL  

### Issue 2: Sort File Opened or Closed by Programmer

**Cause:** `OPEN SORT-FILE` or `CLOSE SORT-FILE` in PROCEDURE DIVISION  
**Solution:** Remove all OPEN/CLOSE for the SD file — only the SORT verb manages it; programmer OPEN/CLOSE causes abend  

### Issue 3: FILE STATUS Declared for Sort File

**Cause:** `FILE STATUS IS SORT-STATUS` added to SELECT SORT-FILE  
**Solution:** SD files do NOT support FILE STATUS — remove it; the sort file has no SELECT with FILE STATUS  

### Issue 4: S0C7 Abend on STUD-SCORE Comparison

**Cause:** STUD-SCORE field contains non-numeric data (STUD-CLASS offset shifted — field misalignment)  
**Solution:** Verify EXAM.RAW LRECL=31 and field offsets: ID(1-5), NAME(6-25), CLASS(26-28), SCORE(29-31)  

## Program Output (SYSOUT)

See [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution log

## Notes

- SD files use `RELEASE` (not WRITE) and `RETURN` (not READ) — unique COBOL sort vocabulary
- Sort work file (SRTDD) must be pre-allocated in JCL or have space available — it is allocated NEW and deleted on completion
- LRECL=31 for sort and output matches the physical record length: 5+20+3+3 = 31 bytes exactly
- Tested on IBM z/OS with Enterprise COBOL
