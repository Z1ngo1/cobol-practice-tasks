# TASK06 - VSAM Client Archive and Deletion

Client database cleanup system using VSAM KSDS dynamic access with archival and deletion operations.

## Business Scenario

A company needs to archive and remove inactive client records from the master database. The system must:
- Accept cutoff date parameter from parameter file
- Read client master file sequentially
- Archive inactive clients to separate archive file
- Delete archived records from master file
- Generate processing report with statistics
- Handle FILE STATUS conditions during deletion

## Files

### Input Files

#### 1. CLIENT-MASTER (VSAM KSDS) - Client Master File

**Access Mode:** INPUT-OUTPUT (Dynamic)  
**Organization:** INDEXED (KSDS)  
**Key:** CLIENT-ID (6 characters, positions 1-6)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| CLIENT-ID | X(6) | 6 | Client ID (Primary Key) |
| CLIENT-NAME | X(20) | 20 | Client full name |
| CLIENT-LAST-DATE | 9(8) | 8 | Last activity date (YYYYMMDD) |

**Sample Data:** [DATA/CLIENT.MASTER.BEFORE](DATA/CLIENT.MASTER.BEFORE)

#### 2. PARAM-FILE (PS) - Cutoff Date Parameter

**Access Mode:** INPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| PARAM-DATE | X(8) | 8 | Cutoff date (YYYYMMDD) |
| FILLER | X(72) | 72 | Unused |

**Logic:** Records with CLIENT-LAST-DATE <= CUTOFF-DATE are considered inactive and archived

**Sample Data:** [DATA/PARAM.FILE.INPUT](DATA/PARAM.FILE.INPUT)

### Output Files

#### 3. ARCHIVE-OLD (PS) - Archived Clients

**Access Mode:** OUTPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| ARCH-ID | X(6) | 6 | Client ID |
| ARCH-NAME | X(20) | 20 | Client full name |
| ARCH-DATE | 9(8) | 8 | Last activity date (YYYYMMDD) |
| FILLER | X(46) | 46 | Unused |

**Expected Output:** [DATA/ARCHIVE.OLD.OUTPUT](DATA/ARCHIVE.OLD.OUTPUT)

#### 4. CLIENT-FILE (VSAM KSDS) - Updated Master File

Active clients remaining after deletion of archived records.

**Expected Final State:** [DATA/CLIENT.MASTER.AFTER](DATA/CLIENT.MASTER.AFTER)

### Error Handling

**FILE STATUS Codes:**
- 00 - Successful operation
- 23 - Record not found (handled during START)
- 92 - Logic error (file not opened or wrong access mode)
- Other codes - I/O errors (program stops with error message)

### Cutoff Date Logic

| CLIENT-LAST-DATE | Condition | Action |
|---|---|---|
| <= CUTOFF-DATE | Inactive | Write to ARCHIVE-OLD, DELETE from VSAM |
| > CUTOFF-DATE | Active | Keep in CLIENT-FILE, no action |

**Cutoff Date:** 20231231 (December 31, 2023)

**Examples:**
- **20230115** (Jan 15, 2023) — INACTIVE: archive and delete
- **20231231** (Dec 31, 2023) — INACTIVE: archive and delete  
- **20240310** (Mar 10, 2024) — ACTIVE: keep in master

## Program Flow

1. **Initialization**
   - Opens CLIENT-FILE (I-O, DYNAMIC), PARAM-FILE (INPUT), ARCH-FILE (OUTPUT)
   - Reads single record from PARAM-FILE, extracts WS-CUTOFF-DATE, displays to SYSOUT
   - Closes PARAM-FILE; initializes REC-READ, REC-DELETE, REC-KEPT counters

2. **Position VSAM for Sequential Read**
   - Moves LOW-VALUES to CLIENT-ID
   - Executes START KEY NOT LESS THAN CLIENT-ID to position at first record
   - FILE STATUS '23' (empty file) handled gracefully

3. **Sequential Read and Process Loop**
   - Performs READ NEXT until EOF; increments REC-READ per record
   - CLIENT-LAST-DATE <= WS-CUTOFF-DATE: writes to ARCH-FILE, DELETEs from VSAM, increments REC-DELETE, displays "ARCH AND DELETE: {CLIENT-ID}"
   - CLIENT-LAST-DATE > WS-CUTOFF-DATE: no action, increments REC-KEPT
   - After DELETE, next READ NEXT automatically advances to the following record

4. **Termination**
   - Closes CLIENT-FILE and ARCH-FILE; validates FILE STATUS on each CLOSE
   - Displays summary to SYSOUT: RECORDS READ / RECORDS DELETE / RECORDS KEPT
   - STOP RUN

## JCL Jobs

### 1. [DEFKSDS.jcl](JCL/DEFKSDS.jcl) - Define VSAM Cluster

Defines KSDS cluster for client master file.

**Key Parameters:**
- RECORDSIZE(34,34) - Fixed 34-byte records (6+20+8)
- KEYS(6 0) - 6-byte key starting at position 0
- TRACKS(15) - Initial allocation
- INDEXED - KSDS organization

### 2. [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Execute

Standard compile-link-go JCL using MYCOMPGO procedure.

**DD Statements:**
- CLTDD - CLIENT-FILE (VSAM KSDS)
- INDD - PARAM-FILE (PS with cutoff date)
- OUTDD - ARCH-FILE (PS for archived records)

**PROC reference:** [JCLPROC/MYCOMPGO.jcl](../../JCLPROC/MYCOMPGO.jcl)

## How to Run

### Step 1: Define VSAM Cluster

**Submit:** [JCL/DEFKSDS.jcl](JCL/DEFKSDS.jcl)  
**Verify:** Check for MAXCC=0 in job output

### Step 2: Load Master Data into VSAM

**Use File Manager**
1. Navigate to VSAM file in ISPF 
2. Open with File Manager (FM)
3. Insert records manually from [DATA/CLIENT.MASTER.BEFORE](DATA/CLIENT.MASTER.BEFORE)

**Alternative:**  
1. Define VSAM with RECORDSIZE(80,80) to match inline format
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Create temporary PS file with exact record length (34 bytes), load data there first, then REPRO to VSAM.
- Example in [JCL SAMPLES/DATAVSAM.jcl](../../JCL%20SAMPLES/DATAVSAM.jcl) uses SORT utility (can also be done with ICETOOL, IEBGENER)

### Step 3: Prepare Parameter File

**Upload via ISPF 3.4**

- Upload [DATA/PARAM.FILE.INPUT](DATA/PARAM.FILE.INPUT) to PS dataset manually via ISPF

**Alternative:**
1. Create PS file with LRECL=80 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length of your file transaction data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 4: Execute Program

**Submit:** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**Check:** SYSOUT for statistics and FILE STATUS messages (see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt))  
**Review:** [DATA/ARCHIVE.OLD.OUTPUT](DATA/ARCHIVE.OLD.OUTPUT) for archived clients

**Alternative:**
If you prefer to compile and run separately, use these jobs:  
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)

### Step 5: Verify Results

**Compare files:**
- CLIENT-FILE - Should contain only active clients (CLIENT-LAST-DATE > CUTOFF-DATE)
- ARCH-FILE - Should contain all inactive clients (CLIENT-LAST-DATE <= CUTOFF-DATE)
- Verify counts: REC-READ = REC-DELETE + REC-KEPT

## Common Issues

### Issue 1: FILE STATUS '92' on DELETE

**Cause:** File opened in INPUT mode instead of I-O, or ACCESS MODE is not DYNAMIC  
**Solution:** Verify SELECT uses ACCESS MODE IS DYNAMIC and file is opened with OPEN I-O

### Issue 2: All records deleted or none deleted

**Cause:** Wrong date comparison or CUTOFF-DATE format mismatch  
**Solution:** Verify YYYYMMDD format in PARAM-FILE; check WS-CUTOFF-DATE displays correctly in SYSOUT; confirm <= logic

### Issue 3: ERROR: PARAM FILE IS EMPTY OR UNREADABLE

**Cause:** PARAM-FILE not allocated or empty  
**Solution:** Verify PARAM-FILE exists with LRECL=80 and first 8 bytes contain a valid YYYYMMDD date

## Program Output (SYSOUT)

Expected execution log - see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for full output example.

## Notes

- Program uses DYNAMIC access mode (allows both sequential READ NEXT and random DELETE operations)
- Date comparison uses <= (less than or equal) - records with exact cutoff date are also archived
- Tested on IBM z/OS with Enterprise COBOL
