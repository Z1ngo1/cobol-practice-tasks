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

**Access Mode:** 
- INPUT-OUTPUT (Dynamic)
**Organization:**
- INDEXED (KSDS)
**Key:**
- CLIENT-ID (6 characters, positions 1-6)

**Record Layout:**
- 01  CLIENT-REC.
  - 05  CLIENT-ID         PIC X(6).
  - 05  CLIENT-NAME       PIC X(20).
  - 05  CLIENT-LAST-DATE  PIC 9(8).  *> YYYYMMDD format

**Sample Data:** [DATA/CLIENT-MASTER-BEFORE](DATA/CLIENT-MASTER-BEFORE)

#### 2. PARAM-FILE (PS) - Cutoff Date Parameter

**Access Mode:** 
- INPUT (Sequential)
**Organization:**
- SEQUENTIAL
**Record Format:**
- Fixed (RECFM=F, LRECL=80)

**Record Layout:**
- 01  PARAM-REC.
  - 05  PARAM-DATE  PIC X(8).   *> YYYYMMDD cutoff date
  - 05  FILLER      PIC X(72).

**Logic:** Records with CLIENT-LAST-DATE <= CUTOFF-DATE are considered inactive and archived

**Sample Data:** [DATA/PARAM-FILE](DATA/PARAM-FILE-INPUT)

### Output Files

#### 3. ARCHIVE-OLD (PS) - Archived Clients

**Access Mode:** 
- OUTPUT (Sequential)
**Organization:**
- SEQUENTIAL
**Record Format:**
- Fixed (RECFM=F, LRECL=80)

**Record Layout:**
- 01  ARCH-REC.
  - 05  ARCH-ID    PIC X(6).
  - 05  ARCH-NAME  PIC X(20).
  - 05  ARCH-DATE  PIC 9(8).
  - 05  FILLER     PIC X(46).

**Expected Output:** [DATA/ARCHIVE-OLD-OUTPUT](DATA/ARCHIVE-OLD-OUTPUT)

#### 4. CLIENT-FILE (VSAM KSDS) - Updated Master File

Active clients remaining after deletion of archived records.

**Expected Final State:** [DATA/CLIENT-MASTER-AFTER](DATA/CLIENT-MASTER-AFTER)

### Cutoff Date Logic

**Cutoff Date:** 20231231 (December 31, 2023)

**Examples:**
- 20230115 (Jan 15, 2023) <= 20231231 → INACTIVE (archive and delete)
- 20231231 (Dec 31, 2023) <= 20231231 → INACTIVE (archive and delete)
- 20240310 (Mar 10, 2024) > 20231231 → ACTIVE (keep in master)

### Error Handling

**FILE STATUS Codes:**
- 00 - Successful operation
- 23 - Record not found (handled during START)
- 92 - Logic error (file not opened or wrong access mode)
- Other codes - I/O errors (program stops with error message)

## Program Flow

1. **Initialization**
   - Opens CLIENT-FILE (VSAM KSDS) in I-O mode with DYNAMIC access
   - Opens PARAM-FILE (PS) for sequential input
   - Opens ARCH-FILE (PS) for sequential output
   - Initializes counters (REC-READ, REC-DELETE, REC-KEPT)
   - Validates FILE STATUS for all OPEN operations ('00' = success)

2. **Read Cutoff Date Parameter**
   - Reads single record from PARAM-FILE
   - Extracts PARAM-DATE (8 bytes in YYYYMMDD format)
   - Moves to WS-CUTOFF-DATE working storage variable
   - Displays cutoff date to SYSOUT for verification
   - Closes PARAM-FILE

3. **Position VSAM for Sequential Processing**
   - Moves LOW-VALUES to CLIENT-ID
   - Executes START CLIENT-FILE KEY IS NOT LESS THAN CLIENT-ID
   - Positions file pointer at first record in key sequence
   - Handles FILE STATUS '23' (empty file) gracefully

4. **Sequential Read and Process Loop**
   - Performs READ NEXT until end of file (EOF)
   - For each CLIENT-REC read:
     - Increments REC-READ counter
     - Compares CLIENT-LAST-DATE with WS-CUTOFF-DATE
     - **If CLIENT-LAST-DATE <= WS-CUTOFF-DATE (inactive client)**:
       - Writes CLIENT-REC to ARCH-FILE (sequential write)
       - Validates ARCH-STATUS = '00'
       - Deletes current record from VSAM using DELETE statement
       - Validates CLIENT-STATUS = '00' after DELETE
       - Increments REC-DELETE counter
       - Displays "ARCH AND DELETE: {CLIENT-ID}" to SYSOUT
     - **If CLIENT-LAST-DATE > WS-CUTOFF-DATE (active client)**:
       - Keeps record in VSAM (no action)
       - Increments REC-KEPT counter
   - READ NEXT automatically advances to next record after DELETE

5. **Termination**
   - Closes CLIENT-FILE and ARCH-FILE
   - Validates FILE STATUS for CLOSE operations
   - Formats counters with edited picture (Z(4)9) to suppress leading zeros
   - Displays statistics report to SYSOUT:
     - RECORDS READ: total records processed
     - RECORDS DELETE: clients archived and removed
     - RECORDS KEPT: active clients remaining
   - Verifies equation: REC-READ = REC-DELETE + REC-KEPT
   - Stops execution with proper return code

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

## How to Run

### Step 1: Define VSAM Cluster

**Submit:** [JCL/DEFKSDS.jcl](JCL/DEFKSDS.jcl)  
**Verify:** Check for MAXCC=0 in job output

### Step 2: Load Master Data into VSAM

**Option A: Use File Manager**
1. Navigate to VSAM file in ISPF 
2. Open with File Manager (FM)
3. Insert records manually from [DATA/CLIENT-MASTER-BEFORE](DATA/CLIENT-MASTER-BEFORE)

**Option B: Use REPRO with inline data**

**⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.

```JCL
//********************************************
//* STEP 1: DEFINE VSAM CLUSTER              
//********************************************
//STEP10   EXEC PGM=IDCAMS                             
//SYSPRINT DD SYSOUT=*                                 
//SYSOUT   DD SYSOUT=*                                 
//SYSIN    DD *                                        
  DEFINE CLUSTER (NAME(YOUR.VSAM.CLUSTER) -
           RECORDSIZE(80 80)               -           
           TRACKS(1 1)                     -           
           KEYS(20 0)                      -          
           CISZ(4096)                      -           
           FREESPACE(10 20)                -           
           INDEXED)                                    
/*
//********************************************
//* STEP 2: LOAD DATA INTO VSAM USING REPRO  
//********************************************
//STEP20   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//INFILE   DD *
YOUR DATA HERE
/*
//SYSIN    DD *
  REPRO INFILE(INFILE) -
        OUTDATASET(YOUR.VSAM.CLUSTER)
/*
```
                              
- Or Create temporary PS file with exact record length (34 bytes), load data there first, then REPRO to VSAM. Example in [JCL SAMPLES/DATAVSAM.jcl](../../JCL%20SAMPLES/DATAVSAM.jcl) uses SORT utility (can also be done with ICETOOL, IEBGENER)

### Step 3: Prepare Parameter File

**Option A: Upload via ISPF 3.4**

Upload  to PS dataset manually via ISPF

**Option B: Create via JCL**

Create PS file with LRECL=80 and insert inline data using IEBGENER:

**⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.

```JCL
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
YOUR DATA HERE
/*
//SYSUT2   DD DSN=YOUR.DATA.SET,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(RECFM=F,LRECL=80,BLKSIZE=80)
```

**Alternative:** Allocate PS file and insert exact length of your file transaction data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 4: Execute Program

**Submit:** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**Check:** SYSOUT for statistics and FILE STATUS messages (see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt))  
**Review:** [DATA/ARCHIVE-OLD-OUTPUT](DATA/ARCHIVE-OLD-OUTPUT) for archived clients

### Step 5: Verify Results

**Compare files:**
- CLIENT-FILE - Should contain only active clients (CLIENT-LAST-DATE > CUTOFF-DATE)
- ARCH-FILE - Should contain all inactive clients (CLIENT-LAST-DATE <= CUTOFF-DATE)
- Verify counts: REC-READ = REC-DELETE + REC-KEPT

## Common Issues

### Issue 1: FILE STATUS '92' on DELETE

**Cause:** File opened in INPUT mode instead of I-O, or wrong access mode (not DYNAMIC)
**Solution:** Verify SELECT statement uses ACCESS MODE IS DYNAMIC and OPEN I-O

### Issue 2: All records deleted or none deleted

**Cause:** Wrong date comparison logic or CUTOFF-DATE format mismatch
**Solution:** Verify YYYYMMDD format in PARAM-FILE, check <= vs < logic, verify WS-CUTOFF-DATE displays correctly

### Issue 3: ERROR: PARAM FILE IS EMPTY OR UNREADABLE

**Cause:** PARAM-FILE not allocated or empty
**Solution:** Create PARAM-FILE with cutoff date, verify LRECL=80, ensure first 8 bytes contain valid date

### Issue 4: START FAILED STATUS

**Cause:** VSAM file empty or not opened correctly
**Solution:** Verify VSAM file contains data, check OPEN I-O succeeded (CLIENT-STATUS = '00')

### Issue 5: Records archived but not deleted

**Cause:** DELETE statement failed silently
**Solution:** Check FILE STATUS after DELETE, verify CLIENT-STATUS = '00', review SYSOUT for error messages

## Program Output (SYSOUT)

Expected execution log - see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for full output example.

## Notes

- Program uses DYNAMIC access mode (allows both sequential READ NEXT and random DELETE operations)
- Date comparison uses <= (less than or equal) - records with exact cutoff date are also archived
- After DELETE, next READ NEXT automatically advances to the following record
- Archive file written sequentially in key order
- Tested on IBM z/OS with Enterprise COBOL
