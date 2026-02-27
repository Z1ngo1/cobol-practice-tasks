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

## Data Format Examples

### Client Master Record Format

**Raw data:** 100001IVAN PETROV          20230115

**Breakdown:**
- 100001 - Client ID (6 chars)
- IVAN PETROV           - Client name (20 chars, padded with spaces)
- 20230115 - Last contact date (8 digits): January 15, 2023

### Parameter File Format

**Raw data:** 20231231 followed by 72 spaces (total 80 bytes)

**Breakdown:**
- 20231231 - Cutoff date (8 chars): December 31, 2023
- FILLER - Remaining 72 bytes (padding to reach 80-byte record)

### Cutoff Date Logic

**Cutoff Date:** 20231231 (December 31, 2023)

**Examples:**
- 20230115 (Jan 15, 2023) <= 20231231 → INACTIVE (archive and delete)
- 20231231 (Dec 31, 2023) <= 20231231 → INACTIVE (archive and delete)
- 20240310 (Mar 10, 2024) > 20231231 → ACTIVE (keep in master)

## Program Logic

### Main Processing Flow

**1. OPEN-FILES**
- Open CLIENT-FILE in I-O mode (INPUT-OUTPUT) with DYNAMIC access
- Open PARAM-FILE for input
- Open ARCH-FILE for output
- Check FILE STATUS for each file (stop if not '00')

**2. READ-CUTOFF-DATE**
- Read PARAM-FILE to get cutoff date (YYYYMMDD format)
- Move PARAM-DATE to WS-CUTOFF-DATE
- Display cutoff date: 'DATE IS: {WS-CUTOFF-DATE}'
- Close PARAM-FILE

**3. Process All Records**

```cobol
MOVE LOW-VALUES TO CLIENT-ID
START CLIENT-FILE KEY IS NOT LESS THAN CLIENT-ID

PERFORM UNTIL EOF
    READ CLIENT-FILE NEXT RECORD
        AT END
            SET EOF TO TRUE
        NOT AT END
            ADD 1 TO REC-READ
            IF CLIENT-LAST-DATE <= WS-CUTOFF-DATE
                WRITE ARCH-REC FROM CLIENT-REC
                DELETE CLIENT-FILE
                ADD 1 TO REC-DELETE
                DISPLAY 'ARCH AND DELETE: ' CLIENT-ID
            ELSE
                ADD 1 TO REC-KEPT
            END-IF
    END-READ
END-PERFORM
```

**4. Termination**
- Close CLIENT-FILE
- Close ARCH-FILE
- Display statistics report:
  - RECORDS READ
  - RECORDS DELETE
  - RECORDS KEPT

### Error Handling

**FILE STATUS Codes:**
- 00 - Successful operation
- 23 - Record not found (handled during START)
- 92 - Logic error (file not opened or wrong access mode)
- Other codes - I/O errors (program stops with error message)

**Business Rule Validations:**
- Date comparison: CLIENT-LAST-DATE <= WS-CUTOFF-DATE (inactive clients)
- Write to archive file before deletion
- Check FILE STATUS after every operation (OPEN, READ, WRITE, DELETE, CLOSE)
- Display error messages and stop if critical errors occur

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

**Important:** Inline DD * data is padded to 80 bytes. You must either:
- Define VSAM with RECORDSIZE(80,80) to match inline format and add FILLER PIC X(46) to FD CLIENT-REC in COBOL program, OR
- Create temporary PS file with exact record length (34 bytes), load data there first, then REPRO to VSAM. Example in [JCL SAMPLES/DATAVSAM.jcl](../../JCL%20SAMPLES/DATAVSAM.jcl) uses SORT utility (can also be done with ICETOOL, IEBGENER)

### Step 3: Prepare Parameter File

Create PARAM-FILE (PS dataset) with cutoff date:

**Option A: Upload via ISPF 3.4**

Upload  to PS dataset manually via ISPF

**Option B: Create via JCL with inline data**

```JCL
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
20231231
/*
//SYSUT2   DD DSN=Z73460.TASK06.PARAM.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(RECFM=F,LRECL=80,BLKSIZE=80)
```

### Step 4: Execute Program

**Submit:** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**Check:** SYSOUT for statistics and FILE STATUS messages  
**Review:** [DATA/ARCHIVE-OLD-OUTPUT](DATA/ARCHIVE-OLD-OUTPUT) for archived clients

### Step 5: Verify Results

**Compare files:**
- CLIENT-FILE - Should contain only active clients (CLIENT-LAST-DATE > CUTOFF-DATE)
- ARCH-FILE - Should contain all inactive clients (CLIENT-LAST-DATE <= CUTOFF-DATE)
- Verify counts: REC-READ = REC-DELETE + REC-KEPT

## Key Concepts Demonstrated

- VSAM Dynamic Access - Sequential read with random delete capability
- START clause - Position file pointer at beginning (KEY IS NOT LESS THAN LOW-VALUES)
- READ NEXT - Sequential browsing through VSAM file
- DELETE statement - Remove current record after READ NEXT
- Date comparison logic - Numeric comparison for YYYYMMDD format (<=)
- Archival pattern - Write before delete to preserve data
- FILE STATUS checking - Validate all file operations
- External parameter file - Read configuration from PS file

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

- Program uses DYNAMIC access mode (allows both sequential and random operations)
- START positions file pointer at beginning using LOW-VALUES
- READ NEXT advances sequentially through file in key order
- DELETE removes current record (must follow READ NEXT)
- After DELETE, next READ NEXT automatically advances to next record
- Archive file written sequentially - order matches VSAM key order
- Date comparison uses <= (less than or equal) - records with exact cutoff date are also archived
- Parameter file uses fixed 80-byte records with RECFM=F
- All counters use PIC 9(5) for up to 99,999 records
- Display formatting uses edited fields (Z(4)9) to suppress leading zeros
- Tested on IBM z/OS with Enterprise COBOL
