# TASK09 - VSAM Duplicate Client Detection

Duplicate client detection system that reads a VSAM KSDS master file, sorts records by name and birthdate, identifies duplicate groups, and generates a detailed report.

## Business Scenario

Company performs periodic data quality checks on the client master database. The system must:
- Read client records from VSAM KSDS master file (CLIENT.MAST.VSAM)
- Sort all records by client name (ASC) and birthdate (ASC)
- Group consecutive records with the same name and birthdate
- If group size > 1, write all records in that group to duplicate report
- Generate summary statistics: total records, duplicate groups, suspicious records

## Files

### Input Files

#### 1. CLIENT.MAST.VSAM (VSAM KSDS) - Client Master File

**Access Mode:** SEQUENTIAL (for full scan and sort input)  
**Organization:** INDEXED (KSDS)  
**KEY:** MAST-ID (PIC X(6))  
**Record Format:** Fixed, LRECL=74

**Record Layout:**
| Field | PIC | Offset | Description |
|---|---|---|---|
| MAST-ID | X(6) | 1-6 | Client ID (Primary Key) |
| MAST-NAME | X(30) | 7-36 | Full name |
| MAST-BIRTH | X(8) | 37-44 | Birthdate (YYYYMMDD) |
| MAST-PSSPRT | X(10) | 45-54 | Passport number |
| MAST-CITY | X(20) | 55-74 | City of residence |

**Sample Data:** [DATA/CLIENT.MAST.VSAM](DATA/CLIENT.MAST.VSAM)

### Output Files

#### 2. DUPLICATE-REPORT (PS) - Duplicate Client Report

**Access Mode:** OUTPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed Block (RECFM=FB, LRECL=80)  

**Expected Output:** [DATA/DUPLCT.REPORT](DATA/DUPLCT.REPORT)

### Error Handling

**FILE STATUS Codes (Report File):**
- 00 - Successful operation
- Other codes - I/O errors (program displays error and stops)

**Group Buffer Overflow:**
- If group exceeds 50 records: WARNING displayed to SYSOUT, excess records skipped
- Processing continues with next records

### Duplicate Detection Logic

**Duplicate Key:** Two or more records sharing the same MAST-NAME AND MAST-BIRTH

**Sort Order:**
- Primary: MAST-NAME ascending
- Secondary: MAST-BIRTH ascending
- Tertiary: MAST-ID ascending (stable ordering within group)

**Report Content:** Only records belonging to groups with size > 1 are written; unique records are silently skipped

**Examples:**
- **IVANOV IVAN**, 19991201: IDs 000101, 000103, 000109 → 3 records (duplicate group)
- **PETROV PAVEL**, 19880520: ID 000104 only → 1 record (unique, not written)
- **SMIRNOVA MARIA**, 20010415: IDs 000105, 000108 → 2 records (duplicate group)
- **POPOVA ANNA**, 19870305: IDs 000102, 000106, 000110 → 3 records (duplicate group)

**Statistics displayed to SYSOUT:**
- TOTAL RECORDS PROCESSED: total count of all records read
- GROUPS WITH DUPLICATES: count of distinct name+birthdate groups with size > 1
- SUSPICIOUS RECORDS FOUND: total count of all records written to report

## Program Flow

1. **Initialization**
   - Opens DUPLICATE-REPORT-FILE; validates REP-STATUS = '00'
   - Initializes counters: TOTAL-REC, TOTAL-GROUPS, TOTAL-DUPS = 0

2. **SORT Statement**
   - Sorts CLIENT-SORT-WORK ascending by SRT-NAME, SRT-BIRTH, SRT-ID
   - USING CLIENT-MASTER-FILE reads all KSDS records into sort work
   - OUTPUT PROCEDURE IS PRCSS-SORT-REC processes sorted records

3. **Output Procedure: PRCSS-SORT-REC**
   - RETURNs first sorted record; sets WS-CUR-NAME and WS-CUR-BIRTH as group key
   - Loop until EOF: RETURNs next record; if NAME+BIRTH match → ADD-TO-GROUP-BUFFER; else → WRITE-DUPLICATE-GROUP, reset group key, add first record of new group
   - After loop: flushes last group via WRITE-DUPLICATE-GROUP

4. **ADD-TO-GROUP-BUFFER**
   - Increments WS-GROUP-COUNT; stores record in WS-GROUP-TABLE if count <= 50
   - Displays buffer overflow warning if count > 50

5. **WRITE-DUPLICATE-GROUP**
   - If WS-GROUP-COUNT > 1: increments TOTAL-GROUPS, writes all group records to report, increments TOTAL-DUPS per record

6. **Termination**
   - Closes DUPLICATE-REPORT-FILE; validates REP-STATUS = '00'
   - Displays statistics banner to SYSOUT; STOP RUN

## JCL Jobs

### 1. [DEFKSDS.jcl](JCL/DEFKSDS.jcl) - Define VSAM KSDS Cluster

Defines KSDS cluster for client master file.

**Key Parameters:**
- RECORDSIZE(74,74) - Fixed 74-byte records
- KEYS(6,0) - 6-byte key starting at position 0
- INDEXED - KSDS organization

### 2. [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure.

- VSAMDD - CLIENT.MAST.VSAM (VSAM KSDS)
- SRTDD - SORTWORK (sort work)
- REPDD - DUPLCT.REPORT (output report)

**PROC reference:** [JCLPROC/MYCOMPGO.jcl](../../JCLPROC/MYCOMPGO.jcl)

## How to Run

### Step 1: Define VSAM Cluster

**Submit** [JCL/DEFKSDS.jcl](JCL/DEFKSDS.jcl)
**Verify:** Check for MAXCC=0 in job output

### Step 2: Load Master Data into VSAM

**Use File Manager**
1. Navigate to VSAM file in ISPF 
2. Open with File Manager (FM)
3. Insert records manually from [DATA/CLIENT.MAST.VSAM](DATA/CLIENT.MAST.VSAM)

**Alternative:**  
1. Define VSAM with RECORDSIZE(80,80) to match inline format
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Create temporary PS file with exact record length (74 bytes), load data there first, then REPRO to VSAM.
- Example in [JCL SAMPLES/DATAVSAM.jcl](../../JCL%20SAMPLES/DATAVSAM.jcl) uses SORT utility (can also be done with ICETOOL, IEBGENER)

### Step 3: Execute Program

**Submit:** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**Check:** SYSOUT for statistics and FILE STATUS messages (see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt))  
**Review:** [DATA/DUPLCT.REPORT](DATA/DUPLCT.REPORT) for detected duplicates

**Alternative:**
If you prefer to compile and run separately, use these jobs:  
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)

### Step 4: Verify Results

- **Count report lines:** Should match SUSPICIOUS RECORDS FOUND in SYSOUT
- **Verify groups:** IVANOV IVAN (3 records), SMIRNOVA MARIA (2 records), POPOVA ANNA (3 records)
- **Check unique records not in report:** PETROV PAVEL (000104), KOZLOV DMITRY (000107)
- **Confirm summary:** TOTAL RECORDS=10, GROUPS=3, SUSPICIOUS=8

## Common Issues

### Issue 1: FILE STATUS '92' or '93' on VSAM Open

**Cause:** VSAM KSDS not defined, damaged, or cluster definition missing  
**Solution:** Re-run JCL/DEFKSDS.jcl to redefine cluster  
Verification: LISTCAT ENTRY(Z73460.TASK9.CLIENT.MAST.VSAM) ALL

### Issue 2: FILE STATUS '23' During SORT Input Phase

**Cause:** VSAM master file is empty or wrong key field offset  
**Solution:** Verify data loaded correctly via REPRO, check KEYS(6,0) matches MAST-ID PIC X(6) at offset 0  
Confirm LISTCAT shows non-zero REC-TOTAL for the cluster

### Issue 3: Abend S0C7 (Data Exception)

**Cause:** Non-numeric or corrupted data in MAST-BIRTH field used during sort key comparison  
**Solution:** Verify MAST-BIRTH is exactly 8 bytes in YYYYMMDD format with no spaces or special characters  
Check INPUT-VSAM data file for malformed records

### Issue 4: Empty Report File (No Duplicates Detected)

**Cause:** VSAM records not matching on NAME+BIRTH key due to wrong layout or trailing spaces mismatch  
**Solution:** Verify VSAM records have correct layout — MAST-NAME starts at offset 7 (length 30), MAST-BIRTH at offset 37 (length 8)  
Check that INPUT-VSAM data was loaded with correct LRECL=74

## Program Output (SYSOUT)

Expected execution log - see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for full output example.

## Notes

- COBOL SORT verb with OUTPUT PROCEDURE allows group detection on sorted stream without intermediate file
- GROUP BUFFER (OCCURS 50 TIMES) holds the entire group before deciding to write or skip — required because group size is unknown until the next different key arrives
- Duplicate key is composite: MAST-NAME (30 chars) + MAST-BIRTH (8 chars); records differing in any other field are still duplicates
- Tested on IBM z/OS with Enterprise COBOL
