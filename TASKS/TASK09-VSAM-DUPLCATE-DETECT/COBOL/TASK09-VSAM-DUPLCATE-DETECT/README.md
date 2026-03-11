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

**Access Mode:**
- SEQUENTIAL (for full scan and sort input)

**Organization:**
- INDEXED (KSDS)

**KEY:**
- MAST-ID (PIC X(6))

**Record Format:**
- Fixed, LRECL=74

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

**Access Mode:**
- OUTPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed Block (RECFM=FB, LRECL=80)

**Expected Output:** [DATA/DUPLICATE-REPORT](DATA/DUPLICATE-REPORT)

### Duplicate Detection Logic

**Duplicate Key:**
- Two or more records are duplicates if they share the same MAST-NAME AND MAST-BIRTH

**Sort Order:**
- Primary: MAST-NAME ascending
- Secondary: MAST-BIRTH ascending
- Tertiary: MAST-ID ascending (for stable ordering within group)

**Group Buffer:**
- Maximum 50 records per group (WS-GROUP-TABLE OCCURS 50 TIMES)
- If group exceeds 50 records, WARNING displayed to SYSOUT

**Report Content:**
- Only records belonging to groups with size > 1 are written
- All members of a duplicate group are written together
- No header line in report (detail lines only)

**Examples:**
- IVANOV IVAN, 19991201: IDs 000101, 000102, 000103 → 3 records (duplicate group)
- PETROV PAVEL, 19880520: ID 000104 only → 1 record (unique, not written)
- SMIRNOVA MARIA, 20010415: IDs 000105, 000106 → 2 records (duplicate group)
- POPOVA ANNA, 19870305: IDs 000108, 000109, 000110 → 3 records (duplicate group)

**Statistics:**
- TOTAL RECORDS PROCESSED: total count of all records read
- GROUPS WITH DUPLICATES: count of distinct name+birthdate groups with size > 1
- SUSPICIOUS RECORDS FOUND: total count of all records written to report

### Error Handling

**FILE STATUS Codes (Report File):**
- 00 - Successful operation
- Other codes - I/O errors (program displays error and stops)

**Group Buffer Overflow:**
- If group exceeds 50 records: WARNING displayed to SYSOUT, excess records skipped
- Processing continues with next records

## Program Flow

1. **Initialization:**
   - Opens DUPLICATE-REPORT-FILE (PS) for output
   - Validates REP-STATUS = '00'
   - Initializes all counters (TOTAL-REC, TOTAL-GROUPS, TOTAL-DUPS = 0)

2. **SORT Statement:**
   - SORT CLIENT-SORT-WORK ON ASCENDING KEY SRT-NAME, SRT-BIRTH, SRT-ID
   - USING CLIENT-MASTER-FILE (reads all KSDS records into sort work)
   - OUTPUT PROCEDURE IS PRCSS-SORT-REC THRU PROCESS-EXIT

3. **Output Procedure: PRCSS-SORT-REC**
   - RETURN first sorted record from CLIENT-SORT-WORK
   - Sets WS-CUR-NAME and WS-CUR-BIRTH as current group key
   - Performs ADD-TO-GROUP-BUFFER
   - Loop UNTIL EOF:
     - RETURN next sorted record
     - Increments TOTAL-REC
     - **IF SRT-NAME = WS-CUR-NAME AND SRT-BIRTH = WS-CUR-BIRTH:**
       - Same group — performs ADD-TO-GROUP-BUFFER
     - **ELSE:**
       - Different group — performs WRITE-DUPLICATE-GROUP (flushes previous group)
       - Updates WS-CUR-NAME and WS-CUR-BIRTH to new group key
       - Resets WS-GROUP-COUNT to 0
       - Performs ADD-TO-GROUP-BUFFER for first record of new group
   - After loop ends: performs WRITE-DUPLICATE-GROUP (flushes last group)
   - GO TO PROCESS-EXIT

4. **ADD-TO-GROUP-BUFFER:**
   - Increments WS-GROUP-COUNT
   - If WS-GROUP-COUNT <= 50: stores record fields in WS-GROUP-TABLE(WS-GROUP-COUNT)
   - If WS-GROUP-COUNT > 50: displays buffer overflow warning

5. **WRITE-DUPLICATE-GROUP:**
   - If WS-GROUP-COUNT > 1 (group has duplicates):
     - Increments TOTAL-GROUPS
     - Loops VARYING WS-INDEX FROM 1 TO WS-GROUP-COUNT:
       - Initializes WS-REPORT-LINE
       - Moves G-ID, G-NAME, G-BIRTH, G-PSSPRT to report line fields
       - Writes REPORT-LINE to DUPLICATE-REPORT-FILE
       - Validates REP-STATUS = '00'
       - Increments TOTAL-DUPS

6. **Termination:**
   - Closes DUPLICATE-REPORT-FILE
   - Validates REP-STATUS = '00'
   - Performs DISPLAY-SUMMARY-REPORT
   - Displays statistics banner to SYSOUT
   - STOP RUN

## JCL Jobs

### 1. [IDCAMS-VSAM.jcl](JCL/IDCAMS-VSAM.jcl) - Define VSAM KSDS Cluster

**Step 1:** Delete old VSAM cluster (IDCAMS DELETE with SET MAXCC=0)
**Step 2:** Define new VSAM KSDS cluster (IDCAMS DEFINE CLUSTER):
- NAME: Z73460.TASK9.CLIENT.MAST.VSAM
- RECORDSIZE(74,74), TRACKS(15), KEYS(6,0)
- CISZ(4096), FREESPACE(10,20), INDEXED

### 2. [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

**Step 1:** Delete old DUPLICATE-REPORT file (IEFBR14)
**Step 2:** Compile and run VSAMJOB9 using MYCOMPGO proc (MEMBER=VSAMJOB9):
- VSAMDD: Z73460.TASK9.CLIENT.MAST.VSAM (DISP=SHR) — input KSDS
- SRTDD: Z73460.TASK9.SORTWORK (NEW,DELETE,DELETE, SPACE=CYL(2,2)) — sort work
- REPDD: Z73460.TASK9.DUPLCT.REPORT (NEW,CATLG,DELETE, FB,LRECL=80) — output report

## How to Run

### Step 1: Define VSAM Cluster

**Submit** [JCL/IDCAMS-VSAM.jcl](JCL/IDCAMS-VSAM.jcl)
**Verify:** IDCAMS step completes with RC=0, cluster defined in catalog

### Step 2: Load Client Master Data

Load [DATA/CLIENT-MAST-VSAM-INPUT](DATA/CLIENT-MAST-VSAM-INPUT) into VSAM KSDS via REPRO or custom load program:
