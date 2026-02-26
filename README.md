# Mainframe Practice Tasks

Practical mainframe development exercises: COBOL, JCL, VSAM, PS files, DB2, CICS, IMS, MQ.

These tasks were created during my self-study journey learning mainframe technologies. They may be useful for others learning COBOL or curious about how classic batch processing works. The code is functional but not perfect - there's always room for improvement, additional validation, error handling, and features. Feel free to learn from them, adapt them, or suggest enhancements.

## Repository Structure

Each task demonstrates a specific mainframe batch scenario with full code, JCL, and test data.


## Technologies

- COBOL (z/OS Enterprise COBOL)
- JCL (Job Control Language)
- VSAM (KSDS, ESDS, RRDS)
- PS files (sequential datasets)
- DB2 SQL with Embedded SQL
- CICS (planned)
- IMS (planned)
- MQ (planned)

## Quick Overview

### VSAM File Operations
Master indexed file processing with random, sequential, and dynamic access modes. Tasks include transaction updates, archival with deletion, duplicate detection, validation, and alternate index browsing.

**Tasks:** 5, 6, 9, 11, 18

### Sequential File Processing (PS Files)
Classic batch processing with PS files. Data enrichment with lookups, multi-level control break reporting, file synchronization with merge algorithm, internal table operations, and sorting.

**Tasks:** 10, 12, 13, 14, 15, 16, 17

### DB2 Embedded SQL
Database operations from COBOL batch programs. Cursor-based updates with positioned UPDATE, complex calculations, bulk inserts with validation, and UPSERT pattern implementation.

**Tasks:** 7, 8, 19, 20

## Task Catalog

### TASK05-VSAM-BANKING
Update account balances with transaction processing.

Banking transaction processor using VSAM KSDS random update. Reads PS transaction file (deposits/withdrawals), updates account master VSAM file with balance changes, generates error report for invalid accounts and insufficient funds.

**Key concepts:** VSAM Random Access, READ/REWRITE, FILE STATUS checking, transaction validation, overflow handling

---

### TASK06-VSAM-CLIENT-ARCHIVE
Archive and delete inactive records from VSAM.

Client database cleanup with VSAM Dynamic Access. Archives inactive clients based on cutoff date parameter, deletes archived records from master file using START, READ NEXT, and DELETE operations.

**Key concepts:** VSAM Dynamic Access, START clause, DELETE, sequential browsing with deletion, date comparison

---

### TASK07-DB2-SALARY-INDEX
Process and update large tables with cursors.

Employee salary indexing with DB2 cursor and positioned update. Reads employee table using cursor, applies category-based salary adjustments, implements batch commits every 100 records with rollback on errors.

**Key concepts:** DECLARE CURSOR FOR UPDATE, FETCH, UPDATE WHERE CURRENT OF, batch COMMIT/ROLLBACK strategy, SQLCODE validation

---

### TASK08-DB2-BONUS-CALC
Calculate complex updates with multiple conditions.

Sales bonus calculator with DB2 cursor. Applies region-based bonus percentages, adds extra bonus for high-volume sellers, enforces cap limit, uses positioned updates with WHERE CURRENT OF.

**Key concepts:** DECLARE CURSOR, complex calculation logic, UPDATE WHERE CURRENT OF, multiple business rules, batch commits

---

### TASK09-VSAM-DUPLICATE-DETECT
Find duplicate records in a large VSAM file.

Duplicate client detection using VSAM sequential read. Identifies potential duplicates by matching FULL-NAME and BIRTH-DATE, generates report with grouped suspicious records, implements control-break logic for grouping.

**Key concepts:** VSAM Sequential Access, duplicate detection, group processing, control break logic

---

### TASK10-INVOICE-GENERATION
Enrich data by looking up reference information.

Invoice generator with VSAM lookup enrichment. Reads order file (PS), looks up product details in VSAM catalog by product ID, calculates total cost, generates enriched invoice file.

**Key concepts:** File enrichment, VSAM Random Read for lookup, data validation, calculation logic

---

### TASK11-CARD-VALIDATION
Validate transactions against a master file.

Credit card transaction validator with VSAM lookup. Reads daily transactions, validates against card master (status, expiry date), splits transactions into approved/declined files with reason codes.

**Key concepts:** VSAM Random Read, date validation (MMYY format), multi-file output, business rules validation

---

### TASK12-CONTROL-BREAK-REPORT
Create multi-level summary reports.

Multi-level control break sales report. Produces hierarchical report with subtotals by shop and region, grand total. Demonstrates classic COBOL reporting with two-level break logic.

**Key concepts:** Control Break (2 levels), PREVIOUS fields, subtotal/total accumulation, formatted output, break processing

---

### TASK13-MASTER-SYNC
Synchronize two sorted files with add/update/delete.

Master file synchronization using merge logic. Matches sorted master and transaction files, applies changes (Add/Update/Delete), produces new master file and error report for unmatched transactions.

**Key concepts:** File matching algorithm, sequential merge, HIGH-VALUES technique, Add/Change/Delete logic, synchronized parallel reads

---

### TASK14-TAX-CALCULATION
Use internal lookup tables for reference data.

Tax calculator with in-memory lookup table. Loads tax rate table into OCCURS array, applies region-specific tax rates to employee salaries, generates detailed tax report with gross/net amounts.

**Key concepts:** Internal tables (OCCURS), table lookup with SEARCH, array indexing, numeric formatting

---

### TASK15-COMMISSION-TIERS
Calculate values based on tiered ranges.

Commission calculator with tiered percentage lookup. Loads commission tiers into memory, performs range search to find appropriate percentage based on sales amount, calculates payout.

**Key concepts:** Range-based lookup, tiered calculation, sequential search, boundary conditions

---

### TASK16-BINARY-SEARCH
Perform efficient binary search in sorted data.

Wholesale invoice generator with binary search. Loads sorted parts catalog into OCCURS array with ASCENDING KEY, uses SEARCH ALL for efficient binary search, calculates order totals.

**Key concepts:** Binary search, SEARCH ALL, ASCENDING KEY clause, INDEXED BY, sorted table operations

---

### TASK17-INTERNAL-SORT
Sort and filter data within a COBOL program.

Honor roll generator with internal SORT. Uses SORT verb with INPUT/OUTPUT procedures, filters passing grades during input, sorts by class and score descending, outputs formatted honor roll.

**Key concepts:** SORT verb, SD (Sort Description), RELEASE/RETURN, INPUT/OUTPUT PROCEDURE, filtering logic

---

### TASK18-LIBRARY-AIX
Search VSAM using alternate index with duplicates.

Library book search using VSAM Alternate Index. Searches books by author name (alternate key with duplicates), retrieves all books for each author using START and READ NEXT with duplicate handling.

**Key concepts:** VSAM AIX, ALTERNATE RECORD KEY WITH DUPLICATES, START positioning, duplicate key browsing

---

### TASK19-DB2-BULK-INSERT
Import data from files into DB2 tables.

Customer import with validation and bulk insert. Reads PS file with new customers, validates data (ID, email format, phone), inserts into DB2, handles duplicate key errors, logs success/failure with batch commits.

**Key concepts:** INSERT statement, data validation, SQLCODE error handling (-803 duplicate), batch commit strategy, error logging

---

### TASK20-DB2-UPSERT
Update existing records or insert new ones (UPSERT).

Employee synchronization with UPSERT logic. Checks if record exists with SELECT, performs UPDATE if found or INSERT if not, logs all changes with before/after values, implements batch commits.

**Key concepts:** UPSERT pattern (SELECT + UPDATE/INSERT), SQLCODE 100 (not found), change tracking, batch commit, delta logging

---

## How to Use

### Prerequisites
- IBM z/OS environment with TSO/ISPF access
  - **Tested on IBM Z Xplore** - free educational platform providing both VSCode extension (Zowe Explorer) and traditional ISPF 3270 terminal access
- COBOL compiler (Enterprise COBOL for z/OS)
- DB2 subsystem (for tasks 7, 8, 19, 20)
- CICS region (for future CICS tasks)
- **Recommended:** File Manager for z/OS (ADFz FM 15.0) for VSAM data loading via ISPF

### Running Tasks

1. Navigate to specific task folder (e.g., `TASKS/`)
2. Read task `README.md` for detailed business scenario and logic
3. Upload COBOL source from `COBOL/` to your z/OS PDS
4. Upload JCL files from `JCL/` to your JCL library
5. Prepare input datasets from `DATA/` folder
6. For VSAM tasks:
   - **Option 1 (Recommended):** Submit `DEFKSDS.jcl` to define cluster, then load data using **File Manager (FM)** utility in ISPF
   - **Option 2:** Create complete workflow JCL combining IDCAMS DEFINE, REPRO data loading, and COMPRUN steps
7. Submit `COMPRUN.jcl` to compile and execute
8. Verify output against expected results in `DATA/`
9. Check `OUTPUT/SYSOUT.txt` for execution logs (if available - not all tasks include OUTPUT folder)

### JCL Samples

The `JCL-SAMPLES/` folder contains reusable template JCL files for common mainframe operations:

- **COMPRUN.jcl** - Standard compile-and-run JCL using MYCOMPGO PROC (allocate, compile, link-edit, execute)
- **JCLCOMP.jcl** - Compile-only template (no execution step)
- **JCLRUN.jcl** - Run-only template (assumes pre-compiled load module exists)

**Difference from task-specific JCL:**
- `JCL-SAMPLES/` contains **generic templates** with placeholder names (e.g., `YOUR.DATASET`, `YOUR.MEMBER.NAME`)
- Task folders contain **customized JCL** with actual dataset names specific to that task (e.g., `Z73460.TASK05.ACCT.MASTER.VSAM`)
- Replace all `YOUR.*` placeholders with your actual dataset names and DD names when adapting templates

**Note:** Most tasks use File Manager for VSAM data loading instead of REPRO JCL, as FM provides better control and validation during development.

### MYCOMPGO Procedure

The `MYCOMPGO` file contains a cataloged procedure (PROC) used by COMPRUN.jcl for standardized compile-and-run operations:

**Parameters:**
- **MEMBER** - program name to compile and execute
- **SRCLIB** - source library containing COBOL programs (default: `Z73460.COB.PRAC`)
- **COPYLIB** - copybook library for COPY statements (default: `Z73460.COPYLIB`)
- **LOADLIB** - load module library for compiled programs (default: `Z73460.LOAD`)

**Steps:**
1. **COMP** - Compile and link-edit using IGYWCL procedure
2. **RUN** - Execute the compiled program with COND check (skips if compile fails)

**Usage:** Reference this PROC in your JCL with `//SETLIB JCLLIB ORDER=YOUR.PROCLIB` and `EXEC MYCOMPGO,MEMBER=programname`

### Typical Task Structure

Each task folder contains:
- **COBOL/** - Commented COBOL source programs with detailed logic explanations
- **JCL/** - Task-specific compile/run JCL and utility jobs (IDCAMS, IEFBR14, SORT)
- **DATA/** - Input test files and expected output files for validation
- **OUTPUT/** - Actual execution results (SYSOUT, JESMSGLG) from z/OS runs *(not present in all tasks)*
- **README.md** - Business scenario, file descriptions, logic breakdown, testing instructions

## About

Self-taught mainframe developer learning COBOL, JCL, VSAM, DB2, CICS, IMS and MQ. All code tested on real IBM z/OS environment.

**Goals:**
- Demonstrate practical mainframe batch programming skills
- Showcase COBOL best practices (error handling, FILE STATUS checks, SQLCODE validation)
- Build reusable examples for common business scenarios (banking, payroll, inventory, reporting)

**Technologies Practiced:**
- COBOL programming (z/OS Enterprise COBOL)
- JCL (Job Control Language)
- VSAM file operations (KSDS with primary and alternate indexes)
- DB2 SQL (Embedded SQL, cursors, transaction management)
- File Manager for z/OS (ADFz FM 15.0) for VSAM data manipulation
- Utilities (IDCAMS, SORT, IEFBR14)

---
