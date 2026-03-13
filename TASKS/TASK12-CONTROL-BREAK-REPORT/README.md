# TASK12 - Control Break Report: Sales by Region and Shop

Hierarchical sales report generator that reads a pre-sorted sales data file and produces a formatted report with shop-level subtotals, region-level subtotals, and a grand total. Demonstrates the classic two-level Control Break (Level Break) algorithm.

## Business Scenario

Company needs a structured sales summary across multiple regions and shops. The system must:
- Read a pre-sorted sales file (sorted by REGION, then by SHOP within region)
- Print each individual sales record as a detail line
- Detect control breaks when REGION or SHOP changes and print subtotals
- Generate hierarchical totals: SHOP subtotal → REGION subtotal → GRAND TOTAL
- Track and print statistics: total regions, total shops, total records

## Control Break Hierarchy

```
LEVEL 1 (MAJOR): REGION change
  → print SHOP subtotal (for last shop in region)
  → print REGION subtotal (sum of all shops in region)

LEVEL 2 (MINOR): SHOP change within same region
  → print SHOP subtotal (for previous shop)
  → print blank separator line
```

**Critical Requirement:** Input file MUST be pre-sorted by REGION (major key), then SHOP (minor key). The program does not sort — it only detects breaks based on previous value comparison.

## Files

### Input Files

#### 1. SALES.DATA (PS) - Sorted Sales Data File

**Access Mode:**
- INPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| SALES-REGION | X(5) | 5 | Region name (major sort key) |
| SALES-SHOP | X(5) | 5 | Shop identifier (minor sort key) |
| SALES-AMOUNT | 9(5)V99 | 7 | Sales amount (implied decimal) |
| FILLER | X(63) | 63 | Unused |

**Sample Data:** [DATA/SALES.DATA](DATA/SALES.DATA)

**Sample Records:**
```
NORTHSHOP10010000   → REGION=NORTH, SHOP=SHOP1, AMOUNT=100.00
NORTHSHOP10005000   → REGION=NORTH, SHOP=SHOP1, AMOUNT=50.00
NORTHSHOP20020000   → REGION=NORTH, SHOP=SHOP2, AMOUNT=200.00
SOUTHSHOP10030000   → REGION=SOUTH, SHOP=SHOP1, AMOUNT=300.00
```

### Output Files

#### 2. SALES.REPORT (PS) - Formatted Hierarchical Sales Report

**Access Mode:**
- OUTPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed Block (RECFM=FB, LRECL=80)

**Expected Output:** [DATA/SALES.REPORT](DATA/SALES.REPORT)

### Accumulator Logic

**TOTAL-SHOP** — running sum for current shop, reset to zero after printing SHOP subtotal  
**TOTAL-REGION** — running sum for current region (accumulates across all shops), added to GRAND-TOTAL and reset after printing REGION subtotal  
**GRAND-TOTAL** — lifetime accumulator, never reset, printed once at end of file

### Error Handling

**FILE STATUS Codes:**
- 00 - Successful operation
- Other codes - I/O errors on open, read, or write (program displays status and STOP RUN)

**CLOSE errors** treated as warnings (program displays warning but does not stop).

## Program Flow

1. **Open Files: OPEN-ALL-FILES**
   - Opens SALES-DATA-FILE (PS, INPUT), validates SALES-DATA-STATUS = '00'
   - Opens SALES-REPORT-FILE (PS, OUTPUT), validates REPORT-STATUS = '00'

2. **Initialization: INIT-FIRST-RECORD**
   - Reads first record from SALES-DATA-FILE
   - Initializes PREV-REGION and PREV-SHOP from first record
   - Sets REGION-COUNT = 1, SHOP-COUNT = 1
   - Increments TOTAL-REGION-COUNT and TOTAL-SHOP-COUNT
   - If file is empty (AT END): sets EOF = 'Y', skips all processing

3. **First Record Processing: PROCESS-FIRST-RECORD**
   - Increments REC-COUNTER
   - Accumulates SALES-AMOUNT into TOTAL-SHOP and TOTAL-REGION
   - Writes first detail line to report

4. **Main Loop: PROCESS-SALES**
   - PERFORM UNTIL EOF:
     - READ SALES-DATA-FILE
     - AT END: perform PRINT-FINAL-TOTALS, set EOF = 'Y'
     - NOT AT END: perform PROCESS-SALES-RECORD

5. **Single Record: PROCESS-SALES-RECORD**
   - Increment REC-COUNTER
   - **Level 1 Break (REGION change):**
     - perform PRINT-SHOP-TOTAL (prints and resets TOTAL-SHOP)
     - perform PRINT-REGION-TOTAL (adds TOTAL-REGION to GRAND-TOTAL, resets)
     - update PREV-REGION, increment TOTAL-REGION-COUNT, reset SHOP-COUNT = 0
     - Note: PREV-SHOP not reset — shop break triggers automatically for first record of new region (standard double-break behavior)
   - **Level 2 Break (SHOP change, same region):**
     - perform PRINT-SHOP-TOTAL
     - write blank separator line
     - update PREV-SHOP, increment TOTAL-SHOP-COUNT and SHOP-COUNT
   - Accumulate SALES-AMOUNT into TOTAL-SHOP and TOTAL-REGION
   - Write detail line: `RECORD: {REGION} {SHOP}: {AMOUNT}`

6. **Shop Subtotal: PRINT-SHOP-TOTAL**
   - If TOTAL-SHOP > 0: write `   --> SUM FOR SHOP: {AMOUNT}` line
   - Reset TOTAL-SHOP = 0

7. **Region Subtotal: PRINT-REGION-TOTAL**
   - Write `====== TOTAL FOR {PREV-REGION}: {AMOUNT} (SHOPS: {SHOP-COUNT})` line
   - Add TOTAL-REGION to GRAND-TOTAL
   - Reset TOTAL-REGION = 0
   - Write blank separator line

8. **Final Totals: PRINT-FINAL-TOTALS**
   - Perform PRINT-SHOP-TOTAL (last shop)
   - Perform PRINT-REGION-TOTAL (last region)
   - Write blank separator
   - Write `********************************`
   - Write `GRAND TOTAL SALES: {AMOUNT}`
   - Write `********************************`
   - Write `REGIONS: {N}`, `TOTAL SHOPS: {N}`, `TOTAL RECORDS: {N}`

9. **Close Files: CLOSE-ALL-FILES**
   - Close both files, display warnings on non-zero status (not fatal)

## JCL Jobs

### [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure.

- PSSDD: SALES.DATA (sorted sales input)
- REPDD: SALES.REPORT (report output)

## How to Run

### Step 1: Allocate and Load Sales Data File

- Upload [DATA/SALES.DATA](DATA/SALES.DATA)

### Step 2: Execute Report Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)
**Review** [DATA/SALES.REPORT](DATA/SALES.REPORT) for expected report output

### Step 3: Verify Results

- **Detail lines:** 12 records printed
- **Shop subtotals:** 8 lines (2 shops × 4 regions)
- **Region totals:** 4 lines (NORTH: 350.00, SOUTH: 650.00, EAST: 750.00, WEST: 500.00)
- **Grand total:** 2250.00
- **Statistics:** REGIONS: 4, TOTAL SHOPS: 8, TOTAL RECORDS: 12

## Common Issues

### Issue 1: Report File Not Allocated

**Cause:** STEP005 (IEFBR14 delete) failed or output REPDD DSN mismatch
**Solution:** Verify STEP005 RC=0, check REPDD DSN matches Z73460.TASK12.SALES.REPORT in both steps

### Issue 2: Last Region or Last Shop Total Missing

**Cause:** PRINT-FINAL-TOTALS not triggered — AT END logic issue
**Solution:** Verify PRINT-FINAL-TOTALS is performed inside AT END clause of READ in PROCESS-SALES loop
Check that REC-COUNTER > 0 condition is satisfied before printing

### Issue 3: GRAND-TOTAL Shows Zero

**Cause:** TOTAL-REGION reset before being added to GRAND-TOTAL in PRINT-REGION-TOTAL
**Solution:** Verify order in PRINT-REGION-TOTAL: ADD TOTAL-REGION TO GRAND-TOTAL must come BEFORE MOVE 0 TO TOTAL-REGION

### Issue 4: SHOP-COUNT Shows 0 in Region Total

**Cause:** SHOP-COUNT reset to 0 during region break before printing region total
**Solution:** Verify PRINT-REGION-TOTAL uses SHOP-COUNT (which still holds count for completed region) and that reset happens AFTER the WRITE

### Issue 5: First Record Not Processed

**Cause:** Program reads first record in INIT-FIRST-RECORD but skips PROCESS-FIRST-RECORD due to EOF flag
**Solution:** Verify IF NOT EOF condition wrapping PROCESS-FIRST-RECORD and PROCESS-SALES calls in MAIN-LOGIC

### Issue 6: Abend S0C7 (Data Exception)

**Cause:** Non-numeric SALES-AMOUNT in input records
**Solution:** Verify SALES-DATA-INPUT SALES-AMOUNT field is exactly 7 numeric digits at offset 10 (5 integer + 2 decimal, no decimal point in data)

### Issue 7: TOTAL-SHOP-COUNT vs SHOP-COUNT Confusion

**Cause:** Two separate shop counters serve different purposes
- **SHOP-COUNT:** shops within current region only (reset on region break), used in region total line `(SHOPS: N)`
- **TOTAL-SHOP-COUNT:** lifetime count of all shops across all regions, reported in final statistics

## Notes

- Input file MUST be pre-sorted; no internal sorting is performed
- Two-pass design: INIT-FIRST-RECORD reads record 1 separately, then PROCESS-SALES reads records 2–N
- FUNCTION TRIM used for clean numeric display in STRING operations
- TOTAL-SHOP reset guard: PRINT-SHOP-TOTAL only writes if TOTAL-SHOP > 0 (prevents spurious zero lines)
- PS-to-PS batch processing
- Tested on IBM z/OS with Enterprise COBOL
