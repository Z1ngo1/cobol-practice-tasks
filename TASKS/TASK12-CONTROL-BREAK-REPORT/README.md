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

**Access Mode:** INPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)

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

**Access Mode:** OUTPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed Block (RECFM=FB, LRECL=80)

**Expected Output:** [DATA/SALES.REPORT](DATA/SALES.REPORT)

### Accumulator Logic

**TOTAL-SHOP** — running sum for current shop, reset to zero after printing SHOP subtotal  
**TOTAL-REGION** — running sum for current region (accumulates across all shops), added to GRAND-TOTAL and reset after printing REGION subtotal  
**GRAND-TOTAL** — lifetime accumulator, never reset, printed once at end of file

### Error Handling

**FILE STATUS Codes (SALES.DATA, SALES.REPORT):**
- 00 - Successful operation
- 35 - File not found on OPEN (program displays status and STOP RUN)
- Other codes - I/O errors on open, read, or write (program displays status and STOP RUN)

CLOSE errors treated as warnings (display only, no STOP RUN).

## Program Flow

1. **Initialization**
   - Opens SALES-DATA-FILE and SALES-REPORT-FILE; validates FILE STATUS '00' on each
   - Reads first record; initializes PREV-REGION, PREV-SHOP, and counters
   - Writes first detail line; accumulates SALES-AMOUNT into TOTAL-SHOP and TOTAL-REGION

2. **Main Processing Loop**
   - Reads records sequentially until EOF
   - **REGION change (Level 1 break):** prints SHOP subtotal, prints REGION subtotal (ADD TOTAL-REGION TO GRAND-TOTAL before reset), updates PREV-REGION, resets SHOP-COUNT
   - **SHOP change within region (Level 2 break):** prints SHOP subtotal, writes blank separator, updates PREV-SHOP, increments TOTAL-SHOP-COUNT and SHOP-COUNT
   - Accumulates SALES-AMOUNT into TOTAL-SHOP and TOTAL-REGION; writes detail line
   - AT END: flushes last SHOP subtotal, last REGION subtotal, then prints GRAND-TOTAL and statistics banner

3. **Termination**
   - Closes both files (non-zero status on CLOSE is warning only, not fatal)
   - STOP RUN

## JCL Jobs

### [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure.

- PSSDD: SALES.DATA (sorted sales input)
- REPDD: SALES.REPORT (report output)

**PROC reference:** [JCLPROC/MYCOMPGO.jcl](../../JCLPROC/MYCOMPGO.jcl)

## How to Run

### Step 1: Allocate and Load Sales Data File

- Upload [DATA/SALES.DATA](DATA/SALES.DATA)

**Alternative:**
1. Create PS file with LRECL=80 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length of your file data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 2: Execute Report Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**Review** [DATA/SALES.REPORT](DATA/SALES.REPORT) for expected report output

**Alternative:**
If you prefer to compile and run separately, use these jobs:
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)

### Step 3: Verify Results

- **Detail lines:** 12 records printed
- **Shop subtotals:** 8 lines (2 shops × 4 regions)
- **Region totals:** 4 lines (NORTH: 350.00, SOUTH: 650.00, EAST: 750.00, WEST: 500.00)
- **Grand total:** 2250.00
- **Statistics:** REGIONS: 4, TOTAL SHOPS: 8, TOTAL RECORDS: 12

## Common Issues

### Issue 1: Report File Not Allocated

**Cause:** IEFBR14 delete step failed or output REPDD DSN mismatch
**Solution:** Verify delete step RC=0; check REPDD DSN matches Z73460.TASK12.SALES.REPORT in both steps

### Issue 2: Abend S0C7 (Data Exception)

**Cause:** Non-numeric SALES-AMOUNT in input records
**Solution:** Verify SALES-AMOUNT field is exactly 7 numeric digits at offset 10 (5 integer + 2 decimal, no decimal point in data)

## Notes

- Input file MUST be pre-sorted; no internal sorting is performed
- Two-pass design: INIT-FIRST-RECORD reads record 1 separately, then PROCESS-SALES reads records 2–N
- PRINT-SHOP-TOTAL only writes if TOTAL-SHOP > 0 — prevents spurious zero lines on region break
- Tested on IBM z/OS with Enterprise COBOL
