# TASK16 - Binary Search (SEARCH ALL): Parts Catalog Lookup

Order invoice generator that loads a parts catalog into an in-memory table and uses COBOL's native binary search (`SEARCH ALL`) to find part prices by ID. Demonstrates the COBOL `SEARCH ALL` verb with `ASCENDING KEY IS` table declaration and sentinel-value pre-initialization for safe binary search operation.

## Business Scenario

Company processes customer orders referencing part IDs from a product catalog. Orders are not required to be sorted. The system must:
- Load PARTS.CATALOG into an in-memory table (max 100 entries), pre-sorted ascending by PART-ID
- Pre-initialize all 100 table slots with sentinel value `90000` before loading real data
- For each order: use `SEARCH ALL` (binary search O(log n)) to find the part price
- If found: calculate TOTAL = PRICE × QUANTITY and write invoice line
- If not found: write `ORDER-NUM NOT FOUND` line to invoice
- Display processing summary to SYSOUT

## Why SEARCH ALL Instead of Linear SEARCH

| Aspect | SEARCH (linear, TASK14/15) | SEARCH ALL (binary, TASK16) |
|---|---|---|
| Algorithm | Sequential scan O(n) | Binary search O(log n) |
| Table declaration | `OCCURS n TIMES INDEXED BY IDX` | `OCCURS n TIMES ASCENDING KEY IS key INDEXED BY IDX` |
| Sort requirement | Not required | Table MUST be sorted by key field |
| Match condition | Any condition (`>=`, `=`, etc.) | Equality only (`WHEN key = value`) |
| Sentinel init | Not required | Required — unused slots must be filled with a value > all real keys |
| Use case | Small tables, tiered lookups | Large tables, exact-match catalog lookups |

## Sentinel Pre-Initialization

Before loading real data, all 100 table slots are filled with `WS-PART-ID = 90000`:

```cobol
PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 100
    MOVE 90000 TO WS-PART-ID(IDX)
    MOVE ZERO  TO WS-PRICE(IDX)
END-PERFORM.
```

**Why this is required:** `SEARCH ALL` performs a binary search across the full OCCURS range (all 100 slots). Unused slots after the last loaded part must have keys larger than any real PART-ID to maintain the ascending sort order. Without initialization, binary search may land on garbage data and produce incorrect results or abend.

## Files

### Input Files

#### 1. PARTS.CATALOG (PS) - Parts Catalog Reference File

**Access Mode:** INPUT (Sequential, loaded once at startup)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  
**Sort Requirement:** MUST be pre-sorted ascending by PART-ID — required by `SEARCH ALL`  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| PART-ID | 9(5) | 5 | Part number (binary search key) |
| PART-PRICE | 9(3)V99 | 5 | Unit price (implied decimal) |

**Sample Data:** [DATA/PARTS.CATALOG](DATA/PARTS.CATALOG)

#### 2. ORDERS.FILE (PS) - Customer Orders File

**Access Mode:** INPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed (RECFM=F, LRECL=80)  
**Sort Requirement:** None — orders can be in any order (unlike TASK13 Match-Merge)  

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| ORDR-NUM | 9(5) | 5 | Order number (written to invoice) |
| ORDR-ID | 9(5) | 5 | Part ID to look up in catalog |
| ORDR-QUANT | 9(3) | 3 | Quantity ordered |

**Sample Data:** [DATA/ORDERS.FILE](DATA/ORDERS.FILE)

### Output Files

#### 3. INVOICE.TXT (PS) - Invoice Report File

**Access Mode:** OUTPUT (Sequential)  
**Organization:** SEQUENTIAL  
**Record Format:** Fixed Block (RECFM=FB, LRECL=80)  

**Expected Output:** [DATA/INVOICE.TXT](DATA/INVOICE.TXT)

### Error Handling

**FILE STATUS Codes (all three files):**
- 00 - Successful operation
- Other codes - I/O errors on open, read, or write (program displays status and STOP RUN)

CLOSE errors treated as warnings (display only, no STOP RUN).

## Program Flow

1. **Initialization**
   - Opens all three files; validates FILE STATUS '00' on each
   - Pre-initializes all 100 CATALOG-TABLE slots: MOVE 90000 TO WS-PART-ID(IDX), MOVE ZERO TO WS-PRICE(IDX)
   - Loads PARTS.CATALOG into table slots 1–9: stores WS-PART-ID(IDX) and WS-PRICE(IDX), increments PARTS-LOADED; closes PARTS.CATALOG after load

2. **Main Processing Loop**
   - Reads ORDERS.FILE sequentially until EOF; increments ORDERS-PROCESSED per record
   - For each order: SET NOT-FOUND TO TRUE, then SEARCH ALL CATALOG-ENTRY WHEN WS-PART-ID(IDX) = ORDR-ID
   - Match found → SET FOUND TO TRUE; COMPUTE WS-TOTAL-COST = WS-PRICE(IDX) × ORDR-QUANT; writes invoice line with order number and total
   - Not found → AT END CONTINUE; writes invoice line with order number and 'NOT FOUND'
   - Increments PARTS-FOUND or PARTS-NOT-FOUND; increments INVOICES-WRITTEN

3. **Termination**
   - Closes remaining files (non-zero status on CLOSE is warning only)
   - Displays summary to SYSOUT: PARTS LOADED / ORDERS PROCESSED / INVOICES WRITTEN / PARTS FOUND / PARTS NOT FOUND
   - STOP RUN

## JCL Jobs

### [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure

- PARTDD: PARTS.CATALOG (parts catalog reference)
- ORDRDD: ORDERS.FILE (customer orders)
- INVODD: INVOICE.TXT (invoice output)

**PROC reference:** [JCLPROC/MYCOMPGO.jcl](../../JCLPROC/MYCOMPGO.jcl)

## How to Run

### Step 1: Allocate and Load Parts Catalog File

Upload [DATA/PARTS.CATALOG](DATA/PARTS.CATALOG) to PS dataset manually via ISPF

**Alternative:**
1. Create PS file with LRECL=80 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 2: Allocate and Load Orders File

Upload [DATA/ORDERS.FILE](DATA/ORDERS.FILE) to PS dataset manually via ISPF

**Alternative:**
1. Create PS file with LRECL=80 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 3: Execute Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)  
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution summary  
**Review** [DATA/INVOICE.TXT](DATA/INVOICE.TXT) for expected invoice output 

**Alternative:**
If you prefer to compile and run separately, use these jobs:
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)

### Step 4: Verify Results

- **Parts loaded:** 9
- **Orders processed:** 12
- **Invoices written:** 12 (all orders produce a line — found or NOT FOUND)
- **Parts found:** 6
- **Parts not found:** 6

## Common Issues

### Issue 1: SEARCH ALL Always Goes to AT END

**Cause:** PARTS.CATALOG not sorted ascending by PART-ID, or `ASCENDING KEY IS WS-PART-ID` missing from OCCURS declaration  
**Solution:** Verify DATA/PARTS-CATALOG-INPUT rows are in ascending PART-ID order; confirm `ASCENDING KEY IS WS-PART-ID` is present in the OCCURS clause

### Issue 2: Corrupt Results After Last Real Part

**Cause:** Sentinel initialization missing — slots after PARTS-LOADED contain binary zeros, violating ascending sort order required by SEARCH ALL  
**Solution:** Verify the PERFORM VARYING loop sets WS-PART-ID(IDX) = 90000 for all 100 slots BEFORE the READ loop loads real data; sentinel value must be > all real PART-IDs (real max is 05000, sentinel is 90000)

### Issue 3: Abend S0C7

**Cause:** Non-numeric PART-PRICE in PARTS.CATALOG (bytes 6–10) or non-numeric ORDR-QUANT in ORDERS.FILE (bytes 11–13)  
**Solution:** Verify PARTS-CATALOG-INPUT PART-PRICE is exactly 5 numeric digits at offset 5; confirm ORDR-QUANT is exactly 3 numeric digits at offset 10

## Program Output (SYSOUT)

See [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution log

## Notes

- `SEARCH ALL` requires `ASCENDING KEY IS` in OCCURS declaration — without it compiler error or wrong behavior
- Sentinel pre-initialization (`90000`) fills unused table slots to maintain ascending order across the full OCCURS range
- Orders file does NOT need to be sorted — binary search works independently per order (unlike TASK13 Match-Merge)
- Tested on IBM z/OS with Enterprise COBOL
