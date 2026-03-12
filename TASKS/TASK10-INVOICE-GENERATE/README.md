# TASK10 - VSAM Invoice Generation System

Invoice generation system that reads daily orders from a PS file, enriches each order with product data from a VSAM KSDS master file via random access, calculates total costs, and writes a formatted invoice file.

## Business Scenario

Company processes daily customer orders and generates invoices with full product details and calculated costs. The system must:
- Read daily orders file (PS) with ORDER-ID, PRODUCT-ID, QUANTITY
- Look up each PRODUCT-ID in VSAM KSDS product master file (random access)
- If product found: calculate TOTAL-COST = QUANTITY × UNIT-PRICE and write invoice line
- If product not found (FILE STATUS '23'): log error to SYSOUT, skip invoice, increment error counter
- Generate summary statistics: total orders processed, invoices created, errors

## Files

### Input Files

#### 1. PROD.MASTER.VSAM (VSAM KSDS) - Product Master File

**Access Mode:**
- RANDOM (direct lookup by PRODUCT-ID key)

**Organization:**
- INDEXED (KSDS)

**KEY:** 
- PRODUCT-ID (PIC X(5))

**Record Format:**
- Fixed, LRECL=32

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| PRODUCT-ID | X(5) | 5 | Product ID (Primary Key) |
| PRODUCT-NAME | X(20) | 20 | Product name |
| UNIT-PRICE | 9(5)V99 | 7 | Unit price (packed, no decimal point) |

[DATA/PROD.MASTER.VSAM](DATA/PROD.MASTER.VSAM)

#### 2. ORDERS.DAILY (PS) - Daily Orders File

**Access Mode:**
- INPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed (RECFM=F, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| ORDER-ID | X(5) | 5 | Order ID |
| ORDER-PRODUCT-ID | X(5) | 5 | Product ID to look up in VSAM |
| ORDER-QUANTITY | 9(3) | 3 | Quantity ordered |
| FILLER | X(67) | 67 | Unused |

**Sample Data:** [DATA/ORDERS.DAILY](DATA/ORDERS.DAILY)

### Output Files

#### 3. INVOICE.FILE (PS) - Invoice Output File

**Access Mode:**
- OUTPUT (Sequential)

**Organization:**
- SEQUENTIAL

**Record Format:**
- Fixed Block (RECFM=FB, LRECL=80)

**Record Layout:**
| Field | PIC | Length | Description |
|---|---|---|---|
| INVOICE-ORDER-ID | X(5) | 5 | Order ID |
| FILLER | X(1) | 1 | Space |
| INVOICE-PRODUCT-NAME | X(20) | 20 | Product name from VSAM |
| FILLER | X(1) | 1 | Space |
| INVOICE-QUANTITY | 9(3) | 3 | Quantity |
| FILLER | X(1) | 1 | Space |
| INVOICE-TOTAL-COST | Z(6).99 | 9 | Calculated total cost |
| FILLER | X(40) | 40 | Unused |

**Expected Output:** [DATA/INVOICE.FILE](DATA/INVOICE.FILE)

### Invoice Calculation Logic

**Formula:**
- TOTAL-COST = ORDER-QUANTITY × UNIT-PRICE

**Product Found (VSAM-STATUS = '00'):**
- Calculate TOTAL-COST
- Write enriched invoice line with PRODUCT-NAME and TOTAL-COST
- Increment TOTAL-INVOICES counter

**Product Not Found (VSAM-STATUS = '23'):**
- Display: ORDER {ORDER-ID}: PRODUCT {PRODUCT-ID} NOT FOUND.
- Increment TOTAL-ERRORS counter
- Skip invoice line — no output record written

**Other VSAM Error (any other status):**
- Display: VSAM READ ERROR: {status}
- Increment TOTAL-ERRORS counter
- STOP RUN

**Examples:**
- ORDER 10001, PRODUCT 00100 (APPLE GREEN), QTY 010 → 010 × 20.50 = 205.00 (written)
- ORDER 10005, PRODUCT 00500 (MILK WHOLE), QTY 050 → 050 × 22.50 = 1125.00 (written)
- ORDER 10006, PRODUCT 99999, QTY 001 → NOT FOUND, error logged, skipped
- ORDER 10010, PRODUCT 88888, QTY 003 → NOT FOUND, error logged, skipped
- ORDER 10007, PRODUCT 00600 (LAPTOP DELL), QTY 002 → 002 × 12000.00 = 24000.00 (written)

### Error Handling

**FILE STATUS Codes (VSAM - PRODUCT-MASTER-FILE):**
- 00 - Product found successfully
- 23 - Product not found (logged, order skipped, processing continues)
- 92/93 - VSAM not defined or damaged (program stops)
- Other - Unexpected VSAM error (program stops)

**FILE STATUS Codes (ORDERS-FILE and INVOICE-FILE):**
- 00 - Successful operation
- Other codes - I/O errors (program displays error and stops)

## Program Flow

1. **Initialization: OPEN-ALL-FILES**
   - Opens PRODUCT-MASTER-FILE (VSAM KSDS, INPUT, RANDOM)
   - Validates VSAM-STATUS = '00'
   - Opens DAILY-ORDERS-FILE (PS, INPUT)
   - Validates ORDERS-STATUS = '00'
   - Opens INVOICE-OUTPUT-FILE (PS, OUTPUT)
   - Validates OUT-STATUS = '00'

2. **Main Loop: PROCESS-ORDERS**
   - Loop PERFORM UNTIL EOF:
     - READ DAILY-ORDERS-FILE AT END SET EOF TO TRUE
     - If ORDERS-STATUS = '00': increment TOTAL-ORDERS, perform PROCESS-ORDER
     - If ORDERS-STATUS ≠ '00': display error, STOP RUN

3. **Single Order Processing: PROCESS-ORDER**
   - MOVE ORDER-PRODUCT-ID TO PRODUCT-ID (set VSAM key)
   - READ PRODUCT-MASTER-FILE (random lookup)
   - EVALUATE VSAM-STATUS:
     - **'00'**: perform WRITE-INVOICE-LINE
     - **'23'**: display NOT FOUND message, increment TOTAL-ERRORS
     - **OTHER**: display VSAM READ ERROR, increment TOTAL-ERRORS, STOP RUN

4. **Write Invoice: WRITE-INVOICE-LINE**
   - COMPUTE CALC-TOTAL-COST = UNIT-PRICE × ORDER-QUANTITY
   - Move ORDER-ID, PRODUCT-NAME, ORDER-QUANTITY, CALC-TOTAL-COST to OUT-REC
   - WRITE OUT-REC
   - Validate OUT-STATUS = '00'
   - Increment TOTAL-INVOICES

5. **Termination: CLOSE-ALL-FILES + DISPLAY-SUMMARY**
   - Close all three files with status validation
   - Display summary banner to SYSOUT:
     - TOTAL ORDERS PROCESSED
     - TOTAL INVOICES CREATED
     - TOTAL ERRORS

## JCL Jobs

### 1. [DEFKSDS.jcl](JCL/DEFKSDS.jcl) - Define VSAM KSDS Cluster

Defines KSDS cluster for product master file.

**Key Parameters:**
- RECORDSIZE(32,32) - Fixed 32-byte records
- KEYS(5,0) - 5-byte key starting at position 0
- INDEXED - KSDS organization

### 2. [COMPRUN.jcl](JCL/COMPRUN.jcl) - Compile and Run

Standard compile-link-go JCL using MYCOMPGO procedure.

- VSAMDD - PROD.MASTER.VSAM (VSAM KSDS)
- ORDD: ORDERS.DAILY (PS input)
- OUTDD: INVOICE.FILE (output report)

## How to Run

### Step 1: Define VSAM Cluster

**Submit** [DEFKSDS.jcl](JCL/DEFKSDS.jcl)
**Verify:** IDCAMS completes RC=0, cluster defined

### Step 2: Load Product Master Data

**Use File Manager**
1. Navigate to VSAM file in ISPF 
2. Open with File Manager (FM)
3. Insert records manually from [DATA/PROD.MASTER.VSAM](DATA/PROD.MASTER.VSAM) 

**Alternative:**  
1. Define VSAM with RECORDSIZE(80,80) to match inline format
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Create temporary PS file with exact record length (32 bytes), load data there first, then REPRO to VSAM.
- Example in [JCL SAMPLES/DATAVSAM.jcl](../../JCL%20SAMPLES/DATAVSAM.jcl) uses SORT utility (can also be done with ICETOOL, IEBGENER)

### Step 3: Allocate Daily Orders File

- Upload [DATA/ORDERS.DAILY](DATA/ORDERS.DAILY) to PS dataset manually via ISPF

**Alternative:**
1. Create PS file with LRECL=80 and insert inline data using IEBGENER:
- **⚠️ Note:** Inline DD * data is padded to 80 bytes. Verify FD includes FILLER to match LRECL/RECORDSIZE.
2. Allocate PS file and insert exact length of your file transaction data using IEBGENER (see [JCL SAMPLES/DATA2PS.jcl](../../JCL%20SAMPLES/DATA2PS.jcl) for example)

### Step 4: Execute Program

**Submit** [JCL/COMPRUN.jcl](JCL/COMPRUN.jcl)
**See** [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for execution summary and error messages
**Review** [DATA/INVOICE.FILE](DATA/INVOICE.FILE) for expected invoice output

**Alternative:**
If you prefer to compile and run separately, use these jobs:  
- [JCL SAMPLES/JCLCOMP.jcl](../../JCL%20SAMPLES/JCLCOMP.jcl)
- [JCL SAMPLES/JCLRUN.jcl](../../JCL%20SAMPLES/JCLRUN.jcl)


### Step 5: Verify Results

- **Count invoice lines:** Should be 10 (12 orders minus 2 not-found errors)
- **Verify not-found errors:** SYSOUT should show ORDER 10006: PRODUCT 99999 NOT FOUND and ORDER 10010: PRODUCT 88888 NOT FOUND
- **Check calculations:** LAPTOP DELL QTY 002 → 24000.00, MILK WHOLE QTY 050 → 1125.00
- **Confirm summary:** TOTAL ORDERS=12, TOTAL INVOICES=10, TOTAL ERRORS=2

## Common Issues

### Issue 1: FILE STATUS '92' or '93' on VSAM Open

**Cause:** VSAM KSDS not defined or cluster definition missing
**Solution:** Re-run JCL/DEFKSDS.jcl, verify IDCAMS RC=0

### Issue 2: FILE STATUS '23' for All Orders

**Cause:** VSAM product master file is empty or wrong key field offset
**Solution:** Verify data loaded via REPRO, check KEYS(5,0) matches PRODUCT-ID PIC X(5) at offset 0

### Issue 3: Abend S0C7 (Data Exception)

**Cause:** Non-numeric data in UNIT-PRICE or ORDER-QUANTITY field
**Solution:** Verify PROD-MASTER-VSAM-INPUT UNIT-PRICE is 7 digits with no spaces (e.g. 0002050)

### Issue 4: Wrong TOTAL-COST in Invoice

**Cause:** UNIT-PRICE misread due to incorrect RECORDSIZE or LRECL mismatch
**Solution:** Verify RECORDSIZE(32,32) in IDCAMS matches FD PRODUCT-MASTER-FILE record structure (5+20+7=32 bytes)

### Issue 5: Invoice File Not Allocated or OUTDD Missing

**Cause:** OUTDD DD statement missing or previous INVOICE.FILE not deleted
**Solution:** Verify STEP005 (IEFBR14 delete) completed RC=0 before STEP010

### Issue 6: All 12 Orders Written to Invoice (No Errors)

**Cause:** VSAM returns '00' even for non-existent keys — PRODUCT-ID key mismatch
**Solution:** Verify ORDER-PRODUCT-ID is correctly moved to PRODUCT-ID before READ

## Program Output (SYSOUT)

Expected execution log — see [OUTPUT/SYSOUT.txt](OUTPUT/SYSOUT.txt) for full output

## Notes

- Program uses VSAM KSDS in RANDOM access mode — each order triggers a direct READ by key
- No sequential scan of VSAM — lookup is O(1) by PRODUCT-ID key
- FILE STATUS '23' is a soft error — processing continues with next order
- Any other non-zero VSAM status is a hard error — program stops immediately
- UNIT-PRICE stored as PIC 9(5)V99 (implied decimal) — no decimal point in physical data
- CALC-TOTAL-COST uses COMP-3 (packed decimal) for efficient arithmetic
- Invoice output uses Z(6).99 edit picture for right-aligned display with decimal point
- Three independent FILE STATUS variables track VSAM, orders, and invoice files separately
- Tested on IBM z/OS with Enterprise COBOL
