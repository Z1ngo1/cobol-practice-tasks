# TASK26 — DB2 + VSAM: Robust Payment Batch

## Overview

Batch COBOL program that processes a payment input file (PS), validates each record, looks up the customer account in VSAM, and updates the customer balance in the DB2 table `TB.TB_CUSTOMER_BALANCE`. All results are logged to SYSOUT and a final return code is set based on error severity.

## Files

| File | Description |
|------|-------------|
| `COBOL/DB2VSM26.cbl` | Main COBOL program — Robust Payment Batch |
| `DCLGEN/TASK26.cpy` | DCLGEN copybook for `TB.TB_CUSTOMER_BALANCE` |
| `JCL/COBDB2CP.jcl` | JCL to compile and run the program |
| `SQL/INSERT.DATA.sql` | Seed data for `TB.TB_CUSTOMER_BALANCE` |
| `DATA/PAYMENT.INPUT` | Payment input PS file (sequential) |
| `DATA/VSAM.ACCOUNTS` | VSAM KSDS customer account file |

## DB2 Objects

| Object | Type | Description |
|--------|------|-------------|
| `TB.TB_CUSTOMER_BALANCE` | Table | Stores customer balance; key: `CUST_ID` |

### DB2 Table Layout — TB.TB_CUSTOMER_BALANCE

| Column | Type | Description |
|--------|------|-------------|
| `CUST_ID` | CHAR(5) | Customer identifier (PK) |
| `CUST_BALANCE` | DECIMAL(9,2) | Current customer balance |

## VSAM Structure

| File | Organization | Key Field | Description |
|------|-------------|-----------|-------------|
| VSAM.ACCOUNTS | KSDS | CUST-ID (5 bytes) | Customer account master |

### VSAM Record Layout

| Field | Length | Description |
|-------|--------|-------------|
| `WS-CUST-ID` | 5 | Customer ID (key) |
| `WS-CUST-STATUS` | 1 | Account status: `A`=Active, `S`=Suspended |
| Other fields | var | Additional account data |

## Payment Input Record Layout

| Field | Description |
|-------|-------------|
| `PAYMENT-ID` | Payment identifier |
| `CUST-ID` | Customer ID to match VSAM |
| `PAYMENT-AMOUNT` | Payment amount (must be > 0) |
| `PAYMENT-TYPE` | Type code: `C`=Credit, `T`=Transfer, `A`=Adjustment |

## Business Logic

Processing is done in 4 phases per payment record:

1. **Validate Input** — Skip if `PAYMENT-ID` is spaces, amount <= 0, or type not in `(C, T, A)`. Log error and increment skip-count.
2. **VSAM Random Read** — Read customer record by `CUST-ID`. If status `23` (not found): log, skip. Any other non-zero status: log error, ROLLBACK, RC=12, stop loop.
3. **Check Account Status** — If `S` (Suspended): log rejected, skip. If active: proceed.
4. **DB2 UPDATE** — Update `CUST_BALANCE` in `TB.TB_CUSTOMER_BALANCE`.
   - SQLCODE `0`: log success, increment success-count.
   - SQLCODE `-911` (deadlock): ROLLBACK, RC=12, stop loop.
   - Any other negative SQLCODE: ROLLBACK, RC=8, stop loop.

**Post-loop Return Code logic:**
- ERROR-COUNT > 10 → RC=16
- ERROR-COUNT > 0 → RC=4
- ERROR-COUNT = 0 → RC=0

## How to Run

1. Create and seed DB2 table:
   ```sql
   -- Run SQL/INSERT.DATA.sql
   INSERT INTO TB.TB_CUSTOMER_BALANCE (CUST_ID, CUST_BALANCE) VALUES ('00100', 10000);
   ```
2. Load VSAM customer accounts file.
3. Prepare `PAYMENT.INPUT` sequential file.
4. Submit JCL: `JCL/COBDB2CP.jcl`
5. Check SYSOUT log and final return code.

## Key Concepts

- **Multi-phase validation** — Input, VSAM lookup, status check, DB2 update
- **VSAM random read** (`READ ... KEY IS`) with status code handling
- **Embedded DB2 SQL** with SQLCODE-based branching
- **ROLLBACK on deadlock** (`-911`) and hard DB2 errors
- **Tiered return code** based on error counts (RC=0/4/8/12/16)
- **Structured logging** — every record outcome written to SYSOUT

## Notes

- Author: STANISLAW
- Date: 2026/01/28
- Task theme: **Robust batch payment processing** combining VSAM account lookup with DB2 balance update and full error handling with appropriate return codes.
