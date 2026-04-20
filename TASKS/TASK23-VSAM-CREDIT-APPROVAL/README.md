# Task 23: VSAM KSDS Credit Approval System

## Overview
This task implements a Credit Approval System that demonstrates interaction between sequential files, VSAM KSDS datasets, and inter-program communication via subprogram calls.

The system processes loan requests by looking up customer data in a VSAM master file and then evaluating creditworthiness using a specialized subprogram.

1. **Main Program (`JOBSUB23`)**: Coordinates the processing flow. It reads loan requests, performs random reads on the VSAM KSDS, calls the credit subprogram, and logs decisions.
2. **Credit Subprogram (`SUB1JB23`)**: Contains the business logic for credit eligibility based on score, late payments, and debt-to-loan ratio.

## Files
- `COBOL/JOBSUB23.cbl`: Main driver program (VSAM lookup & I/O).
- `COBOL/SUB1JB23.cbl`: Subprogram - Credit eligibility checker.
- `JCL/DEFKSDS.jcl`: IDCAMS job to define the VSAM KSDS cluster.
- `JCL/COMPRUN.jcl`: Job to create input data, compile components, and execute the system.
- `DATA/LOAN.REQUESTS`: Sequential input file with loan applications.
- `OUTPUT/APPROVAL.LOG`: Generated report with approval/rejection decisions.

## Record Layouts

### VSAM Customer Master (`CREDIT.MASTER`) — LRECL=80, Key=1-6
| Field | Position | Format | Description |
| :--- | :--- | :--- | :--- |
| `CUST-ID` | 1-6 | X(6) | Customer Identifier (KSDS Key) |
| `CREDIT-SCORE` | 7-9 | 9(3) | FICO-like credit score |
| `LATE-PAYMENTS`| 10-11 | 9(2) | Number of late payments |
| `CURRENT-DEBT` | 12-19 | 9(6)V99 | Total current debt |
| `FILLER` | 20-80 | X(61) | Reserved |

### Input Loan Request (`LOAN.REQUESTS`) — LRECL=80
| Field | Position | Format | Description |
| :--- | :--- | :--- | :--- |
| `CUST-ID` | 1-6 | X(6) | Customer Identifier |
| `LOAN-AMT` | 7-14 | 9(6)V99 | Requested Loan Amount |
| `FILLER` | 15-80 | X(66) | Reserved |

## Processing Logic

### 1. VSAM Lookup
The main program uses the `CUST-ID` from the loan request to perform a random read (`READ ... KEY IS ...`) on the VSAM KSDS.
- If `FILE STATUS` is `23` (Key not found), the request is automatically rejected.
- If other errors occur, the program terminates.

### 2. Credit Evaluation (`SUB1JB23`)
The subprogram receives customer data and loan details. It applies the following rules in order:
- **Rule 1**: Credit Score < 600 -> REJECTED (Reason: Poor Credit Score).
- **Rule 2**: Late Payments >= 3 -> REJECTED (Reason: Too Many Late Payments).
- **Rule 3**: (Current Debt + Loan Amount) > (Credit Score * 200) -> REJECTED (Reason: Debt Exceeds Limit).
- **Rule 4**: If all checks pass -> APPROVED.

## JCL Steps
- **`STEP005`**: Deletes previous output and temporary datasets.
- **`STEP010`**: Uses `IEBGENER` to populate the `LOAN.REQUESTS` file with sample data.
- **`STEP013`**: Compiles the credit subprogram (`SUB1JB23`).
- **`STEP015`**: Compiles the main program (`JOBSUB23`), links it with the subprogram, and executes the module.

## Key COBOL Concepts Used
- **VSAM KSDS Management**: Random access using `SELECT ... ASSIGN TO ... ORGANIZATION IS INDEXED`.
- **Subprogram Communication**: Using `CALL ... USING` to pass credit parameters and receive decisions.
- **Error Handling**: Checking `FILE STATUS` for VSAM operations.
- **Structured Reports**: Using `STRING` and `FUNCTION TRIM` for formatted output logs.

## How to Run
1. Run `JCL/DEFKSDS.jcl` to define the VSAM KSDS cluster.
2. Load the VSAM file with initial customer data (using REPRO or a separate load program).
3. Submit `JCL/COMPRUN.jcl` to process the loan requests.
4. Check `Z73460.TASK23.APPROVAL.LOG` for results.

## Notes
- Ensure the VSAM cluster exists before running the main job.
- The subprogram must be compiled before the main program for successful linking.
- Tested on IBM z/OS.
