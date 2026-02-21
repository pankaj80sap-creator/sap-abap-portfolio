# CO15 BAdI Enhancement - Enterprise Validation Design

## 1) BAdI Definition
- **BAdI Name**: `WORKORDER_CONFIRM`
- **Interface**: `IF_EX_WORKORDER_CONFIRM`
- **Validation Method**: `AT_SAVE`
- **Implementation Class**: `ZCL_IM_CO15_VALIDATION`

`AT_SAVE` is triggered during CO15 save processing before the confirmation is committed to update tables. Any `MESSAGE ... TYPE 'E'` raised in this method stops the save.

## 2) Business Validation Scope
The implementation enforces these business rules:
1. Order must exist and not be in **TECO** / **CLSD**.
2. Mandatory confirmation fields are validated:
   - Final Confirmation indicator (`AFRU-AUERU`)
   - Yield Quantity (`AFRU-LMNGA > 0`)
   - Storage Location (`AFRU-LGORT`)
3. Duplicate confirmation prevention based on operational key fields.
4. Goods movement prerequisites validated using reservation data (`AFKO-RSNUM` / `RESB`).

## 3) DB Tables Used
- `AUFK`: Order header + object number (`OBJNR`) for status checks.
- `AFKO`: Production order header data including reservation number.
- `AFRU`: Current and historical confirmations for duplicate check.
- `RESB`: Goods movement reservation consistency.
- `JEST` (status table): Active system status validation for TECO/CLSD.

## 4) Message Class Proposal (`ZPP_CO15`)
Create message class `ZPP_CO15` in `SE91` with entries:

| Msg No | Type | Text |
|---|---|---|
| 001 | E | Order number missing in confirmation payload |
| 002 | E | AFKO record not found for order & |
| 003 | E | AUFK record not found for order & |
| 004 | E | Order & is technically completed or closed; confirmation not allowed |
| 005 | E | Final confirmation flag is mandatory for order & |
| 006 | E | Yield quantity must be greater than zero for order & |
| 007 | E | Storage location is mandatory for order & |
| 008 | E | Reservation number missing in order master data for order & |
| 009 | E | Duplicate confirmation detected for order & operation & |
| 010 | E | No active reservation item found for order & |
| 011 | E | Goods movement inconsistency for order & material & |
| 099 | E | Technical validation error: & |

## 5) Trigger Point in CO15 Runtime
1. User enters confirmation data in **CO15**.
2. On **Save**, SAP standard confirmation framework calls BAdI `WORKORDER_CONFIRM`.
3. `IF_EX_WORKORDER_CONFIRM~AT_SAVE` executes in custom class `ZCL_IM_CO15_VALIDATION`.
4. Validation reads `AUFK`, `AFKO`, `AFRU`, `RESB` (and status table `JEST`).
5. On violation, class raises `MESSAGE E...` from message class `ZPP_CO15`.
6. Save is blocked; user remains in CO15 for correction.

## 6) Notes for S/4HANA AMS Transport
- Create enhancement implementation in `SE19` and bind class `ZCL_IM_CO15_VALIDATION`.
- Activate class and implementation in same transport request.
- Unit-test with scenarios:
  - TECO order
  - missing final confirmation
  - yield = 0
  - missing storage location
  - duplicate confirmation attempt
  - missing/invalid reservation
