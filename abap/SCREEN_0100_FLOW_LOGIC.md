# Screen 0100 Flow Logic (Module Pool `ZMP_TRANSFER_ORDER`)

## 1) Screen 0100 Design (Screen Painter)

- **Main Screen**: `0100`
- **Element 1**: Delivery input field bound to `GV_DELIVERY`
- **Element 2**: Tabstrip control `TS_MAIN` with two tabs:
  - Tab key `TAB_HDR` → Header area (subscreen `0110`)
  - Tab key `TAB_ITM` → Item area (subscreen `0120`)
- **Subscreen 0110** (Header details): fields bound to `GS_HEADER-*`
- **Subscreen 0120** (Items): table control `TC_ITEMS` bound to `GT_ITEMS` / `GS_ITEM`

## 2) Suggested GUI Status (`ZMP_TO_STAT`)

Function codes:
- `FETCH`    → Read delivery + transfer order data
- `CREATE`   → Append new TO item
- `DELETE`   → Mark selected rows for deletion
- `SAVE`     → Persist changes
- `TAB_HDR`  → Activate Header tab
- `TAB_ITM`  → Activate Items tab
- `BACK`, `EXIT`, `CANC`

## 3) Flow Logic for Screen 0100

```abap
PROCESS BEFORE OUTPUT.
  MODULE status_0100.
  MODULE prepare_table_control.

  LOOP AT gt_items WITH CONTROL tc_items CURSOR tc_items-current_line.
    MODULE move_item_to_screen.
  ENDLOOP.

PROCESS AFTER INPUT.
  LOOP AT gt_items.
    MODULE move_item_from_screen.
    MODULE mark_updated_items.
  ENDLOOP.

  MODULE user_command_0100.
```

## 4) Runtime Behavior

1. User enters `GV_DELIVERY` and presses **Fetch**.
2. `FETCH` triggers delivery validation and DB read from `LIKP`, `LTAK`, `LTAP`.
3. Header data appears in tab 1 (`GS_HEADER`).
4. Item rows appear in tab 2 (`GT_ITEMS`) via table control.
5. User can:
   - create a new row (`CREATE`, action = `C`)
   - edit quantity `ANFME` (auto-mark action = `U`)
   - select row checkbox and `DELETE` (action = `D`)
6. `SAVE` runs mock/real persistence block and refreshes snapshot table (`GT_ITEMS_DB`).

## 5) Notes for Enterprise Hardening

- Replace mock persistence with WM standard API / BAPI where available.
- Add authority-check logic for warehouse number (`LTAK-LGNUM`).
- Add enqueue/dequeue for transfer order before save.
- Add application log (BAL) for auditability.
- Integrate conversion exits for `VBELN`, `MATNR` in screen modules.
