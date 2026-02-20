*&---------------------------------------------------------------------*
*& Include          ZMP_TRANSFER_ORDER_F01
*&---------------------------------------------------------------------*

"----------------------------------------------------------------------
" FORM validate_delivery
" Purpose : Validate mandatory Delivery Number input
"----------------------------------------------------------------------
FORM validate_delivery.
  CLEAR gv_has_error.

  IF gv_delivery IS INITIAL.
    gv_has_error = abap_true.
    MESSAGE e398(00) WITH 'Delivery Number is mandatory'.
  ENDIF.
ENDFORM.

"----------------------------------------------------------------------
" FORM fetch_transfer_order_data
" Purpose : Read Header and Item details linked to delivery number
"----------------------------------------------------------------------
FORM fetch_transfer_order_data.
  CLEAR: gs_header, gt_items, gt_items_db, gv_next_tapos.

  "Header fetch: Delivery + Transfer Order Header (example realistic join)
  SELECT SINGLE
         likp~vbeln,
         likp~vbtyp,
         likp~kunnr,
         likp~wadat_ist,
         ltak~lgnum,
         ltak~tanum,
         ltak~bname,
         ltak~qname,
         ltak~bdatu
    FROM likp
    LEFT OUTER JOIN ltak
      ON ltak~betyp = 'J'          "Reference category: outbound delivery
     AND ltak~benum = likp~vbeln
    WHERE likp~vbeln = @gv_delivery
    INTO CORRESPONDING FIELDS OF @gs_header.

  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH |No data found for Delivery { gv_delivery }|.
    RETURN.
  ENDIF.

  "Item fetch from LTAP using identified transfer order number
  IF gs_header-tanum IS NOT INITIAL.
    SELECT tanum,
           tapos,
           matnr,
           charg,
           werks,
           lgort,
           vltyp,
           vlpla,
           nltyp,
           nlpla,
           anfme,
           altme,
           meins
      FROM ltap
      WHERE tanum = @gs_header-tanum
      INTO TABLE @DATA(lt_ltap).

    LOOP AT lt_ltap ASSIGNING FIELD-SYMBOL(<ls_ltap>).
      APPEND VALUE ty_item(
               vbeln_vl = gv_delivery
               tanum    = <ls_ltap>-tanum
               tapos    = <ls_ltap>-tapos
               matnr    = <ls_ltap>-matnr
               charg    = <ls_ltap>-charg
               werks    = <ls_ltap>-werks
               lgort    = <ls_ltap>-lgort
               vltyp    = <ls_ltap>-vltyp
               vlpla    = <ls_ltap>-vlpla
               nltyp    = <ls_ltap>-nltyp
               nlpla    = <ls_ltap>-nlpla
               anfme    = <ls_ltap>-anfme
               altme    = <ls_ltap>-altme
               meins    = <ls_ltap>-meins ) TO gt_items.
    ENDLOOP.
  ENDIF.

  gt_items_db = gt_items.
  gv_data_loaded = abap_true.

  DESCRIBE TABLE gt_items LINES gv_lines.
  MESSAGE s398(00) WITH |{ gv_lines } item(s) loaded for Delivery { gv_delivery }|.
ENDFORM.

"----------------------------------------------------------------------
" FORM create_item
" Purpose : Add a new TO item in-memory with default proposal
"----------------------------------------------------------------------
FORM create_item.
  DATA(ls_new_item) = VALUE ty_item( ).

  IF gv_delivery IS INITIAL.
    MESSAGE e398(00) WITH 'Enter Delivery Number before creating item'.
    RETURN.
  ENDIF.

  "Derive next item number (+10 convention)
  gv_next_tapos = 10.
  LOOP AT gt_items INTO gs_item.
    IF gs_item-tapos >= gv_next_tapos.
      gv_next_tapos = gs_item-tapos + 10.
    ENDIF.
  ENDLOOP.

  ls_new_item-vbeln_vl = gv_delivery.
  ls_new_item-tanum    = gs_header-tanum.
  ls_new_item-tapos    = gv_next_tapos.
  ls_new_item-anfme    = '1'.
  ls_new_item-altme    = 'PC'.
  ls_new_item-meins    = 'PC'.
  ls_new_item-action   = 'C'.
  ls_new_item-changed  = abap_true.

  APPEND ls_new_item TO gt_items.
  MESSAGE s398(00) WITH |New item { ls_new_item-tapos } created|.
ENDFORM.

"----------------------------------------------------------------------
" FORM delete_selected_items
" Purpose : Mark checked rows for deletion
"----------------------------------------------------------------------
FORM delete_selected_items.
  DATA(lv_deleted) = 0.

  LOOP AT gt_items INTO gs_item.
    IF gs_item-mark = abap_true.
      gs_item-action  = 'D'.
      gs_item-changed = abap_true.
      MODIFY gt_items FROM gs_item INDEX sy-tabix.
      lv_deleted = lv_deleted + 1.
    ENDIF.
  ENDLOOP.

  IF lv_deleted = 0.
    MESSAGE s398(00) WITH 'No item selected for deletion'.
  ELSE.
    MESSAGE s398(00) WITH |{ lv_deleted } item(s) marked for deletion|.
  ENDIF.
ENDFORM.

"----------------------------------------------------------------------
" FORM save_changes
" Purpose : Persist C/U/D operations (mock-ready enterprise pattern)
"----------------------------------------------------------------------
FORM save_changes.
  DATA: lt_create TYPE STANDARD TABLE OF ty_item,
        lt_update TYPE STANDARD TABLE OF ty_item,
        lt_delete TYPE STANDARD TABLE OF ty_item.

  LOOP AT gt_items INTO gs_item WHERE changed = abap_true.
    CASE gs_item-action.
      WHEN 'C'. APPEND gs_item TO lt_create.
      WHEN 'U'. APPEND gs_item TO lt_update.
      WHEN 'D'. APPEND gs_item TO lt_delete.
      WHEN OTHERS.
        "ignore
    ENDCASE.
  ENDLOOP.

  IF lt_create IS INITIAL AND lt_update IS INITIAL AND lt_delete IS INITIAL.
    MESSAGE s398(00) WITH 'No changes to save'.
    RETURN.
  ENDIF.

  "In productive implementation, replace mock behavior with business API/FM
  IF gc_mode_mock = abap_true.
    DELETE gt_items WHERE action = 'D'.
    LOOP AT gt_items INTO gs_item.
      CLEAR: gs_item-action, gs_item-changed, gs_item-mark.
      MODIFY gt_items FROM gs_item INDEX sy-tabix.
    ENDLOOP.

    gt_items_db = gt_items.
    MESSAGE s398(00) WITH 'Changes saved successfully (mock mode)'.
    RETURN.
  ENDIF.

  "Example real DB handling (kept as template, can be replaced by WM BAPI)
  TRY.
      LOOP AT lt_create INTO gs_item.
        "INSERT ltap FROM CORRESPONDING FIELDS OF gs_item.
      ENDLOOP.

      LOOP AT lt_update INTO gs_item.
        "UPDATE ltap SET anfme = gs_item-anfme WHERE tanum = gs_item-tanum AND tapos = gs_item-tapos.
      ENDLOOP.

      LOOP AT lt_delete INTO gs_item.
        "DELETE FROM ltap WHERE tanum = gs_item-tanum AND tapos = gs_item-tapos.
      ENDLOOP.

      COMMIT WORK AND WAIT.
      MESSAGE s398(00) WITH 'Changes saved successfully'.
    CATCH cx_root INTO DATA(lx_root).
      ROLLBACK WORK.
      MESSAGE e398(00) WITH lx_root->get_text( ).
  ENDTRY.
ENDFORM.
