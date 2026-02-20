*&---------------------------------------------------------------------*
*& Include          ZMP_TRANSFER_ORDER_PAI
*&---------------------------------------------------------------------*

"----------------------------------------------------------------------
" PAI Module: user_command_0100
" Purpose   : Central OK_CODE handling for screen 0100
"----------------------------------------------------------------------
MODULE user_command_0100 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN gc_tab_header OR gc_tab_item.
      gv_tab = save_ok.

    WHEN 'FETCH'.
      PERFORM validate_delivery.
      IF gv_has_error IS INITIAL.
        PERFORM fetch_transfer_order_data.
      ENDIF.

    WHEN 'CREATE'.
      PERFORM create_item.

    WHEN 'DELETE'.
      PERFORM delete_selected_items.

    WHEN 'SAVE'.
      PERFORM save_changes.

    WHEN OTHERS.
      "No processing required
  ENDCASE.
ENDMODULE.

"----------------------------------------------------------------------
" PAI Module: move_item_from_screen
" Purpose   : Capture modified row from table control
"----------------------------------------------------------------------
MODULE move_item_from_screen INPUT.
  MODIFY gt_items FROM gs_item INDEX tc_items-current_line.
ENDMODULE.

"----------------------------------------------------------------------
" PAI Module: mark_updated_items
" Purpose   : Set update action if quantity changed
"----------------------------------------------------------------------
MODULE mark_updated_items INPUT.
  READ TABLE gt_items_db INDEX tc_items-current_line INTO gs_item_db.
  IF sy-subrc = 0.
    IF gs_item-anfme <> gs_item_db-anfme AND gs_item-action IS INITIAL.
      gs_item-action = 'U'.
      gs_item-changed = abap_true.
      MODIFY gt_items FROM gs_item INDEX tc_items-current_line.
    ENDIF.
  ENDIF.
ENDMODULE.
