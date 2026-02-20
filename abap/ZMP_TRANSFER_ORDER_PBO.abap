*&---------------------------------------------------------------------*
*& Include          ZMP_TRANSFER_ORDER_PBO
*&---------------------------------------------------------------------*

"----------------------------------------------------------------------
" PBO Module: status_0100
" Purpose   : Set PF-STATUS, title and UI context
"----------------------------------------------------------------------
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZMP_TO_STAT'.
  gv_title = |Transfer Order Management - Delivery { gv_delivery }|.
  SET TITLEBAR 'ZMP_TO_TTL' WITH gv_title.

  "Default tab and subscreen assignment
  IF gv_tab IS INITIAL.
    gv_tab = gc_tab_header.
  ENDIF.

  CASE gv_tab.
    WHEN gc_tab_header.
      gv_subscr = '0110'.
    WHEN gc_tab_item.
      gv_subscr = '0120'.
    WHEN OTHERS.
      gv_subscr = '0110'.
  ENDCASE.
ENDMODULE.

"----------------------------------------------------------------------
" PBO Module: prepare_table_control
" Purpose   : Keep table control line count synchronized
"----------------------------------------------------------------------
MODULE prepare_table_control OUTPUT.
  DESCRIBE TABLE gt_items LINES gv_lines.
  tc_items-lines = gv_lines.
ENDMODULE.

"----------------------------------------------------------------------
" PBO Module: move_item_to_screen
" Purpose   : Move current item to table control work area
"----------------------------------------------------------------------
MODULE move_item_to_screen OUTPUT.
  READ TABLE gt_items INDEX tc_items-current_line INTO gs_item.
ENDMODULE.
