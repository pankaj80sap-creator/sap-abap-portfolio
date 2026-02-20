*&---------------------------------------------------------------------*
*& Include          ZMP_TRANSFER_ORDER_TOP
*&---------------------------------------------------------------------*

"----------------------------------------------------------------------
" Type definitions for Header and Item processing
"----------------------------------------------------------------------
TYPES: BEGIN OF ty_header,
         vbeln_vl TYPE likp-vbeln,
         vbtyp    TYPE likp-vbtyp,
         kunnr    TYPE likp-kunnr,
         wadat    TYPE likp-wadat_ist,
         lgnum    TYPE ltak-lgnum,
         tanum    TYPE ltak-tanum,
         bname    TYPE ltak-bname,
         qname    TYPE ltak-qname,
         bdatu    TYPE ltak-bdatu,
       END OF ty_header.

TYPES: BEGIN OF ty_item,
         mark      TYPE abap_bool,      "Selection checkbox
         vbeln_vl  TYPE likp-vbeln,
         tanum     TYPE ltap-tanum,
         tapos     TYPE ltap-tapos,
         matnr     TYPE ltap-matnr,
         charg     TYPE ltap-charg,
         werks     TYPE ltap-werks,
         lgort     TYPE ltap-lgort,
         vltyp     TYPE ltap-vltyp,
         vlpla     TYPE ltap-vlpla,
         nltyp     TYPE ltap-nltyp,
         nlpla     TYPE ltap-nlpla,
         anfme     TYPE ltap-anfme,
         altme     TYPE ltap-altme,
         meins     TYPE ltap-meins,
         action    TYPE c LENGTH 1,     "C/U/D action indicator
         changed   TYPE abap_bool,
       END OF ty_item.

"----------------------------------------------------------------------
" Constants
"----------------------------------------------------------------------
CONSTANTS: gc_tab_header TYPE syucomm VALUE 'TAB_HDR',
           gc_tab_item   TYPE syucomm VALUE 'TAB_ITM',
           gc_mode_mock  TYPE abap_bool VALUE abap_true.

"----------------------------------------------------------------------
" Screen / UI handling
"----------------------------------------------------------------------
DATA: ok_code     TYPE sy-ucomm,
      save_ok     TYPE sy-ucomm,
      gv_tab      TYPE sy-ucomm VALUE gc_tab_header,
      gv_subscr   TYPE sy-dynnr,
      gv_lines    TYPE i,
      gv_title    TYPE sy-title,
      gv_edit     TYPE abap_bool VALUE abap_true.

CONTROLS: tc_items TYPE TABLEVIEW USING SCREEN 0100,
          ts_main  TYPE TABSTRIP.

"----------------------------------------------------------------------
" Data containers
"----------------------------------------------------------------------
DATA: gs_header     TYPE ty_header,
      gt_items      TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY,
      gt_items_db   TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY,
      gs_item       TYPE ty_item,
      gs_item_db    TYPE ty_item,
      gv_delivery   TYPE likp-vbeln,
      gv_msg_text   TYPE string.

"----------------------------------------------------------------------
" Selection support / helper flags
"----------------------------------------------------------------------
DATA: gv_data_loaded TYPE abap_bool,
      gv_has_error   TYPE abap_bool,
      gv_next_tapos  TYPE ltap-tapos.
