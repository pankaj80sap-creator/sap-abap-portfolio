"----------------------------------------------------------------------
" Class        : ZCL_IM_CO15_VALIDATION
" BAdI         : WORKORDER_CONFIRM
" Interface    : IF_EX_WORKORDER_CONFIRM
" Method       : AT_SAVE
" Purpose      : Validate CO15 order confirmation before update task
"----------------------------------------------------------------------
CLASS zcl_im_co15_validation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ex_workorder_confirm.

  PRIVATE SECTION.
    CONSTANTS:
      gc_msgid TYPE symsgid VALUE 'ZPP_CO15'.

    METHODS validate_order_status
      IMPORTING
        iv_aufnr TYPE aufnr.

    METHODS validate_mandatory_fields
      IMPORTING
        is_afru  TYPE afru
        is_afko  TYPE afko.

    METHODS validate_goods_movement
      IMPORTING
        iv_aufnr TYPE aufnr
        iv_rsnum TYPE rsnum.

    METHODS validate_duplicate_confirmation
      IMPORTING
        is_afru TYPE afru.
ENDCLASS.

CLASS zcl_im_co15_validation IMPLEMENTATION.

  METHOD if_ex_workorder_confirm~at_save.
    "------------------------------------------------------------------
    " This method is called by CO15 at SAVE, before confirmation update.
    " MESSAGE type 'E' stops the save and keeps user in CO15 screen.
    "------------------------------------------------------------------
    TRY.
        DATA(lv_aufnr) = is_afru-aufnr.

        IF lv_aufnr IS INITIAL.
          MESSAGE e001(gc_msgid) WITH 'Order number missing in confirmation payload'.
        ENDIF.

        SELECT SINGLE *
          FROM afko
          WHERE aufnr = @lv_aufnr
          INTO @DATA(ls_afko).
        IF sy-subrc <> 0.
          MESSAGE e002(gc_msgid) WITH lv_aufnr.
        ENDIF.

        " Rule-1: Status validation (order must not be TECO/CLSD)
        validate_order_status( iv_aufnr = lv_aufnr ).

        " Rule-2: Mandatory fields in CO15 payload
        validate_mandatory_fields(
          is_afru = is_afru
          is_afko = ls_afko ).

        " Rule-3: Duplicate confirmation must not be posted
        validate_duplicate_confirmation( is_afru = is_afru ).

        " Rule-4: Goods movement and reservation consistency
        validate_goods_movement(
          iv_aufnr = lv_aufnr
          iv_rsnum = ls_afko-rsnum ).

      CATCH cx_sy_open_sql_db INTO DATA(lx_sql).
        MESSAGE e099(gc_msgid) WITH lx_sql->get_text( ).
      CATCH cx_sy_conversion_error INTO DATA(lx_conv).
        MESSAGE e099(gc_msgid) WITH lx_conv->get_text( ).
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE e099(gc_msgid) WITH lx_root->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD validate_order_status.
    " AUFK read for authorization/object integrity validation
    SELECT SINGLE aufnr, objnr, auart, werks
      FROM aufk
      WHERE aufnr = @iv_aufnr
      INTO @DATA(ls_aufk).
    IF sy-subrc <> 0.
      MESSAGE e003(gc_msgid) WITH iv_aufnr.
    ENDIF.

    " Validate against active system status TECO / CLSD via OBJNR
    SELECT SINGLE jest~stat
      FROM jest
      WHERE jest~objnr = @ls_aufk-objnr
        AND jest~inact = @space
        AND jest~stat IN ( 'I0045', " TECO
                           'I0046' ) " CLSD
      INTO @DATA(lv_stat).

    IF sy-subrc = 0.
      MESSAGE e004(gc_msgid) WITH iv_aufnr.
    ENDIF.
  ENDMETHOD.

  METHOD validate_mandatory_fields.
    " Final confirmation indicator is mandatory for business scenario
    IF is_afru-aueru IS INITIAL.
      MESSAGE e005(gc_msgid) WITH is_afru-aufnr.
    ENDIF.

    " Yield quantity must be greater than zero
    IF is_afru-lmnga IS INITIAL OR is_afru-lmnga <= 0.
      MESSAGE e006(gc_msgid) WITH is_afru-aufnr.
    ENDIF.

    " Storage location mandatory for backflush / GI relevant confirmations
    IF is_afru-lgort IS INITIAL.
      MESSAGE e007(gc_msgid) WITH is_afru-aufnr.
    ENDIF.

    " If order has reservation profile, reservation number must exist
    IF is_afko-rsnum IS INITIAL.
      MESSAGE e008(gc_msgid) WITH is_afru-aufnr.
    ENDIF.
  ENDMETHOD.

  METHOD validate_duplicate_confirmation.
    " Duplicate check based on order + operation + posting date + yield
    SELECT SINGLE rueck
      FROM afru
      WHERE aufnr = @is_afru-aufnr
        AND vornr = @is_afru-vornr
        AND budat = @is_afru-budat
        AND lmnga = @is_afru-lmnga
        AND stokz = @space
      INTO @DATA(lv_rueck_existing).

    IF sy-subrc = 0 AND lv_rueck_existing <> is_afru-rueck.
      MESSAGE e009(gc_msgid)
        WITH is_afru-aufnr is_afru-vornr.
    ENDIF.
  ENDMETHOD.

  METHOD validate_goods_movement.
    " Validate that at least one active reservation item exists
    SELECT SINGLE rsnum, rspos, matnr, bdmng, enmng
      FROM resb
      WHERE rsnum = @iv_rsnum
        AND xloek = @space
      INTO @DATA(ls_resb).

    IF sy-subrc <> 0.
      MESSAGE e010(gc_msgid) WITH iv_aufnr.
    ENDIF.

    " Business protection: remaining qty should not be negative
    IF ls_resb-enmng > ls_resb-bdmng.
      MESSAGE e011(gc_msgid) WITH iv_aufnr ls_resb-matnr.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
