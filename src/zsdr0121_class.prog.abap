*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_CLASS
*&---------------------------------------------------------------------*



CLASS lcl_alv_toolbar_0100 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_event_handler_0100 IMPLEMENTATION.

  METHOD catch_hotspot.


    CASE e_column_id.
      WHEN 'DOCNUM'.
        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-docnum IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0100-docnum.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_NFE_RET'.
        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-docnum_nfe_ret IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0100-docnum_nfe_ret.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'MBLNR_MB1B'.
        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-mblnr_mb1b IS NOT INITIAL ).


*---> 19.07.2023 19:13:01 - Migração S4 - DL
*        SET PARAMETER ID 'MBN'  FIELD wa_saida_0100-mblnr_mb1b.
*        SET PARAMETER ID 'MJA'  FIELD wa_saida_0100-mjahr_mb1b.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

      CALL FUNCTION 'MIGO_DIALOG'
      EXPORTING
        i_action            = 'A04'
        i_refdoc            = 'R02'
        i_notree            = 'X'
        i_no_auth_check     = ' '
        i_deadend           = 'X'
        i_skip_first_screen = 'X'
        i_okcode            = 'OK_GO'
        i_mblnr             = wa_saida_0100-mblnr_mb1b
        i_mjahr             = wa_saida_0100-mjahr_mb1b.
*<--- 19.07.2023 19:13:01 - Migração S4 - DL

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
