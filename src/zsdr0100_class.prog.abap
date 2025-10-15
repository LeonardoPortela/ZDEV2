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

    DATA: tl_parametros TYPE ustyp_t_parameters.

*      TY_TOOLBAR-ICON      = ICON_SYSTEM_UNDO.
*      TY_TOOLBAR-FUNCTION  = 'DISP_NFE_AJUSTE'.
*      TY_TOOLBAR-TEXT      = 'Lib. NF-e p/ Ajuste'.
*      TY_TOOLBAR-BUTN_TYPE = 0.
*      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*      CLEAR TY_TOOLBAR.


  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_event_handler_0100 IMPLEMENTATION.

  METHOD catch_hotspot.

    DATA: rt_bukrs  TYPE RANGE OF j_1bnfdoc-bukrs,
          rs_bukrs  LIKE LINE OF rt_bukrs,
          rt_branch TYPE RANGE OF j_1bnfdoc-branch,
          rs_branch LIKE LINE OF rt_branch,
          rt_docnum TYPE RANGE OF j_1bnfdoc-docnum,
          rs_docnum LIKE LINE OF rt_docnum.

    CASE e_column_id.
      WHEN 'DOCNUM'.

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id.
        CHECK ( sy-subrc EQ 0 ) AND ( wa_saida_0100-docnum IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0100-docnum.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_RETORNO'.

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id.
        CHECK ( sy-subrc EQ 0 ) AND ( wa_saida_0100-docnum_retorno IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0100-docnum_retorno.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_RFL'.

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id.
        CHECK ( sy-subrc EQ 0 ) AND ( wa_saida_0100-docnum_rfl IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0100-docnum_rfl.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'QTDE_BAIXADA'.

        FREE: rt_bukrs,
              rt_branch,
              rt_docnum.

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id.
        CHECK ( sy-subrc EQ 0 ).

        rs_bukrs-sign     = 'I'.
        rs_bukrs-option   = 'EQ'.
        rs_bukrs-low      = wa_saida_0100-bukrs.
        APPEND rs_bukrs  TO rt_bukrs.

        rs_branch-sign    = 'I'.
        rs_branch-option  = 'EQ'.
        rs_branch-low     = wa_saida_0100-branch.
        APPEND rs_branch TO rt_branch.

        rs_docnum-sign    = 'I'.
        rs_docnum-option  = 'EQ'.
        rs_docnum-low     = wa_saida_0100-docnum.
        APPEND rs_docnum TO rt_docnum.

        SUBMIT zsdr0132_listar
          WITH s_bukrs   IN rt_bukrs
          WITH s_branch  IN rt_branch
          WITH s_docnum  IN rt_docnum
           AND RETURN.

*        REFRESH: tl_bdc.
*        PERFORM f_preencher_dynpro USING:
*                 'X' 'ZSDR0132'  '0100',
*                 ' ' 'BDC_CURSOR'  'P_LISTAR',
*                 ' ' 'BDC_OKCODE'  '=ONLI',
*                 ' ' 'P_LISTAR'  'X',
*                 ' ' 'S_BUKRS-LOW'  wa_saida_0100-bukrs,
*                 ' ' 'S_BRANCH-LOW' wa_saida_0100-branch,
*                 ' ' 'S_DOCNUM-LOW' wa_saida_0100-docnum.
*
*        opt-dismode = 'E'.
*        opt-defsize = ' '.
*        opt-racommit = 'X'.
*
*        CALL TRANSACTION 'ZSDT0182'  USING tl_bdc OPTIONS FROM opt.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
