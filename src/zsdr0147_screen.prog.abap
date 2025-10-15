*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_SCREEN
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF SCREEN 0101 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (25) text-200 FOR FIELD s_lote.
SELECT-OPTIONS:  s_lote     FOR zsdt0225-id_seq      MODIF ID g1 NO INTERVALS.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (25) text-201 FOR FIELD s_bukrs.
SELECT-OPTIONS:  s_bukrs    FOR ekpo-bukrs           MODIF ID g1 NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (25) text-204 FOR FIELD s_werks.
SELECT-OPTIONS:  s_werks    FOR ekpo-werks           MODIF ID g1 NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (25) text-202 FOR FIELD s_ebeln.
SELECT-OPTIONS:  s_ebeln    FOR ekko-ebeln           MODIF ID g1 NO INTERVALS.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (25) text-203 FOR FIELD s_data.
SELECT-OPTIONS:  s_data     FOR ekpo-aedat           MODIF ID g1.
SELECTION-SCREEN END   OF LINE.
*SELECTION-SCREEN POSITION 71.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS    :  p_nfatu    RADIOBUTTON GROUP gr1    MODIF ID g1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT (12) text-211 FOR FIELD p_nfatu.
PARAMETERS    :  p_sfatu    RADIOBUTTON GROUP gr1    MODIF ID g1.
SELECTION-SCREEN COMMENT (21) text-210 FOR FIELD p_sfatu.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN END   OF BLOCK b1.
SELECTION-SCREEN END   OF SCREEN 0101.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'S_BUKRS-LOW'.
      screen-required = '2'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.

  CLEAR ok_code.

  SET PF-STATUS 'ZLESR0152'.
  SET TITLEBAR 'ZLESR0152'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  l_editar = abap_false.

  CASE ok_code.
    WHEN 'VARIANTE'.
      PERFORM f_get_variant.

    WHEN 'SAVE'.
      PERFORM f_save_variant.

    WHEN 'SELECAO'.
      PERFORM f_selecao_dados    USING 0
                              CHANGING l_erro.
      IF l_erro = abap_false.
        PERFORM f_processa_dados USING abap_true.
      ENDIF.

    WHEN 'GERA_OV'.
      PERFORM f_gerar_ov.

    WHEN 'ESTORNAR'.
      PERFORM f_estornar_doctos.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  IF ok_code = 'SELECAO'.
    FREE: t_rows.
    CALL METHOD g_grid->set_selected_rows
      EXPORTING
        it_index_rows = t_rows.
  ENDIF.

  FREE ok_code.

ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  CLEAR ok_code2.
  SET PF-STATUS 'ZSDR0145_SCREEN'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  l_editar = abap_false.

  CASE ok_code2.
    WHEN 'SAVE'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CLEAR: rsvar-variant, rsvar-vtext.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
