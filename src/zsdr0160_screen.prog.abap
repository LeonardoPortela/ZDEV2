*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_SCREEN
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:   p_painel RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND us1.
  PARAMETERS:   p_status RADIOBUTTON GROUP g1.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 04.
    PARAMETERS:   p_formbl RADIOBUTTON GROUP g2 DEFAULT 'X' MODIF ID gr2 USER-COMMAND us2.
    SELECTION-SCREEN COMMENT (25) TEXT-300 FOR FIELD p_formbl.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 04.
    PARAMETERS:   p_geralo RADIOBUTTON GROUP g2             MODIF ID gr2.
    SELECTION-SCREEN COMMENT (25) TEXT-301 FOR FIELD p_geralo.
  SELECTION-SCREEN END   OF LINE.

*-----comentado #129705-20.12.2023-JT-inicio
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 04.
    PARAMETERS:   p_estorn RADIOBUTTON GROUP g2             MODIF ID gr2.
    SELECTION-SCREEN COMMENT (25) TEXT-303 FOR FIELD p_estorn.
  SELECTION-SCREEN END   OF LINE.
*-----comentado #129705-20.12.2023-JT-fim

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 04.
    PARAMETERS:   p_reenv  RADIOBUTTON GROUP g2             MODIF ID gr2.
    SELECTION-SCREEN COMMENT (40) TEXT-304 FOR FIELD p_reenv.
  SELECTION-SCREEN END   OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 04.
    PARAMETERS:   p_notafi RADIOBUTTON GROUP g2             MODIF ID gr2.
    SELECTION-SCREEN COMMENT (25) TEXT-302 FOR FIELD p_notafi.
  SELECTION-SCREEN END   OF LINE.

  "SD - Ganho Peso Automatico Algodao US #145369 - WPP
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 04.
    PARAMETERS:   p_ganper RADIOBUTTON GROUP g2             MODIF ID gr2.
    SELECTION-SCREEN COMMENT (25) TEXT-305 FOR FIELD p_ganper.
  SELECTION-SCREEN END   OF LINE.
  "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim

SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_werks   FOR zsdt0330-werks      MODIF ID gr5,
                  s_data    FOR zsdt0330-data_carga MODIF ID gr5 DEFAULT sy-datum,
                  s_nr_rom  FOR zsdt0330-nr_romaneio MODIF ID gr6, "SD - Ganho Peso Automatico Algodao US #145369 - WPP
                  s_id      FOR zsdt0330-id_carga   MODIF ID gr1,
                  s_safra   FOR zsdt0330-safra      MODIF ID gr1,
                  s_docnum  FOR zsdt0330-docnum     MODIF ID gr3,
                  s_vbeln   FOR zsdt0330-vbeln      MODIF ID gr4,
                  s_solic   FOR zsdt0063-nro_sol_ov MODIF ID gr4,
                  s_dataov  FOR zsdt0330-data_carga MODIF ID gr4 DEFAULT sy-datum.
  PARAMETERS    : p_agua AS CHECKBOX                MODIF ID gr1 DEFAULT 'X',
                  p_suce AS CHECKBOX                MODIF ID gr1 DEFAULT 'X',
                  p_erro AS CHECKBOX                MODIF ID gr1 DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'

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
    IF screen-group1 = 'GR1'.

      IF screen-name CS 'S_SAFRA' AND p_ganper EQ abap_true. "SD - Ganho Peso Automatico Algodao US #145369 - WPP
        screen-active = '1'.
      ELSE.

        IF p_painel = abap_true OR p_notafi = abap_true OR p_reenv = abap_true
           OR p_ganper EQ abap_true. "SD - Ganho Peso Automatico Algodao US #145369 - WPP
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.

      ENDIF.
    ENDIF.
    IF screen-group1 = 'GR2'.
      IF p_status = abap_true.
        screen-input = '1'.
      ELSE.
        screen-input = '0'.
      ENDIF.
    ENDIF.
    IF screen-group1 = 'GR3'.
      IF p_notafi = abap_true AND p_painel = abap_off.
        screen-input = '1'.
        screen-active = '1'.
      ELSE.
        screen-input = '0'.
        screen-active = '0'.
      ENDIF.
    ENDIF.
    IF screen-group1 = 'GR4'.
      IF p_reenv  = abap_true AND p_painel = abap_off.
        screen-input = '1'.
        screen-active = '1'.
      ELSE.
        screen-input = '0'.
        screen-active = '0'.
      ENDIF.
    ENDIF.
    IF screen-group1 = 'GR5'.
      IF p_reenv  = abap_true AND p_painel = abap_off.
        screen-input = '0'.
        screen-active = '0'.
      ELSE.
        screen-input = '1'.
        screen-active = '1'.
      ENDIF.
    ENDIF.

    "SD - Ganho Peso Automatico Algodao US #145369 - WPP
    IF screen-group1 = 'GR6'.
      IF p_ganper  = abap_true AND p_painel = abap_off.
        screen-input = '1'.
        screen-active = '1'.
      ELSE.
        screen-input = '0'.
        screen-active = '0'.
      ENDIF.
    ENDIF.
    "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim

    MODIFY SCREEN.
  ENDLOOP.

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZLESR0152'.
  SET TITLEBAR 'ZLESR0152'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0200 OUTPUT.

  DATA: wa_fcode TYPE sy-ucomm,
        it_fcode LIKE TABLE OF wa_fcode.

  CASE abap_true.
    WHEN p_formbl.
      APPEND '&GERALOTE'   TO it_fcode.
      APPEND '&EMAIL'      TO it_fcode.
      APPEND '&INTEGRAR'   TO it_fcode.
      APPEND '&REENV_NF'   TO it_fcode.
      APPEND '&REENVIO_OV' TO it_fcode.
      APPEND '&GANH_PERD'  TO it_fcode. "SD - Ganho Peso Automatico Algodao US #145369 - WPP

    WHEN p_geralo.
      APPEND '&PROCESSA'   TO it_fcode.
      APPEND '&REENV_NF'   TO it_fcode.
      APPEND '&REENVIO_OV' TO it_fcode.
      APPEND '&GANH_PERD'  TO it_fcode. "SD - Ganho Peso Automatico Algodao US #145369 - WPP

*-----comentado #129705-20.12.2023-JT-inicio
    WHEN p_estorn.
      APPEND '&GERALOTE'   TO it_fcode.
      APPEND '&EMAIL'      TO it_fcode.
      APPEND '&INTEGRAR'   TO it_fcode.
      APPEND '&REENV_NF'   TO it_fcode.
      APPEND '&PROCESSA'   TO it_fcode.
      APPEND '&REENV_NF'   TO it_fcode.
      APPEND '&REENVIO_OV' TO it_fcode.
      APPEND '&GANH_PERD'  TO it_fcode. "SD - Ganho Peso Automatico Algodao US #145369 - WPP
*-----comentado #129705-20.12.2023-JT-fim

    WHEN p_reenv.
      APPEND '&GERALOTE'  TO it_fcode.
      APPEND '&EMAIL'     TO it_fcode.
      APPEND '&INTEGRAR'  TO it_fcode.
      APPEND '&REENV_NF'  TO it_fcode.
      APPEND '&PROCESSA'  TO it_fcode.
      APPEND '&REENV_NF'  TO it_fcode.
      APPEND '&GANH_PERD'  TO it_fcode. "SD - Ganho Peso Automatico Algodao US #145369 - WPP

    WHEN p_notafi.
      APPEND '&GERALOTE'   TO it_fcode.
      APPEND '&EMAIL'      TO it_fcode.
      APPEND '&INTEGRAR'   TO it_fcode.
      APPEND '&PROCESSA'   TO it_fcode.
      APPEND '&REENVIO_OV' TO it_fcode.
      APPEND '&GANH_PERD'  TO it_fcode. "SD - Ganho Peso Automatico Algodao US #145369 - WPP

      ""SD - Ganho Peso Automatico Algodao US #145369 - WPP
    WHEN p_ganper.
      APPEND '&GERALOTE'  TO it_fcode.
      APPEND '&EMAIL'     TO it_fcode.
      APPEND '&INTEGRAR'  TO it_fcode.
      APPEND '&REENV_NF'  TO it_fcode.
      APPEND '&PROCESSA'  TO it_fcode.
      APPEND '&REENV_NF'  TO it_fcode.
      APPEND '&REENVIO_OV' TO it_fcode.
      ""SD - Ganho Peso Automatico Algodao US #145369 - WPP

  ENDCASE.

  SET PF-STATUS 'ZLESR0152B' EXCLUDING it_fcode.
  SET TITLEBAR 'ZLESR0152B'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  FREE: t_rows[].

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  CASE ok_code.
    WHEN '&REFRESH'.
      PERFORM f_selecao_dados.
      PERFORM f_processa_dados.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0200 INPUT.

  CASE ok_code.
    WHEN '&REFRESH'.
      CASE abap_true.
        WHEN p_formbl.
          PERFORM f_selecao_fardos.
          PERFORM f_processa_fardos.
        WHEN p_geralo.
          PERFORM f_selecao_lotes.
          PERFORM f_processa_lotes.
        WHEN p_notafi.
          PERFORM f_selecao_notas_fiscais.
          PERFORM f_processa_notas_fiscais.
        WHEN p_reenv.
          PERFORM f_selecao_reenvio_ov.
          PERFORM f_processa_reenvio_ov.
      ENDCASE.

    WHEN '&PROCESSA'.
      PERFORM f_reprocessa_fardos.
*     PERFORM f_selecao_fardos.
*     PERFORM f_processa_fardos.

    WHEN '&GERALOTE' OR '&EMAIL' OR '&INTEGRAR'.
      PERFORM f_reprocessa_lotes USING ok_code.
*     PERFORM f_selecao_fardos.
*     PERFORM f_processa_fardos.

    WHEN '&REENV_NF'.
      PERFORM f_reenvia_nota_fiscal.
*     PERFORM f_selecao_fardos.
*     PERFORM f_processa_fardos.

    WHEN '&REENVIO_OV'.
      PERFORM f_reenvia_ordem_venda.
      PERFORM f_selecao_reenvio_ov.
      PERFORM f_processa_reenvio_ov.

      "SD - Ganho Peso Automatico Algodao US #145369 - WPP
    WHEN '&GANH_PERD'.

      PERFORM f_processar_mov_ganho_perda.
      PERFORM f_selecao_ganho_perda_peso.
      PERFORM f_processa_ganho_perda.

      "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
