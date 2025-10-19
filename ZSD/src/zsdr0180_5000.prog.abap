*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5000
*&---------------------------------------------------------------------*

DATA: it_exclude_fcode TYPE ui_functions.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5000 OUTPUT.


  SET PF-STATUS 'PF5000' EXCLUDING it_exclude_fcode.

*-CS2019001891 - 28.06.2021 - JT - inicio
  CASE vg_subt_lote.
    WHEN '5420'.
      tabstrip_tab4 = icon_import_all_requests && g_nome_transp.
    WHEN '5520'.
      tabstrip_tab5 = icon_import_all_requests && g_nome_transp.
    WHEN '5620'.
      tabstrip_tab6 = icon_import_all_requests && g_nome_transp.
  ENDCASE.
*-CS2019001891 - 28.06.2021 - JT - fim

  "Abas visíveis para cada opção da seleção
  LOOP AT SCREEN.
    IF p_spart2 IS NOT INITIAL.

*-CS2019001891 - 28.06.2021 - JT - inicio
*     IF p_fcorp IS NOT INITIAL.
      IF g_tp_tela = '5420'.
        IF screen-name = 'TABSTRIP_TAB1' OR
          screen-name  = 'TABSTRIP_TAB2' OR
          screen-name  = 'TABSTRIP_TAB3' OR
          screen-name  = 'TABSTRIP_TAB5' OR
          screen-name  = 'TABSTRIP_TAB6' OR
          screen-name  = 'TABSTRIP_TAB7' OR
          screen-name  = 'TABSTRIP_TAB8'.
          screen-active    = 0.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
*     ELSEIF p_ftpng IS NOT INITIAL.
      ELSEIF g_tp_tela = '5520'.
        IF screen-name = 'TABSTRIP_TAB1' OR
          screen-name  = 'TABSTRIP_TAB2' OR
          screen-name  = 'TABSTRIP_TAB3' OR
          screen-name  = 'TABSTRIP_TAB4' OR
          screen-name  = 'TABSTRIP_TAB6' OR
          screen-name  = 'TABSTRIP_TAB7' OR
          screen-name  = 'TABSTRIP_TAB8'.
          screen-active    = 0.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
*     ELSEIF p_ftroo IS NOT INITIAL.
      ELSEIF g_tp_tela = '5620'.
        IF screen-name = 'TABSTRIP_TAB1' OR
          screen-name  = 'TABSTRIP_TAB2' OR
          screen-name  = 'TABSTRIP_TAB3' OR
          screen-name  = 'TABSTRIP_TAB4' OR
          screen-name  = 'TABSTRIP_TAB5' OR
          screen-name  = 'TABSTRIP_TAB7' OR
          screen-name  = 'TABSTRIP_TAB8'.
          screen-active    = 0.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*-CS2019001891 - 28.06.2021 - JT - fim
    ELSEIF p_spart3 IS NOT INITIAL.
      IF screen-name = 'TABSTRIP_TAB1' OR
         screen-name = 'TABSTRIP_TAB2' OR
         screen-name = 'TABSTRIP_TAB3' OR
         screen-name = 'TABSTRIP_TAB4' OR
         screen-name = 'TABSTRIP_TAB5' OR
         screen-name = 'TABSTRIP_TAB6'.
        screen-active    = 0.
        screen-invisible = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF p_spart4 IS NOT INITIAL.
*      IF p_filial IS NOT INITIAL.
*        IF screen-name = 'TABSTRIP_TAB2' OR
*           screen-name = 'TABSTRIP_TAB3' OR
*           screen-name = 'TABSTRIP_TAB4' OR
*          screen-name  = 'TABSTRIP_TAB5' OR
*          screen-name  = 'TABSTRIP_TAB6' OR
*          screen-name  = 'TABSTRIP_TAB7' OR
*          screen-name  = 'TABSTRIP_TAB8'.
*          screen-active    = 0.
*          screen-invisible = 0.
*          MODIFY SCREEN.
*        ENDIF.
      IF p_transp IS NOT INITIAL OR
             p_logcor IS NOT INITIAL.
        IF screen-name = 'TABSTRIP_TAB1' OR
           screen-name = 'TABSTRIP_TAB3' OR
           screen-name = 'TABSTRIP_TAB4' OR
          screen-name  = 'TABSTRIP_TAB5' OR
          screen-name  = 'TABSTRIP_TAB6' OR
          screen-name  = 'TABSTRIP_TAB7' OR
          screen-name  = 'TABSTRIP_TAB8'.
          screen-active    = 0.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
*      ELSEIF p_corplt IS NOT INITIAL.
*        IF screen-name = 'TABSTRIP_TAB4' OR
*          screen-name  = 'TABSTRIP_TAB5' OR
*          screen-name  = 'TABSTRIP_TAB6' OR
*          screen-name  = 'TABSTRIP_TAB7' OR
*          screen-name  = 'TABSTRIP_TAB8'.
*          screen-active    = 0.
*          screen-invisible = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ELSEIF p_corpcg IS NOT INITIAL.
*        IF screen-name = 'TABSTRIP_TAB1' OR
*           screen-name = 'TABSTRIP_TAB4' OR
*          screen-name  = 'TABSTRIP_TAB5' OR
*          screen-name  = 'TABSTRIP_TAB6' OR
*          screen-name  = 'TABSTRIP_TAB7' OR
*          screen-name  = 'TABSTRIP_TAB8'.
*          screen-active    = 0.
*          screen-invisible = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ELSEIF p_corppt IS NOT INITIAL.
*        IF screen-name = 'TABSTRIP_TAB1' OR
*           screen-name = 'TABSTRIP_TAB2' OR
*           screen-name = 'TABSTRIP_TAB4' OR
*          screen-name  = 'TABSTRIP_TAB5' OR
*          screen-name  = 'TABSTRIP_TAB6' OR
*          screen-name  = 'TABSTRIP_TAB7' OR
*          screen-name  = 'TABSTRIP_TAB8'.
*          screen-active    = 0.
*          screen-invisible = 0.
*          MODIFY SCREEN.
*        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "Títulos
  IF vg_subt_lote EQ '5120'.
    SET TITLEBAR 'T5120'.
  ELSEIF vg_subt_lote EQ '5130'.
    SET TITLEBAR 'T5130'.
  ELSEIF vg_subt_lote EQ '5140'.
    SET TITLEBAR 'T5140' WITH vg_lote_editar.
  ELSEIF vg_subt_lote EQ '5230'.
    SET TITLEBAR 'T5230'.
  ELSEIF vg_subt_lote EQ '5220'.
    SET TITLEBAR 'T5220'.
  ELSEIF vg_subt_lote EQ '5320'.
    SET TITLEBAR 'T5320'.
  ELSEIF vg_subt_lote EQ '5420'.
    SET TITLEBAR 'T5420'.
  ELSEIF vg_subt_lote EQ '5520'.
    SET TITLEBAR 'T5520'.
  ELSEIF vg_subt_lote EQ '5620'.
    SET TITLEBAR 'T5620'.
  ELSEIF vg_subt_lote EQ '5720'.
    SET TITLEBAR 'T5720'.
  ELSEIF vg_subt_lote EQ '5730'.
    SET TITLEBAR 'T5730'.
  ELSEIF vg_subt_lote EQ '5740'.
    SET TITLEBAR 'T5740' WITH vg_lote_editar.
  ELSEIF vg_subt_lote EQ '5820'.
    SET TITLEBAR 'T5820'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5000_exit INPUT.

  IF vg_subt_lote EQ '5130'.

    PERFORM desbloqueia_sol_5130.

    vg_subt_lote = '5120'.
    PERFORM clear_5120.
    LEAVE TO SCREEN 5000.
  ELSEIF vg_subt_lote EQ '5140'.

    PERFORM desbloqueia_sol_5140.

    CALL FUNCTION 'ZDENQUEUE_SD_LOTE_INSUMOS'
      EXPORTING
        chave = vg_lote_editar.

    vg_subt_lote = '5120'.
    PERFORM clear_5120.
    LEAVE TO SCREEN 5000.
  ELSEIF vg_subt_lote EQ '5120'.
    LEAVE TO SCREEN 0.
  ELSEIF vg_subt_lote EQ '5220'.
    LEAVE TO SCREEN 0.
  ELSEIF vg_subt_lote EQ '5230'.
    LEAVE TO SCREEN 0.
  ELSEIF vg_subt_lote EQ '5320'.
    LEAVE TO SCREEN 0.
  ELSEIF vg_subt_lote EQ '5420'.
    LEAVE TO SCREEN 0.
  ELSEIF vg_subt_lote EQ '5520'.
    LEAVE TO SCREEN 0.
  ELSEIF vg_subt_lote EQ '5620'.
    LEAVE TO SCREEN 0.
  ELSEIF vg_subt_lote EQ '5720'.
    LEAVE TO SCREEN 0.
  ELSEIF vg_subt_lote EQ '5730'.

    PERFORM desbloqueia_sol_5730.

    vg_subt_lote = '5720'.
    PERFORM clear_5720.
    LEAVE TO SCREEN 5000.
  ELSEIF vg_subt_lote EQ '5740'.

    PERFORM desbloqueia_sol_5740.

    CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
      EXPORTING
        chave = vg_lote_editar.

    vg_subt_lote = '5720'.
    PERFORM clear_5720.
    LEAVE TO SCREEN 5000.
  ELSEIF vg_subt_lote EQ '5820'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  EXCLUIR_BOTOES  OUTPUT
*&---------------------------------------------------------------------*
MODULE excluir_botoes OUTPUT.

  DATA: wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

  CLEAR: it_exclude_fcode.

  IF vg_subt_lote NE '5140'.
    wa_exclude_fcode = 'DELETELOTE'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
  ENDIF.

  IF vg_subt_lote NE '5740'.
    wa_exclude_fcode = 'DELETECG'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
  ENDIF.

  IF ( vg_subt_lote NE '5130' AND vg_subt_lote NE '5140' AND vg_subt_lote NE '5230' AND
       vg_subt_lote NE '5220' AND vg_subt_lote NE '5730' AND vg_subt_lote NE '5740' ).
    wa_exclude_fcode = 'SAVE'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
  ENDIF.

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TABSTRIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tabstrip_active_tab_set OUTPUT.

  tabstrip-activetab = g_tabstrip-pressed_tab.
  CASE g_tabstrip-pressed_tab.
    WHEN c_tabstrip-tab1.
*      g_tabstrip-subscreen = '5100'.
    WHEN c_tabstrip-tab2.
      g_tabstrip-subscreen = '5200'.
    WHEN c_tabstrip-tab3.
*      g_tabstrip-subscreen = '5300'.
    WHEN c_tabstrip-tab4.
*      g_tabstrip-subscreen = '5400'.
    WHEN c_tabstrip-tab5.
*      g_tabstrip-subscreen = '5500'.
    WHEN c_tabstrip-tab6.
*      g_tabstrip-subscreen = '5600'.
    WHEN c_tabstrip-tab7.
      g_tabstrip-subscreen = '5700'.
    WHEN c_tabstrip-tab8.
*      g_tabstrip-subscreen = '5800'.
    WHEN OTHERS.
      "DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TABSTRIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tabstrip_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tabstrip-tab1.
      vg_subt_lote = '5120'.
      PERFORM clear_5120.
      g_tabstrip-pressed_tab = c_tabstrip-tab1.
    WHEN c_tabstrip-tab2.
      IF vg_subt_lote NE '5130' AND vg_subt_lote NE '5140'.
        vg_subt_lote = '5230'.
        g_tabstrip-pressed_tab = c_tabstrip-tab2.
      ENDIF.
    WHEN c_tabstrip-tab3.
      IF vg_subt_lote NE '5130' AND vg_subt_lote NE '5140'.
        vg_subt_lote = '5320'.
        g_tabstrip-pressed_tab = c_tabstrip-tab3.
      ENDIF.
    WHEN c_tabstrip-tab4.
      g_tabstrip-pressed_tab = c_tabstrip-tab4.
    WHEN c_tabstrip-tab5.
      g_tabstrip-pressed_tab = c_tabstrip-tab5.
    WHEN c_tabstrip-tab6.
      g_tabstrip-pressed_tab = c_tabstrip-tab6.
    WHEN c_tabstrip-tab7.
      g_tabstrip-pressed_tab = c_tabstrip-tab7.
      PERFORM clear_5720.
      vg_subt_lote = '5720'.
    WHEN c_tabstrip-tab8.
      IF vg_subt_lote NE '5730' AND vg_subt_lote NE '5740'.
        g_tabstrip-pressed_tab = c_tabstrip-tab8.
        vg_subt_lote = '5820'.
      ENDIF.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
