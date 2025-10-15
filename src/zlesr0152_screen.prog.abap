*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_SCREEN
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS    : p_nfterc  RADIOBUTTON GROUP g0 USER-COMMAND f_modifica_tela  DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i00.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS    : p_nfprop  RADIOBUTTON GROUP g0.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i01.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS    : p_opcao1  RADIOBUTTON GROUP g1 DEFAULT 'X' MODIF ID sc1. "USER-COMMAND act DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i02 MODIF ID sc1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS    : p_opcao2  RADIOBUTTON GROUP g1 MODIF ID sc1.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i03 MODIF ID sc1.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_werks   FOR vbap-werks NO INTERVALS MODIF ID sc1,
                  s_vbeln   FOR vbak-vbeln NO INTERVALS MODIF ID sc1,
                  s_kunnr   FOR vbak-kunnr NO INTERVALS MODIF ID sc1,
                  s_dataov  FOR vbak-erdat MODIF ID sc1,
                  s_datanf  FOR vbak-erdat MODIF ID sc1, " OBLIGATORY.
                  s_docnum  FOR j_1bnfdoc-docnum MODIF ID sc1. " OBLIGATORY. "*-CS2024000522-10.09.2024-JT-#151259
  PARAMETERS: p_popup   TYPE char01        NO-DISPLAY.                       "*-CS2024000522-10.09.2024-JT-#151259

SELECTION-SCREEN END   OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS    : p_opcao3  RADIOBUTTON GROUP g2 MODIF ID sc2 DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i04 MODIF ID sc2.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS    : p_opcao4  RADIOBUTTON GROUP g2 MODIF ID sc2.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i05 MODIF ID sc2.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_ebeln  FOR ekko-ebeln NO INTERVALS MODIF ID sc2,
                  s_reswk  FOR ekko-reswk NO INTERVALS NO-EXTENSION MODIF ID sc2,
                  s_aedat  FOR ekko-aedat MODIF ID sc2 ,
                  s_erdat  FOR vbak-erdat MODIF ID sc2. " OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK b5.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF  p_nfterc = 'X'.
    IF s_datanf[] IS INITIAL.
      MESSAGE s024(sd) WITH TEXT-201 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.
*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_modifica_tela.
  PERFORM f_set_cockpit.

  LOOP AT SCREEN.
    IF screen-name = 'P_OPCAO2'.
      screen-input = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'P_OPCAO4'.
      screen-input = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'S_DATANF-LOW'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'S_RESWK-LOW'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'S_ERDAT-LOW'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

END-OF-SELECTION.
** US - 92467 - Inicio - CBRAND
  IF p_nfterc = 'X'.
    PERFORM f_selecao_dados.

    IF t_jlin[] IS INITIAL.
      MESSAGE s024(sd) WITH TEXT-100 TEXT-101 DISPLAY LIKE 'W'.
      STOP.
    ENDIF.

    PERFORM f_processa_dados.
    PERFORM f_alv_saida.
  ELSE.

    IF s_ebeln[] IS INITIAL.
      IF s_reswk[] IS INITIAL.
        MESSAGE s024(sd) WITH TEXT-201 DISPLAY LIKE 'E'.
        STOP.
      ELSE.
        IF s_erdat[] IS INITIAL.
          MESSAGE s024(sd) WITH TEXT-201 DISPLAY LIKE 'E'.
          STOP.
        ENDIF.
      ENDIF.
    ENDIF.

    PERFORM f_selecao_dados_nf_propria.
    PERFORM f_processa_dados.
    PERFORM f_alv_saida.

  ENDIF.
** US - 92467 - Fim - CBRAND
**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.

  DATA: tl_fcode TYPE TABLE OF sy-ucomm. "*-CS2024000522-18.07.2024-JT-#143588

*-CS2024000522-18.07.2024-JT-#143588-inicio
  IF l_cockpit <> '01'.
    APPEND '&EST_REM' TO tl_fcode.
  ENDIF.
*-CS2024000522-18.07.2024-JT-#143588-fim

*-CS2024000522-11.09.2024-JT-#151751-inicio
  IF p_popup = abap_false.
    APPEND '&VOLTAR'  TO tl_fcode.
  ENDIF.
*-CS2024000522-11.09.2024-JT-#151751-fim

  SET PF-STATUS 'ZLESR0152' EXCLUDING tl_fcode. "*-CS2024000522-18.07.2024-JT-#143588

  CASE l_cockpit.
    WHEN '01'.
      SET TITLEBAR 'ZLESR0152_01'.
    WHEN '03'.
      SET TITLEBAR 'ZLESR0152_02'.
  ENDCASE.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  DATA: l_estorno(1)  TYPE c.  "*-CS2024000522-18.07.2024-JT-#143588

  FREE: t_rows[], t_vbeln[].

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  CASE ok_code.
    WHEN '&REFRESH'.
      IF     l_cockpit = '03'.
* US - 92467 - Inicio - CBRABD
* NF de transferência

        LOOP AT s_ebeln.
          IF s_ebeln-low NE w_alv_transf-rem_vgbel.
            APPEND VALUE #( sign = 'I' option = 'EQ' low =  w_alv_transf-rem_vgbel )   TO s_ebeln.
          ENDIF.
        ENDLOOP.

        PERFORM f_selecao_dados_nf_propria.
        PERFORM f_processa_dados.
        " REFRESH s_ebeln.

      ELSE.
        PERFORM f_selecao_dados.  "*-cs2024000522-18.07.2024-jt-#143588-
        PERFORM f_processa_dados.
      ENDIF.
    WHEN '&LOGPROC'.
      IF lines( t_rows[] ) = 0.
        MESSAGE s024(sd) WITH TEXT-140 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF  l_cockpit = '03'.
* US - 92467 - Inicio - CBRABD
* NF de transferência
        CLEAR t_vbeln_transf[].
        LOOP AT t_rows     INTO w_rows.
          READ TABLE t_alv_transf INTO w_alv_transf INDEX w_rows-index.
          w_vbeln_transf-vbeln  = w_alv_transf-rem_vbeln.
          w_vbeln_transf-ebeln  = w_alv_transf-rem_vgbel.
          w_vbeln_transf-ebelp  = w_alv_transf-rem_vgpos.
          APPEND w_vbeln_transf     TO t_vbeln_transf.
        ENDLOOP.

*------------------------
*---- exibe log
*------------------------
        CALL FUNCTION 'ZSD_LOG_PROC_CONTA_ORDEM'
          TABLES
            t_vbeln_transf = t_vbeln_transf
          EXCEPTIONS
            no_log         = 1
            OTHERS         = 2.

        IF sy-subrc <> 0.
          MESSAGE s024(sd) WITH TEXT-141 DISPLAY LIKE 'E'.
        ENDIF.

* US - 92467 - Fim - CBRABD
      ELSE.
        LOOP AT t_rows     INTO w_rows.
          READ TABLE t_alv INTO w_alv INDEX w_rows-index.
          w_vbeln-vbeln       = w_alv-vbeln_venda.
          w_vbeln-nf_venda    = w_alv-nf_venda.
          APPEND w_vbeln     TO t_vbeln.
        ENDLOOP.

*------------------------
*---- exibe log
*------------------------
        CALL FUNCTION 'ZSD_LOG_PROC_CONTA_ORDEM'
          TABLES
            t_vbeln_venda = t_vbeln
          EXCEPTIONS
            no_log        = 1
            OTHERS        = 2.

        IF sy-subrc <> 0.
          MESSAGE s024(sd) WITH TEXT-141 DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.
    WHEN '&LIMPALOG'.
      IF  l_cockpit = '03'.
* US - 92467 - Inicio - CBRABD
* NF de transferência
        CLEAR t_vbeln_transf[].
        IF lines( t_rows[] ) = 0.
          MESSAGE s024(sd) WITH TEXT-140 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        LOOP AT t_rows     INTO w_rows.
          READ TABLE t_alv_transf INTO w_alv_transf INDEX w_rows-index.
          w_vbeln_transf-vbeln  = w_alv_transf-rem_vbeln.
          w_vbeln_transf-ebeln  = w_alv_transf-rem_vgbel.
          w_vbeln_transf-ebelp  = w_alv_transf-rem_vgpos.
          APPEND w_vbeln_transf     TO t_vbeln_transf.
        ENDLOOP.

*------------------------
*---- limpa erros do log
*------------------------
        zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_limpa_erros_log(
          EXPORTING
            t_vbeln_transf = t_vbeln_transf ).

        PERFORM f_processa_dados.

* US - 92467 - Fim - CBRABD
      ELSE.
        IF lines( t_rows[] ) = 0.
          MESSAGE s024(sd) WITH TEXT-140 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        LOOP AT t_rows     INTO w_rows.
          READ TABLE t_alv INTO w_alv INDEX w_rows-index.
          w_vbeln-vbeln       = w_alv-vbeln_venda.
          w_vbeln-nf_venda    = w_alv-nf_venda.
          APPEND w_vbeln     TO t_vbeln.
        ENDLOOP.

*------------------------
*---- limpa erros do log
*------------------------
        zcl_remessa_terceiro=>zif_remessa_terceiro~set_limpa_erros_log(
          EXPORTING
            t_vbeln_venda = t_vbeln ).

        PERFORM f_processa_dados.
      ENDIF.

*-CS2024000522-18.07.2024-JT-#143588-inicio
    WHEN '&EST_REM'.
      IF l_cockpit = '01'.
        IF lines( t_rows[] ) = 0.
          MESSAGE s024(sd) WITH TEXT-140 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF lines( t_rows[] ) > 1.
          MESSAGE s024(sd) WITH TEXT-160 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        READ TABLE t_rows INTO w_rows INDEX 1.
        READ TABLE t_alv INTO w_alv INDEX w_rows-index.

        CLEAR w_alv_transf.
        MOVE-CORRESPONDING w_alv     TO w_alv_transf.
        MOVE w_alv-remessa_dummy     TO w_alv_transf-rem_vbeln.

        PERFORM f_estorna_remessa USING w_alv_transf CHANGING l_estorno.

        IF l_estorno IS NOT INITIAL.
          MESSAGE 'Estorno realizado com sucesso!' TYPE 'I'.
          PERFORM f_selecao_dados.
          PERFORM f_processa_dados.
        ENDIF.
      ENDIF.
*-CS2024000522-18.07.2024-JT-#143588-fim

    WHEN '&ESTORNO'.
      IF lines( t_rows[] ) = 0.
        MESSAGE s024(sd) WITH TEXT-140 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      IF  l_cockpit = '03'.
* US - 92467 - Inicio - CBRABD
* NF de transferência
        PERFORM f_estorno_transf.
        PERFORM f_processa_dados.
* US - 92467 - Fim - CBRABD
      ELSE.
*------------------------
*---- estorno documentos
*------------------------
        PERFORM f_estorno_cte.
        PERFORM f_processa_dados.
      ENDIF.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR '&VOLTAR'.   "*-CS2024000522-11.09.2024-JT-#151751
      IF g_custom_container IS NOT INITIAL.
*        CALL METHOD g_custom_container->free.
      ENDIF.
      LEAVE TO SCREEN 0.
    WHEN 'GER_REM'.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
