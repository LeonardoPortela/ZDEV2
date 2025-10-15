*&---------------------------------------------------------------------*
*&  Include           MZLES001P200
*&---------------------------------------------------------------------*
************************************************************************
*"                     GRUPO ANDRÉ MAGGI                              "*
*"--------------------------------------------------------------------"*
*" Autor        : Robson Motta             - BBKO                     "*
*" Data         : 21/07/2010                                          "*
*" Objetivo     : Definição de parametros de tela para uso em subtela "*
*" Versão       : V001  - Request: DEVK908588                         "*
*" Nota         : Inserir este include no fim definição Global        "*
*"--------------------------------------------------------------------"*
*" Histórico de modificações                                          "*
*" Data         :                                                     "*
*" Autor        :                                                     "*
*" Descrição    :                                                     "*
*" Versão       :                                                     "*
************************************************************************
TABLES: zlest0015,
        zlest0016,
        zlest0013,
        vttk.

SELECTION-SCREEN BEGIN OF SCREEN 1200 AS SUBSCREEN NESTING LEVEL 4.
SELECTION-SCREEN BEGIN OF BLOCK zb01.
SELECT-OPTIONS: s_trans  FOR  zlest0015-transportador MODIF ID io1,
                s_posto  FOR  zlest0015-posto         MODIF ID io1,
                s_lote   FOR  zlest0015-lote          MODIF ID io1,
                s_conhe  FOR  zlest0013-conhec        MODIF ID io1,
                s_perio  FOR  vttk-erdat              MODIF ID io1 OBLIGATORY,
                s_fecham FOR  vttk-erdat              MODIF ID io1,
                s_venci  FOR  vttk-erdat              MODIF ID io1.
PARAMETERS:     p_stats  TYPE ztatuslote              MODIF ID io1 OBLIGATORY.
SELECTION-SCREEN COMMENT 46(60) p_dscsta FOR FIELD p_stats.
SELECTION-SCREEN END OF BLOCK zb01.
SELECTION-SCREEN END OF SCREEN 1200.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'IO1'.
      IF vg_ctrl_inout_1200 IS INITIAL.
        screen-input  = '1'.
      ELSE.
        screen-input   = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
* Posiciona o cursor na tela
  IF vg_ctrl_inout_1200 IS INITIAL.
    SET CURSOR FIELD 'S_TRANS-LOW'.
  ENDIF.

* Valida status
AT SELECTION-SCREEN ON p_stats.
  IF vg_save_ok IS INITIAL AND
    ( NOT ok_code IS INITIAL AND ok_code(4) NE 'TAB_' ).
*   Qdo check de campos obrigatórios o vg_save_ok = initial
    vg_save_ok = ok_code.
  ENDIF.
  IF vg_save_ok = 'EXECOBJ'.
    CLEAR p_dscsta.
    IF NOT p_stats IS INITIAL.
      vg_dd_name  = 'ZSTATUSLOTE'.
      vg_dd_value = p_stats.
      CALL FUNCTION 'DDUT_DOMVALUE_TEXT_GET'
        EXPORTING
          name          = vg_dd_name
          value         = vg_dd_value
        IMPORTING
          dd07v_wa      = vg_wa_dd07v
        EXCEPTIONS
          not_found     = 1
          illegal_input = 2
          OTHERS        = 3.
      IF sy-subrc IS INITIAL.
        p_dscsta =  vg_wa_dd07v-ddtext.
      ELSE.
        CLEAR vg_save_ok.
        MESSAGE e000 WITH 'Status inválido'.
      ENDIF.
    ENDIF.
  ENDIF.

* Executa funcionalidade pertinentes a tela de seleção
AT SELECTION-SCREEN ON BLOCK zb01.
  IF vg_save_ok IS INITIAL AND
    ( NOT ok_code IS INITIAL AND ok_code(4) NE 'TAB_' ).
*   Qdo check de campos obrigatórios o vg_save_ok = initial
    vg_save_ok = ok_code.
  ENDIF.
  CASE vg_save_ok.
    WHEN 'EXECOBJ'.
      PERFORM novos_conhecimentos_200.
      PERFORM salva_valores_selecao_1200.
      CLEAR vg_save_ok.
    WHEN OTHERS.
  ENDCASE.
