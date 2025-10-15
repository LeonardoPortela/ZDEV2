*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_SCREEN
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS    :  p_bukrs   TYPE t001-bukrs.
SELECT-OPTIONS:  s_branch   FOR t001w-j_1bbranch,
                 s_dt_sol   FOR sy-datum,
                 s_dt_rec   FOR sy-datum,
                 s_nr_cg    FOR zsdt0001-nro_cg      NO INTERVALS,
                 s_nr_rom   FOR zsdt0001-nr_romaneio NO INTERVALS,
                 s_safra    FOR zsdt0001-nr_safra    NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS : p_pend AS CHECKBOX DEFAULT 'X' USER-COMMAND us1,
             p_conc AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK b2.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF p_bukrs IS INITIAL.
    MESSAGE s024(sd) WITH 'Obrigatório informar Organização de Vendas.' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT SINGLE bukrs
    INTO @DATA(l_bukrs)
    FROM t001
   WHERE bukrs = @p_bukrs.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Empresa informada incorreta.' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZSD_BUKRS'
                      ID 'ACTVT' FIELD '03'  "exibir
                      ID 'BUKRS' FIELD p_bukrs.
  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Sem autorização para Pesquisar Empresa:' p_bukrs DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF s_branch[] IS INITIAL.
    IF s_nr_rom[] IS NOT INITIAL AND s_safra IS NOT INITIAL.
      MESSAGE s024(sd) WITH 'Obrigatório informar Filial Faturamento!'DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

  IF s_dt_sol[] IS INITIAL.
    IF s_dt_rec[] IS INITIAL.
      MESSAGE s024(sd) WITH 'Obrigatório informar Data Solicitação R.A.'DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

  IF s_nr_rom[] IS INITIAL AND s_nr_cg[] IS INITIAL.
    IF s_safra[] IS NOT INITIAL.
      MESSAGE s024(sd) WITH 'Obrigatório informar Romaneio ou Carga.'DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

  IF s_nr_rom[] IS NOT INITIAL.
    IF s_safra[] IS INITIAL.
      MESSAGE s024(sd) WITH 'Obrigatório informar a Safra!'DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

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
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN '&REFRESH'.
      PERFORM f_selecao_dados.
      PERFORM f_processa_dados.

    WHEN 'GERAR_SOL'.
      PERFORM f_gerar_solicitacao.
      PERFORM f_selecao_dados.
      PERFORM f_processa_dados.

    WHEN 'CANCEL_SOL'.
      PERFORM f_cancelar_solicitacao.
      PERFORM f_selecao_dados.
      PERFORM f_processa_dados.

*    WHEN 'ASSIN_ELET'.
*      PERFORM f_envia_assinatura_bry.
*      PERFORM f_selecao_dados.
*      PERFORM f_processa_dados.

    WHEN 'VER_DOCTOS'.
      PERFORM f_imprimir_ra_assinada.

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
