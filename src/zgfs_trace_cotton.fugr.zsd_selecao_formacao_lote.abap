FUNCTION zsd_selecao_formacao_lote.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NRO_SOL_OV) TYPE  OBJNUM
*"     REFERENCE(I_POSNR) TYPE  POSNR_VA OPTIONAL
*"     REFERENCE(I_CONTRATO) TYPE  TEXT50 OPTIONAL
*"     REFERENCE(I_SOMENTE_EXIBE) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_EDITAR) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_REFERENCIA) TYPE  NUMC10 OPTIONAL
*"  EXPORTING
*"     VALUE(E_BACK) TYPE  CHAR1
*"  TABLES
*"      T_RETORNO STRUCTURE  ZSDT0328
*"----------------------------------------------------------------------

  t_entrada[]     = t_retorno[].
  g_somente_exibe = i_somente_exibe.
  g_editar        = i_editar.
  g_referencia    = i_referencia.

  FREE: t_saida, t_retorno, g_back.

*-----------------------
* seleao dos lotes
*-----------------------
  PERFORM f_selecao USING i_nro_sol_ov
                          i_posnr
                          i_contrato.

  IF t_lotes[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não foram encontradas Instruções!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-----------------------
* escolha dos lotes
*-----------------------
  CALL SCREEN 100 STARTING AT 20   3
                    ENDING AT 170 18.

  LOOP AT t_saida             INTO w_saida.
    MOVE-CORRESPONDING w_saida  TO w_retorno.
    APPEND w_retorno            TO t_retorno.
  ENDLOOP.

  e_back = g_back.

ENDFUNCTION.
