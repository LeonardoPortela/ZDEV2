FUNCTION zsdmf_consulta_cct.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------

*-------------------------------
* Set datas
*-------------------------------
  g_dt_lista_de  = sy-datum - 365.
  g_dt_lista_ate = sy-datum.
  g_nao_integra  = abap_true.

*-------------------------------
* Efetua pre-selecao
*-------------------------------
  PERFORM f_selecao.
  PERFORM f_monta_saida.

*-------------------------------
* Informacoes ALV
*-------------------------------
  CALL SCREEN 200 STARTING AT  32 01
                  ENDING   AT 160 20.

ENDFUNCTION.
