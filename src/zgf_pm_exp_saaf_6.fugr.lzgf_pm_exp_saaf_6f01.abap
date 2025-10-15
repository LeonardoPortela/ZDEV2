*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_EXP_SAAF_6F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_INSERIR_TODA_TABELA_06 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE SY-DATUM SY-UZEIT INTO LV_TIMES.
* Selecionar todos os registros para serem enviados
  SELECT CODE KURZTEXT INTO TABLE IT_QPCT
    FROM QPCT
    WHERE KATALOGART 	= LC_S
      AND CODEGRUPPE = LC_FPN
      AND INAKTIV NE LC_X.
*
  REFRESH IT_B_PLAN_SAAF.
*
  LOOP AT IT_QPCT.
    CLEAR IT_B_PLAN_SAAF.
    IT_B_PLAN_SAAF-S_COD_OPERACAO = IT_QPCT-S_COD_OPERACAO.
    IT_B_PLAN_SAAF-S_DESC_OPERACAO = IT_QPCT-S_DESC_OPERACAO.
    IT_B_PLAN_SAAF-S_DESC_REDUZIDA = IT_QPCT-S_DESC_OPERACAO.
    APPEND IT_B_PLAN_SAAF.
  ENDLOOP.
* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
