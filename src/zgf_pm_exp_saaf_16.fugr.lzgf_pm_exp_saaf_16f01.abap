*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_EXP_SAAF_16F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_16
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_INSERIR_TODA_TABELA_16 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE SY-DATUM SY-UZEIT INTO LV_TIMES.
* Selecionar todos os registros para serem enviados
  SELECT CODE KURZTEXT CODEGRUPPE INTO TABLE IT_QPCT
    FROM QPCT
    WHERE KATALOGART 	= LC_R
      AND CODEGRUPPE IN ('F-MOTIVO','H-MOTIVO')
      AND INAKTIV NE LC_X.

  SORT IT_QPCT ASCENDING BY S_COD_OPERACAO.
*
  REFRESH IT_B_CAUS_SAAF.
*
  LOOP AT IT_QPCT.
    CLEAR IT_B_CAUS_SAAF.
*
    IT_B_CAUS_SAAF-S_COD_CAUSA_MANUT = IT_QPCT-S_COD_OPERACAO.
    IT_B_CAUS_SAAF-S_DESC = IT_QPCT-S_DESC_OPERACAO.
    IT_B_CAUS_SAAF-IB_MANUT_FILTRO = SWITCH #( IT_QPCT-S_CODE(1) WHEN 'H' THEN '0' ELSE '1' ).
    IT_B_CAUS_SAAF-IB_VINCULAR_COMBOIO = LC_0.
    APPEND IT_B_CAUS_SAAF.
  ENDLOOP.

* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*
ENDFORM.
