*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_EXP_SAAF_10F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_INSERIR_TODA_TABELA_10 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE SY-DATUM SY-UZEIT INTO LV_TIMES.
* Selecionar todos os registros para serem enviados
  CLEAR IT_ZVAL[].
  SELECT ZVAL CONST
    FROM ZTPARAM
    INTO CORRESPONDING FIELDS OF TABLE IT_PARAM
    WHERE PARAM EQ LC_TP_OBJ
    AND ABASTEC EQ LC_X.
  LOOP AT IT_PARAM.
    IT_ZVAL-ZVAL = IT_PARAM-ZVAL.
    APPEND IT_ZVAL.
  ENDLOOP.
*
  SELECT EQUNR EQART INTO TABLE IT_EQUI_M
    FROM EQUI FOR ALL ENTRIES IN IT_ZVAL
    WHERE EQTYP   = IT_ZVAL-ZVAL.
  SORT IT_EQUI_M ASCENDING BY EQUNR.
*
  SELECT * INTO TABLE IT_VEI_OP_SAAF
    FROM ZTPM_VEI_OP_SAAF FOR ALL ENTRIES IN IT_EQUI_M
    WHERE EQART = IT_EQUI_M-EQART.
  SORT IT_VEI_OP_SAAF ASCENDING BY EQART.
*
  REFRESH IT_R_VE_OP_SAAF.
*
  LOOP AT IT_EQUI_M.
    CLEAR IT_R_VE_OP_SAAF.
*
    READ TABLE IT_VEI_OP_SAAF WITH KEY EQART = IT_EQUI_M-EQART
      BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IT_R_VE_OP_SAAF-S_COD_OPERACAO = IT_VEI_OP_SAAF-CODE.
      IT_R_VE_OP_SAAF-S_COD_VEICULO = IT_EQUI_M-EQUNR.
      APPEND IT_R_VE_OP_SAAF.
    ELSE.
      CONTINUE.
    ENDIF.
  ENDLOOP.

* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
ENDFORM.
