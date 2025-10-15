*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_EXP_SAAF_15F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_15
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_INSERIR_TODA_TABELA_15 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE SY-DATUM SY-UZEIT INTO LV_TIMES.
* Selecionar todos os registros para serem enviados
  SELECT M~MATNR T~MAKTX T~MAKTG INTO TABLE IT_LOCAS
    FROM MARA AS M INNER JOIN MAKT AS T ON
               M~MATNR EQ T~MATNR
    WHERE M~MTART = LC_IBAU
      AND M~LVORM = ''.
  SORT IT_LOCAS ASCENDING BY MATNR.
*
  REFRESH IT_B_COMP_SAAF.

  DELETE IT_LOCAS[] WHERE MATNR(1) NE 'F'.

  LOOP AT IT_LOCAS.
    CLEAR IT_B_COMP_SAAF.
*
    IT_B_COMP_SAAF-S_COD_COMPARTIMENTO = IT_LOCAS-MATNR.
    IT_B_COMP_SAAF-S_DESC_RED = IT_LOCAS-MAKTX.
    IT_B_COMP_SAAF-S_DESC = IT_LOCAS-MAKTG.
    APPEND IT_B_COMP_SAAF.
  ENDLOOP.

* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
