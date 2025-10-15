*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_EXP_SAAF_9F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_09
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_INSERIR_TODA_TABELA_09 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE SY-DATUM SY-UZEIT INTO LV_TIMES.
* Selecionar todos os registros para serem enviados
  SELECT TPLNR MSGRP PLTXT
    INTO TABLE IT_IFLO
    FROM IFLO
   WHERE SPRAS = LC_PT
     AND FLTYP = LC_C
     AND IWERK = '1521'.

  SORT IT_IFLO ASCENDING BY S_COD_LOCAL.
*
  REFRESH IT_LOC_AB_SAAF.
*
  LOOP AT IT_IFLO.
    CLEAR IT_LOC_AB_SAAF.
*
    LV_LANGU = SY-LANGU.
    LV_TPLNR = IT_IFLO-S_COD_LOCAL.

    CALL FUNCTION 'BAPI_FUNCLOC_GETSTATUS'
      EXPORTING
        FUNCTLOCATION = LV_TPLNR
        LANGUAGE      = LV_LANGU
      TABLES
        SYSTEM_STATUS = IT_STATUS
        USER_STATUS   = IT_STATUS.
*
    READ TABLE IT_STATUS WITH KEY STATUS = LC_I0076.  "Inativo
    IF SY-SUBRC IS INITIAL.
      CONTINUE.
    ENDIF.
*
    READ TABLE IT_STATUS WITH KEY STATUS = LC_I0320.  "Inativo
    IF SY-SUBRC IS INITIAL.
      CONTINUE.
    ENDIF.
*
    IT_LOC_AB_SAAF-S_COD_LOCAL         = IT_IFLO-S_SALA.
    IT_LOC_AB_SAAF-S_DESC_LOCAL        = IT_IFLO-S_DESC_LOCAL.
    IT_LOC_AB_SAAF-IB_VINCULAR_COMBOIO = LC_0.
    APPEND IT_LOC_AB_SAAF.
  ENDLOOP.

* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
