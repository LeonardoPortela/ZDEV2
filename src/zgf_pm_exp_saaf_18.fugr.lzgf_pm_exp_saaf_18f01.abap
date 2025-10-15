*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_18
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_INSERIR_TODA_TABELA_18.
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
  SELECT EQUNR HERST HERLD TYPBZ OBJNR INTO TABLE IT_EQUI
    FROM ITOB FOR ALL ENTRIES IN IT_ZVAL
    WHERE EQTYP   = IT_ZVAL-ZVAL
          AND IWERK = '1521'.

  SORT IT_EQUI ASCENDING BY S_COD_VEICULO.
*
  REFRESH IT_C_VEIC_SAAF.
*
  LOOP AT IT_EQUI.
    CLEAR IT_C_VEIC_SAAF.
*
    REFRESH IT_DIIMPT.
*
    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
      EXPORTING
        I_EQUNR   = IT_EQUI-S_COD_VEICULO
      TABLES
        ET_DIIMPT = IT_DIIMPT.
    IF NOT IT_DIIMPT[] IS INITIAL.
* se encontrou o Point,
      DELETE IT_DIIMPT WHERE LOCAS = ''.
      DELETE IT_DIIMPT WHERE INACT = LC_X.
      LOOP AT IT_DIIMPT INTO WA_DIIMPT.
        IF WA_DIIMPT-MPTYP = LC_F OR
           WA_DIIMPT-MPTYP = LC_H.
*
          IT_C_VEIC_SAAF-S_COD_VEICULO = IT_EQUI-S_COD_VEICULO.
          IT_C_VEIC_SAAF-S_COD_COMPARTIMENTO =  WA_DIIMPT-LOCAS.
          APPEND IT_C_VEIC_SAAF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
* Elimina duplocados
  SORT IT_C_VEIC_SAAF ASCENDING BY S_COD_VEICULO S_COD_COMPARTIMENTO.
  DELETE ADJACENT DUPLICATES FROM IT_C_VEIC_SAAF.
* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*
ENDFORM.
