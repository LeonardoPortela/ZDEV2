*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_EXP_SAAF_5F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_INSERIR_TODA_TABELA_05 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE SY-DATUM SY-UZEIT INTO LV_TIMES.
* Selecionar todos os registros para serem enviados
  SELECT * INTO TABLE IT_PROD
     FROM ZTFTPM_LUBRI.
  SORT IT_PROD ASCENDING BY CODE_MAT.
*
*  SELECT FLUID_TYPE TYPE_TEXT INTO TABLE IT_T370
*      FROM T370FLD_T FOR ALL ENTRIES IN IT_PROD
*     WHERE FLUID_TYPE = IT_PROD-CODE_MAT
*       AND LANG_KEY = LC_PT.
*  SORT IT_T370 ASCENDING BY FLUID_TYPE.
**
  REFRESH IT_M_PROD_SAAF.
*
  LOOP AT IT_PROD.
    CLEAR IT_M_PROD_SAAF.
*    READ TABLE IT_T370 WITH KEY FLUID_TYPE = IT_PROD-CODE_MAT
*       BINARY SEARCH.


    SELECT SINGLE *
   FROM MAKT
   INTO @DATA(_MATERIAL)
  WHERE MATNR = @IT_PROD-CONJUNTO.

    SELECT SINGLE A~WGBEZ
      FROM T023T AS A
      INNER JOIN MARA AS B ON B~MATKL = A~MATKL
     INTO IT_M_PROD_SAAF-S_GRUPO_PRODUTO
     WHERE B~MATNR = _MATERIAL-MATNR.

    IT_M_PROD_SAAF-I_TIPO        = 0.
    IT_M_PROD_SAAF-IB_FILTRO     = SWITCH #( IT_PROD-CATEGORIA WHEN 'F' THEN 1 WHEN 'O' THEN 2 WHEN 'C' THEN 3 ELSE 0 ).
    IT_M_PROD_SAAF-S_COD_PRODUTO = IT_PROD-CONJUNTO.
    IT_M_PROD_SAAF-S_DESCRICAO = _MATERIAL-MAKTX.
    IT_M_PROD_SAAF-S_DESCRICAO_REDUZIDA = _MATERIAL-MAKTX.
    APPEND IT_M_PROD_SAAF.
  ENDLOOP.
* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
