*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_EXP_SAAF_19F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_19
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_INSERIR_TODA_TABELA_19 .
  DATA EQUIPAMENTOS_ABAST TYPE RANGE OF EQTYP.

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
    APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = IT_PARAM-ZVAL ) TO EQUIPAMENTOS_ABAST.
*    IT_ZVAL-ZVAL = IT_PARAM-ZVAL.
*    APPEND IT_ZVAL.
  ENDLOOP.
*
*  SELECT EQUNR HERST HERLD TYPBZ OBJNR INTO TABLE IT_EQUI
*    FROM ITOB FOR ALL ENTRIES IN IT_ZVAL
*    WHERE EQTYP   = IT_ZVAL-ZVAL.
*  SORT IT_EQUI ASCENDING BY S_COD_VEICULO.

  REFRESH IT_B_CENT_SAAF.

  SELECT C~KOSTL T~LTEXT T~KTEXT INTO TABLE IT_B_CENT_SAAF
    FROM CSKS AS C
INNER JOIN CSKT AS T ON C~KOKRS EQ T~KOKRS AND C~KOSTL EQ T~KOSTL AND C~DATBI EQ T~DATBI
*INNER JOIN ITOB AS I ON C~KOSTL EQ I~KOSTL
   WHERE C~KOKRS = LC_MAGI
     AND C~DATBI => SY-DATUM
     AND C~BUKRS EQ '0015'
     AND C~GSBER EQ '1521'
     AND T~SPRAS EQ LC_PT.
*     AND I~EQTYP IN EQUIPAMENTOS_ABAST.

*  LOOP AT IT_EQUI.
*    CLEAR IT_B_CENT_SAAF.
*    CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
*      EXPORTING
*        EQUIPMENT        = IT_EQUI-S_COD_VEICULO
*      IMPORTING
*        DATA_GENERAL_EXP = WA_DATA_GEN.
** Select dentro de Loop com chave completa
*    SELECT C~KOSTL T~LTEXT T~KTEXT INTO TABLE IT_CSKS
*      FROM CSKS AS C INNER JOIN CSKT AS T
*                   ON C~KOKRS EQ T~KOKRS AND
*                      C~KOSTL EQ T~KOSTL AND
*                      C~DATBI EQ T~DATBI
*      WHERE C~KOKRS = LC_MAGI
*       AND C~KOSTL = WA_DATA_GEN-COSTCENTER
*        AND C~DATBI => SY-DATUM
*        AND C~BUKRS = WA_DATA_GEN-COMP_CODE
*        AND T~SPRAS = LC_PT.
**
*    LOOP AT IT_CSKS.
*      IT_B_CENT_SAAF-S_COD_CENTRO_CUSTO = IT_CSKS-KOSTL.
*      IT_B_CENT_SAAF-S_DESC = IT_CSKS-LTEXT.
*      IT_B_CENT_SAAF-S_DESC_RED = IT_CSKS-KTEXT.
*      APPEND IT_B_CENT_SAAF.
*    ENDLOOP.
*  ENDLOOP.

* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*
ENDFORM.
