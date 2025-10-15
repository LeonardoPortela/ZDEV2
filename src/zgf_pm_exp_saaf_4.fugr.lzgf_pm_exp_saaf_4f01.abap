*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_EXP_SAAF_4F01.
*----------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_INSERIR_TODA_TABELA_04 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE SY-DATUM SY-UZEIT INTO LV_TIMES.
* Selecionar todos os registros para serem enviados
  SELECT PERNR CNAME CPF_NR DEPARTAMENTO FUNCAO GBDAT
INTO TABLE IT_USER
    FROM ZHCMT0007
   WHERE WERKS = '1521'
     AND BUKRS = '0015'.
*
  REFRESH IT_M_USER_SAAF.
*
  LOOP AT IT_USER.
    CLEAR IT_M_USER_SAAF.

    CALL FUNCTION 'CONVERSION_EXIT_ZDATE_OUTPUT'
      EXPORTING
        INPUT  = IT_USER-DT_DATA_NASCIMENTO
      IMPORTING
        OUTPUT = IT_M_USER_SAAF-DT_DATA_NASCIMENTO.

    SELECT SINGLE GESCH
      FROM PA0002 INTO @DATA(_SEXO)
     WHERE PERNR = @IT_USER-S_ID_TECLADO.

    SELECT SINGLE IDENT_NR
      FROM PA0465
      INTO IT_M_USER_SAAF-S_RG
    WHERE PERNR = IT_USER-S_ID_TECLADO.

    CASE _SEXO.
      WHEN 1.
        IT_M_USER_SAAF-S_SEXO = 'M'.
      WHEN 2.
        IT_M_USER_SAAF-S_SEXO = 'F'.
      WHEN OTHERS.
    ENDCASE.

    IF IT_M_USER_SAAF-S_RG IS INITIAL.
      IT_M_USER_SAAF-S_RG = ' '.
    ENDIF.

    IT_M_USER_SAAF-S_CPF          = IT_USER-S_CPF.
    IT_M_USER_SAAF-I_USUARIO_TIPO = 0.
    IT_M_USER_SAAF-S_PERFIL       = IT_USER-S_PERFIL.
    IT_M_USER_SAAF-S_LOTACAO      = IT_USER-S_LOTACAO.
    IT_M_USER_SAAF-S_NOME         = IT_USER-S_NOME.
    IT_M_USER_SAAF-S_SENHA        = '1'.
    IT_M_USER_SAAF-S_ID_TECLADO   = IT_USER-S_ID_TECLADO.
    IT_M_USER_SAAF-S_BARCODE      = IT_USER-S_ID_TECLADO.
    APPEND IT_M_USER_SAAF.
  ENDLOOP.
* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_  _saaf.
*  ztpm_exp_p_saaf-codigo     = it_.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
