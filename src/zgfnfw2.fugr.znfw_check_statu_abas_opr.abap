FUNCTION znfw_check_statu_abas_opr.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_OPERACAO) TYPE  ZFIWRT0017-OPERACAO
*"  EXPORTING
*"     REFERENCE(E_STATUS_OPR) TYPE  ZFIWRT0017-ABA_BLQ
*"  TABLES
*"      ET_STATUS_ABAS STRUCTURE  ZFIWRT0017 OPTIONAL
*"  EXCEPTIONS
*"      STATUS_NOT_FOUND
*"      OPERACAO_NOT_FOUND
*"----------------------------------------------------------------------
  DATA: tl_0017     TYPE TABLE OF zfiwrt0017 WITH HEADER LINE,
        wl_notfound.

  REFRESH: tl_0017, et_status_abas.

** LOG DE ACAO DE BLQ/LIBERAÇÃO
  SELECT *
    FROM zfiwrt0017
    INTO TABLE tl_0017
     WHERE operacao EQ i_operacao.

**> Deixa apenas a ultima ação do usuarios com relacao as abas
  SORT: tl_0017 BY operacao aba dzeile DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_0017 COMPARING operacao aba. "dzeile.
**<

  IF tl_0017[] IS INITIAL.
    e_status_opr = 'B'.
    tl_0017-aba    = 'TAB_1'.
    tl_0017-aba_blq = e_status_opr.
    APPEND tl_0017 TO et_status_abas.
    CLEAR: tl_0017.

    tl_0017-aba    = 'TAB_2'.
    tl_0017-aba_blq = e_status_opr.
    APPEND tl_0017 TO et_status_abas.
    CLEAR: tl_0017.

    tl_0017-aba    = 'TAB_3'.
    tl_0017-aba_blq = e_status_opr.
    APPEND tl_0017 TO et_status_abas.
    CLEAR: tl_0017.

    tl_0017-aba    = 'TAB_4'.
    tl_0017-aba_blq = e_status_opr.
    APPEND tl_0017 TO et_status_abas.
    CLEAR: tl_0017.

    tl_0017-aba    = 'TAB_4'.
    tl_0017-aba_blq = e_status_opr.
    APPEND tl_0017 TO et_status_abas.
    CLEAR: tl_0017.

    tl_0017-aba    = 'TAB_5'.
    tl_0017-aba_blq = e_status_opr.
    APPEND tl_0017 TO et_status_abas.
    CLEAR: tl_0017.

    tl_0017-aba    = 'TAB_6'.
    tl_0017-aba_blq = e_status_opr.
    APPEND tl_0017 TO et_status_abas.
    CLEAR: tl_0017.
    RAISE status_not_found.

    tl_0017-aba    = 'TAB_7'.
    tl_0017-aba_blq = e_status_opr.
    APPEND tl_0017 TO et_status_abas.
    CLEAR: tl_0017.
    RAISE status_not_found.


    tl_0017-aba    = 'TAB_8'.
    tl_0017-aba_blq = e_status_opr.
    APPEND tl_0017 TO et_status_abas.
    CLEAR: tl_0017.
    RAISE status_not_found.

  ENDIF.

  e_status_opr = 'L'.
*  LOOP AT tl_0017.
** Dados Gerais
  CLEAR: tl_0017.
  READ TABLE tl_0017
    WITH KEY aba =  'TAB_1'.

  IF tl_0017-aba_blq NE 'L'.
    e_status_opr = 'B'.
  ENDIF.
** Texto Complementar da Operação
  CLEAR: tl_0017.
  READ TABLE tl_0017
    WITH KEY aba =  'TAB_2'.

  IF tl_0017-aba_blq NE 'L'.
    e_status_opr = 'B'.
  ENDIF.
** Impostos
  CLEAR: tl_0017.
  READ TABLE tl_0017
    WITH KEY aba =  'TAB_3'.

  IF tl_0017-aba_blq NE 'L'.
    e_status_opr = 'B'.
  ENDIF.
** Msgs da Nota Fiscal
  CLEAR: tl_0017.
  READ TABLE tl_0017
    WITH KEY aba =  'TAB_4'.

  IF tl_0017-aba_blq NE 'L'.
    e_status_opr = 'B'.
  ENDIF.
** Contabilização
  CLEAR: tl_0017.
  READ TABLE tl_0017
    WITH KEY aba =  'TAB_5'.

  IF tl_0017-aba_blq NE 'L'.
    e_status_opr = 'B'.
  ENDIF.
** Movimentação do Estoque
  CLEAR: tl_0017.
  READ TABLE tl_0017
    WITH KEY aba =  'TAB_6'.

  IF tl_0017-aba_blq NE 'L'.
    e_status_opr = 'B'.
  ENDIF.
*  ENDLOOP.

** CFOP
  CLEAR: tl_0017.
  READ TABLE tl_0017
    WITH KEY aba =  'TAB_7'.

  IF tl_0017-aba_blq NE 'L'.
    e_status_opr = 'B'.
  ENDIF.

** NCM
  CLEAR: tl_0017.
  READ TABLE tl_0017
    WITH KEY aba =  'TAB_8'.

  IF tl_0017-aba_blq NE 'L'.
    e_status_opr = 'B'.
  ENDIF.

SORT tl_0017[] BY aba ASCENDING.

  et_status_abas[] = tl_0017[].

ENDFUNCTION.
