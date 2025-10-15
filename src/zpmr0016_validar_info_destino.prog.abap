*&---------------------------------------------------------------------*
*&  Include           ZPMR0016_VALIDAR_INFO_DESTINO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& FORM CHECAR_INFO_DESTINO                                            *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

FORM VALIDA_INFO_DESTINO.

  CLEAR: LOC_INSTALACAO, WA_SAIDA_EMPRESTIMO_EQUI.

  DATA: IT_VALUES TYPE TABLE OF RGSB4,
        WA_VALUES TYPE RGSB4.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      LEVEL           = 0
      SETNR           = 'MAGGI_CENTROS_MODULO_PM'
      TABLE           = 'T001W'
      CLASS           = '0000'
      NO_DESCRIPTIONS = 'X'
      NO_RW_INFO      = 'X'
      FIELDNAME       = 'WERKS'
    TABLES
      SET_VALUES      = IT_VALUES
    EXCEPTIONS
      SET_NOT_FOUND   = 1
      OTHERS          = 2.

  READ TABLE IT_SAIDA_EMPRESTIMO_EQUI INTO WA_SAIDA_EMPRESTIMO_EQUI WITH KEY
             IWERK = TBX_CENTRO_DESTINO.

  IF SY-SUBRC IS INITIAL.
    MESSAGE W836(SD) WITH TEXT-046 TEXT-047.

    RETURN_STATUS = 'X'.
    EXIT.
  ENDIF.

* Checa se o centro de planejamento informado é válido
  READ TABLE IT_VALUES INTO WA_VALUES WITH KEY
             FROM = TBX_CENTRO_DESTINO.

  IF SY-SUBRC IS INITIAL.

*  Concatena local de instalação do centro ( Ex.: "1801.FRO" )
    CONCATENATE TBX_CENTRO_DESTINO '.FRO' INTO LOC_INSTALACAO.

    SELECT SINGLE *
      FROM ILOA
      INTO WA_ILOA
     WHERE SWERK = TBX_CENTRO_DESTINO
       AND TPLNR = LOC_INSTALACAO.

*  Verifica se existe um local de instalação do centro destino
    CHECK WA_ILOA IS INITIAL.
    MESSAGE W836(SD) WITH TEXT-004 TEXT-005.
    RETURN_STATUS = 'X'.

  ELSE.
    MESSAGE W836(SD) WITH TEXT-006.
    RETURN_STATUS = 'X'.
  ENDIF.

ENDFORM.
                    "VALIDA_INFO_DESTINO
