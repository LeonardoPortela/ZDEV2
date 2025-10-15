FUNCTION Z_FI_GRAVA_RETURN .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      OUTRETURN STRUCTURE  ZFIE_RET_DOCUMENT
*"----------------------------------------------------------------------

  DATA: IT_TABELA TYPE TABLE OF ZOB_RET_FIDC_GEO WITH HEADER LINE,
        WA_RET_DOC  TYPE ZFIE_RET_DOCUMENT.

  CLEAR: IT_TABELA[].

  LOOP AT OUTRETURN INTO WA_RET_DOC.
    MOVE-CORRESPONDING WA_RET_DOC TO IT_TABELA.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZOBDCFIGEO'
      IMPORTING
        NUMBER                  = IT_TABELA-SEQ_REGISTRO
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    IF SY-SUBRC IS INITIAL.

      CALL FUNCTION 'OIL_DATE_TO_TIMESTAMP'
        EXPORTING
          I_DATE   = SY-DATUM
          I_TIME   = SY-UZEIT
        IMPORTING
          E_TSTAMP = IT_TABELA-TIMESTAMP.

      APPEND IT_TABELA.
    ENDIF.
  ENDLOOP.

  IF IT_TABELA[] IS NOT INITIAL.
    MODIFY ZOB_RET_FIDC_GEO FROM TABLE IT_TABELA.
    COMMIT WORK.
  ENDIF.

ENDFUNCTION.
