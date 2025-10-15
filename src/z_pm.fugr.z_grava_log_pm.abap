FUNCTION Z_GRAVA_LOG_PM.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TP_MSG) TYPE  BAPI_MTYPE OPTIONAL
*"     REFERENCE(I_MENSAGEM) TYPE  BAPI_MSG OPTIONAL
*"     REFERENCE(I_TCODE) TYPE  TCODE OPTIONAL
*"  TABLES
*"      T_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  DATA: GT_005 TYPE TABLE OF ZPMR0005,
        GW_005 TYPE ZPMR0005,

        GW_RETURN LIKE LINE OF T_RETURN,

        LV_NUMBER TYPE N LENGTH 10.

  IF  I_TP_MSG   IS NOT INITIAL
  AND I_MENSAGEM IS NOT INITIAL
  AND I_TCODE    IS NOT INITIAL.
    CLEAR GW_005.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZLOGPM'
      IMPORTING
        NUMBER                  = LV_NUMBER
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    GW_005-ID_LOG   = LV_NUMBER.
    GW_005-USUARIO  = SY-UNAME.
    IF I_TCODE IS INITIAL.
      GW_005-PROGRAMA = SY-TCODE.
    ELSE.
      GW_005-PROGRAMA = I_TCODE.
    ENDIF.
    GW_005-DATA     = SY-DATUM.
    GW_005-HORA     = SY-UZEIT.
    GW_005-TIPO_MSG = I_TP_MSG.
    GW_005-MENSAGEM = I_MENSAGEM.

    INSERT INTO ZPMR0005 VALUES GW_005.

  ELSEIF T_RETURN IS INITIAL.
    LOOP AT T_RETURN INTO GW_RETURN.
      CLEAR GW_005.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR             = '01'
          OBJECT                  = 'ZLOGPM'
        IMPORTING
          NUMBER                  = LV_NUMBER
        EXCEPTIONS
          INTERVAL_NOT_FOUND      = 1
          NUMBER_RANGE_NOT_INTERN = 2
          OBJECT_NOT_FOUND        = 3
          QUANTITY_IS_0           = 4
          QUANTITY_IS_NOT_1       = 5
          INTERVAL_OVERFLOW       = 6
          BUFFER_OVERFLOW         = 7
          OTHERS                  = 8.

      GW_005-ID_LOG   = LV_NUMBER.
      GW_005-MANDT    = SY-MANDT.
      GW_005-USUARIO  = SY-UNAME.
      GW_005-PROGRAMA = SY-TCODE.
      GW_005-DATA     = SY-DATUM.
      GW_005-HORA     = SY-UZEIT.
      GW_005-TIPO_MSG = GW_RETURN-TYPE.
      GW_005-MENSAGEM = GW_RETURN-MESSAGE.

      APPEND GW_005 TO GT_005.

    ENDLOOP.

    INSERT ZPMR0005 FROM TABLE GT_005.

  ENDIF.

ENDFUNCTION.
