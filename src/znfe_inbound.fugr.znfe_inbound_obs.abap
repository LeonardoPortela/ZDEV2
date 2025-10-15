FUNCTION ZNFE_INBOUND_OBS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CHAVE) TYPE  ZDE_CHAVE_DOC_E OPTIONAL
*"     REFERENCE(I_NOTA) TYPE REF TO  ZCL_NFE_INBOUND OPTIONAL
*"     REFERENCE(I_TDID) TYPE  TDID DEFAULT 'ZCON'
*"     REFERENCE(I_RETORNAR) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(TEXT) TYPE  STRING
*"  RAISING
*"      ZCX_NFE_INBOUND_EXCEPTION
*"      ZCX_CADASTRO
*"----------------------------------------------------------------------

  DATA: LC_NAME   LIKE THEAD-TDNAME,
        LC_OBJECT LIKE THEAD-TDOBJECT,
        TL_TLINES LIKE TLINE OCCURS 0 WITH HEADER LINE,
        IT_TDLINE TYPE TABLE OF TDLINE WITH HEADER LINE,
        I_TEXTO   TYPE STRING.

  CASE I_TDID.
    WHEN 'ZCON'.
      GB_TEXTO_OBS = 'Informações Complementares de interesse do Contribuinte'.
    WHEN 'ZFIS'.
      GB_TEXTO_OBS = 'Informações Adicionais de Interesse do Fisco'.
  ENDCASE.

  IF I_NOTA IS NOT INITIAL.
    OBJ_NFE_INBOUND = I_NOTA.
  ELSE.
    CREATE OBJECT OBJ_NFE_INBOUND.
    OBJ_NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_CHAVE ).
  ENDIF.

  DATA(WA_INFO) = OBJ_NFE_INBOUND->GET_INFO_NOTA( ).

  LC_OBJECT = 'ZNFEIN'.
  CONCATENATE 'NFE_INBOUND_' WA_INFO-NFE_BASE-CHAVE_NFE INTO LC_NAME.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = I_TDID
      LANGUAGE                = SY-LANGU
      NAME                    = LC_NAME
      OBJECT                  = LC_OBJECT
    TABLES
      LINES                   = TL_TLINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  CASE SY-SUBRC.
    WHEN 0.
      IF I_RETORNAR IS INITIAL.
        PERFORM VISUALIZAR_TEXTO TABLES TL_TLINES.
      ELSE.

        LOOP AT TL_TLINES INTO DATA(WA_TLINES).
          IT_TDLINE = WA_TLINES-TDLINE.
          APPEND IT_TDLINE.
        ENDLOOP.

        CALL FUNCTION 'CONVERT_TABLE_TO_STRING'
          EXPORTING
            I_TABLINE_LENGTH = 132
          IMPORTING
            E_STRING         = TEXT
          TABLES
            IT_TABLE         = IT_TDLINE.
      ENDIF.
    WHEN OTHERS.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO I_TEXTO.
      ZCL_NFE_INBOUND=>GERA_ERRO_GERAL( I_TEXTO = I_TEXTO ).
  ENDCASE.

ENDFUNCTION.
