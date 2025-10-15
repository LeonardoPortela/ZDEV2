
CLEAR v_valut.

v_valut = st_informe-valut.

CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = v_valut
      IMPORTING
        output = v_valut.

*    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
*      EXPORTING
*        date_internal            = st_informe-valut
*      IMPORTING
*        date_external            = st_informe-valut
*      EXCEPTIONS
*        date_internal_is_invalid = 1
*        OTHERS                   = 2.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

    REPLACE '.' WITH '/' INTO v_valut.
    REPLACE '.' WITH '/' INTO v_valut.






















