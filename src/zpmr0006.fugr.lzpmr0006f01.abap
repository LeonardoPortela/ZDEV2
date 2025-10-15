*----------------------------------------------------------------------*
***INCLUDE LZPMR0006F01.
*----------------------------------------------------------------------*
FORM F_DELETE_EQUIPMENT.
  LOOP AT TOTAL[] INTO TOTAL.
    IF <MARK> IS NOT INITIAL.
      DATA(_CODIGO) = CONV CHAR3( TOTAL+3(3) ).
      DATA(_SEARCH) = |%{ _CODIGO }%|.

      SELECT *
        FROM IFLO
        INTO TABLE @DATA(_LOCATIONS)
       WHERE TPLNR LIKE @_SEARCH.

      LOOP AT _LOCATIONS INTO DATA(_LOCATION).
        SELECT SINGLE *
          FROM JEST
          INTO @DATA(_STATUS)
         WHERE OBJNR = @_LOCATION-OBJNR
           AND STAT  = 'I0076'
           AND INACT = @SPACE.

        IF SY-SUBRC IS NOT INITIAL.
          MESSAGE |O tipo { _CODIGO } está sendo usado no local de instalação { _LOCATION-TPLNR }| TYPE 'E'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.
