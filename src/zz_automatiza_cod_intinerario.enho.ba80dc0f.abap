"Name: \PR:SAPL0VTR\FO:VIM_MODIFY_DETAIL_SCREEN\SE:END\EI
ENHANCEMENT 0 ZZ_AUTOMATIZA_COD_INTINERARIO.

IF sy-ucomm EQ 'NEWL' and V_TVRO_COM-ROUTE IS INITIAL.

DATA: ls_tvro TYPE tvro.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr                   = '01'
      object                        = 'ZZITINER'
   IMPORTING
     number                        = V_TVRO_COM-ROUTE
   EXCEPTIONS
     interval_not_found            = 1
     number_range_not_intern       = 2
     object_not_found              = 3
     quantity_is_0                 = 4
     quantity_is_not_1             = 5
     interval_overflow             = 6
     buffer_overflow               = 7
     OTHERS                        = 8.
IF sy-subrc is INITIAL.

  DO.

      SELECT SINGLE *
        FROM tvro
        INTO ls_tvro
        WHERE route = V_TVRO_COM-ROUTE.
      IF sy-subrc IS INITIAL .
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZZITINER'
          IMPORTING
            number                  = V_TVRO_COM-ROUTE
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
      ELSE.
        EXIT.
      ENDIF.

    ENDDO.

  EXPORT LV_ROUTE FROM V_TVRO_COM-ROUTE TO MEMORY ID 'ZLESR0162_ROUTE_NUMBER'.

  LOOP AT screen.
    IF screen-name EQ 'V_TVRO_COM-ROUTE'.
       screen-input = 0.
       MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  ENDIF.

ENDIF.
ENDENHANCEMENT.
