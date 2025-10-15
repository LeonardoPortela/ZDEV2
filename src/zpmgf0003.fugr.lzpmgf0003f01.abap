*----------------------------------------------------------------------*
***INCLUDE LZPMGF0003F01.
*----------------------------------------------------------------------*

FORM zf_gera_sequencial.

  TRANSLATE zpmt0070-typbz TO UPPER CASE.
  TRANSLATE zpmt0070-zdesc TO UPPER CASE.
  IF zpmt0070-zdesc IS INITIAL OR zpmt0070-typbz IS INITIAL .
    MESSAGE 'Campo Modelo e descrição obrigatórios' TYPE 'E' DISPLAY LIKE 'E'.
    vim_abort_saving = 'X'.
  ELSE.

    SELECT SINGLE *
      FROM zpmt0070
      INTO @DATA(ls_zpmt0070)
      WHERE zdesc = @zpmt0070-zdesc AND
            typbz = @zpmt0070-typbz.
    IF sy-subrc = 0.
      MESSAGE 'Já existe um registro com essa chave' TYPE 'E' DISPLAY LIKE 'E'.
      vim_abort_saving = 'X'.
    ELSE.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZIDMODELO'
        IMPORTING
          number                  = zpmt0070-zid
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
    ENDIF.
  ENDIF.


ENDFORM.

FORM f_verifica_eliminar.

  TYPES: BEGIN OF typ_extract,
           zview TYPE zpmt0070,   "or ztable, the one with the maintenance dialog!
           flags TYPE vimtbflags,
         END OF typ_extract.

  DATA: wa_zpmt0070 TYPE typ_extract.

  LOOP AT extract INTO wa_zpmt0070.
    CHECK wa_zpmt0070-flags-vim_mark EQ 'M'.
    SELECT SINGLE *
            FROM zpmr0001
            INTO @DATA(wa_zpmr0001)
            WHERE typbz = @wa_zpmt0070-zview-typbz.
    CHECK sy-subrc = 0.
    CONCATENATE 'Modelo:' wa_zpmt0070-zview-typbz 'utilizado na tabela ZPMR0001' INTO DATA(lv_erro) SEPARATED BY space.
    MESSAGE lv_erro  TYPE 'E' DISPLAY LIKE 'E'.
    vim_abort_saving = 'X'.
    EXIT.
  ENDLOOP.

ENDFORM.
