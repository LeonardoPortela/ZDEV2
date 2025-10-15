*----------------------------------------------------------------------*
***INCLUDE LZPMGF0002F01.
*----------------------------------------------------------------------*
FORM zf_gera_sequencial.

  TRANSLATE zpmt0069-zdesc TO UPPER CASE.
  TRANSLATE zpmt0069-herst TO UPPER CASE.

  IF zpmt0069-zdesc IS INITIAL OR zpmt0069-herst IS INITIAL .
    MESSAGE 'Campo Fabricante e descrição obrigatórios' TYPE 'E' DISPLAY LIKE 'E'.
    vim_abort_saving = 'X'.
  ELSE.

    SELECT SINGLE *
      FROM zpmt0069
      INTO @DATA(ls_zpmt0069)
      WHERE zdesc = @zpmt0069-zdesc AND
            herst = @zpmt0069-herst.
    IF sy-subrc = 0.
      MESSAGE 'Já existe um registro com essa chave' TYPE 'E' DISPLAY LIKE 'E'.
      vim_abort_saving = 'X'.
    ELSE.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZIDFABRI'
        IMPORTING
          number                  = zpmt0069-zid
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
           zview TYPE zpmt0069,   "or ztable, the one with the maintenance dialog!
           flags TYPE vimtbflags,
         END OF typ_extract.

  DATA: wa_zpmt0069 TYPE typ_extract.

* loop at marked lines


  LOOP AT extract INTO wa_zpmt0069.
    CHECK wa_zpmt0069-flags-vim_mark EQ 'M'.
    SELECT SINGLE *
            FROM zpmr0001
            INTO @DATA(wa_zpmr0001)
            WHERE herst = @wa_zpmt0069-zview-herst.
    CHECK sy-subrc = 0.
    CONCATENATE 'Fabricante:' wa_zpmt0069-zview-herst 'utilizado na tabela ZPMR0001' INTO DATA(lv_erro) SEPARATED BY space.
    MESSAGE lv_erro  TYPE 'E' DISPLAY LIKE 'E'.
    vim_abort_saving = 'X'.
    EXIT.
  ENDLOOP.

ENDFORM.
