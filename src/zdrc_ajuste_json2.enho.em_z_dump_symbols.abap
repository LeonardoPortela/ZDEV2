METHOD z_dump_symbols .

  DATA: lv_properties TYPE STANDARD TABLE OF string,
        lv_itemval    TYPE string,
        w_zdrct0004   TYPE zdrct0004.

  FIELD-SYMBOLS: <value>  TYPE any,
                 <symbol> LIKE LINE OF it_symbols.

  LOOP AT it_symbols ASSIGNING <symbol>.
    ASSIGN <symbol>-value->* TO <value>.

    IF <value> IS INITIAL.
      SELECT SINGLE *
        INTO w_zdrct0004
        FROM zdrct0004
       WHERE campo = <symbol>-name.

      IF sy-subrc = 0 AND w_zdrct0004-gerar_quando_nulo = abap_true.
        lv_itemval = dump_int( data = w_zdrct0004-valor_quando_nulo type_descr = <symbol>-type convexit = <symbol>-convexit_out ).
        CONCATENATE <symbol>-header lv_itemval INTO lv_itemval.
        APPEND lv_itemval TO lv_properties.
      ENDIF.
    ELSEIF mv_compress IS INITIAL OR <value> IS NOT INITIAL OR <symbol>-compressable EQ abap_false.
      lv_itemval = dump_int( data = <value> type_descr = <symbol>-type convexit = <symbol>-convexit_out ).
      CONCATENATE <symbol>-header lv_itemval INTO lv_itemval.
      APPEND lv_itemval TO lv_properties.
    ENDIF.
  ENDLOOP.

  CONCATENATE LINES OF lv_properties INTO r_json SEPARATED BY `,`.
  CONCATENATE `{` r_json `}` INTO r_json.

ENDMETHOD.
