FUNCTION ZNFW_BUSCA_PARAMETROS_BAPI_NEW.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_FUNCTION_NAME) TYPE  ZFIWRT2001-FUNCTION_NAME
*"     REFERENCE(I_ZFIWRT0008) TYPE  ZFIWRT0008
*"     REFERENCE(I_ZFIWRT0012) TYPE  ZFIWRT0012
*"  EXPORTING
*"     REFERENCE(MATERIALDOCUMENT) TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC
*"     REFERENCE(MATDOCUMENTYEAR) TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR
*"  TABLES
*"      IT_ZFIWRT0009
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
  TYPE-POOLS abap.

  DATA: tl_2001     TYPE TABLE OF zfiwrt2001 WITH HEADER LINE,
        tl_2002     TYPE TABLE OF zfiwrt2002 WITH HEADER LINE,
        tl_2002_tab TYPE TABLE OF zfiwrt2002 WITH HEADER LINE.
*      tl_2003 type table of zfiwrt2003 with header line.

  DATA: ptab TYPE abap_func_parmbind_tab,
        ptab_line TYPE abap_func_parmbind,
        etab TYPE abap_func_excpbind_tab,
        etab_line TYPE abap_func_excpbind,
        func TYPE string.



  DATA:   wl_table(60).

  zfiwrt0008   = i_zfiwrt0008.
  zfiwrt0012   = i_zfiwrt0012.
  zfiwrt0009[] = it_zfiwrt0009[].


  SELECT *
    FROM zfiwrt2001
    INTO TABLE tl_2001
     WHERE BWART         EQ i_zfiwrt0012-BWART
       AND function_name EQ i_function_name.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM zfiwrt2002
      INTO TABLE tl_2002
       FOR ALL ENTRIES IN tl_2001
        WHERE BWART         EQ i_zfiwrt0012-BWART
          AND function_name EQ tl_2001-function_name
          AND par_name  EQ tl_2001-par_name.
  ENDIF.
  LOOP AT tl_2002
    WHERE par_input EQ 'S'.
    READ TABLE tl_2001
      WITH KEY par_name  = tl_2002-par_name
               par_type  = 30.
    IF sy-subrc IS INITIAL.

      APPEND tl_2002 TO tl_2002_tab.
    ENDIF.
  ENDLOOP.


  SORT: tl_2002 BY par_name.
  LOOP AT tl_2002.
    READ TABLE tl_2001
      WITH KEY function_name = tl_2002-function_name
               par_name      = tl_2002-par_name.

    IF sy-subrc IS INITIAL.
      CASE tl_2001-par_type.
        WHEN 10  "Importing
*            or 20  "Exporting
          OR 40. "Changing
          PERFORM transfere_valor USING tl_2002.

        WHEN 30. "tables

      ENDCASE.
    ENDIF.
    UNASSIGN: <fs_field>, <fs_field2>.
  ENDLOOP.


  LOOP AT zfiwrt0009.
*---> 19.07.2023 12:24:53 - Migração S4 - DL
    SORT tl_2002_tab BY par_name.
*<--- 19.07.2023 12:24:53 - Migração S4 - DL
    LOOP AT tl_2002_tab.
      PERFORM transfere_valor USING tl_2002_tab.

      AT END OF par_name.
        CONCATENATE tl_2002-par_name '[]' INTO wl_table.
        ASSIGN (wl_table) TO <fs_table>.
        ASSIGN (tl_2002-par_name) TO <fs_wa>.
        APPEND <fs_wa> TO <fs_table>.
      ENDAT.
    ENDLOOP.
  ENDLOOP.

  LOOP AT tl_2001.
    ptab_line-name = tl_2001-par_name.
    ptab_line-kind = tl_2001-par_type.
    IF tl_2001-par_type EQ 30.
      CONCATENATE tl_2001-par_name '[]' INTO wl_table.
      ASSIGN (wl_table) TO <fs_table>.
      GET REFERENCE OF <fs_table> INTO ptab_line-value.
    ELSE.
      ASSIGN (tl_2001-par_name) TO <fs_field2>.
      GET REFERENCE OF <fs_field2> INTO ptab_line-value.
    ENDIF.
    INSERT ptab_line INTO TABLE ptab.

  ENDLOOP.

  etab_line-name = 'OTHERS'.
  etab_line-value = 10.
  INSERT etab_line INTO TABLE etab.

  func = i_function_name.
  CALL FUNCTION func
    PARAMETER-TABLE
      ptab
    EXCEPTION-TABLE
      etab.

ENDFUNCTION.
