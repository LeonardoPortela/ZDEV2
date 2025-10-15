FUNCTION znfw_busca_parametros_bapi.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_FUNCTION_NAME) TYPE  ZFIWRT2001-FUNCTION_NAME
*"     VALUE(I_ZFIWRT0008) TYPE  ZFIWRT0008
*"     REFERENCE(I_ZFIWRT0012) TYPE  ZFIWRT0012
*"  EXPORTING
*"     REFERENCE(MATERIALDOCUMENT) TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC
*"     REFERENCE(MATDOCUMENTYEAR) TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR
*"  TABLES
*"      IT_ZFIWRT0009 STRUCTURE  ZFIWRT0009
*"      RETURN STRUCTURE  BAPIRET2
*"      IT_ZFIWRT0010 STRUCTURE  ZFIWRT0010 OPTIONAL
*"----------------------------------------------------------------------

*** Stefanini - IR237366 - 29/05/2025 - LAZAROSR - Início de Alteração
*"   Adicionado parâmetro IT_ZFIWRT0010 na função
*** Stefanini - IR237366 - 29/05/2025 - LAZAROSR - Fim de Alteração

  TYPE-POOLS abap.

  DATA: tl_2001     TYPE TABLE OF zfiwrt2001 WITH HEADER LINE,
        tl_2002     TYPE TABLE OF zfiwrt2002 WITH HEADER LINE,
        tl_2002_tab TYPE TABLE OF zfiwrt2002 WITH HEADER LINE.

  DATA: ptab      TYPE abap_func_parmbind_tab,
        ptab_line TYPE abap_func_parmbind,
        etab      TYPE abap_func_excpbind_tab,
        etab_line TYPE abap_func_excpbind,
        func      TYPE string.

  DATA: wl_table(60).
  DATA: wl_work(60).
  DATA: l_cobl_i LIKE cobl,
        l_cobl_e LIKE cobl.

*** Stefanini - IR237366 - 03/07/2025 - FINC - Início de Alteração
  CONSTANTS: cc_bapi_migo_create TYPE rs38l_fnam VALUE 'BAPI_GOODSMVT_CREATE',
             cc_icm3             TYPE j_1btaxtyp VALUE 'ICM3'.
*** Stefanini - IR237366 - 03/07/2025 - FINC - Fim de Alteração

  REFRESH: ptab, etab,tl_2001,tl_2002,tl_2002_tab . "parametros estoque

  zfiwrt0009[] = it_zfiwrt0009[].
*** Stefanini - IR237366 - 29/05/2025 - LAZAROSR - Início de Alteração
  t_zfiwrt0010 = it_zfiwrt0010[].
  SORT t_zfiwrt0010 BY seq_lcto itmnum taxtyp.
*** Stefanini - IR237366 - 29/05/2025 - LAZAROSR - Fim de Alteração

  IF i_zfiwrt0012-bwart = 'Z01'.
    LOOP AT zfiwrt0009.
      SELECT SINGLE spart
        FROM mara
        INTO @DATA(vspart)
        WHERE matnr = @zfiwrt0009-matnr.
*    L_COBL_I-KUNNR = L_VBRK-KUNRG.
      l_cobl_i-matnr = zfiwrt0009-matnr.
*      L_COBL_I-FKART = L_VBRK-FKART.
*    L_COBL_I-KDAUF = W_GOODSMVT_ITEM-SALES_ORD.
*    L_COBL_I-KDPOS = W_GOODSMVT_ITEM-S_ORD_ITEM.
      l_cobl_i-bukrs = i_zfiwrt0008-bukrs.
      l_cobl_i-gjahr = i_zfiwrt0008-budat(4).
      l_cobl_i-werks = zfiwrt0009-bwkey.
      l_cobl_i-gsber = i_zfiwrt0008-branch.
      l_cobl_i-vkorg = i_zfiwrt0008-bukrs.
      l_cobl_i-vtweg = '10'.
      l_cobl_i-spart = vspart.

      CALL FUNCTION 'COPA_PROFITABILITY_SEGMENT'
        EXPORTING
          dialog              = ' '
          i_cobl              = l_cobl_i
        IMPORTING
          e_cobl              = l_cobl_e
        EXCEPTIONS
          abnormal_leave      = 1
          btrans_not_relevant = 2
          error_copa          = 3
          OTHERS              = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid
                TYPE sy-msgty
                NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      i_zfiwrt0008-paobjnr = l_cobl_e-paobjnr.
      UPDATE zfiwrt0008 SET paobjnr = i_zfiwrt0008-paobjnr
      WHERE seq_lcto = i_zfiwrt0008-seq_lcto.
      COMMIT WORK.
      EXIT. "uma vez
    ENDLOOP.
  ENDIF.
  zfiwrt0008   = i_zfiwrt0008.
  zfiwrt0012   = i_zfiwrt0012.


  SELECT *
    FROM zfiwrt2001
    INTO TABLE tl_2001
     WHERE bwart         EQ i_zfiwrt0012-bwart
       AND function_name EQ i_function_name.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM zfiwrt2002
      INTO TABLE tl_2002
       FOR ALL ENTRIES IN tl_2001
        WHERE bwart         EQ i_zfiwrt0012-bwart
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

  "Limpar workareas
  LOOP AT tl_2001.
    CASE tl_2001-par_type.
      WHEN 10 OR 30.
        wl_work = tl_2001-par_name.
        ASSIGN (wl_work) TO <fs_wa>.
        CLEAR  <fs_wa>.
        UNASSIGN <fs_wa>.
    ENDCASE.
  ENDLOOP.
  "Limpar workareas

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


  LOOP AT tl_2002_tab.
    CONCATENATE tl_2002_tab-par_name '[]' INTO wl_table.
    ASSIGN (wl_table) TO <fs_table>.
    REFRESH: <fs_table>.
    UNASSIGN <fs_table>.
  ENDLOOP.


  LOOP AT zfiwrt0009.
*---> 19.07.2023 12:23:14 - Migração S4 - DL
    SORT tl_2002_tab BY par_name.
*<--- 19.07.2023 12:23:14 - Migração S4 - DL

*** Stefanini - IR237366 - 29/05/2025 - LAZAROSR - Início de Alteração
    TRY.
        PERFORM atribuir_valor_zfiwrs0005 USING zfiwrt0009.
      CATCH cx_root.
        " Erro ao atribuir valor...
    ENDTRY.
*** Stefanini - IR237366 - 29/05/2025 - LAZAROSR - Fim de Alteração

    LOOP AT tl_2002_tab.
      PERFORM transfere_valor USING tl_2002_tab.

      AT END OF par_name.
        CONCATENATE tl_2002_tab-par_name '[]' INTO wl_table.
        ASSIGN (wl_table) TO <fs_table>.
        ASSIGN (tl_2002_tab-par_name) TO <fs_wa>.
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
