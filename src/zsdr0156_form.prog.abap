MODULE user_command_0100 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'T0100'.

  CREATE OBJECT lo_report_100.

ENDMODULE.


FORM action_process.

  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      lo_report_100->limpar_dados_100( ).
      FREE : lo_report_100.
      "LEAVE PROGRAM.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'BT_SALVAR_100'.
      lo_report_100->valida_header_100( ).
      IF zsde0407_header-bukrs IS NOT INITIAL AND zsde0407_header-id_seq IS NOT INITIAL
        AND zsde0407_header-matkl IS NOT INITIAL AND zsde0407_header-matnr IS NOT INITIAL
        AND zsde0407_header-safra IS NOT INITIAL AND zsde0407_header-werks IS NOT INITIAL
        AND zsde0407_header-zkunnr IS NOT INITIAL
        "AND ( zsde0407_header-zstcd1 IS NOT INITIAL OR zsde0407_header-zstcd2 OR zsde0407_header-zstcd3 )
        AND zsde0407_header-ztpcalc IS NOT INITIAL
        AND zsde0407_header-ztpcalcdesc IS NOT INITIAL
        AND zsde0407_header-ztpclass IS NOT INITIAL.
        lo_report_100->bt_salvar_100( ).
      ELSE.
        MESSAGE 'Necessário preencher todos os campos antes de salvar' TYPE 'I'.
      ENDIF.


    WHEN 'BT_EDITAR_100'.
      lo_report_100->bt_editar_100( ).

    WHEN 'BT_DELETAR_100'.
      lo_report_100->bt_deletar_100( ).

    WHEN 'ENTER'.
      lo_report_100->valida_header_100( ).
  ENDCASE.
ENDFORM.

FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module EXIBE_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE exibe_alv OUTPUT.

  DATA: lt_fieldcat TYPE lvc_t_fcat.

  FREE: gt_saida_0200.

  SELECT *
    FROM zsdt0225
    INTO TABLE @DATA(lt_0225)
    WHERE bukrs        IN @p_bukrs
      AND werks        IN @p_werks
      AND cl_codigo    IN @p_clien
      AND cod_material IN @p_matnr
      AND safra        IN @p_safra
      AND id_seq       IN @p_idlot
      AND dt_fatura    IN @p_perio.
  IF sy-subrc IS INITIAL.
    DATA(lt_0225_aux) = lt_0225.
    SORT lt_0225_aux BY cl_codigo.
    DELETE ADJACENT DUPLICATES FROM lt_0225_aux COMPARING cl_codigo.

    SELECT *
      FROM kna1
      INTO TABLE @DATA(lt_kna1)
      FOR ALL ENTRIES IN @lt_0225_aux
      WHERE kunnr = @lt_0225_aux-cl_codigo
        AND spras = @sy-langu.
    IF sy-subrc IS INITIAL.
      SORT lt_kna1 BY kunnr.
    ENDIF.

    lt_0225_aux = lt_0225.
    SORT lt_0225_aux BY cod_material.
    DELETE ADJACENT DUPLICATES FROM lt_0225_aux COMPARING cod_material.

    SELECT a~matnr,a~matkl,b~maktx
      FROM mara AS a
      INNER JOIN makt AS b
      ON b~matnr = a~matnr
     AND b~spras = @sy-langu
      INTO TABLE @DATA(lt_mara)
      FOR ALL ENTRIES IN @lt_0225_aux
      WHERE a~matnr = @lt_0225_aux-cod_material.
    IF sy-subrc IS INITIAL.
      SORT lt_mara BY matnr.

      SELECT *
        FROM t023t
        INTO TABLE @DATA(lt_023t)
        FOR ALL ENTRIES IN @lt_mara
        WHERE matkl = @lt_mara-matkl
          AND spras = @sy-langu.
      IF sy-subrc IS INITIAL.
        SORT lt_023t BY matkl.
      ENDIF.

    ENDIF.

    lt_0225_aux = lt_0225.
    SORT lt_0225_aux BY docnum.
    DELETE ADJACENT DUPLICATES FROM lt_0225_aux COMPARING docnum.

    SELECT *
      FROM j_1bnfdoc
      INTO TABLE @DATA(lt_j_1bnfdoc)
      FOR ALL ENTRIES IN @lt_0225_aux
      WHERE docnum = @lt_0225_aux-docnum.
    IF sy-subrc IS INITIAL.
      SORT lt_j_1bnfdoc BY docnum.
    ENDIF.

    LOOP AT lt_0225 ASSIGNING FIELD-SYMBOL(<fs_0225>).
      APPEND INITIAL LINE TO gt_saida_0200 ASSIGNING FIELD-SYMBOL(<fs_saida_0200>).

      READ TABLE lt_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_j_1bnfdoc>)
      WITH KEY docnum = <fs_0225>-docnum
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF <fs_j_1bnfdoc>-nfnum IS NOT INITIAL.
          <fs_saida_0200>-status          = '1'.
        ELSE.
          <fs_saida_0200>-status          = '2'.
        ENDIF.
      ENDIF.

      <fs_saida_0200>-emp_emissora    = <fs_0225>-bukrs_serv.
      <fs_saida_0200>-centro_emissor  = <fs_0225>-werks_serv.
      <fs_saida_0200>-id_seq          = <fs_0225>-id_seq.
      <fs_saida_0200>-cod_cliente     = <fs_0225>-cl_codigo.

      READ TABLE lt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>)
      WITH KEY kunnr = <fs_0225>-cl_codigo
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_saida_0200>-des_cliente     = <fs_kna1>-name1.
        <fs_saida_0200>-cnpj_cliente    = <fs_kna1>-stcd1.
      ENDIF.

      <fs_saida_0200>-tp_ov           = <fs_0225>-auart.
      <fs_saida_0200>-data_fatura     = <fs_0225>-dt_fatura.
      <fs_saida_0200>-qtd_base        = <fs_0225>-peso_vinculado.
      <fs_saida_0200>-vlr_brl         = <fs_0225>-vlr_brl.
      <fs_saida_0200>-vlr_usd         = <fs_0225>-vlr_usd.
      <fs_saida_0200>-tx_dolar        = <fs_0225>-tax_dolar.
      <fs_saida_0200>-cod_mat_base    = <fs_0225>-cod_material.

      READ TABLE lt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>)
      WITH KEY matnr = <fs_0225>-cod_material
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_saida_0200>-descr_mat_base  = <fs_mara>-maktx.
        <fs_saida_0200>-grp_material = <fs_mara>-matkl.

        READ TABLE lt_023t ASSIGNING FIELD-SYMBOL(<fs_023t>)
        WITH KEY matkl = <fs_mara>-matkl
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <fs_saida_0200>-desc_gp_material = <fs_023t>-wgbez60.
        ENDIF.
      ENDIF.

      <fs_saida_0200>-safra          = <fs_0225>-safra.
      <fs_saida_0200>-tp_classif     = <fs_0225>-tp_class.
      <fs_saida_0200>-navio          = <fs_0225>-navio.
      <fs_saida_0200>-local_operacao = <fs_0225>-local_operacao.
      <fs_saida_0200>-nr_ov          = <fs_0225>-nr_ov.
      <fs_saida_0200>-fatura         = <fs_0225>-fatura.
      <fs_saida_0200>-docnum         = <fs_0225>-docnum.

    ENDLOOP.

  ENDIF.

  IF go_container_0200 IS INITIAL.

    CREATE OBJECT:
    go_container_0200 EXPORTING container_name = 'CC_NFPS_FAT',
    go_alv_0200 EXPORTING i_parent = go_container_0200.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZSDE_ALV_NFPS_FAT'
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc = 0.

      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).

        CASE <fs_fieldcat>-fieldname.
          WHEN 'STATUS'.
            <fs_fieldcat>-coltext = 'Status'.
          WHEN 'EMP_EMISSORA'.
            <fs_fieldcat>-coltext = 'Emp. Emissora'.
          WHEN 'CENTRO_EMISSOR'.
            <fs_fieldcat>-coltext = 'Centro Emissor'.
          WHEN 'ID_SEQ'.
            <fs_fieldcat>-coltext = 'Id. Seq'.
          WHEN 'COD_CLIENTE'.
            <fs_fieldcat>-coltext = 'Cod. Cliente'.
          WHEN 'DES_CLIENTE'.
            <fs_fieldcat>-coltext = 'Des. Cliente'.
          WHEN 'CNPJ_CLIENTE'.
            <fs_fieldcat>-coltext = 'CNPJ Cliente'.
          WHEN 'TP_OV'.
            <fs_fieldcat>-coltext = 'TP. Ov'.
          WHEN 'DATA_FATURA'.
            <fs_fieldcat>-coltext = 'Dt. Movimento'.
          WHEN 'QTD_BASE'.
            <fs_fieldcat>-coltext = 'Qtd Base'.
          WHEN 'VLR_BRL'.
            <fs_fieldcat>-coltext = 'Vlr BRL'.
          WHEN 'VLR_USD'.
            <fs_fieldcat>-coltext = 'Vlr USD'.
          WHEN 'TX_DOLAR'.
            <fs_fieldcat>-coltext = 'TX Dólar'.
          WHEN 'COD_MAT_BASE'.
            <fs_fieldcat>-coltext = 'Cod. Material Base'.
          WHEN 'DESCR_MAT_BASE'.
            <fs_fieldcat>-coltext = 'Desc. Material Base'.
          WHEN 'GRP_MATERIAL'.
            <fs_fieldcat>-coltext = 'GRP Material'.
          WHEN 'DESC_GP_MATERIAL'.
            <fs_fieldcat>-coltext = 'Desc. Grp. Material'.
          WHEN 'SAFRA'.
            <fs_fieldcat>-coltext = 'Safra'.
          WHEN 'TP_CLASSIF'.
            <fs_fieldcat>-coltext = 'TP. Classif'.
          WHEN 'NAVIO'.
            <fs_fieldcat>-coltext = 'Navio'.
          WHEN 'LOCAL_OPERACAO'.
            <fs_fieldcat>-coltext = 'Local Operação'.
          WHEN 'NR_OV'.
            <fs_fieldcat>-coltext = 'Nr. OV'.
          WHEN 'FATURA'.
            <fs_fieldcat>-coltext = 'Fatura'.
          WHEN 'DOCNUM'.
            <fs_fieldcat>-coltext = 'Nr. Documento'.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.

    ENDIF.

    DATA: ls_layout  TYPE lvc_s_layo. "lvc_s_layo,

    ls_layout-stylefname = 'CELLTAB'.
    ls_layout-cwidth_opt = abap_true.
    ls_layout-grid_title = 'NFPS Faturadas'.

    CALL METHOD go_alv_0200->set_table_for_first_display
      EXPORTING
        is_layout       = ls_layout
        is_variant      = ls_variant
        i_save          = 'A'
      CHANGING
        it_outtab       = gt_saida_0200
        it_fieldcatalog = lt_fieldcat.

  ELSE.

    CALL METHOD go_alv_0200->refresh_table_display( ).

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'STATUS_0300'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ALV_CONSULTA_LOTE OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE alv_consulta_lote OUTPUT.

  DATA: lt_fieldcat_0300 TYPE lvc_t_fcat,
        ls_layout_0300   TYPE lvc_s_layo. "lvc_s_layo,

  FREE: gt_saida_0300.

  SELECT *
    FROM zsdt0407
    INTO TABLE @DATA(lt_0407)
    WHERE bukrs  IN @p_bukrs
      AND safra  IN @p_safra
      AND id_seq IN @p_idlot.
  IF sy-subrc IS INITIAL.
    DATA(lt_0407_aux) = lt_0407.

    SORT lt_0407_aux BY zkunnr.
    DELETE ADJACENT DUPLICATES FROM lt_0407_aux COMPARING zkunnr.

    SELECT *
      FROM kna1
      INTO TABLE @DATA(lt_kna1_0300)
      FOR ALL ENTRIES IN @lt_0407_aux
      WHERE kunnr = @lt_0407_aux-zkunnr
        AND spras = @sy-langu.
    IF sy-subrc IS INITIAL.
      SORT lt_kna1_0300 BY kunnr.
    ENDIF.

    lt_0407_aux = lt_0407.
    SORT lt_0407_aux BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_0407_aux COMPARING matnr.

    SELECT a~matnr,a~matkl,b~maktx
      FROM mara AS a
      INNER JOIN makt AS b
      ON b~matnr = a~matnr
     AND b~spras = @sy-langu
      INTO TABLE @DATA(lt_mara_0300)
      FOR ALL ENTRIES IN @lt_0407_aux
      WHERE a~matnr = @lt_0407_aux-matnr.
    IF sy-subrc IS INITIAL.
      SORT lt_mara_0300 BY matnr.
    ENDIF.

    LOOP AT lt_0407 ASSIGNING FIELD-SYMBOL(<fs_0407>).

      APPEND INITIAL LINE TO gt_saida_0300 ASSIGNING FIELD-SYMBOL(<fs_saida_0300>).

      <fs_saida_0300>-bukrs        = <fs_0407>-bukrs.
      <fs_saida_0300>-werks        = <fs_0407>-werks.
      <fs_saida_0300>-id_seq       = <fs_0407>-id_seq.
      <fs_saida_0300>-cl_codigo    = <fs_0407>-zkunnr.

      READ TABLE lt_kna1_0300 ASSIGNING FIELD-SYMBOL(<fs_kna1_0300>)
      WITH KEY kunnr = <fs_0407>-zkunnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_saida_0300>-name1        = <fs_kna1_0300>-name1.
        <fs_saida_0300>-stcd1        = <fs_kna1_0300>-stcd1.
        <fs_saida_0300>-stcd2        = <fs_kna1_0300>-stcd2.
        <fs_saida_0300>-stcd3        = <fs_kna1_0300>-stcd3.
      ENDIF.

      READ TABLE lt_mara_0300 ASSIGNING FIELD-SYMBOL(<fs_mara_0300>)
      WITH KEY matnr = <fs_0407>-matnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_saida_0300>-cod_material = <fs_mara_0300>-matnr.
        <fs_saida_0300>-maktx        = <fs_mara_0300>-maktx.
      ENDIF.

      <fs_saida_0300>-tp_class     = <fs_0407>-ztpclass.
      <fs_saida_0300>-safra        = <fs_0407>-safra.
      <fs_saida_0300>-tp_calc      = <fs_0407>-ztpcalc.
      <fs_saida_0300>-matkl        = <fs_0407>-matkl.
      <fs_saida_0300>-usuario      = <fs_0407>-usuario.

    ENDLOOP.

    SORT gt_saida_0300 BY id_seq DESCENDING.
  ENDIF.

  IF go_container_0300 IS INITIAL.

    CREATE OBJECT:
    go_container_0300 EXPORTING container_name = 'CC_CONSULTA_LOTE',
    go_alv_0300 EXPORTING i_parent = go_container_0300.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZSDE_ALV_CONSULTA_LOTE'
      CHANGING
        ct_fieldcat            = lt_fieldcat_0300
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc = 0.

      LOOP AT lt_fieldcat_0300 ASSIGNING FIELD-SYMBOL(<fs_fieldcat_0300>).

        CASE <fs_fieldcat_0300>-fieldname.
          WHEN 'BUKRS'.
            <fs_fieldcat_0300>-coltext = 'Emp. Emissora'.
          WHEN 'WERKS'.
            <fs_fieldcat_0300>-coltext = 'Centro Emissor'.
          WHEN 'ID_SEQ'.
            <fs_fieldcat_0300>-coltext = 'Id. Seq'.
            <fs_fieldcat_0300>-hotspot = abap_true.
          WHEN 'CL_CLIENTE'.
            <fs_fieldcat_0300>-coltext = 'Cod. Cliente'.
          WHEN 'NAME1'.
            <fs_fieldcat_0300>-coltext = 'Desc. Cliente'.
          WHEN 'STCD1'.
            <fs_fieldcat_0300>-coltext = 'CNPJ Cliente'.
          WHEN 'STCD2'.
            <fs_fieldcat_0300>-coltext = 'CPF'.
          WHEN 'STCD3'.
            <fs_fieldcat_0300>-coltext = 'Insc. Estadual'.
          WHEN 'COD_MATERIAL'.
            <fs_fieldcat_0300>-coltext = 'Cod. Material Base'.
          WHEN 'MAKTX'.
            <fs_fieldcat_0300>-coltext = 'Desc. Material Base'.
          WHEN 'MATKL'.
            <fs_fieldcat_0300>-coltext = 'GRP Material'.
          WHEN 'TP_CLASS'.
            <fs_fieldcat_0300>-coltext = 'TP. Classif'.
          WHEN 'SAFRA'.
            <fs_fieldcat_0300>-coltext = 'Safra'.
          WHEN 'TP_CALC'.
            <fs_fieldcat_0300>-coltext = 'TP. Calc'.
          WHEN 'USUARIO'.
            <fs_fieldcat_0300>-coltext = 'Usuário'.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.

    ENDIF.

    CREATE OBJECT lo_report_100.

    ls_layout_0300-stylefname = 'CELLTAB'.
    ls_layout_0300-cwidth_opt = abap_true.

    SET HANDLER: lo_report_100->on_hotspot_01 FOR go_alv_0300.

    CALL METHOD go_alv_0300->set_table_for_first_display
      EXPORTING
        is_layout       = ls_layout
        is_variant      = ls_variant
        i_save          = 'A'
      CHANGING
        it_outtab       = gt_saida_0300
        it_fieldcatalog = lt_fieldcat_0300.

  ELSE.

    CALL METHOD go_alv_0300->refresh_table_display( ).

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'LEAVE' OR 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'LEAVE' OR 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
