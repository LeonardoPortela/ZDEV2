FUNCTION z_gera_ov_contrato.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(I_VLR) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_NO_PRINT) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_ID_DOCUMENTO) TYPE  ZSDT0310-ID_DOCUMENTO OPTIONAL
*"  EXPORTING
*"     VALUE(E_XSTRING_DOCUMENT) TYPE  XSTRING
*"  TABLES
*"      VBELN STRUCTURE  ZSDT0041 OPTIONAL
*"----------------------------------------------------------------------

  DATA: it_vbak             TYPE TABLE OF vbak,
        wa_vbak             TYPE vbak,
        wa_tvakt            TYPE tvakt,
        wa_tvkbt            TYPE tvkbt,
        it_vbap             TYPE TABLE OF zsds004,
        it_vbap2            TYPE TABLE OF zsds004,
        wa_vbap2            TYPE zsds004,
        wa_vbap             TYPE zsds004,
        wa_t052             TYPE t052,
        wa_t052u            TYPE t052u,
        it_vbkd             TYPE TABLE OF vbkd INITIAL SIZE 0 WITH HEADER LINE,
        it_ztext            TYPE TABLE OF ttext INITIAL SIZE 0 WITH HEADER LINE,
        wa_ztext            TYPE ttext,
        wa_vbkd             TYPE vbkd,
        wa_info_c           TYPE kna1,
        wa_tvko             TYPE tvko,
        wa_tvtwt            TYPE tvtwt,
        wa_tspat            TYPE tspat,
        wa_t001             TYPE t001,
        wa_werks            TYPE t001w,
        wa_vstel            TYPE t001w,
        wa_tcurt            TYPE tcurt,
        vl_formname         TYPE tdsfname,
        vl_name             TYPE rs38l_fnam,
        ls_options          TYPE ssfcompop,
        ls_job_output_info  TYPE ssfcrescl,
        ls_otfdata          TYPE tsfotf,
        ls_bin_fsize        TYPE i,
        ls_xstring_document TYPE xstring,
        t_lines             TYPE STANDARD TABLE OF tline,
        w_pdf_files         TYPE zsde_pdf_files,
        t_pdf_files         TYPE zsdt_pdf_files,
        ls_merged_document  TYPE xstring,
        control             TYPE ssfctrlop,
        vg_cpf              TYPE c LENGTH 14,
        vg_cnpj             TYPE c LENGTH 18,
        cnpj                TYPE c LENGTH 18,
        tx_date             TYPE string,
        vg_name             LIKE thead-tdname,
        vg_language         LIKE thead-tdspras,
        vg_object           LIKE thead-tdobject,
*        VG_LINE       TYPE TABLE OF TLINE INITIAL SIZE 0 WITH HEADER LINE,
        vg_linez            TYPE TABLE OF tline INITIAL SIZE 0 WITH HEADER LINE, "ztline initial size 0 with header line,
        it_matnr            TYPE TABLE OF tline INITIAL SIZE 0 WITH HEADER LINE,
        wa_linez            TYPE ztline,
        vg_tabix            TYPE sy-tabix,
        v_matant            TYPE vbap-matnr,
        it_konv             TYPE TABLE OF konv INITIAL SIZE 0 WITH HEADER LINE,
        wa_konv             TYPE konv,
        vl_total            TYPE vbap-netwr,
        vl_quantidade       TYPE vbap-kwmeng,
        vl_unidade          TYPE konv-kmein,
        vl_preco_unit       TYPE vbap-netpr,
        wekrs               TYPE kna1-kunnr,
        r_vbeln             TYPE RANGE OF vbeln,
        coeficiente         TYPE p DECIMALS 5,
        tcode               TYPE syst_tcode,
        p_vlr(1),
*
        l_seq               TYPE char1,             "*-CS2019001753-13.06.2023-#110407-JT
        l_partic            TYPE char1,             "*-CS2019001753-13.06.2023-#110407-JT
        l_eletronica        TYPE char1,             "*-CS2019001753-13.06.2023-#110407-JT
        t_partic            TYPE zsde0074_t,        "*-CS2019001753-13.06.2023-#110407-JT
        w_partic            TYPE zsde0074,          "*-CS2019001753-13.06.2023-#110407-JT
        t_0041              TYPE TABLE OF zsdt0041, "*-CS2019001753-13.06.2023-#110407-JT
        w_0041              TYPE zsdt0041,          "*-CS2019001753-13.06.2023-#110407-JT
        w_0310              TYPE zsdt0310,          "*-CS2019001753-13.06.2023-#110407-JT
        t_0316              TYPE TABLE OF zsdt0316, "*-CS2019001753-13.06.2023-#110407-JT
        w_0316              TYPE zsdt0316.          "*-CS2019001753-13.06.2023-#110407-JT

  "*-CS2019001753-13.06.2023-#110407-JT-inicio
  FREE: t_pdf_files,
        e_xstring_document,
        w_0310,
        t_0316,
        t_partic,
        l_partic,
        l_eletronica.
  "*-CS2019001753-13.06.2023-#110407-JT-fim

  IF i_vbeln IS NOT INITIAL.
    r_vbeln = VALUE #(  ( option = 'EQ' sign = 'I' low = i_vbeln ) ).
  ELSE.
    r_vbeln = VALUE #( FOR ls IN vbeln ( option = 'EQ' sign = 'I' low = ls-vbeln ) ).
  ENDIF.

  p_vlr = i_vlr.

  SELECT * INTO TABLE it_vbak
    FROM vbak
   WHERE vbeln IN r_vbeln.

  DATA(qtd_line) =  REDUCE i( INIT x = 0 FOR l1 IN it_vbak WHERE ( vbeln IS NOT INITIAL ) NEXT x = x + 1 ) .

  LOOP AT it_vbak INTO wa_vbak.

    IF wa_vbak-vbeln IS INITIAL.
      MESSAGE 'Ordem de Venda não encontrada!' TYPE 'S'.
      EXIT.
    ENDIF.

    DATA(_tabix) = sy-tabix.

    SELECT SINGLE *
      FROM zsdt0041
      INTO @DATA(wa_0041)
      WHERE vbeln EQ @wa_vbak-vbeln.

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE doc_simulacao
        FROM zsdt0090
        INTO wa_0041-doc_simulacao
        WHERE vbeln EQ wa_vbak-vbeln.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0040
      INTO @DATA(wa_0040)
      WHERE doc_simulacao EQ @wa_0041-doc_simulacao.

    SELECT FROM v_konv FIELDS * WHERE kschl IN ( 'PR00' , 'ICVA' , 'ICBS' , 'RB00' ) AND knumv EQ @wa_vbak-knumv INTO CORRESPONDING FIELDS OF TABLE @it_konv .

    SELECT SINGLE * INTO wa_tvkbt
      FROM tvkbt
      WHERE vkbur EQ wa_vbak-vkbur.

    SELECT SINGLE * INTO wa_tvakt
      FROM tvakt
      WHERE spras EQ sy-langu
      AND   auart EQ wa_vbak-auart.

    SELECT  *
     FROM vbap
     INTO CORRESPONDING FIELDS OF TABLE it_vbap
         WHERE vbeln EQ wa_vbak-vbeln.

    SELECT *
     FROM vbep
     INTO TABLE @DATA(it_vbep)
      FOR  ALL ENTRIES IN @it_vbap
       WHERE vbeln EQ @wa_vbak-vbeln
         AND posnr EQ @it_vbap-posnr.

    LOOP AT it_vbep INTO DATA(_vbep) WHERE lifsp EQ '12'.
      DELETE it_vbap WHERE posnr EQ _vbep-posnr.
    ENDLOOP.

    DELETE it_vbep WHERE lifsp EQ '12'.

    SELECT * INTO TABLE it_vbkd
      FROM vbkd
     WHERE vbeln EQ wa_vbak-vbeln.

    SELECT SINGLE * INTO wa_tcurt
      FROM tcurt
     WHERE spras EQ sy-langu
       AND waers EQ wa_vbak-waerk.

    IF NOT it_vbkd[] IS INITIAL.
      READ TABLE it_vbkd INTO wa_vbkd INDEX 1.

      IF NOT wa_vbkd-zterm IS INITIAL.

        SELECT SINGLE * INTO wa_t052u
          FROM t052u
         WHERE zterm EQ wa_vbkd-zterm
          AND  spras EQ sy-langu.

        IF NOT wa_t052u IS INITIAL.
          wa_ztext = SWITCH #( wa_vbkd-zterm
                                " VV Venda a Vista
                                WHEN 'I003' THEN COND #(
                                                         WHEN wa_0040-meio_pago EQ 'A' THEN 'A PRAZO COM ENTREGA POSTERIOR' ELSE 'A VISTA'
                                                       )
                                " TS Troca Safra e Troca a Vista
                                WHEN 'I001' OR 'I004' THEN 'A PRAZO COM ENTREGA POSTERIOR ("TRC")'
                                " AD adiantamento
                                WHEN 'I002' THEN 'A PRAZO COM ENTREGA POSTERIOR ("ADTO")'
                                ELSE wa_t052u-text1
                           ).
        ENDIF.

      ENDIF.

    ENDIF.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro = wa_vbak-kunnr
        p_partype  = 'C'
      CHANGING
        wa_info_c  = wa_info_c.

    IF wa_info_c-stkzn IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = wa_info_c-stcd1
        IMPORTING
          output = vg_cnpj.
      wa_info_c-knurl = vg_cnpj.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
        EXPORTING
          input  = wa_info_c-stcd2
        IMPORTING
          output = vg_cpf.
      wa_info_c-knurl = vg_cpf.
    ENDIF.

    wa_info_c-txjcd = wa_info_c-txjcd(2).

    "Org. de Vendas
    SELECT SINGLE * INTO wa_tvko
      FROM tvko
     WHERE vkorg EQ wa_vbak-vkorg.

    SELECT SINGLE * INTO wa_t001
      FROM t001
     WHERE bukrs  EQ wa_tvko-bukrs.

    "Canal de Distribuição
    SELECT SINGLE * INTO wa_tvtwt
      FROM tvtwt
     WHERE spras EQ sy-langu
       AND vtweg EQ wa_vbak-vtweg.

    "Setor de atividade
    SELECT SINGLE * INTO wa_tspat
      FROM tspat
     WHERE spras EQ sy-langu
       AND spart EQ wa_vbak-spart.

    it_vbap2[] = it_vbap[].

    SORT: it_vbap  BY matnr,
          it_vbap2 BY matnr,
          it_konv  BY kposn.

    DELETE ADJACENT DUPLICATES FROM it_vbap COMPARING matnr.

    TRY .
        DATA(v_posnr) = it_vbep[ lifsp = '12' ]-posnr.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    DELETE it_vbap WHERE posnr EQ v_posnr.


    LOOP AT it_vbap INTO wa_vbap.

      vg_tabix = sy-tabix.

      CLEAR: wa_vbap-netwr ,
             wa_vbap-kwmeng,
             vl_total      ,
             vl_quantidade ,
             vl_unidade    ,
             vl_preco_unit .

      LOOP AT it_vbap2 INTO wa_vbap2 WHERE matnr = wa_vbap-matnr.

        TRY .
            wa_konv = it_konv[ kposn = wa_vbap2-posnr kschl = 'PR00' ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        ADD wa_vbap2-netwr  TO vl_total.       " Valor Total
        ADD wa_vbap2-kwmeng TO vl_quantidade.  " Quantidade

        IF wa_vbap2-mwsbp IS NOT INITIAL.

          TRY .
              DATA(v_icva) = it_konv[ kposn = wa_vbap2-posnr kschl = 'ICVA' ]-kbetr.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          TRY .
              DATA(v_icbs) = it_konv[ kposn = wa_vbap2-posnr kschl = 'ICBS' ]-kbetr.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          TRY .
              v_icva = v_icva / it_konv[ kposn = wa_vbap2-posnr kschl = 'ICVA' ]-kawrt.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          TRY .
              v_icbs = v_icbs / it_konv[ kposn = wa_vbap2-posnr kschl = 'ICBS' ]-kawrt.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          TRY .
              coeficiente = 1 - ( ( v_icbs * ( v_icva / 100 ) ) / 100 ).
            CATCH cx_sy_zerodivide.
          ENDTRY.

          TRY .
              vl_preco_unit = wa_konv-kbetr / coeficiente.
            CATCH cx_sy_zerodivide.
          ENDTRY.

        ELSE.
          vl_preco_unit  =   wa_konv-kbetr.    " Preco unitário
        ENDIF.

      ENDLOOP.

      IF wa_vbak-spart EQ '02' OR wa_vbak-spart EQ '04'.
        IF wa_vbap-vrkme NE wa_konv-kmein.
          CASE wa_konv-kmein.
            WHEN 'TO'. vl_quantidade = vl_quantidade / 1000.
            WHEN 'KG'. vl_quantidade = vl_quantidade * 1000.
          ENDCASE.
        ENDIF.
      ENDIF.

*      VL_TOTAL  = VL_PRECO_UNIT * VL_QUANTIDADE.
      vl_total = vl_total + wa_vbap-mwsbp.

*      TRY .
*          DATA(DESC_ABS_) = IT_KONV[ KPOSN = WA_VBAP2-POSNR KSCHL = 'RB00' ]-KBETR.
*        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*      ENDTRY.

*      TRY .
*          DESC_ABS_ = DESC_ABS_ / COEFICIENTE.
*        CATCH CX_SY_ZERODIVIDE.
*      ENDTRY.

*      DATA BASE_LIQ TYPE KBETR.

*      TRY .
*          BASE_LIQ =  ( ( DESC_ABS_ / VL_QUANTIDADE ) + WA_KONV-KBETR ).
*        CATCH CX_SY_ZERODIVIDE.
*          CLEAR BASE_LIQ.
*      ENDTRY.

*      IF BASE_LIQ NE WA_KONV-KBETR.
*        ADD DESC_ABS_ TO VL_TOTAL.
*      ENDIF.

      wa_vbap-netwr  =  vl_total.      " Valor Total
      wa_vbap-kwmeng =  vl_quantidade. " Quantidade

      wa_vbap-vrkme  =  vl_unidade.    " Unidade

      SELECT SINGLE normt
        FROM mara
        INTO wa_vbap-normt
        WHERE matnr EQ wa_vbap-matnr.

      wa_vbap-kbetr  =  vl_preco_unit. " Preco unitário
      wa_vbap-kmein  =  wa_konv-kmein. " Unidade

      "Escritório de Vendas
      SELECT SINGLE * INTO wa_werks
        FROM t001w
       WHERE werks EQ wa_vbap-werks.

      wekrs = |{ wa_vbap-werks ALPHA = IN }|.

      SELECT SINGLE * INTO @DATA(wa_kna1)
        FROM kna1
        WHERE kunnr EQ @wekrs.

      "Centro fornecedor
      SELECT SINGLE * INTO wa_vstel
        FROM t001w
       WHERE werks EQ wa_vbap-vstel.

      FREE: vg_line[].
      PERFORM reade_text USING 'GRUN' sy-langu wa_vbap-matnr 'MATERIAL'.

      CALL FUNCTION 'ZFORMATA_LINES'
        TABLES
          lines   = vg_line
          lines_s = it_matnr.

      LOOP AT it_matnr.
        wa_vbap-desc_uni = |{ wa_vbap-desc_uni } { it_matnr-tdline }|.
      ENDLOOP.

      FREE: it_matnr[], vg_line[].

      wa_vbap-matnr = |{ wa_vbap-matnr ALPHA = OUT }|.

      MODIFY it_vbap INDEX vg_tabix
      FROM wa_vbap
      TRANSPORTING matnr netwr kwmeng netpr kbetr kmein normt desc_uni.

    ENDLOOP.

    SORT it_vbap BY matnr.

    IF ( NOT wa_vbak-audat IS INITIAL ) AND ( NOT wa_werks IS INITIAL ).

      CALL FUNCTION 'Z_DATA_CIDADE_TEXTO'
        EXPORTING
          pais    = wa_werks-land1
          date    = wa_vbak-audat
          txjcd   = wa_werks-txjcd
        IMPORTING
          tx_date = tx_date.

    ENDIF.

*-CS2019001753-13.06.2023-#110407-JT-inicio
*-participantes da OV (gerador contratos)
    IF i_id_documento IS NOT INITIAL.
      SELECT SINGLE *
        INTO w_0310
        FROM zsdt0310
       WHERE id_documento = i_id_documento.

      IF sy-subrc = 0.
        SELECT *
          INTO TABLE t_0316
          FROM zsdt0316
         WHERE nr_doc_gerado    = w_0310-nr_doc_gerado
           AND id_doc_agrupador = w_0310-id_documento.
      ENDIF.

      SORT t_0316 BY cliente.

      IF w_0310-tipo_doc_digital = 'S'.
        LOOP AT t_0316 INTO w_0316.
          CLEAR w_partic.
          IF w_0316-cliente = abap_true.
            w_partic-partic_linha1_a = | { w_0316-nome } - { 'Comprador(a)' } |.
          ELSE.
            w_partic-partic_linha1_a = | { wa_t001-butxt } - { 'Vendedora' } |.
            w_partic-partic_linha2_a = | { 'Rep. por' } { w_0316-nome } |.
          ENDIF.

          APPEND w_partic           TO t_partic.
        ENDLOOP.

      ELSE.
        l_seq = 1.

        LOOP AT t_0316 INTO w_0316.
          IF l_seq = 1.
            l_seq                      = 2.

            CLEAR w_partic.
            IF w_0316-cliente = abap_true.
              w_partic-partic_traco1_a = '_______________________________________'.
              w_partic-partic_linha1_a = | { w_0316-nome } - { 'Comprador(a)' } |.
            ELSE.
              w_partic-partic_traco1_a = '_______________________________________'.
              w_partic-partic_linha1_a = | { wa_t001-butxt } - { 'Vendedora' } |.
              w_partic-partic_linha2_a = | { 'Rep. por' } { w_0316-nome } |.
            ENDIF.

          ELSEIF l_seq = 2.
            l_seq                      = 1.

            IF w_0316-cliente = abap_true.
              w_partic-partic_traco1_b = '_______________________________________'.
              w_partic-partic_linha1_b = | { w_0316-nome } - { 'Comprador(a)' } |.
            ELSE.
              w_partic-partic_traco1_b = '_______________________________________'.
              w_partic-partic_linha1_b = | { wa_t001-butxt } - { 'Vendedora' } |.
              w_partic-partic_linha2_b = | { 'Rep. por' } { w_0316-nome } |.
            ENDIF.

            APPEND w_partic           TO t_partic.
          ENDIF.
        ENDLOOP.

        IF l_seq = 2.
          APPEND w_partic             TO t_partic.
        ENDIF.
      ENDIF.

      l_partic     = COND #( WHEN t_partic[] IS NOT INITIAL     THEN abap_true
                                                                ELSE abap_off ).
      l_eletronica = COND #( WHEN w_0310-tipo_doc_digital = 'N' THEN abap_off
                                                                ELSE abap_true ).
    ENDIF.
*-CS2019001753-13.06.2023-#110407-JT-inicio

    vl_formname = 'ZSDF0007'.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = vl_formname
      IMPORTING
        fm_name            = vl_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    PERFORM reade_text USING '0002' sy-langu wa_vbak-vbeln 'VBBK'.

    IF NOT vg_line[] IS INITIAL.
      wa_linez-tdformat = 'N'.
      wa_linez-tdline   = 'Observações:'.
      APPEND wa_linez TO vg_linez.
    ENDIF.

    CALL FUNCTION 'ZFORMATA_LINES'
      TABLES
        lines   = vg_line
        lines_s = vg_linez.

    IF NOT vg_line[] IS INITIAL.
      wa_linez-tdformat = '*'.
      wa_linez-tdline   = ''.
      APPEND wa_linez TO vg_linez.
    ENDIF.

*  IMPRESORA
    ls_options-tddest   = 'LOCL'.     "Disposit. saída
    ls_options-tdimmed  = abap_true.  "Saída Imediata
    ls_options-tdnewid  = abap_true.  "Nova Ordem SPOOL

    IF i_vbeln IS INITIAL.
      ls_options-tdcovtitle = |Ordem_{ wa_vbak-vbeln }_{ sy-uname }_{ sy-datum }_{ sy-uzeit }|. "Titulo
    ELSE.
      ls_options-tdcovtitle = |Simulador_{ wa_0041-doc_simulacao ALPHA = OUT }_{ sy-uname }_{ sy-datum }_{ sy-uzeit }|. "Titulo
    ENDIF.

    IF i_vbeln IS INITIAL AND qtd_line >= 2.
      CASE _tabix.
        WHEN 1.
          control-no_open  = abap_false.
          control-no_close = abap_true.
        WHEN qtd_line.
          control-no_open  = abap_true.
          control-no_close = abap_false.
        WHEN OTHERS.
          control-no_open  = abap_true.
          control-no_close = abap_true.
      ENDCASE.
    ENDIF.

*-CS2019001753-27.02.2023-#65723-JT-inicio
    IF i_no_print = abap_true.
      FREE: ls_job_output_info.
      control-no_dialog = abap_true.
      control-langu     = sy-langu.
      control-getotf    = abap_true.
    ENDIF.
*-CS2019001753-27.02.2023-#65723-JT-fim

    CALL FUNCTION vl_name
      EXPORTING
        control_parameters = control
        wa_t001            = wa_t001
        wa_werks           = wa_werks
        wa_vstel           = wa_vstel
        wa_vbak            = wa_vbak
        tt_vbap            = wa_vbap
        wa_vbkd            = wa_vbkd
        wa_info_c          = wa_info_c
        wa_tvtwt           = wa_tvtwt
        wa_tspat           = wa_tspat
        wa_ztext           = wa_ztext
        wa_tcurt           = wa_tcurt
        tx_date            = tx_date
        wa_tvakt           = wa_tvakt
        wa_tvkbt           = wa_tvkbt
        wg_preco_unit      = p_vlr
        wa_0040            = wa_0040
*       WA_0041            = WA_0041
        wa_kna1            = wa_kna1
        it_partic          = t_partic     "*-CS2019001753-13.06.2023-#110407-JT
        i_partic           = l_partic     "*-CS2019001753-13.06.2023-#110407-JT
        i_eletronica       = l_eletronica "*-CS2019001753-13.06.2023-#110407-JT
        user_settings      = ' '
        output_options     = ls_options
        tcode              = sy-tcode
      IMPORTING
        job_output_info    = ls_job_output_info   "*-CS2019001753-13.06.2023-#110407-JT
      TABLES
        it_vbap            = it_vbap
        it_lines           = vg_linez
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*-CS2019001753-27.02.2023-#65723-JT-inicio
    IF i_no_print = abap_true.
      MOVE ls_job_output_info-otfdata[] TO ls_otfdata[].

*-------------------------------------------
*-----format xstring
*-------------------------------------------
      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          format                = 'PDF'
        IMPORTING
          bin_filesize          = ls_bin_fsize
          bin_file              = ls_xstring_document
        TABLES
          otf                   = ls_otfdata[]
          lines                 = t_lines
        EXCEPTIONS
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          err_bad_otf           = 4
          OTHERS                = 5.

      IF sy-subrc = 0.
        FREE: w_pdf_files.
        w_pdf_files-data      =  ls_xstring_document.
        w_pdf_files-len       =  xstrlen( ls_xstring_document ).
        APPEND w_pdf_files   TO t_pdf_files.
      ENDIF.
    ENDIF.
*-CS2019001753-27.02.2023-#65723-JT-fim

  ENDLOOP.

*-CS2019001753-27.02.2023-#65723-JT-inicio
*-----------------------------------------
* agrupa documentos
*-----------------------------------------
  IF i_no_print = abap_true.
    TRY.
        e_xstring_document = zcl_faturamento=>zif_faturamento~get_instance(
                           )->get_merge_pdf( EXPORTING t_pdf_files = t_pdf_files
                           ).

      CATCH zcx_faturamento.
      CATCH zcx_error.
    ENDTRY.
  ENDIF.
*-CS2019001753-27.02.2023-#65723-JT-fim

ENDFUNCTION.
