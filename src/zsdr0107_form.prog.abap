*======================================================================*
* PROJETO            : SAP Ninjas                                      *
* PROGRAMA           : ZSDR0107                                        *
* TRANSACAO          : ZSDT0199                                        *
* DESCRICAO          : Relatório Impostos Retidos                      *
*======================================================================*
* AUTOR              : Ronaldo Freitas                                 *
* Solicitante        : Rogerval Dias                                   *
* DATA               : 15.08.2024                                      *
*======================================================================*
*                      HISTORICO DE MUDANÇAS                           *
*======================================================================*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*======================================================================*
*&---------------------------------------------------------------------*
*&  Include           ZSDR0107_FORM
*&---------------------------------------------------------------------*

FORM fm_selecao.

  TYPES: BEGIN OF ty_docnum,
           docnum TYPE j_1bnfe_active-docnum,
         END OF ty_docnum.

  DATA: it_docnum TYPE TABLE OF ty_docnum,
        wa_docnum TYPE ty_docnum.

  TRY.
      IF p_chave IS NOT INITIAL.
        FREE p_docnum.
        LOOP AT p_chave INTO DATA(w_chave).

          SELECT SINGLE docnum
            FROM j_1bnfe_active
            INTO @DATA(vl_docnum)
        WHERE regio   = @w_chave-low(2)
          AND nfyear  = @w_chave-low+2(2)
          AND nfmonth = @w_chave-low+4(2)
          AND stcd1   = @w_chave-low+6(14)
          AND model   = @w_chave-low+20(2)
          AND serie   = @w_chave-low+22(3)
          AND nfnum9  = @w_chave-low+25(9)
          AND docnum9 = @w_chave-low+34(9)
          AND cdv     = @w_chave-low+43(1).

          CHECK sy-subrc IS INITIAL.

          wa_docnum-docnum = vl_docnum.
          APPEND wa_docnum TO it_docnum.
          CLEAR wa_docnum.

        ENDLOOP.

        IF it_docnum[] IS INITIAL.
          MESSAGE s836(sd) WITH 'Nenhum documento encontrado!' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

      ENDIF.

      IF it_docnum[] IS INITIAL.
        SELECT * FROM j_1bnfdoc " Documento
          INTO TABLE @DATA(it_1bnfdoc)
          WHERE docnum  IN @p_docnum
            AND bukrs   IN @p_bukrs
            AND branch  IN @p_branch
            AND nfenum  IN @p_nfenum
            AND series  IN @p_series
            AND pstdat  IN @p_pstdat
            AND parid   IN @p_parid.
      ELSE.
        SELECT * FROM j_1bnfdoc " Documento
          INTO TABLE @it_1bnfdoc
          FOR ALL ENTRIES IN @it_docnum
          WHERE docnum  EQ @it_docnum-docnum
            AND bukrs   IN @p_bukrs
            AND branch  IN @p_branch
            AND nfenum  IN @p_nfenum
            AND series  IN @p_series
            AND pstdat  IN @p_pstdat
            AND parid   IN @p_parid.
      ENDIF.

      IF sy-subrc IS INITIAL.
        SORT it_1bnfdoc BY docnum.

      ELSE.
        MESSAGE s836(sd) WITH 'Nenhum documento encontrado!' DISPLAY LIKE 'E'.
      ENDIF.

      CHECK it_1bnfdoc IS NOT INITIAL.

      SELECT * FROM j_1bnflin
        INTO TABLE @DATA(it_1bnflin)
        FOR ALL ENTRIES IN @it_1bnfdoc
        WHERE docnum EQ @it_1bnfdoc-docnum
          AND matkl  IN @p_matkl.

      IF sy-subrc IS INITIAL.
        SORT it_1bnflin BY docnum.
      ENDIF.

      CHECK it_1bnflin IS NOT INITIAL.

      SELECT vk~*, vf~vbeln AS vbelnvf, vf~vbelv
       INTO TABLE @DATA(it_vbak)
        FROM vbak AS vk
        INNER JOIN vbfa AS vf
        ON vk~vbeln = vf~vbelv
        FOR ALL ENTRIES  IN @it_1bnflin
        WHERE vk~vbeln   IN @p_vbeln
          AND vk~auart   IN @p_auart
          AND vf~vbeln   EQ @it_1bnflin-refkey(10)
          AND vf~vbtyp_n EQ 'M'.

      IF sy-subrc IS INITIAL.
        SORT it_vbak BY vbelnvf.
      ENDIF.

      SELECT * FROM j_1bnfe_active
        INTO TABLE @DATA(it_1bnfe_active)
        FOR ALL ENTRIES IN @it_1bnfdoc
        WHERE docnum EQ @it_1bnfdoc-docnum.

      IF sy-subrc IS INITIAL.
        SORT it_1bnfe_active BY docnum.
      ENDIF.

      SELECT * FROM zsdt0343_t2
        INTO TABLE @DATA(it_343_t2)
        FOR ALL ENTRIES IN @it_1bnfdoc
        WHERE bukrs  EQ @it_1bnfdoc-bukrs
          AND branch EQ @it_1bnfdoc-branch
          AND cancel NE 'X' .
      IF sy-subrc IS INITIAL.
        SORT it_343_t2 BY bukrs branch uf kschl.
      ENDIF.

      SELECT *
       INTO TABLE @DATA(it_vbrp)
        FROM vbrp
        FOR ALL ENTRIES IN @it_1bnflin
        WHERE vbeln  EQ @it_1bnflin-refkey(10)
          AND posnr  EQ @it_1bnflin-refitm.

      IF sy-subrc IS INITIAL.

        SORT it_vbrp BY vbeln posnr.

        SELECT *
          INTO TABLE @DATA(it_vbrk)
          FROM vbrk
          FOR ALL ENTRIES IN @it_vbrp
          WHERE vbeln  EQ @it_vbrp-vbeln.
        IF sy-subrc IS INITIAL.

          SORT it_vbrk BY vbeln.

          SELECT * FROM prcd_elements
           INTO TABLE @DATA(it_konv)
           FOR ALL ENTRIES IN @it_vbrk
           WHERE kschl IN @p_kschl
             AND knumv EQ @it_vbrk-knumv.

          IF sy-subrc IS INITIAL.
            SORT it_konv BY knumv kschl.
          ENDIF.
        ENDIF.
      ENDIF.

* Cabeçalho da nota fiscal
      LOOP AT it_1bnflin ASSIGNING FIELD-SYMBOL(<fs_1bnflin>).
        APPEND INITIAL LINE TO it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

        <fs_saida>-cfop        = <fs_1bnflin>-cfop.
        <fs_saida>-refitm      = <fs_1bnflin>-refitm.
        <fs_saida>-matnr       = <fs_1bnflin>-matnr.
        <fs_saida>-maktx       = <fs_1bnflin>-maktx.
        <fs_saida>-menge       = <fs_1bnflin>-menge.
        <fs_saida>-meins       = <fs_1bnflin>-meins.
*        <fs_saida>-vlr_unit    = <fs_1bnflin>-netwr.
*        <fs_saida>-base_icms   = <fs_1bnflin>-vicmsstret.
*        <fs_saida>-vlr_icms    = <fs_1bnflin>-vbcstret.
*        <fs_saida>-isentos     = <fs_1bnflin>-vicmsstdeson.

        READ TABLE it_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_1bnfdoc>) WITH KEY docnum = <fs_1bnflin>-docnum
                                                                   BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <fs_saida>-branch  = <fs_1bnfdoc>-branch.
          <fs_saida>-nfenum  = <fs_1bnfdoc>-nfenum.
          <fs_saida>-cpf_cnpj = <fs_1bnfdoc>-cpf.
          IF <fs_1bnfdoc>-cgc IS NOT INITIAL.
            <fs_saida>-cpf_cnpj = <fs_1bnfdoc>-cgc.
          ENDIF.
          <fs_saida>-series      = <fs_1bnfdoc>-series.
          <fs_saida>-docnum      = <fs_1bnfdoc>-docnum.
*          <fs_saida>-belnr       = <fs_1bnfdoc>-belnr.
          <fs_saida>-parid       = <fs_1bnfdoc>-parid.
          <fs_saida>-pstdat      = <fs_1bnfdoc>-pstdat+6(2) && '/' && <fs_1bnfdoc>-pstdat+4(2) && '/' && <fs_1bnfdoc>-pstdat(4).
          <fs_saida>-valor_nf    = <fs_1bnfdoc>-nftot.
          <fs_saida>-uf_c_for    = <fs_1bnfdoc>-regio.
*          <fs_saida>-insc_est    = <fs_1bnfdoc>-munins.
*          <fs_saida>-outros      = <fs_1bnfdoc>-observat.
*          <fs_saida>-terminal    = <fs_1bnfdoc>-id_intermed.

        ELSE.
          <fs_saida>-act = abap_true.
          CONTINUE.
        ENDIF.

        READ TABLE it_1bnfe_active ASSIGNING FIELD-SYMBOL(<fs_1bnfe_active>) WITH KEY docnum = <fs_1bnflin>-docnum
                                                                             BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          <fs_saida>-scssta = <fs_1bnfe_active>-scssta.
          <fs_saida>-docsta = <fs_1bnfe_active>-docsta.
          IF <fs_1bnfe_active>-docsta IS NOT INITIAL.
            <fs_saida>-chavea = |{ <fs_1bnfe_active>-regio }{ <fs_1bnfe_active>-nfyear }{ <fs_1bnfe_active>-nfmonth }{ <fs_1bnfe_active>-stcd1 }{ <fs_1bnfe_active>-model }{ <fs_1bnfe_active>-serie }{ <fs_1bnfe_active>-nfnum9 }{ <fs_1bnfe_active>-docnum9
  }{ <fs_1bnfe_active>-cdv }|.
          ENDIF.

        ENDIF.

        READ TABLE it_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp>) WITH KEY vbeln = <fs_1bnflin>-refkey(10)
                                                                      posnr = <fs_1bnflin>-refitm
                                                             BINARY SEARCH.

        IF sy-subrc IS INITIAL.

          READ TABLE it_vbrk ASSIGNING FIELD-SYMBOL(<fs_vbrk>) WITH KEY vbeln = <fs_vbrp>-vbeln
                                                                 BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            <fs_saida>-act = abap_true.
            CONTINUE.
          ENDIF.
        ELSE.
          <fs_saida>-act = abap_true.
          CONTINUE.
        ENDIF.

        IF <fs_vbrk>-knumv IS ASSIGNED.

          READ TABLE it_konv ASSIGNING FIELD-SYMBOL(<fs_konv>) WITH KEY knumv = <fs_vbrk>-knumv
                                                                        kschl = 'ZSFE'
                                                               BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-fator_f = <fs_saida>-fator_f + <fs_konv>-kwert.
          ENDIF.

          READ TABLE it_konv ASSIGNING <fs_konv> WITH KEY knumv = <fs_vbrk>-knumv
                                                                        kschl = 'ZGFE'
                                                               BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-fator_f = <fs_saida>-fator_f + <fs_konv>-kwert.
          ENDIF.

          READ TABLE it_konv ASSIGNING <fs_konv> WITH KEY knumv = <fs_vbrk>-knumv
                                                                        kschl = 'ZAFE'
                                                               BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-fator_f = <fs_saida>-fator_f + <fs_konv>-kwert.
          ENDIF.

          READ TABLE it_konv ASSIGNING <fs_konv> WITH KEY knumv = <fs_vbrk>-knumv
                                                          kschl = 'ZMFE'
                                                          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-fator_f = <fs_saida>-fator_f + <fs_konv>-kwert.
          ENDIF.

          READ TABLE it_konv ASSIGNING <fs_konv> WITH KEY knumv = <fs_vbrk>-knumv
                                                          kschl = 'ZFCF'
                                                          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-fator_f = <fs_saida>-fator_f + <fs_konv>-kwert.
          ENDIF.

          READ TABLE it_konv ASSIGNING <fs_konv> WITH KEY knumv = <fs_vbrk>-knumv
                                                          kschl = 'ZFOF'
                                                          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-fator_f = <fs_saida>-fator_f + <fs_konv>-kwert.
          ENDIF.
* ------------------------
          READ TABLE it_konv ASSIGNING <fs_konv> WITH KEY knumv = <fs_vbrk>-knumv
                                                                        kschl = 'ZSFA'
                                                               BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-fator_i = <fs_konv>-kwert.
          ENDIF.

          READ TABLE it_konv ASSIGNING <fs_konv> WITH KEY knumv = <fs_vbrk>-knumv
                                                                        kschl = 'ZGFA'
                                                               BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-fator_fa = <fs_konv>-kwert.
          ENDIF.

          READ TABLE it_konv ASSIGNING <fs_konv> WITH KEY knumv = <fs_vbrk>-knumv
                                                                        kschl = 'ZIMM'
                                                               BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-fator_im = <fs_konv>-kwert.
          ENDIF.
*------------------------------
          READ TABLE it_konv ASSIGNING <fs_konv> WITH KEY knumv = <fs_vbrk>-knumv
                                                                        kschl = 'ZFCI'
                                                               BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-fator_ima = <fs_konv>-kwert.
          ENDIF.

          READ TABLE it_konv ASSIGNING <fs_konv> WITH KEY knumv = <fs_vbrk>-knumv
                                                                        kschl = 'ZFOI'
                                                               BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_saida>-fator_ima = <fs_saida>-fator_ima + <fs_konv>-kwert.
          ENDIF.

        ENDIF.
      ENDLOOP.

      IF it_saida IS NOT INITIAL.
        DELETE it_saida WHERE act EQ abap_true.
      ENDIF.

    CATCH cx_root.
      MESSAGE s836(sd) WITH 'Erro na seleção dos dados!' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.


MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0. "LEAVE PROGRAM.
    WHEN 'SAIR'.
      LEAVE PROGRAM.
    WHEN 'VOLTAR'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

FORM fm_cria_fieldcat .
  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

  git_fcat_pend = VALUE lit_fieldcat_aux(
( tabname = 'IT_SAIDA'  fieldname = ' BRANCH    '        coltext = '  Filial  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' BRANCH    '         )
( tabname = 'IT_SAIDA'  fieldname = ' CFOP      '        coltext = '  Cfop  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' CFOP      '         )
( tabname = 'IT_SAIDA'  fieldname = ' NFENUM    '        coltext = '  NF nº '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' NFENUM    '         )
( tabname = 'IT_SAIDA'  fieldname = ' SERIES    '        coltext = '  Serie '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' SERIES    '         )
( tabname = 'IT_SAIDA'  fieldname = ' DOCNUM    '        coltext = '  Docnum Fiscal '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' DOCNUM    '         )
( tabname = 'IT_SAIDA'  fieldname = ' REFITM    '        coltext = '  Item  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' REFITM    '         )
*( tabname = 'IT_SAIDA'  fieldname = ' BELNR     '        coltext = '  Doc Contábil  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' BELNR     '         )
*( tabname = 'IT_SAIDA'  fieldname = ' INSC_EST  '        coltext = '  Insc. Estadual  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' INSC_EST  '         )
( tabname = 'IT_SAIDA'  fieldname = ' PARID     '        coltext = '  Cliente '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' PARID     '         )
( tabname = 'IT_SAIDA'  fieldname = ' CPF_CNPJ  '        coltext = '  CPF/CNPJ  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' CPF_CNPJ  '         )
*( tabname = 'IT_SAIDA'  fieldname = ' UF_C_FOR  '        coltext = '  UF Cli/For  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' UF_C_FOR  '         )
( tabname = 'IT_SAIDA'  fieldname = ' MATNR     '        coltext = '  Produto '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' MATNR     '         )
( tabname = 'IT_SAIDA'  fieldname = ' MAKTX     '        coltext = '  Descr. Material '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' MAKTX     '         )
( tabname = 'IT_SAIDA'  fieldname = ' PSTDAT    '        coltext = '  Data  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' PSTDAT    '         )
( tabname = 'IT_SAIDA'  fieldname = ' MENGE     '        coltext = '  Quantidade  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' MENGE     '         )
( tabname = 'IT_SAIDA'  fieldname = ' MEINS     '        coltext = '  Unidade Medida  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' MEINS     '         )
( tabname = 'IT_SAIDA'  fieldname = ' VALOR_NF  '        coltext = '  Valor NF  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' VALOR_NF  '         )
( tabname = 'IT_SAIDA'  fieldname = ' FATOR_F   '        coltext = '  Valor FETHAB  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' FATOR_F   '         )
( tabname = 'IT_SAIDA'  fieldname = ' FATOR_I   '        coltext = '  Valor IAGRO   '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' FATOR_I   '         )
( tabname = 'IT_SAIDA'  fieldname = ' FATOR_FA  '        coltext = '  Valor FABOV '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' FATOR_FA  '         )
( tabname = 'IT_SAIDA'  fieldname = ' FATOR_IM  '        coltext = '  Valor IMAmt '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' FATOR_IM  '         )
( tabname = 'IT_SAIDA'  fieldname = ' FATOR_IMA '        coltext = '  Valor IMAFIR  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' FATOR_IMA '         )
*( tabname = 'IT_SAIDA'  fieldname = ' BASE_ICMS '        coltext = '  Base Icms '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' BASE_ICMS '         )
*( tabname = 'IT_SAIDA'  fieldname = ' VLR_ICMS  '        coltext = '  Vlr. ICMS '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' VLR_ICMS  '         )
*( tabname = 'IT_SAIDA'  fieldname = ' OUTROS    '        coltext = '  Outros  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' OUTROS    '         )
*( tabname = 'IT_SAIDA'  fieldname = ' ISENTOS   '        coltext = '  Isentos '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' ISENTOS   '         )
*( tabname = 'IT_SAIDA'  fieldname = ' VLR_UNIT  '        coltext = '  Vlr. Unit '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' VLR_UNIT  '         )
*( tabname = 'IT_SAIDA'  fieldname = ' TERMINAL  '        coltext = '  Terminal  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' TERMINAL  '         )
( tabname = 'IT_SAIDA'  fieldname = ' SCSSTA    '        coltext = '  Status SAP  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' SCSSTA    '         )
( tabname = 'IT_SAIDA'  fieldname = ' DOCSTA    '        coltext = '  Status NFE  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' DOCSTA    '         )
( tabname = 'IT_SAIDA'  fieldname = ' CHAVEA    '        coltext = '  CHAVE Nf-e  '     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = ' CHAVEA    '         )
).

ENDFORM.

FORM fm_alv.
  gs_variant-report = sy-repid.

  PERFORM fm_cria_fieldcat.

  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(

  EXPORTING
    i_titulo  = 'Relatório Impostos Retidos - ' &&  lva_data
     i_filtros = VALUE zif_screen_linha_filtro_t(
   )

  CHANGING
    alv = gob_gui_alv_grid
    )
    EQ abap_true.

    wa_layout-sel_mode   = 'A'.
    wa_layout-zebra      = 'X'.

    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = gs_variant
        i_save                        = 'A'
        is_layout                     = wa_layout
        i_default                     = 'X'
      CHANGING
        it_outtab                     = it_saida
        it_fieldcatalog               = git_fcat_pend
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.
ENDFORM.

FORM fm_exibirdados .
  IF it_saida[] IS NOT INITIAL.
    CALL SCREEN '0100'.
  ELSE.
    MESSAGE s836(sd) WITH 'Nenhum documento encontrado!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
ENDFORM.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST0100'.
  PERFORM fm_alv.
ENDMODULE.
