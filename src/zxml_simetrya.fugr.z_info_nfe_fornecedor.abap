FUNCTION z_info_nfe_fornecedor.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_INFO_NOTAS STRUCTURE  ZIB_NFE_FORN
*"----------------------------------------------------------------------
  TYPES : BEGIN OF ty_cnpj ,
            cnpj  TYPE lfa1-stcd1,
            cnpj2 TYPE lfa1-stcd2,
          END OF   ty_cnpj .

  TYPES : BEGIN OF ty_parid_filter ,
            parid  TYPE j_1bnfdoc-parid,
            partyp TYPE j_1bnfdoc-partyp,
          END OF   ty_parid_filter.

  DATA: it_parid_filter TYPE TABLE OF ty_parid_filter WITH HEADER LINE.


  "Tabelas
  DATA: t_lfa1    TYPE TABLE OF lfa1,
        t_lfa2    TYPE TABLE OF lfa1,
        t_kna1    TYPE TABLE OF kna1,
        t_kna2    TYPE TABLE OF kna1,
        vdoctyp   TYPE TABLE OF lxhme_range_c1,
        t_cnpj    TYPE TABLE OF ty_cnpj,
        t_acttab  TYPE TABLE OF j_1bnfe_active,
        t_doc_aux TYPE TABLE OF j_1bnfdoc,
        t_doc     TYPE TABLE OF j_1bnfdoc
        .
  "Work Areas
  DATA: wa_info_notas TYPE zib_nfe_forn,
        wa_lfa1       TYPE lfa1,
        wa_kna1       TYPE kna1,
        wa_filial     TYPE j_1bbranch,
        wa_ib_nota    TYPE zib_nota_fiscal_sap,
        wa_parid      TYPE lxhme_range_c10,
        wa_zib_nfe    TYPE zib_nfe_forn,
        wa_doc        TYPE j_1bnfdoc,
        wa_acttab     TYPE j_1bnfe_active,
        wdoctyp       TYPE lxhme_range_c1,
        it_parid      TYPE lxhme_range_c10_t,
        wa_cnpj       TYPE ty_cnpj.
  "Variaveis
  DATA: vg_filial         TYPE j_1bbranc_,
        vg_serie          TYPE i,
        vg_numero         TYPE i,
        vg_stcd3          TYPE stcd3,
        vg_stcd3_psq      TYPE stcd3,
        vl_cancel	        TYPE j_1bcancel,
        vl_form           TYPE j_1bform,
        vl_nu_chave_serie	TYPE j_1bseries,
        vl_serie_int_aux  TYPE i,
        vl_serie_srt_aux  TYPE c LENGTH 3,
        vl_nr_nf          TYPE j_1bnfdoc-nfenum,
        vl_gravou         TYPE c LENGTH 1.

  RANGES: rg_nr_nf          FOR j_1bnfdoc-nfenum,
          rg_serie          FOR j_1bnfdoc-series,
          rg_parid          FOR j_1bnfdoc-parid.
  vl_gravou := 'N'.

  DELETE it_info_notas WHERE nu_chave_modelo IS INITIAL OR
                             nu_chave_numero IS INITIAL OR
                             nu_chave_serie  IS INITIAL OR
                             nu_chave_cnpj   IS INITIAL.

  LOOP AT it_info_notas INTO wa_info_notas .
    wa_cnpj-cnpj = wa_info_notas-nu_chave_cnpj.
    wa_cnpj-cnpj2 = wa_info_notas-nu_chave_cnpj.
    APPEND wa_cnpj TO t_cnpj.
  ENDLOOP.

  DATA(it_stcd1) = t_cnpj[].
  DELETE it_stcd1 WHERE cnpj IS INITIAL.
  SORT it_stcd1 BY cnpj.
  DELETE ADJACENT DUPLICATES FROM it_stcd1 COMPARING cnpj.

  DATA(it_stcd2) = t_cnpj[].
  DELETE it_stcd2 WHERE cnpj2 IS INITIAL.
  SORT it_stcd2 BY cnpj2.
  DELETE ADJACENT DUPLICATES FROM it_stcd2 COMPARING cnpj2.

  IF it_stcd1[] IS NOT INITIAL.
    SELECT *
      FROM lfa1
      INTO TABLE t_lfa1
       FOR ALL ENTRIES IN it_stcd1
     WHERE stcd1 EQ it_stcd1-cnpj
       AND land1 EQ 'BR'.

    SELECT  *
      FROM kna1
      INTO TABLE t_kna1
       FOR ALL ENTRIES IN it_stcd1
     WHERE stcd1 EQ it_stcd1-cnpj
       AND land1 EQ 'BR'.
  ENDIF.

  IF it_stcd2[] IS NOT INITIAL.
    SELECT *
      FROM lfa1
      INTO TABLE t_lfa2
       FOR ALL ENTRIES IN it_stcd2
     WHERE stcd2 EQ it_stcd2-cnpj2
       AND land1 EQ 'BR'.

    SELECT  *
      FROM kna1
      INTO TABLE t_kna2
       FOR ALL ENTRIES IN it_stcd2
     WHERE stcd2 EQ it_stcd2-cnpj2
       AND land1 EQ 'BR'.
  ENDIF.

  CLEAR: it_stcd2[], it_stcd1[].

  SORT : t_lfa1 BY  stcd1 stcd3,
         t_lfa2 BY  stcd2 stcd3,
         t_kna1 BY  stcd1 stcd3,
         t_kna2 BY  stcd2 stcd3.


  LOOP AT it_info_notas INTO wa_info_notas.

    IF wa_info_notas-docnum EQ '0000000000'.
      CLEAR wa_info_notas-docnum.
    ENDIF.

    IF ( wa_info_notas-docnum IS NOT INITIAL ).

      UPDATE zib_nfe_forn
         SET st_nota      = wa_info_notas-st_nota
             nu_code      = wa_info_notas-nu_code
             nu_protocolo = wa_info_notas-nu_protocolo
             dt_protocolo = wa_info_notas-dt_protocolo
             hr_protocolo = wa_info_notas-hr_protocolo
       WHERE nu_chave_numero EQ wa_info_notas-nu_chave_numero
         AND nu_chave_serie  EQ wa_info_notas-nu_chave_serie
         AND nu_chave_cnpj   EQ wa_info_notas-nu_chave_cnpj
         AND nu_chave_modelo EQ wa_info_notas-nu_chave_modelo
         AND st_nota         NE wa_info_notas-st_nota
         AND nu_code         NE wa_info_notas-nu_code
         AND nu_protocolo    NE wa_info_notas-nu_protocolo
         AND hr_protocolo    NE wa_info_notas-hr_protocolo.

      IF ( sy-subrc EQ 0 ).
        vl_gravou := 'S'.
      ENDIF.

    ELSE.
      SELECT SINGLE * INTO wa_zib_nfe
        FROM zib_nfe_forn
       WHERE nu_chave_numero EQ wa_info_notas-nu_chave_numero
         AND nu_chave_serie  EQ wa_info_notas-nu_chave_serie
         AND nu_chave_cnpj   EQ wa_info_notas-nu_chave_cnpj
         AND nu_chave_modelo EQ wa_info_notas-nu_chave_modelo.

      IF sy-subrc IS NOT INITIAL.
        MODIFY zib_nfe_forn FROM wa_info_notas.
        vl_gravou := 'S'.
      ENDIF.

      CLEAR: wa_lfa1,
             wa_kna1,
             wa_doc,
             wa_parid,
             vl_nu_chave_serie,
             vl_nr_nf,
             vl_cancel,
             it_parid[],
             it_parid_filter[],
             rg_nr_nf[],
             rg_serie[],
             rg_nr_nf,
             rg_serie.

      REFRESH: t_doc,
               t_acttab.

      CASE wa_info_notas-st_nota.
        WHEN '2' OR '3'.
          vl_cancel = 'X'.
      ENDCASE.

      IF wa_info_notas-nu_chave_regiao EQ '13'.
        "Fornecedor
        LOOP AT t_lfa1 INTO wa_lfa1 WHERE stcd1 =  wa_info_notas-nu_chave_cnpj.
          SHIFT wa_lfa1-stcd3 LEFT DELETING LEADING '0'.
          SHIFT wa_info_notas-nu_ie LEFT DELETING LEADING '0'.
          IF wa_lfa1-stcd3 = wa_info_notas-nu_ie.
            wa_parid-sign   = 'I'.
            wa_parid-option = 'EQ'.
            wa_parid-low    = wa_lfa1-lifnr.
            wa_parid-high   = wa_lfa1-lifnr.
            APPEND wa_parid TO it_parid.
            APPEND VALUE #( parid = wa_lfa1-lifnr partyp = 'V' ) TO it_parid_filter.
            EXIT.
          ENDIF.
        ENDLOOP.
        LOOP AT t_lfa2 INTO wa_lfa1 WHERE stcd2 =  wa_info_notas-nu_chave_cnpj.
          SHIFT wa_lfa1-stcd3 LEFT DELETING LEADING '0'.
          SHIFT wa_info_notas-nu_ie LEFT DELETING LEADING '0'.
          IF wa_lfa1-stcd3 = wa_info_notas-nu_ie.
            wa_parid-sign   = 'I'.
            wa_parid-option = 'EQ'.
            wa_parid-low    = wa_lfa1-lifnr.
            wa_parid-high   = wa_lfa1-lifnr.
            APPEND wa_parid TO it_parid.
            APPEND VALUE #( parid = wa_lfa1-lifnr partyp = 'V' ) TO it_parid_filter.
            EXIT.
          ENDIF.
        ENDLOOP.

        "Cliente
        LOOP AT t_kna1 INTO wa_kna1 WHERE stcd1 =  wa_info_notas-nu_chave_cnpj.
          SHIFT wa_kna1-stcd3 LEFT DELETING LEADING '0'.
          SHIFT wa_info_notas-nu_ie LEFT DELETING LEADING '0'.
          IF wa_kna1-stcd3 = wa_info_notas-nu_ie.
            wa_parid-sign   = 'I'.
            wa_parid-option = 'EQ'.
            wa_parid-low    = wa_kna1-kunnr.
            wa_parid-high   = wa_kna1-kunnr.
            APPEND wa_parid TO it_parid.
            APPEND VALUE #( parid = wa_kna1-kunnr partyp = 'C' ) TO it_parid_filter.
            EXIT.
          ENDIF.
        ENDLOOP.
        LOOP AT t_kna2 INTO wa_kna1 WHERE stcd2 =  wa_info_notas-nu_chave_cnpj.
          SHIFT wa_kna1-stcd3 LEFT DELETING LEADING '0'.
          SHIFT wa_info_notas-nu_ie LEFT DELETING LEADING '0'.
          IF wa_kna1-stcd3 = wa_info_notas-nu_ie.
            wa_parid-sign   = 'I'.
            wa_parid-option = 'EQ'.
            wa_parid-low    = wa_kna1-kunnr.
            wa_parid-high   = wa_kna1-kunnr.
            APPEND wa_parid TO it_parid.
            APPEND VALUE #( parid = wa_kna1-kunnr partyp = 'C' ) TO it_parid_filter.
            EXIT.
          ENDIF.
        ENDLOOP.

      ELSE.
        "Fornecedor
        LOOP AT t_lfa1 INTO wa_lfa1 WHERE stcd1 =  wa_info_notas-nu_chave_cnpj.
          wa_parid-sign   = 'I'.
          wa_parid-option = 'EQ'.
          wa_parid-low    = wa_lfa1-lifnr.
          wa_parid-high   = wa_lfa1-lifnr.
          APPEND wa_parid TO it_parid.
          APPEND VALUE #( parid = wa_lfa1-lifnr partyp = 'V' ) TO it_parid_filter.
        ENDLOOP.
        LOOP AT t_lfa2 INTO wa_lfa1 WHERE stcd2 =  wa_info_notas-nu_chave_cnpj.
          wa_parid-sign   = 'I'.
          wa_parid-option = 'EQ'.
          wa_parid-low    = wa_lfa1-lifnr.
          wa_parid-high   = wa_lfa1-lifnr.
          APPEND wa_parid TO it_parid.
          APPEND VALUE #( parid = wa_lfa1-lifnr partyp = 'V' ) TO it_parid_filter.
        ENDLOOP.
        "Cliente
        LOOP AT t_kna1 INTO wa_kna1 WHERE stcd1 =  wa_info_notas-nu_chave_cnpj.
          wa_parid-sign   = 'I'.
          wa_parid-option = 'EQ'.
          wa_parid-low    = wa_kna1-kunnr.
          wa_parid-high   = wa_kna1-kunnr.
          APPEND wa_parid TO it_parid.
          APPEND VALUE #( parid = wa_kna1-kunnr partyp = 'C' ) TO it_parid_filter.
        ENDLOOP.
        LOOP AT t_kna2 INTO wa_kna1 WHERE stcd2 =  wa_info_notas-nu_chave_cnpj.
          wa_parid-sign   = 'I'.
          wa_parid-option = 'EQ'.
          wa_parid-low    = wa_kna1-kunnr.
          wa_parid-high   = wa_kna1-kunnr.
          APPEND wa_parid TO it_parid.
          APPEND VALUE #( parid = wa_kna1-kunnr partyp = 'C' ) TO it_parid_filter.
        ENDLOOP.

      ENDIF.

      IF (
           ( ( wa_info_notas-nu_chave_serie GE '890' ) AND ( wa_info_notas-nu_chave_serie LE '899' ) ) OR
           ( ( wa_info_notas-nu_chave_serie GE '900' ) AND ( wa_info_notas-nu_chave_serie LE '999' ) )
         )

         AND wa_info_notas-nu_ie IS NOT INITIAL.

        vg_stcd3 = wa_info_notas-nu_ie.

        TRY.
            CLEAR: wa_parid.
            wa_parid-sign   = 'I'.
            wa_parid-option = 'EQ'.
            zcl_fornecedores=>zif_parceiros~get_instance(
            )->set_parceiro_ie( i_insc_estatual = vg_stcd3
            )->get_id_parceiro( IMPORTING e_parceiro = wa_parid-low ).
            IF wa_parid-low IS NOT INITIAL.
              APPEND wa_parid TO it_parid.
              APPEND VALUE #( parid = wa_parid-low partyp = 'V' ) TO it_parid_filter.
            ENDIF.
          CATCH zcx_parceiros INTO DATA(ex_parceiros_k).
        ENDTRY.

        TRY.
            CLEAR: wa_parid.
            wa_parid-sign   = 'I'.
            wa_parid-option = 'EQ'.
            zcl_clientes=>zif_parceiros~get_instance(
            )->set_parceiro_ie( i_insc_estatual = vg_stcd3
            )->get_id_parceiro( IMPORTING e_parceiro = wa_parid-low ).
            IF wa_parid-low IS NOT INITIAL.
              APPEND wa_parid TO it_parid.
              APPEND VALUE #( parid = wa_parid-low partyp = 'C' ) TO it_parid_filter.
            ENDIF.
          CATCH zcx_parceiros INTO DATA(ex_parceiros_d).
        ENDTRY.

*        CLEAR: VG_STCD3_PSQ.
*        VG_STCD3 = WA_INFO_NOTAS-NU_IE.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            INPUT  = VG_STCD3
*          IMPORTING
*            OUTPUT = VG_STCD3_PSQ.
*
*        CONCATENATE '%' VG_STCD3_PSQ INTO VG_STCD3_PSQ.
*
*        SELECT SINGLE *
*          FROM LFA1 INTO @DATA(_WL_LFA1)
*         WHERE STCD3 LIKE @VG_STCD3_PSQ.
*
*        IF SY-SUBRC = 0.
*          WA_PARID-SIGN   = 'I'.
*          WA_PARID-OPTION = 'EQ'.
*          WA_PARID-LOW    = _WL_LFA1-LIFNR.
*          WA_PARID-HIGH   = _WL_LFA1-LIFNR.
*          APPEND WA_PARID TO IT_PARID.
*        ENDIF.
*
*        SELECT SINGLE *
*          FROM KNA1 INTO @DATA(_WL_KNA1)
*         WHERE STCD3 LIKE @VG_STCD3_PSQ.
*
*        IF SY-SUBRC = 0.
*          WA_PARID-SIGN   = 'I'.
*          WA_PARID-OPTION = 'EQ'.
*          WA_PARID-LOW    = _WL_KNA1-KUNNR.
*          WA_PARID-HIGH   = _WL_KNA1-KUNNR.
*          APPEND WA_PARID TO IT_PARID.
*        ENDIF.
      ENDIF.

      IF NOT it_parid[] IS INITIAL.

        vl_cancel = c_x.

        CLEAR: vdoctyp, wdoctyp.

        wdoctyp-sign   = 'I'.
        wdoctyp-option = 'EQ'.

        IF wa_info_notas-nu_chave_modelo EQ '55'.
          wdoctyp-low    = '1'.
          wdoctyp-high   = '1'.
          APPEND wdoctyp TO vdoctyp.
          wdoctyp-low    = '2'.
          wdoctyp-high   = '2'.
          APPEND wdoctyp TO vdoctyp.
          wdoctyp-low    = '6'.
          wdoctyp-high   = '6'.
          APPEND wdoctyp TO vdoctyp.
        ELSEIF ( wa_info_notas-nu_chave_modelo EQ '57' ) OR ( wa_info_notas-nu_chave_modelo EQ '67'  ).
          wdoctyp-low    = '1'.
          wdoctyp-high   = '1'.
          APPEND wdoctyp TO vdoctyp.
          wdoctyp-low    = '2'.
          wdoctyp-high   = '2'.
          APPEND wdoctyp TO vdoctyp.
          wdoctyp-low    = '4'.
          wdoctyp-high   = '4'.
          APPEND wdoctyp TO vdoctyp.
        ELSE.
          CONTINUE.
        ENDIF.

        "---------------------Nr Nota
        rg_nr_nf-sign   = 'I'.
        rg_nr_nf-option = 'EQ'.
        rg_nr_nf-low    = wa_info_notas-nu_chave_numero.
        rg_nr_nf-high   = wa_info_notas-nu_chave_numero.
        APPEND rg_nr_nf.

        rg_nr_nf-sign   = 'I'.
        rg_nr_nf-option = 'EQ'.
        rg_nr_nf-low    = wa_info_notas-nu_chave_numero.
        rg_nr_nf-high   = wa_info_notas-nu_chave_numero.
        SHIFT rg_nr_nf-low  LEFT DELETING LEADING '0'.
        SHIFT rg_nr_nf-high LEFT DELETING LEADING '0'.
        APPEND rg_nr_nf.

        rg_nr_nf-sign   = 'I'.
        rg_nr_nf-option = 'EQ'.
        rg_nr_nf-low    = wa_info_notas-nu_chave_numero.
        rg_nr_nf-high   = wa_info_notas-nu_chave_numero.
        SHIFT rg_nr_nf-low  LEFT DELETING LEADING space.
        SHIFT rg_nr_nf-high LEFT DELETING LEADING space.
        APPEND rg_nr_nf.
        "---------------------
        "---------------------Serie
        rg_serie-sign   = 'I'.
        rg_serie-option = 'EQ'.
        rg_serie-low    = wa_info_notas-nu_chave_serie.
        rg_serie-high   = wa_info_notas-nu_chave_serie.
        APPEND rg_serie.

        rg_serie-sign   = 'I'.
        rg_serie-option = 'EQ'.
        rg_serie-low    = wa_info_notas-nu_chave_serie+2(1).
        rg_serie-high   = wa_info_notas-nu_chave_serie+2(1).
        APPEND rg_serie.

        IF strlen( wa_info_notas-nu_chave_serie ) = 3.
          TRY.
              vl_serie_int_aux = wa_info_notas-nu_chave_serie.
              vl_serie_srt_aux = vl_serie_int_aux.
              CONDENSE vl_serie_srt_aux NO-GAPS.
              IF strlen( vl_serie_srt_aux ) <> strlen( wa_info_notas-nu_chave_serie ).
                rg_serie-sign   = 'I'.
                rg_serie-option = 'EQ'.
                rg_serie-low    = wa_info_notas-nu_chave_serie+1(2).
                rg_serie-high   = wa_info_notas-nu_chave_serie+1(2).
                APPEND rg_serie.
              ENDIF.
            CATCH cx_sy_conversion_no_number.
          ENDTRY.
        ENDIF.

        rg_serie-sign   = 'I'.
        rg_serie-option = 'EQ'.
        rg_serie-low    = wa_info_notas-nu_chave_serie.
        rg_serie-high   = wa_info_notas-nu_chave_serie.
        SHIFT rg_serie-low  LEFT DELETING LEADING '0'.
        SHIFT rg_serie-high LEFT DELETING LEADING '0'.
        APPEND rg_serie.

        rg_serie-sign   = 'I'.
        rg_serie-option = 'EQ'.
        rg_serie-low    = wa_info_notas-nu_chave_serie.
        rg_serie-high   = wa_info_notas-nu_chave_serie.
        SHIFT rg_serie-low  LEFT DELETING LEADING space.
        SHIFT rg_serie-high LEFT DELETING LEADING space.
        APPEND rg_serie.
        "---------------------

        SELECT * INTO TABLE t_doc"WA_DOC
          FROM j_1bnfdoc
         WHERE nfe     EQ c_x
           AND direct  EQ '1'
           AND doctyp  IN vdoctyp
           AND model   EQ wa_info_notas-nu_chave_modelo
           AND nfenum  IN rg_nr_nf"EQ WA_INFO_NOTAS-NU_CHAVE_NUMERO
           AND series  IN rg_serie"WA_INFO_NOTAS-NU_CHAVE_SERIE
           AND parid   IN it_parid
           AND form    EQ vl_form
           AND cancel  NE vl_cancel.

        t_doc_aux[] = t_doc[].
        LOOP AT t_doc_aux INTO DATA(wl_doc_ck).
          READ TABLE it_parid_filter WITH KEY parid  = wl_doc_ck-parid
                                              partyp = wl_doc_ck-partyp.
          IF sy-subrc NE 0.
            DELETE t_doc WHERE docnum EQ wl_doc_ck-docnum.
          ENDIF.
        ENDLOOP.

        IF t_doc[] IS INITIAL.
          rg_serie-sign   = 'I'.
          rg_serie-option = 'EQ'.
          rg_serie-low    = wa_info_notas-nu_chave_serie+2(1).
          rg_serie-high   = wa_info_notas-nu_chave_serie+2(1).
          APPEND rg_serie.

          rg_serie-sign   = 'I'.
          rg_serie-option = 'EQ'.
          rg_serie-low    = wa_info_notas-nu_chave_serie.
          rg_serie-high   = wa_info_notas-nu_chave_serie.
          SHIFT rg_serie-low  LEFT DELETING LEADING '0'.
          SHIFT rg_serie-high LEFT DELETING LEADING '0'.
          APPEND rg_serie.

          rg_serie-sign   = 'I'.
          rg_serie-option = 'EQ'.
          rg_serie-low    = wa_info_notas-nu_chave_serie.
          rg_serie-high   = wa_info_notas-nu_chave_serie.
          SHIFT rg_serie-low  LEFT DELETING LEADING space.
          SHIFT rg_serie-high LEFT DELETING LEADING space.
          APPEND rg_serie.
          "---------------------

          SELECT * INTO TABLE t_doc" WA_DOC
            FROM j_1bnfdoc
           WHERE nfe     EQ c_x
             AND direct  EQ '1'
             AND doctyp  IN vdoctyp
             AND model   EQ wa_info_notas-nu_chave_modelo
             AND nfenum  IN rg_nr_nf"EQ WA_INFO_NOTAS-NU_CHAVE_NUMERO
             AND series  IN rg_serie"WA_INFO_NOTAS-NU_CHAVE_SERIE
             AND parid   IN it_parid
             AND form    EQ vl_form
             AND cancel  NE vl_cancel.

          t_doc_aux[] = t_doc[].
          LOOP AT t_doc_aux INTO wl_doc_ck.
            READ TABLE it_parid_filter WITH KEY parid  = wl_doc_ck-parid
                                                partyp = wl_doc_ck-partyp.
            IF sy-subrc NE 0.
              DELETE t_doc WHERE docnum EQ wl_doc_ck-docnum.
            ENDIF.
          ENDLOOP.

        ENDIF.

*        IF NOT SY-SUBRC IS INITIAL.
*
*          MOVE  WA_INFO_NOTAS-NU_CHAVE_SERIE TO VG_SERIE.
*          WRITE VG_SERIE TO VL_NU_CHAVE_SERIE.
*          SHIFT VL_NU_CHAVE_SERIE LEFT DELETING LEADING SPACE.
*
*          SHIFT WA_INFO_NOTAS-NU_CHAVE_NUMERO LEFT DELETING LEADING SPACE.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              INPUT  = WA_INFO_NOTAS-NU_CHAVE_NUMERO
*            IMPORTING
*              OUTPUT = VL_NR_NF.
*
*          SELECT SINGLE * INTO WA_DOC
*            FROM J_1BNFDOC
*           WHERE NFE    EQ C_X
*             AND DIRECT EQ '1'
*             AND DOCTYP  IN VDOCTYP
*             AND MODEL  EQ WA_INFO_NOTAS-NU_CHAVE_MODELO
*             AND NFENUM EQ VL_NR_NF"WA_INFO_NOTAS-NU_CHAVE_NUMERO
*             AND SERIES EQ VL_NU_CHAVE_SERIE
*             AND PARID  IN IT_PARID
*             AND FORM   EQ VL_FORM
*             AND CANCEL NE VL_CANCEL.
*
**          IF NOT SY-SUBRC IS INITIAL.
**            MOVE  WA_INFO_NOTAS-NU_CHAVE_NUMERO TO VG_NUMERO.
**            MOVE VG_NUMERO TO VNU_CHAVE_NUMERO.
**            SHIFT VNU_CHAVE_NUMERO LEFT DELETING LEADING SPACE.
**
**            SELECT SINGLE * INTO WA_DOC
**              FROM J_1BNFDOC
**             WHERE NFE    EQ C_X
**               AND DIRECT EQ '1'
**               AND DOCTYP IN VDOCTYP
**               AND MODEL  EQ WA_INFO_NOTAS-NU_CHAVE_MODELO
**               AND NFENUM EQ VNU_CHAVE_NUMERO
**               AND SERIES EQ VL_NU_CHAVE_SERIE
**               AND PARID  IN IT_PARID
**               AND FORM   EQ VL_FORM
**               AND CANCEL NE VL_CANCEL.
**          ENDIF.
*
**          IF ( NOT SY-SUBRC IS INITIAL ) AND ( 1 = 2 ).
**
**            SELECT SINGLE * INTO WA_ACTTAB
**              FROM J_1BNFE_ACTIVE
**             WHERE MODEL   EQ WA_INFO_NOTAS-NU_CHAVE_MODELO
**               AND NFNUM9  EQ WA_INFO_NOTAS-NU_CHAVE_NUMERO
**               AND SERIE   EQ WA_INFO_NOTAS-NU_CHAVE_SERIE
**               AND STCD1   EQ WA_INFO_NOTAS-NU_CHAVE_CNPJ
**               AND CANCEL  NE VL_CANCEL.
**
**            IF SY-SUBRC IS INITIAL.
**
**              SELECT SINGLE * INTO WA_DOC
**                FROM J_1BNFDOC
**               WHERE NFE    EQ C_X
**                 AND DIRECT EQ '1'
**                 AND DOCTYP IN VDOCTYP
**                 AND MODEL  EQ WA_INFO_NOTAS-NU_CHAVE_MODELO
**                 AND DOCNUM EQ WA_ACTTAB-DOCNUM
**                 AND PARID  IN IT_PARID
**                 AND FORM   EQ VL_FORM
**                 AND CANCEL NE VL_CANCEL.
**
**              CLEAR: WA_ACTTAB.
**
**            ENDIF.
**          ENDIF.
*        ENDIF.

        IF t_doc IS INITIAL."WA_DOC IS INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_lfa1-lifnr
            IMPORTING
              output = wa_lfa1-lifnr.

          IF strlen( wa_lfa1-lifnr ) LE 4.

            MOVE wa_lfa1-lifnr TO vg_filial.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = vg_filial
              IMPORTING
                output = vg_filial.

            SELECT SINGLE * INTO wa_filial
              FROM j_1bbranch
              WHERE branch EQ vg_filial.
          ELSE.
            sy-subrc = 4.
          ENDIF.

          IF ( strlen( wa_lfa1-lifnr ) LE 4 ) AND ( sy-subrc IS INITIAL ).

*            CLEAR : RG_PARID[], RG_PARID.
*
*            RG_PARID-SIGN    = 'I'.
*            RG_PARID-OPTION  = 'CP'.
*            CONCATENATE WA_FILIAL-BUKRS WA_FILIAL-BRANCH INTO RG_PARID-LOW.
*
*            SHIFT RG_PARID-LOW LEFT DELETING LEADING '0'.
*
*            CONCATENATE '*' RG_PARID-LOW INTO RG_PARID-LOW.
*
*            APPEND RG_PARID.
*
*            SELECT SINGLE * INTO WA_DOC
*              FROM J_1BNFDOC
*             WHERE NFE     EQ C_X
*               AND DIRECT  EQ '1'
*               AND DOCTYP  IN VDOCTYP
*               AND MODEL   EQ WA_INFO_NOTAS-NU_CHAVE_MODELO
*               AND NFENUM  IN RG_NR_NF"WA_INFO_NOTAS-NU_CHAVE_NUMERO
*               AND SERIES  EQ RG_SERIE"VL_NU_CHAVE_SERIE"WA_INFO_NOTAS-NU_CHAVE_SERIE
*               AND PARTYP  EQ 'B'
*               AND PARID   IN RG_PARID"EQ WA_LFA1-LIFNR
*               AND FORM    EQ VL_FORM
*               AND CANCEL  NE VL_CANCEL.

            CLEAR : wa_acttab.

            SELECT * INTO TABLE t_acttab"WA_ACTTAB
              FROM j_1bnfe_active
             WHERE model   EQ wa_info_notas-nu_chave_modelo
               AND nfnum9  EQ wa_info_notas-nu_chave_numero
               AND serie   EQ wa_info_notas-nu_chave_serie
               AND stcd1   EQ wa_info_notas-nu_chave_cnpj
               AND direct  EQ '1'
               AND partyp  EQ 'B'
              " AND CANCEL  NE VL_CANCEL
              .

            IF sy-subrc IS INITIAL.

              SELECT * INTO TABLE t_doc "WA_DOC
                FROM j_1bnfdoc
                FOR ALL ENTRIES IN t_acttab
               WHERE docnum EQ t_acttab-docnum.

            ENDIF.


            "            IF NOT SY-SUBRC IS INITIAL.
*
*              SELECT SINGLE * INTO WA_DOC
*                FROM J_1BNFDOC
*               WHERE NFE    EQ C_X
*                 AND DIRECT EQ '1'
*                 AND DOCTYP  IN VDOCTYP
*                 AND MODEL  EQ WA_INFO_NOTAS-NU_CHAVE_MODELO
*                 AND NFENUM EQ WA_INFO_NOTAS-NU_CHAVE_NUMERO
*                 AND SERIES EQ VL_NU_CHAVE_SERIE
*                 AND PARTYP EQ 'B'
*                 AND PARID  EQ WA_LFA1-LIFNR
*                 AND FORM   EQ VL_FORM
*                 AND CANCEL NE VL_CANCEL.
            "              IF NOT SY-SUBRC IS INITIAL.
*                MOVE  WA_INFO_NOTAS-NU_CHAVE_NUMERO TO VG_NUMERO.
*                MOVE VG_NUMERO TO VNU_CHAVE_NUMERO.
*                SHIFT VNU_CHAVE_NUMERO LEFT DELETING LEADING SPACE.
*
*                SELECT SINGLE * INTO WA_DOC
*                  FROM J_1BNFDOC
*                 WHERE NFE    EQ C_X
*                   AND DIRECT EQ '1'
*                   AND DOCTYP IN VDOCTYP
*                   AND MODEL  EQ WA_INFO_NOTAS-NU_CHAVE_MODELO
*                   AND NFENUM EQ VNU_CHAVE_NUMERO
*                   AND SERIES EQ VL_NU_CHAVE_SERIE
*                   AND PARTYP EQ 'B'
*                   AND PARID  EQ WA_LFA1-LIFNR
*                   AND FORM   EQ VL_FORM
*                   AND CANCEL NE VL_CANCEL.
            "              ENDIF.
*              IF ( NOT SY-SUBRC IS INITIAL ) AND ( 1 = 2 ).
*
*                SELECT SINGLE * INTO WA_ACTTAB
*                  FROM J_1BNFE_ACTIVE
*                 WHERE MODEL   EQ WA_INFO_NOTAS-NU_CHAVE_MODELO
*                   AND NFNUM9  EQ WA_INFO_NOTAS-NU_CHAVE_NUMERO
*                   AND SERIE   EQ WA_INFO_NOTAS-NU_CHAVE_SERIE
*                   AND STCD1   EQ WA_INFO_NOTAS-NU_CHAVE_CNPJ
*                   AND CANCEL  NE VL_CANCEL.
*
*                IF SY-SUBRC IS INITIAL.
*
*                  SELECT SINGLE * INTO WA_DOC
*                    FROM J_1BNFDOC
*                   WHERE NFE    EQ C_X
*                     AND DIRECT EQ '1'
*                     AND DOCTYP IN VDOCTYP
*                     AND MODEL  EQ WA_INFO_NOTAS-NU_CHAVE_MODELO
*                     AND DOCNUM EQ WA_ACTTAB-DOCNUM
*                     AND PARTYP EQ 'B'
*                     AND PARID  EQ WA_LFA1-LIFNR
*                     AND FORM   EQ VL_FORM
*                     AND CANCEL NE VL_CANCEL.
*
*                  CLEAR: WA_ACTTAB.
*
*                ENDIF.
*              ENDIF.
            "            ENDIF.
          ENDIF.
        ENDIF.

        "IF NOT WA_DOC IS INITIAL.
        IF NOT t_doc IS INITIAL.
          SORT t_doc BY cancel DESCENDING.
          LOOP AT t_doc INTO wa_doc .
            CLEAR: wa_acttab, wa_ib_nota.

            CALL FUNCTION 'J_1B_NFE_XML_RAED_ACTIVE_TAB'
              EXPORTING
                i_docnum = wa_doc-docnum
              IMPORTING
                e_acttab = wa_acttab
              EXCEPTIONS
                no_entry = 1
                OTHERS   = 2.

            IF NOT wa_acttab IS INITIAL.

              wa_ib_nota-nu_documento_sap = wa_doc-docnum.
              wa_info_notas-docnum        = wa_doc-docnum.
              wa_ib_nota-xmlvers          = wa_info_notas-xmlvers.
              wa_ib_nota-tp_authcod       = wa_info_notas-st_nota.
              wa_ib_nota-regiao           = wa_info_notas-nu_chave_regiao.
              wa_ib_nota-authcod          = wa_info_notas-nu_protocolo.
              wa_ib_nota-dt_authcod       = wa_info_notas-dt_protocolo.
              wa_ib_nota-vlr_nota         = wa_info_notas-vlr_nota.

              IF wa_ib_nota-tp_authcod EQ 1.
                wa_ib_nota-code       = '100'.
              ELSE.
                wa_ib_nota-code       = '101'.
              ENDIF.
              wa_ib_nota-docnum9    = wa_info_notas-nu_chave_aleator.
              wa_ib_nota-docstat    = '1'.
              wa_ib_nota-cdv        = wa_info_notas-nu_chave_dv.

              MODIFY zib_nfe_forn FROM wa_info_notas.
              vl_gravou = abap_true.

              PERFORM altera_status USING wa_ib_nota wa_acttab wa_doc.

              "Check se Active foi alterada corretamente
              SELECT SINGLE *
                FROM j_1bnfe_active INTO @DATA(_wl_active)
               WHERE docnum = @wa_doc-docnum.

              DATA(_active_error) = ''.
              IF sy-subrc NE 0.
                _active_error = 'X'.
              ELSE.
                CONCATENATE _wl_active-regio
                            _wl_active-nfyear
                            _wl_active-nfmonth
                            _wl_active-stcd1
                            _wl_active-model
                            _wl_active-serie
                            _wl_active-nfnum9
                            _wl_active-docnum9
                            _wl_active-cdv INTO DATA(_chave_nfe_tmp).

                CONDENSE _chave_nfe_tmp NO-GAPS.

                IF ( wa_info_notas-nu_chave <> _chave_nfe_tmp ) OR ( strlen( _chave_nfe_tmp ) <> 44 ).
                  _active_error = 'X'.
                ENDIF.

              ENDIF.

              CHECK _active_error IS INITIAL.

              wa_info_notas-atualizado = abap_true.

              wa_info_notas-id_identificador = 4.
              wa_info_notas-user_identificador = sy-uname.
              wa_info_notas-data_identificador = sy-datum.
              wa_info_notas-hora_identificador = sy-uzeit.

              MODIFY zib_nfe_forn FROM wa_info_notas.

              IF NOT wa_info_notas-docnum IS INITIAL.

                SELECT SINGLE * INTO zcte_info_nota
                  FROM zcte_info_nota
                 WHERE docnum_nf = wa_info_notas-docnum.
                IF sy-subrc IS INITIAL.
                  zcte_info_nota-docnum9 = wa_info_notas-nu_chave_aleator.
                  zcte_info_nota-cdv     = wa_info_notas-nu_chave_dv.
                  CONCATENATE wa_acttab-regio
                              wa_acttab-nfyear
                              wa_acttab-nfmonth
                              wa_acttab-stcd1
                              wa_acttab-model
                              wa_acttab-serie
                              wa_acttab-nfnum9
                              wa_acttab-docnum9
                              wa_acttab-cdv      INTO zcte_info_nota-chave.
                  MODIFY zcte_info_nota.
                ENDIF.
              ENDIF.
            ELSE.
              MODIFY zib_nfe_forn FROM wa_info_notas.
            ENDIF.
          ENDLOOP.
        ELSE.
          MODIFY zib_nfe_forn FROM wa_info_notas.
        ENDIF.
        "ELSE.
        "  MODIFY ZIB_NFE_FORN FROM WA_INFO_NOTAS.
      ENDIF.

      COMMIT WORK.

    ENDIF.
  ENDLOOP.

  IF vl_gravou = 'S'.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFUNCTION.
