FUNCTION zromaneio_vinculado_nf.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM
*"     REFERENCE(I_CK_VINC_ZMEMO00) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_CK_CCT_CP) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_CK_CFOP_E_ZMEMO00) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_ZSDT0001_RO_VINC) TYPE  ZSDT0001_RO_VINC_T
*"     REFERENCE(E_ROMANEIO_COMPLETO) TYPE  CHAR01
*"     REFERENCE(E_CCT_CP) TYPE  CHAR01
*"     REFERENCE(E_ZLEST0146_CP) TYPE  ZLEST0146
*"----------------------------------------------------------------------

  RANGES: r_nr_nf            FOR j_1bnfdoc-nfenum,
          r_serie            FOR j_1bnfdoc-series,
          r_cfop_s           FOR j_1bnflin-cfop,
          r_cfop_aux           FOR j_1bnflin-cfop.

  DATA: wl_zsdt0001_ro_vinc TYPE zsdt0001_ro_vinc,
        wl_zsdt0001_e       TYPE zsdt0001,
        wl_zsdt0001_s       TYPE zsdt0001,
        tg_zsdt0001_e       TYPE TABLE OF zsdt0001 WITH HEADER LINE,
        it_cfops            TYPE TABLE OF lxhme_range_c10,
        "TG_LIN_E            TYPE TABLE OF J_1BNFLIN WITH HEADER LINE,
        tg_doc_rom_e_tmp    TYPE TABLE OF j_1bnfdoc WITH HEADER LINE,
        v_ch_referencia     TYPE zsdt0001-ch_referencia,
        v_id_referencia_rom TYPE zsdt0001-id_referencia,
        v_nr_romaneio       TYPE zsdt0001-nr_romaneio,
        v_vbeln             TYPE zdoc_rem,
        wl_znom_rem_nf_s_c  TYPE znom_reme_notas,
        wl_znom_rem_nf_f    TYPE znom_reme_notas,
        v_candat            TYPE j_1bnfdoc-candat,
        v_nf9               TYPE c LENGTH 9,
        v_nf6               TYPE c LENGTH 6,
        v_nfnum_rom         TYPE zsdt0001-nfnum,
        v_docnum_rom_e      TYPE j_1bnfdoc-docnum,
        wl_rfl_dt_corte     TYPE j_1bnfdoc-docdat,
        wl_zlest0146        TYPE zlest0146,
        lt_zlest0147        TYPE zlest0147_t,
        lt_zlest0168        TYPE zlest0168_t,
        v_doc_rateio        TYPE char01,
        v_refkey            TYPE j_1bnflin-refkey.

  RANGES: lra_referencia_rom FOR zsdt0001-nr_romaneio.

  CLEAR: e_zsdt0001_ro_vinc[], wl_rfl_dt_corte, wl_znom_rem_nf_s_c, wl_znom_rem_nf_f, e_romaneio_completo, e_cct_cp, e_zlest0146_cp, r_cfop_s[].

  IF i_ck_cfop_e_zmemo00 EQ abap_true.

    zcl_im_cl_fluxo_exportacao=>get_cfop(
      EXPORTING
        i_formacao_lote   = abap_true
        i_comercializacao = abap_true
        i_fim_especifico  = abap_true
      RECEIVING
        r_cfop            = it_cfops
    ).

    CHECK it_cfops[] IS NOT INITIAL.

    "Somente CFOP RFL
    r_cfop_s-sign   = 'I'.
    r_cfop_s-option = 'EQ'.
    r_cfop_s-low    = '5505AA'. APPEND r_cfop_s.
    r_cfop_s-low    = '5505AB'. APPEND r_cfop_s.
    r_cfop_s-low    = '6505AA'. APPEND r_cfop_s.
    r_cfop_s-low    = '6505AB'. APPEND r_cfop_s.

  ENDIF.

  CHECK i_docnum IS NOT INITIAL.

  SELECT SINGLE *
    FROM j_1bnfdoc INTO @DATA(_wl_j_1bnfdoc)
   WHERE docnum EQ @i_docnum
     AND candat EQ @v_candat
     AND cancel EQ @space.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM setleaf INTO @DATA(_wl_setleaf)
   WHERE setname = 'DUE_NF_EMISSAO_CORTE'.

  IF ( sy-subrc EQ 0 ) AND ( _wl_setleaf-valfrom IS NOT INITIAL ).
    wl_rfl_dt_corte = _wl_setleaf-valfrom.
  ENDIF.

  CASE _wl_j_1bnfdoc-direct.
    WHEN '1'. "Entrada

      IF i_ck_vinc_zmemo00 EQ abap_true.
        SELECT SINGLE *
          FROM znom_reme_notas INTO wl_znom_rem_nf_s_c
         WHERE docnum    EQ i_docnum
           AND tp_nf_rem IN ( 'S', 'C' ).

        SELECT SINGLE *
          FROM znom_reme_notas INTO wl_znom_rem_nf_f
         WHERE docnum    EQ i_docnum
           AND tp_nf_rem EQ 'F'.

        "Se utilizou NF na opção "Sem Vinc.Form Lote" primeiro que na opção com "Vinculo Form. Lote", desconsiderar vinculo.
        IF ( wl_znom_rem_nf_s_c IS NOT INITIAL ) AND ( wl_znom_rem_nf_f IS INITIAL ).
          RETURN.
        ENDIF.
      ENDIF.

*---------------------------------------------------------------------------------------*
*     Check CFOP Entrada
*---------------------------------------------------------------------------------------*
      SELECT SINGLE *
        FROM j_1bnflin INTO @DATA(_wl_lin_e)
       WHERE docnum EQ @_wl_j_1bnfdoc-docnum
         AND cfop   IN @it_cfops.

      CHECK sy-subrc EQ 0.

*---------------------------------------------------------------------------------------*
*     Buscar Romaneio de Entrada
*---------------------------------------------------------------------------------------*
      CLEAR: wl_zsdt0001_e, v_ch_referencia.

      SELECT SINGLE *
        FROM zmmt_ee_zgr_docs AS de INTO @DATA(_wl_ee_zgr_docs)
       WHERE docnum EQ @_wl_j_1bnfdoc-docnum.

      IF sy-subrc EQ 0.
        SELECT SINGLE *
          FROM zmmt_ee_zgr INTO @DATA(_wl_ee_zgr)
         WHERE obj_key EQ @_wl_ee_zgr_docs-obj_key.

        IF ( sy-subrc EQ 0 ) AND ( _wl_ee_zgr-ch_referencia IS NOT INITIAL ).
          v_ch_referencia = _wl_ee_zgr-ch_referencia.

          SELECT SINGLE *
            FROM zsdt0001 INTO wl_zsdt0001_e
           WHERE ch_referencia EQ v_ch_referencia.
        ENDIF.
      ENDIF.

      IF wl_zsdt0001_e IS INITIAL.

        CLEAR: v_nfnum_rom.
        IF _wl_j_1bnfdoc-nfenum IS NOT INITIAL.
          v_nfnum_rom  = _wl_j_1bnfdoc-nfenum.
        ELSEIF _wl_j_1bnfdoc-nfnum IS NOT INITIAL.
          v_nfnum_rom  = _wl_j_1bnfdoc-nfnum.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = v_nfnum_rom
          IMPORTING
            output = v_nfnum_rom.

        SELECT SINGLE *
          FROM zsdt0001 INTO wl_zsdt0001_e
         WHERE bukrs        EQ _wl_j_1bnfdoc-bukrs
           AND branch       EQ _wl_j_1bnfdoc-branch
           AND parid        EQ _wl_j_1bnfdoc-parid
           AND docdat       EQ _wl_j_1bnfdoc-docdat
           AND nfnum        EQ v_nfnum_rom
           AND tp_movimento EQ 'E'.
      ENDIF.

      CHECK wl_zsdt0001_e IS NOT INITIAL.

*---------------------------------------------------------------------------------------*
*     Buscar Romaneio Saida
*---------------------------------------------------------------------------------------*
      CLEAR: lra_referencia_rom[].

      v_id_referencia_rom = wl_zsdt0001_e-nr_romaneio.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = v_id_referencia_rom
        IMPORTING
          output = v_id_referencia_rom.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = v_id_referencia_rom ) TO lra_referencia_rom.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = v_id_referencia_rom
        IMPORTING
          output = v_id_referencia_rom.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = v_id_referencia_rom ) TO lra_referencia_rom.


      CLEAR: wl_zsdt0001_s.
      SELECT SINGLE *
        FROM zsdt0001 INTO wl_zsdt0001_s
       WHERE bukrs          EQ wl_zsdt0001_e-bukrs
         AND branch         EQ wl_zsdt0001_e-branch
         AND id_referencia  IN lra_referencia_rom
         AND tp_movimento   EQ 'S'
         AND nr_safra       EQ wl_zsdt0001_e-nr_safra.

      CHECK ( sy-subrc = 0 ).

      "Se peso Romaneio Saída for igual o Peso Romaneio Entrada
      CHECK ( wl_zsdt0001_s-peso_liq EQ wl_zsdt0001_e-peso_liq ).

      "IF ( WL_ZSDT0001_S-NRO_NF_PROD IS INITIAL ).
      IF wl_zsdt0001_s-doc_rem IS NOT INITIAL.

        CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
          EXPORTING
            i_vbeln  = wl_zsdt0001_s-doc_rem
            i_direct = '2' "Saída
          IMPORTING
            e_docnum = wl_zsdt0001_s-nro_nf_prod.

        IF wl_zsdt0001_s-nro_nf_prod IS INITIAL.
          CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
            EXPORTING
              i_vbeln  = wl_zsdt0001_s-doc_rem
              i_direct = '1' "Entrada
            IMPORTING
              e_docnum = wl_zsdt0001_s-nro_nf_prod.
        ENDIF.

      ELSEIF ( wl_zsdt0001_s-doc_material IS NOT INITIAL ) AND ( wl_zsdt0001_s-ano_material IS NOT INITIAL ).

        CONCATENATE wl_zsdt0001_s-doc_material wl_zsdt0001_s-ano_material INTO v_refkey.

        SELECT SINGLE *
          FROM j_1bnflin INTO @DATA(_wl_lin)
         WHERE refkey = @v_refkey.

        IF sy-subrc EQ 0.
          SELECT SINGLE *
            FROM j_1bnfe_active INTO @DATA(_wl_active)
           WHERE docnum     = @_wl_lin-docnum
             AND cancel     = @abap_false
             AND docsta     = '1'.

          IF sy-subrc EQ 0.
            wl_zsdt0001_s-nro_nf_prod = _wl_lin-docnum.
          ENDIF.
        ENDIF.

      ENDIF.

      CHECK wl_zsdt0001_s-nro_nf_prod IS NOT INITIAL.
      "ENDIF.

      SELECT SINGLE *
        FROM j_1bnfdoc INTO @DATA(_wl_doc_s)
       WHERE docnum EQ @wl_zsdt0001_s-nro_nf_prod
         AND candat EQ @v_candat
         AND cancel EQ @space.

      CHECK ( sy-subrc         EQ 0           ) AND
            ( _wl_doc_s-nfenum IS NOT INITIAL ).

      SELECT SINGLE *
        FROM j_1bnflin INTO @DATA(_wl_lin_s)
       WHERE docnum EQ @_wl_doc_s-docnum
         AND cfop   IN @r_cfop_s.

      CHECK sy-subrc EQ 0.

      IF  wl_rfl_dt_corte IS NOT INITIAL.
        CHECK _wl_doc_s-docdat >=  wl_rfl_dt_corte.
      ENDIF.

      CLEAR: wl_zsdt0001_ro_vinc.
      wl_zsdt0001_ro_vinc-ch_referencia    = wl_zsdt0001_e-ch_referencia.
      wl_zsdt0001_ro_vinc-ch_ref_vinc      = wl_zsdt0001_s-ch_referencia.
      wl_zsdt0001_ro_vinc-docnum_vinc      = wl_zsdt0001_s-nro_nf_prod.
      wl_zsdt0001_ro_vinc-nfenum_vinc      = _wl_doc_s-nfenum.
      wl_zsdt0001_ro_vinc-prop_vinc        = 100.
      wl_zsdt0001_ro_vinc-peso_liq         = wl_zsdt0001_s-peso_liq.
      wl_zsdt0001_ro_vinc-peso_fiscal      = wl_zsdt0001_s-peso_fiscal.
      wl_zsdt0001_ro_vinc-docnum           = _wl_j_1bnfdoc-docnum.
      APPEND wl_zsdt0001_ro_vinc TO e_zsdt0001_ro_vinc.

      e_romaneio_completo = abap_true.

      "Checar CCT Contrapartida
      IF ( i_ck_cct_cp EQ abap_true ) AND lines( e_zsdt0001_ro_vinc[] ) EQ 1.

        READ TABLE e_zsdt0001_ro_vinc INTO DATA(_wl_rom_vinc) INDEX 1.
        CHECK sy-subrc EQ 0.

        "Check se Documento está registrado no CCT
        CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
          EXPORTING
            i_docnum     = _wl_rom_vinc-docnum_vinc
          IMPORTING
            e_zlest0146  = wl_zlest0146
            e_doc_rateio = v_doc_rateio.

        IF ( wl_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).
          e_cct_cp       = abap_true.
          e_zlest0146_cp = wl_zlest0146.
        ENDIF.
      ENDIF.

    WHEN '2'. "Saída

*---------------------------------------------------------------------------------------*
*     Check CFOP Saida
*---------------------------------------------------------------------------------------*
      SELECT SINGLE *
        FROM j_1bnflin INTO _wl_lin_s
       WHERE docnum EQ _wl_j_1bnfdoc-docnum
         AND cfop   IN r_cfop_s.

      CHECK sy-subrc EQ 0.

*---------------------------------------------------------------------------------------*
*     Busca Romaneio de Saída Formação Lote
*---------------------------------------------------------------------------------------*

      IF  wl_rfl_dt_corte IS NOT INITIAL.
        CHECK _wl_j_1bnfdoc-docdat >=  wl_rfl_dt_corte.
      ENDIF.

      CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
        EXPORTING
          i_docnum = _wl_j_1bnfdoc-docnum
          i_direct = '2'
        IMPORTING
          e_vbeln  = v_vbeln.

      SELECT SINGLE *
        FROM zsdt0001 INTO wl_zsdt0001_s
       WHERE doc_rem       EQ v_vbeln
         AND tp_movimento  EQ 'S'.

      CHECK ( sy-subrc EQ 0 ) AND ( wl_zsdt0001_s-id_referencia IS NOT INITIAL ). "Romaneio Completo

      CLEAR: v_nr_romaneio.

      v_nr_romaneio = wl_zsdt0001_s-id_referencia.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = v_nr_romaneio
        IMPORTING
          output = v_nr_romaneio.

      "Busca Romaneio Entrada
      CLEAR: tg_zsdt0001_e[].
      SELECT *
        FROM zsdt0001 INTO TABLE tg_zsdt0001_e
       WHERE bukrs        EQ wl_zsdt0001_s-bukrs
         AND branch       EQ wl_zsdt0001_s-branch
         AND nr_romaneio  EQ v_nr_romaneio
         AND nr_safra     EQ wl_zsdt0001_s-nr_safra
         AND tp_movimento EQ 'E'.

      CHECK lines( tg_zsdt0001_e[] ) EQ 1.

      READ TABLE tg_zsdt0001_e INDEX 1.

      CHECK tg_zsdt0001_e-ch_referencia IS NOT INITIAL.

      "Se peso Romaneio Saída for igual o Peso Romaneio Entrada
      CHECK ( wl_zsdt0001_s-peso_liq EQ tg_zsdt0001_e-peso_liq ).

      CLEAR: v_docnum_rom_e.

      SELECT SINGLE *
        FROM zmmt_ee_zgr INTO _wl_ee_zgr
       WHERE ch_referencia EQ tg_zsdt0001_e-ch_referencia.

      IF sy-subrc EQ 0.

        SELECT SINGLE *
          FROM zmmt_ee_zgr_docs AS de INTO _wl_ee_zgr_docs
         WHERE obj_key EQ _wl_ee_zgr-obj_key
           AND EXISTS (  SELECT docnum
                           FROM j_1bnfdoc AS dc
                          WHERE dc~docnum EQ de~docnum
                            AND candat    EQ v_candat
                            AND cancel    EQ space ).

        IF ( sy-subrc EQ 0 ) AND ( _wl_ee_zgr_docs-docnum IS NOT INITIAL ).
          v_docnum_rom_e = _wl_ee_zgr_docs-docnum.
        ENDIF.

      ENDIF.

      "Localizar Documento Fiscal de Entrada
      IF ( v_docnum_rom_e IS INITIAL ).

        CLEAR: tg_doc_rom_e_tmp[], v_candat.

        PERFORM f_prepare_range_serie TABLES r_serie
                                       USING tg_zsdt0001_e-series.
        IF r_serie[] IS NOT INITIAL.

          SELECT *
            FROM j_1bnfdoc INTO TABLE tg_doc_rom_e_tmp
           WHERE bukrs  EQ tg_zsdt0001_e-bukrs
             AND branch EQ tg_zsdt0001_e-branch
             AND nfenum EQ tg_zsdt0001_e-nfnum
             AND parid  EQ tg_zsdt0001_e-parid
             AND docdat EQ tg_zsdt0001_e-docdat
             AND series IN r_serie
             AND direct EQ '1'
             AND candat EQ v_candat
             AND cancel EQ space
             AND doctyp IN ('1','2','6').

          IF tg_doc_rom_e_tmp[] IS INITIAL.

            v_nf9 = tg_zsdt0001_e-nfnum.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = v_nf9
              IMPORTING
                output = v_nf9.

            IF strlen( v_nf9 ) <= 6.
              v_nf6 = v_nf9.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = v_nf6
                IMPORTING
                  output = v_nf6.

              SELECT *
                FROM j_1bnfdoc INTO TABLE tg_doc_rom_e_tmp
               WHERE bukrs  EQ tg_zsdt0001_e-bukrs
                 AND branch EQ tg_zsdt0001_e-branch
                 AND nfnum  EQ v_nf6
                 AND parid  EQ tg_zsdt0001_e-parid
                 AND docdat EQ tg_zsdt0001_e-docdat
                 AND series IN r_serie
                 AND direct EQ '1'
                 AND candat EQ v_candat
                 AND cancel EQ space
                 AND doctyp IN ('1','2','6').
            ENDIF.

          ENDIF.

        ENDIF. "IF R_SERIE[] IS NOT INITIAL.

        LOOP AT tg_doc_rom_e_tmp.
          v_docnum_rom_e = tg_doc_rom_e_tmp-docnum.
        ENDLOOP.

      ENDIF.

      CHECK v_docnum_rom_e IS NOT INITIAL.

*---------------------------------------------------------------------------------------*
*     Check CFOP Entrada
*---------------------------------------------------------------------------------------*
      SELECT SINGLE *
        FROM j_1bnflin INTO _wl_lin_e
       WHERE docnum EQ v_docnum_rom_e
         AND cfop   IN it_cfops.

      CHECK sy-subrc EQ 0.

      IF i_ck_vinc_zmemo00 EQ abap_true.
        SELECT SINGLE *
          FROM znom_reme_notas INTO wl_znom_rem_nf_s_c
         WHERE docnum    EQ v_docnum_rom_e
           AND tp_nf_rem IN ( 'S', 'C' ).

        SELECT SINGLE *
          FROM znom_reme_notas INTO wl_znom_rem_nf_f
         WHERE docnum    EQ v_docnum_rom_e
           AND tp_nf_rem EQ 'F'.

        "Se utilizou NF na opção "Sem Vinc.Form Lote" primeiro que na opção com "Vinculo Form. Lote", desconsiderar vinculo.
        IF ( wl_znom_rem_nf_s_c IS NOT INITIAL ) AND ( wl_znom_rem_nf_f IS INITIAL ).
          RETURN.
        ENDIF.
      ENDIF.

      CLEAR: wl_zsdt0001_ro_vinc.
      wl_zsdt0001_ro_vinc-ch_referencia    = wl_zsdt0001_s-ch_referencia.
      wl_zsdt0001_ro_vinc-ch_ref_vinc      = tg_zsdt0001_e-ch_referencia.
      wl_zsdt0001_ro_vinc-docnum_vinc      = v_docnum_rom_e.
      wl_zsdt0001_ro_vinc-prop_vinc        = 100.
      wl_zsdt0001_ro_vinc-peso_liq         = tg_zsdt0001_e-peso_liq.
      wl_zsdt0001_ro_vinc-peso_fiscal      = tg_zsdt0001_e-peso_fiscal.
      wl_zsdt0001_ro_vinc-docnum           = _wl_j_1bnfdoc-docnum.
      APPEND wl_zsdt0001_ro_vinc TO e_zsdt0001_ro_vinc.

      e_romaneio_completo = abap_true.

      "Checar CCT Contrapartida
      IF ( i_ck_cct_cp EQ abap_true ) AND lines( e_zsdt0001_ro_vinc[] ) EQ 1.

        READ TABLE e_zsdt0001_ro_vinc INTO _wl_rom_vinc INDEX 1.
        CHECK sy-subrc EQ 0.

        "Check se Documento está registrado no CCT
        CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
          EXPORTING
            i_docnum     = _wl_rom_vinc-docnum_vinc
          IMPORTING
            e_zlest0146  = wl_zlest0146
            e_doc_rateio = v_doc_rateio.

        IF ( wl_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).
          e_cct_cp       = abap_true.
          e_zlest0146_cp = wl_zlest0146.
        ENDIF.
      ENDIF.

  ENDCASE.



ENDFUNCTION.
