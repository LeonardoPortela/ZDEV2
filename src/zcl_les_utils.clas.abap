class ZCL_LES_UTILS definition
  public
  final
  create public .

public section.

  interfaces IF_STAB_CONSTANTS .

  constants:
    lc_stvavr           TYPE c LENGTH 17 value 'EUDR_CONFIG_MATNR' ##NO_TEXT.
  constants:
    lc_formacao_lote    TYPE c LENGTH 01 value 'F' ##NO_TEXT.
  constants:
    lc_industrializacao TYPE c LENGTH 01 value 'I' ##NO_TEXT.
  constants:
    lc_pedido           TYPE c LENGTH 01 value 'P' ##NO_TEXT.
  constants:
    lc_fob              TYPE c LENGTH 03 value 'FOB' ##NO_TEXT.
  constants:
    lc_cif              TYPE c LENGTH 03 value 'CIF' ##NO_TEXT.
  constants:
    lc_s_eudr           TYPE c LENGTH 01 value 'S' ##NO_TEXT.
  constants:
    lc_n_neudr          TYPE c LENGTH 01 value 'N' ##NO_TEXT.
  constants:
    lc_a_hidrido        TYPE c LENGTH 01 value 'A' ##NO_TEXT.
  constants:
    lc_eudr             TYPE c LENGTH 04 value 'EUDR' ##NO_TEXT.
  constants:
    lc_neudr            TYPE c LENGTH 08 value 'Não EUDR' ##NO_TEXT.
  constants:
    lc_hidrido          TYPE c LENGTH 07 value 'Hídrido' ##NO_TEXT.
  constants:
    lc_ov               TYPE c LENGTH 02 value 'OV' ##NO_TEXT.
  constants:
    lc_pd               TYPE c LENGTH 02 value 'PD' ##NO_TEXT.
  constants:
    lc_zub              TYPE c LENGTH 03 value 'ZUB' ##NO_TEXT.
  constants:
    lc_true             TYPE c LENGTH 04 value 'true' ##NO_TEXT.
  constants:
    lc_false            TYPE c LENGTH 05 value 'false' ##NO_TEXT.
  constants:
    lc_null             TYPE c LENGTH 04 value 'null' ##NO_TEXT.
  constants:
    lc_transgenico_r    TYPE c LENGTH 01 value 'R' ##NO_TEXT.
  constants:
    lc_convencional_c   TYPE c LENGTH 01 value 'C' ##NO_TEXT.
  constants:
    lc_transgenico_rr   TYPE c LENGTH 02 value 'RR' ##NO_TEXT.
  constants:
    lc_convencional_co  TYPE c LENGTH 02 value 'CO' ##NO_TEXT.
  constants:
    lc_sim              TYPE c LENGTH 17 value 'S' ##NO_TEXT.
  constants LC_TP_MOVIMENTO_ENTRADA type CHAR01 value 'E' ##NO_TEXT.
  constants LC_TP_MOVIMENTO_SAIDA type CHAR01 value 'S' ##NO_TEXT.

  class-methods GET_ROMANEIO_DOCUMENTO_FISCAL
    importing
      !I_DOCNUM type J_1BDOCNUM optional
      value(R_DOCNUM) type RSIS_T_RANGE optional
    exporting
      !E_ROMANEIOS type ZSDT0001_DOCNUM_T
    returning
      value(R_ROMANEIO) type ZSDT0001_DOCNUM .
  class-methods GET_DEPARA_ROMANEIO_COMPLETO
    importing
      !I_CH_REFERENCIA type ZCH_REF optional
      value(I_CH_REFERENCIA_R) type RSIS_T_RANGE optional
    exporting
      !E_ROMANEIOS_VINCULADOS type ZSDT0001_T
      !E_DEPARA_ROMANEIOS type ZLESS0003_T
    returning
      value(R_ROMANEIO_VINCULADO) type ZSDT0001 .
  class-methods CHECK_DOC_FISCAL_ROM_COMPLETO
    importing
      !I_DOCNUM type J_1BDOCNUM optional
      value(R_DOCNUM) type RSIS_T_RANGE optional
    exporting
      !E_DOCNUM_ROM_COMPLETO type ZSDT0001_DOCNUM_T
      !E_DEPARA_ROMANEIOS type ZLESS0003_T .
  class-methods GET_DOCUMENTO_FISCAL_ROMANEIO
    importing
      !I_CH_REFERENCIA type ZCH_REF optional
      value(I_CH_REFERENCIA_R) type RSIS_T_RANGE optional
    exporting
      !E_DOCNUMS_FISCAIS_ROMANEIOS type ZLESS0005_T
    returning
      value(R_DOCNUM_FISCAL_ROMANEIO) type ZLESS0005 .
  PROTECTED SECTION.

private section.

  class-data GS_MATERIAL type ZSTRUCT_GET_MATERIAL_EUDR .
  class-data GS_FILIAL type ZSTRUCT_GET_FILIAL_EUDR .
  class-data GS_RETORNO_FILIAL_EUDR type ZDE_RET_FILIAL_EUDR .
  class-data GS_RETORNO_VALIDA_OV_EUDR type ZDE_RESPONSE_VALIDA_OV_EUDR .
  class-data GV_MATNR type TVARVC-LOW .
  class-data GS_REQUEST_OV_FOB type ZSTRUCT_POST_VALIDA_OV_EUDR .
ENDCLASS.



CLASS ZCL_LES_UTILS IMPLEMENTATION.


  METHOD CHECK_DOC_FISCAL_ROM_COMPLETO.

    CLEAR: E_DOCNUM_ROM_COMPLETO[], E_DEPARA_ROMANEIOS[].

    IF I_DOCNUM IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = I_DOCNUM ) TO R_DOCNUM.
    ENDIF.

    CHECK R_DOCNUM[] IS NOT INITIAL.

    "Verificar se romaneio é originado da ZMM0127 -->>>
*    SELECT *
*      FROM ZSDT0001
*      INTO TABLE @DATA(LT_ZSDT0001)
*      WHERE DOCNUM IN @R_DOCNUM[].
*
*    IF SY-SUBRC IS INITIAL.
*      LOOP AT LT_ZSDT0001 INTO DATA(LS_ZSDT0001).
*        APPEND VALUE #( DOCNUM = LS_ZSDT0001-DOCNUM ) TO E_DOCNUM_ROM_COMPLETO.
*        DELETE R_DOCNUM WHERE LOW EQ LS_ZSDT0001-DOCNUM.
*      ENDLOOP.
*    ENDIF.
    "Verificar se romaneio é originado da ZMM0127 <----

    CHECK R_DOCNUM[] IS NOT INITIAL.

    ZCL_LES_UTILS=>GET_ROMANEIO_DOCUMENTO_FISCAL( EXPORTING R_DOCNUM    = R_DOCNUM
                                                  IMPORTING E_ROMANEIOS = E_DOCNUM_ROM_COMPLETO ).

    CHECK E_DOCNUM_ROM_COMPLETO[] IS NOT INITIAL.

    DATA(LRA_CH_REFERENCIA) = VALUE RSIS_T_RANGE( FOR LWA_ROM_DOC_FISCAL IN E_DOCNUM_ROM_COMPLETO (
                                                   SIGN = 'I'
                                                   OPTION = 'EQ'
                                                   LOW = LWA_ROM_DOC_FISCAL-CH_REFERENCIA ) ).

    ZCL_LES_UTILS=>GET_DEPARA_ROMANEIO_COMPLETO(
      EXPORTING
        I_CH_REFERENCIA_R  = LRA_CH_REFERENCIA
      IMPORTING
        E_DEPARA_ROMANEIOS = E_DEPARA_ROMANEIOS
    ).

*    SORT E_DEPARA_ROMANEIOS BY CH_REFERENCIA.
*    LOOP AT LIT_ROMANEIOS_DOC_FISCAL INTO DATA(LWA_ROM_DOC).
*
*      READ TABLE E_DEPARA_ROMANEIOS WITH KEY CH_REFERENCIA = LWA_ROM_DOC-CH_REFERENCIA BINARY SEARCH TRANSPORTING NO FIELDS.
*      CHECK SY-SUBRC EQ 0.
*
*      APPEND INITIAL LINE TO E_DOCNUM_ROM_COMPLETO ASSIGNING FIELD-SYMBOL(<FS_DOCNUM_ROM_COMPLETO>).
*
*      MOVE-CORRESPONDING LWA_ROM_DOC TO <FS_DOCNUM_ROM_COMPLETO>.
*
*    ENDLOOP.

  ENDMETHOD.


  METHOD get_depara_romaneio_completo.

    DATA: lit_zsdt0001              TYPE TABLE OF zless0004,
          lit_zsdt0001_saida_vinc   TYPE TABLE OF zsdt0001,
          lit_zsdt0001_entrada_vinc TYPE TABLE OF zsdt0001.

    CLEAR: e_romaneios_vinculados, e_depara_romaneios, r_romaneio_vinculado.

    CHECK i_ch_referencia IS NOT INITIAL OR i_ch_referencia_r[] IS NOT INITIAL.

    IF i_ch_referencia IS NOT INITIAL.
      APPEND
      VALUE #( sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
               option = zcl_les_utils=>if_stab_constants~mc_option_equal
               low    = i_ch_referencia ) TO i_ch_referencia_r.
    ENDIF.

    CHECK i_ch_referencia_r[] IS NOT INITIAL.

*----------------------------------------------------------------------------------------------------------------------*
*   Seleção Romaneios
*----------------------------------------------------------------------------------------------------------------------*

    SELECT *
      FROM zsdt0001
      INTO CORRESPONDING FIELDS OF TABLE @lit_zsdt0001
     WHERE ch_referencia IN @i_ch_referencia_r.

    CHECK lit_zsdt0001[] IS NOT INITIAL.

*----------------------------------------------------------------------------------------------------------------------*
*   Processamento Romaneio Entrada
*----------------------------------------------------------------------------------------------------------------------*

    DATA(lit_zsdt0001_e) = lit_zsdt0001.
    DELETE lit_zsdt0001_e WHERE tp_movimento NE 'E'. "Entrada

    IF lit_zsdt0001_e IS NOT INITIAL.

      LOOP AT lit_zsdt0001_e ASSIGNING FIELD-SYMBOL(<fs_zsdt0001_e>).
        <fs_zsdt0001_e>-nr_refer_in      = CONV char9( |{ <fs_zsdt0001_e>-nr_romaneio ALPHA = IN }| ).
        <fs_zsdt0001_e>-nr_refer_out     = CONV char9( |{ <fs_zsdt0001_e>-nr_romaneio ALPHA = OUT }| ).
      ENDLOOP.

      SELECT *
        FROM zsdt0001 INTO TABLE lit_zsdt0001_saida_vinc
         FOR ALL ENTRIES IN lit_zsdt0001_e
       WHERE bukrs            EQ lit_zsdt0001_e-bukrs
         AND branch           EQ lit_zsdt0001_e-branch
         AND ( id_referencia  EQ lit_zsdt0001_e-nr_refer_in OR
               id_referencia  EQ lit_zsdt0001_e-nr_refer_out )
         AND tp_movimento     EQ 'S'
         AND nr_safra         EQ lit_zsdt0001_e-nr_safra.

      "Fluxo por vinculo por Id Carga - Inicio
      DATA(lit_zsdt0001_e_aux) = lit_zsdt0001_e[].
      DELETE lit_zsdt0001_e_aux WHERE id_carga IS INITIAL.
      IF lit_zsdt0001_e_aux[] IS NOT INITIAL.
        SELECT *
        FROM zsdt0001 APPENDING TABLE lit_zsdt0001_saida_vinc
         FOR ALL ENTRIES IN lit_zsdt0001_e_aux
       WHERE bukrs            EQ lit_zsdt0001_e_aux-bukrs
         AND branch           EQ lit_zsdt0001_e_aux-branch
         AND id_carga         EQ lit_zsdt0001_e_aux-id_carga
         AND tp_movimento     EQ 'S'
         AND nr_safra         EQ lit_zsdt0001_e_aux-nr_safra.
      ENDIF.
      "Fluxo por vinculo por Id Carga - Fim

      SORT lit_zsdt0001_saida_vinc BY ch_referencia.
      DELETE ADJACENT DUPLICATES FROM lit_zsdt0001_saida_vinc COMPARING ch_referencia.

      LOOP AT lit_zsdt0001_e ASSIGNING <fs_zsdt0001_e>.

        LOOP AT lit_zsdt0001_saida_vinc INTO DATA(lwa_zsdt0001_saida_vinc) WHERE bukrs           EQ <fs_zsdt0001_e>-bukrs
                                                                             AND branch          EQ <fs_zsdt0001_e>-branch
                                                                             AND (
                                                                                    ( id_referencia EQ <fs_zsdt0001_e>-nr_refer_in OR id_referencia EQ <fs_zsdt0001_e>-nr_refer_out )  OR
                                                                                    ( id_carga      EQ <fs_zsdt0001_e>-id_carga  AND id_carga IS NOT INITIAL )
                                                                                 )
                                                                             AND tp_movimento    EQ 'S'
                                                                             AND nr_safra        EQ <fs_zsdt0001_e>-nr_safra.

          "Alimenta Tabela Saida Romaneios Vinculados
          APPEND INITIAL LINE TO e_romaneios_vinculados ASSIGNING FIELD-SYMBOL(<fs_romaneio_vinc>).
          MOVE-CORRESPONDING lwa_zsdt0001_saida_vinc TO <fs_romaneio_vinc>.

          "Alimenta Tabela Saida Depara Romaneios Romaneios
          APPEND INITIAL LINE TO e_depara_romaneios ASSIGNING FIELD-SYMBOL(<fs_depara_romaneio>).
          <fs_depara_romaneio>-ch_referencia    = <fs_zsdt0001_e>-ch_referencia.
          <fs_depara_romaneio>-peso_liq         = <fs_zsdt0001_e>-peso_liq.

          <fs_depara_romaneio>-ch_ref_vinc      = lwa_zsdt0001_saida_vinc-ch_referencia.
          <fs_depara_romaneio>-peso_liq_vinc    = lwa_zsdt0001_saida_vinc-peso_liq.

        ENDLOOP.

      ENDLOOP.

    ENDIF.

*----------------------------------------------------------------------------------------------------------------------*
*   Processamento Romaneio Saida
*----------------------------------------------------------------------------------------------------------------------*

    DATA(lit_zsdt0001_s) = lit_zsdt0001.
    DELETE lit_zsdt0001_s WHERE tp_movimento NE 'S'. "Saida
    DELETE lit_zsdt0001_s WHERE id_referencia IS INITIAL AND id_carga IS INITIAL.

    IF lit_zsdt0001_s IS NOT INITIAL.

      LOOP AT lit_zsdt0001_s ASSIGNING FIELD-SYMBOL(<fs_zsdt0001_s>).
        <fs_zsdt0001_s>-nr_romaneio_vinc = CONV char9( |{ <fs_zsdt0001_s>-id_referencia ALPHA = IN }| ).
      ENDLOOP.

      DATA(lit_zsdt0001_s_aux) = lit_zsdt0001_s[].
      DELETE lit_zsdt0001_s_aux WHERE nr_romaneio_vinc IS INITIAL.
      IF lit_zsdt0001_s_aux[] IS NOT INITIAL.
        SELECT *
          FROM zsdt0001 INTO TABLE lit_zsdt0001_entrada_vinc
           FOR ALL ENTRIES IN lit_zsdt0001_s_aux
         WHERE bukrs            EQ lit_zsdt0001_s_aux-bukrs
           AND branch           EQ lit_zsdt0001_s_aux-branch
           AND nr_romaneio      EQ lit_zsdt0001_s_aux-nr_romaneio_vinc
           AND tp_movimento     EQ 'E'
           AND nr_safra         EQ lit_zsdt0001_s_aux-nr_safra.
      ENDIF.

      "Fluxo por vinculo por Id Carga - Inicio
      lit_zsdt0001_s_aux = lit_zsdt0001_s[].
      DELETE lit_zsdt0001_s_aux WHERE id_carga IS INITIAL.
      IF lit_zsdt0001_s_aux[] IS NOT INITIAL.
        SELECT *
        FROM zsdt0001 APPENDING TABLE lit_zsdt0001_entrada_vinc
         FOR ALL ENTRIES IN lit_zsdt0001_s_aux
       WHERE bukrs            EQ lit_zsdt0001_s_aux-bukrs
         AND branch           EQ lit_zsdt0001_s_aux-branch
         AND id_carga         EQ lit_zsdt0001_s_aux-id_carga
         AND tp_movimento     EQ 'E'
         AND nr_safra         EQ lit_zsdt0001_s_aux-nr_safra.
      ENDIF.
      "Fluxo por vinculo por Id Carga - Fim

      SORT lit_zsdt0001_entrada_vinc BY ch_referencia.
      DELETE ADJACENT DUPLICATES FROM lit_zsdt0001_entrada_vinc COMPARING ch_referencia.

      LOOP AT lit_zsdt0001_s ASSIGNING <fs_zsdt0001_s>.

        LOOP AT lit_zsdt0001_entrada_vinc INTO DATA(lwa_zsdt0001_entrada_vinc) WHERE bukrs         EQ <fs_zsdt0001_s>-bukrs
                                                                                 AND branch        EQ <fs_zsdt0001_s>-branch
                                                                                 AND (
                                                                                       ( nr_romaneio EQ <fs_zsdt0001_s>-nr_romaneio_vinc  ) OR
                                                                                       ( id_carga    EQ <fs_zsdt0001_s>-id_carga AND id_carga IS NOT INITIAL  )
                                                                                      )
                                                                                 AND tp_movimento  EQ 'E'
                                                                                 AND nr_safra      EQ <fs_zsdt0001_s>-nr_safra.

          "Alimenta Tabela Saida Romaneios Vinculados
          APPEND INITIAL LINE TO e_romaneios_vinculados ASSIGNING <fs_romaneio_vinc>.
          MOVE-CORRESPONDING lwa_zsdt0001_entrada_vinc TO <fs_romaneio_vinc>.

          "Alimenta Tabela Saida Depara Romaneios Romaneios
          APPEND INITIAL LINE TO e_depara_romaneios ASSIGNING <fs_depara_romaneio>.
          <fs_depara_romaneio>-ch_referencia    = <fs_zsdt0001_s>-ch_referencia.
          <fs_depara_romaneio>-peso_liq         = <fs_zsdt0001_s>-peso_liq.

          <fs_depara_romaneio>-ch_ref_vinc      = lwa_zsdt0001_entrada_vinc-ch_referencia.
          <fs_depara_romaneio>-peso_liq_vinc    = lwa_zsdt0001_entrada_vinc-peso_liq.


        ENDLOOP.

      ENDLOOP.

    ENDIF.


*----------------------------------------------------------------------------------------------------------------------*
*   Preenchimento Variaveis Saida Metodo
*----------------------------------------------------------------------------------------------------------------------*
    IF i_ch_referencia IS NOT INITIAL.
      IF e_romaneios_vinculados IS NOT INITIAL.
        r_romaneio_vinculado = e_romaneios_vinculados[ 1 ].
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD get_documento_fiscal_romaneio.

    DATA: lva_candat        TYPE j_1bnfdoc-candat.

    CLEAR: r_docnum_fiscal_romaneio, e_docnums_fiscais_romaneios[].

    IF i_ch_referencia IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_ch_referencia ) TO i_ch_referencia_r.
    ENDIF.

    CHECK i_ch_referencia_r[] IS NOT INITIAL.

    SELECT *
      FROM zsdt0001 INTO TABLE @DATA(lit_romaneios)
      WHERE ch_referencia IN @i_ch_referencia_r.

    CHECK lit_romaneios[] IS NOT INITIAL.

*---------------------------------------------------------------------------------------------------*
*   Seleção Documentos Fiscais Romaneio Entrada
*---------------------------------------------------------------------------------------------------*
    DATA(lit_zsdt001_e) = lit_romaneios[].
    DELETE lit_zsdt001_e WHERE tp_movimento NE 'E'.

    IF lit_zsdt001_e[] IS NOT INITIAL.
      SELECT *
        FROM zmmt_ee_zgr INTO TABLE @DATA(lit_zmmt_ee_zgr)
        FOR ALL ENTRIES IN @lit_zsdt001_e
       WHERE ch_referencia EQ @lit_zsdt001_e-ch_referencia.

      IF lit_zmmt_ee_zgr[] IS NOT INITIAL.
        SELECT *
          FROM zmmt_ee_zgr_docs AS de INTO TABLE @DATA(lit_zmmr_ee_zgr_docs)
          FOR ALL ENTRIES IN @lit_zmmt_ee_zgr
         WHERE obj_key EQ @lit_zmmt_ee_zgr-obj_key
           AND EXISTS (  SELECT docnum
                           FROM j_1bnfdoc AS dc
                          WHERE dc~docnum EQ de~docnum
                            AND candat    EQ @lva_candat
                            AND cancel    EQ @space ).

        DELETE lit_zmmr_ee_zgr_docs WHERE docnum IS INITIAL.
      ENDIF.
    ENDIF.
*---------------------------------------------------------------------------------------------------*
*   Seleção Documentos Fiscais Romaneio Saida
*---------------------------------------------------------------------------------------------------*





*---------------------------------------------------------------------------------------------------*
*   Processamento Romaneios
*---------------------------------------------------------------------------------------------------*
    SORT lit_zmmr_ee_zgr_docs BY obj_key.
    LOOP AT lit_romaneios INTO DATA(lwa_romaneio).

      APPEND INITIAL LINE TO e_docnums_fiscais_romaneios ASSIGNING FIELD-SYMBOL(<fs_docnum_fiscal_romaneio>).

      <fs_docnum_fiscal_romaneio>-ch_referencia = lwa_romaneio-ch_referencia.
      <fs_docnum_fiscal_romaneio>-ROMANEIO_COMPLETO = lwa_romaneio-ROMANEIO_COMPLETO.

      CASE lwa_romaneio-tp_movimento.
        WHEN 'S'.

          CLEAR: lwa_romaneio-nro_nf_prod.

          IF lwa_romaneio-doc_rem IS NOT INITIAL.

            CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
              EXPORTING
                i_vbeln  = lwa_romaneio-doc_rem
                i_direct = '2' "Saída
              IMPORTING
                e_docnum = lwa_romaneio-nro_nf_prod.

            IF lwa_romaneio-nro_nf_prod IS INITIAL.
              CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
                EXPORTING
                  i_vbeln  = lwa_romaneio-doc_rem
                  i_direct = '1' "Entrada
                IMPORTING
                  e_docnum = lwa_romaneio-nro_nf_prod.
            ENDIF.

          ELSEIF ( lwa_romaneio-doc_material IS NOT INITIAL ) AND ( lwa_romaneio-ano_material IS NOT INITIAL ).

            CONCATENATE lwa_romaneio-doc_material lwa_romaneio-ano_material INTO DATA(lva_refkey).

            SELECT SINGLE *
              FROM j_1bnflin INTO @DATA(_wl_lin)
             WHERE refkey = @lva_refkey.

            IF sy-subrc EQ 0.
              lwa_romaneio-nro_nf_prod = _wl_lin-docnum.
            ENDIF.

          ENDIF.

          IF lwa_romaneio-nro_nf_prod IS NOT INITIAL.
            SELECT SINGLE *
              FROM j_1bnfe_active INTO @DATA(_wl_active)
             WHERE docnum     EQ @lwa_romaneio-nro_nf_prod
               AND cancel     EQ @abap_false
               AND docsta     EQ '1'
               AND scssta     NE '2'.

            IF sy-subrc EQ 0.
              <fs_docnum_fiscal_romaneio>-docnum = lwa_romaneio-nro_nf_prod.
            ENDIF.
          ENDIF.

        WHEN 'E'.

          LOOP AT lit_zmmt_ee_zgr INTO DATA(lwa_zmmt_ee_zgr) WHERE ch_referencia = <fs_docnum_fiscal_romaneio>-ch_referencia.
            READ TABLE lit_zmmr_ee_zgr_docs INTO DATA(lwa_zmmr_ee_zgr_docs) WITH KEY obj_key = lwa_zmmt_ee_zgr-obj_key BINARY SEARCH.
            CHECK sy-subrc EQ 0 AND lwa_zmmr_ee_zgr_docs-docnum IS NOT INITIAL.
            <fs_docnum_fiscal_romaneio>-docnum = lwa_zmmr_ee_zgr_docs-docnum.
            EXIT.
          ENDLOOP.

      ENDCASE.

    ENDLOOP.

*----------------------------------------------------------------------------------------------------------------------*
*   Preenchimento Variaveis Saida Metodo
*----------------------------------------------------------------------------------------------------------------------*

    IF i_ch_referencia IS NOT INITIAL.
      IF e_docnums_fiscais_romaneios[] IS NOT INITIAL.
        r_docnum_fiscal_romaneio = e_docnums_fiscais_romaneios[ 1 ].
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_romaneio_documento_fiscal.

    TYPES: BEGIN OF ty_doc_fiscal,
             docnum    TYPE j_1bnfdoc-docnum,
             bukrs     TYPE j_1bnfdoc-bukrs,
             branch    TYPE j_1bnfdoc-branch,
             parid     TYPE j_1bnfdoc-parid,
             docdat    TYPE j_1bnfdoc-docdat,
             direct    TYPE j_1bnfdoc-direct,
             nfenum    TYPE j_1bnfdoc-nfenum,
             nfnum     TYPE j_1bnfdoc-nfnum,
             docref    TYPE j_1bnfdoc-docref,
             nfnum_rom TYPE zsdt0001-nfnum,
             doc_rem   TYPE zdoc_rem,
           END OF ty_doc_fiscal.

    DATA: lit_j_1bnfdoc        TYPE TABLE OF ty_doc_fiscal,
          lit_zsdt0001_e_sel_1 TYPE TABLE OF zsdt0001,
          lit_zsdt0001_e_sel_2 TYPE TABLE OF zsdt0001,
          lit_zsdt0001_s       TYPE TABLE OF zsdt0001.

    CLEAR: r_romaneio, e_romaneios.

    CHECK i_docnum IS NOT INITIAL OR r_docnum IS NOT INITIAL.

    IF i_docnum IS NOT INITIAL.
      APPEND
      VALUE #( sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
               option = zcl_les_utils=>if_stab_constants~mc_option_equal
               low    = i_docnum ) TO r_docnum.
    ENDIF.

    CHECK r_docnum IS NOT INITIAL.

*----------------------------------------------------------------------------------------------------------------------*
*   Seleção Documentos Fiscais
*----------------------------------------------------------------------------------------------------------------------*

    SELECT docnum, bukrs, branch, parid, docdat, direct, nfenum, nfnum, docref
      FROM j_1bnfdoc
      INTO CORRESPONDING FIELDS OF TABLE @lit_j_1bnfdoc
     WHERE docnum IN @r_docnum.

    CHECK lit_j_1bnfdoc IS NOT INITIAL.

*----------------------------------------------------------------------------------------------------------------------*
*   Processamento Romaneio Entrada
*----------------------------------------------------------------------------------------------------------------------*

    DATA(tl_doc_e) = lit_j_1bnfdoc.
    DELETE tl_doc_e WHERE direct NE '1'. "Entrada

    IF tl_doc_e IS NOT INITIAL.

      LOOP AT tl_doc_e ASSIGNING FIELD-SYMBOL(<fs_doc_e>).
        IF <fs_doc_e>-nfenum IS NOT INITIAL.
          <fs_doc_e>-nfnum_rom = <fs_doc_e>-nfenum.
        ELSEIF <fs_doc_e>-nfnum IS NOT INITIAL.
          <fs_doc_e>-nfnum_rom = <fs_doc_e>-nfnum.
        ENDIF.
      ENDLOOP.

      SELECT obj_key, docnum
        FROM zmmt_ee_zgr_docs AS de INTO TABLE @DATA(tl_ee_zgr_docs)
         FOR ALL ENTRIES IN @tl_doc_e
       WHERE docnum EQ @tl_doc_e-docnum.

      IF tl_ee_zgr_docs IS NOT INITIAL.
        SELECT ch_referencia, obj_key
          FROM zmmt_ee_zgr INTO TABLE @DATA(tl_ee_zgr)
          FOR ALL ENTRIES IN @tl_ee_zgr_docs
         WHERE obj_key EQ @tl_ee_zgr_docs-obj_key.
      ENDIF.

      IF tl_ee_zgr IS NOT INITIAL.
        SELECT *
          FROM zsdt0001 APPENDING CORRESPONDING FIELDS OF TABLE @lit_zsdt0001_e_sel_1
          FOR ALL ENTRIES IN @tl_ee_zgr
         WHERE ch_referencia EQ @tl_ee_zgr-ch_referencia.
      ENDIF.

      SELECT *
        FROM zsdt0001 APPENDING CORRESPONDING FIELDS OF TABLE @lit_zsdt0001_e_sel_2
        FOR ALL ENTRIES IN @tl_doc_e
      WHERE bukrs        EQ @tl_doc_e-bukrs
        AND branch       EQ @tl_doc_e-branch
        AND parid        EQ @tl_doc_e-parid
        AND docdat       EQ @tl_doc_e-docdat
        AND nfnum        EQ @tl_doc_e-nfnum_rom
        AND tp_movimento EQ @lc_tp_movimento_entrada.

      SORT: tl_ee_zgr_docs       BY docnum,
            tl_ee_zgr            BY obj_key,
            lit_zsdt0001_e_sel_1 BY ch_referencia,
            lit_zsdt0001_e_sel_2 BY bukrs branch parid docdat nfnum tp_movimento.

      LOOP AT tl_doc_e ASSIGNING <fs_doc_e>.
        DATA(_romaneio_found) = abap_false.
        READ TABLE tl_ee_zgr_docs INTO DATA(lwa_ee_zgr_docs) WITH KEY docnum = <fs_doc_e>-docnum BINARY SEARCH.
        CHECK sy-subrc EQ 0.

        READ TABLE tl_ee_zgr INTO DATA(lwa_tl_ee_zgr) WITH KEY obj_key = lwa_ee_zgr_docs-obj_key BINARY SEARCH.
        CHECK sy-subrc EQ 0.

        IF lwa_tl_ee_zgr-ch_referencia IS NOT INITIAL.
          READ TABLE lit_zsdt0001_e_sel_1 INTO DATA(lwa_zsdt0001_e) WITH KEY ch_referencia = lwa_tl_ee_zgr-ch_referencia BINARY SEARCH.
          IF sy-subrc EQ 0.
            _romaneio_found = abap_true.
          ENDIF.
        ENDIF.

        IF _romaneio_found EQ abap_false.
          READ TABLE lit_zsdt0001_e_sel_2 INTO lwa_zsdt0001_e WITH KEY bukrs        = <fs_doc_e>-bukrs
                                                                       branch       = <fs_doc_e>-branch
                                                                       parid        = <fs_doc_e>-parid
                                                                       docdat       = <fs_doc_e>-docdat
                                                                       nfnum        = <fs_doc_e>-nfnum_rom
                                                                       tp_movimento = lc_tp_movimento_entrada BINARY SEARCH.
          IF sy-subrc EQ 0.
            _romaneio_found = abap_true.
          ENDIF.
        ENDIF.

        IF _romaneio_found = abap_true.
          APPEND INITIAL LINE TO e_romaneios ASSIGNING FIELD-SYMBOL(<fs_romaneio_out>).
          MOVE-CORRESPONDING lwa_zsdt0001_e TO <fs_romaneio_out>.
          <fs_romaneio_out>-docnum = <fs_doc_e>-docnum.
        ENDIF.

      ENDLOOP.

    ENDIF.

*----------------------------------------------------------------------------------------------------------------------*
*   Processamento Romaneio Saida
*----------------------------------------------------------------------------------------------------------------------*

    DATA(tl_doc_s) = lit_j_1bnfdoc.
    DELETE tl_doc_s WHERE direct NE '2'. "// Saida

    IF tl_doc_s IS NOT INITIAL.
      LOOP AT tl_doc_s ASSIGNING FIELD-SYMBOL(<fs_doc_s>).
        CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
          EXPORTING
            i_docnum    = <fs_doc_s>-docnum
            i_direct    = '2'
            i_check_aut = abap_false
          IMPORTING
            e_vbeln     = <fs_doc_s>-doc_rem.
      ENDLOOP.

      DELETE tl_doc_s WHERE doc_rem IS INITIAL.

      IF tl_doc_s[] IS NOT INITIAL.
        SELECT *
          FROM zsdt0001
          INTO CORRESPONDING FIELDS OF TABLE @lit_zsdt0001_s
           FOR ALL ENTRIES IN @tl_doc_s
         WHERE doc_rem      EQ @tl_doc_s-doc_rem
           AND tp_movimento EQ @lc_tp_movimento_saida.

        SORT lit_zsdt0001_s BY doc_rem.
        LOOP AT tl_doc_s ASSIGNING <fs_doc_s>.
          _romaneio_found = abap_false.

          READ TABLE lit_zsdt0001_s INTO DATA(lwa_zsdt0001_s) WITH KEY doc_rem = <fs_doc_s>-doc_rem BINARY SEARCH.
          IF sy-subrc EQ 0.
            _romaneio_found = abap_true.
          ENDIF.

          IF _romaneio_found = abap_true.
            APPEND INITIAL LINE TO e_romaneios ASSIGNING <fs_romaneio_out>.
            MOVE-CORRESPONDING lwa_zsdt0001_s TO <fs_romaneio_out>.
            <fs_romaneio_out>-docnum = <fs_doc_s>-docnum.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

*----------------------------------------------------------------------------------------------------------------------*
*   Preenchimento Variaveis Saida Metodo
*----------------------------------------------------------------------------------------------------------------------*

    IF i_docnum IS NOT INITIAL.
      IF e_romaneios IS NOT INITIAL.
        r_romaneio = e_romaneios[ 1 ].
      ENDIF.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
