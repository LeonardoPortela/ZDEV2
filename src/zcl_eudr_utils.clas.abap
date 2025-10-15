class ZCL_EUDR_UTILS definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_produtor,
        protocolo       TYPE string,
        cnpj            TYPE stcd1,
        cpf             TYPE stcd2,
        farmer_document TYPE string,
        ie              TYPE stcd3,
        producername    TYPE name1,
        land1           TYPE land1,
      END OF ty_produtor .
  types:
    BEGIN OF ty_fazenda,
        farm_id         TYPE string,
        code            TYPE string,
        farmer_document TYPE string,
        ie              TYPE stcd3,
        fazenda         TYPE string,
        geojson         TYPE zde_areas_t,
        data            TYPE sydatum,
        hora            TYPE syuzeit,
      END OF ty_fazenda .

  class-data:
    gt_produtor      TYPE TABLE OF ty_produtor .
  class-data:
    gt_fazenda       TYPE TABLE OF ty_fazenda .
  class-data:
    gt_fazenda_final TYPE TABLE OF ty_fazenda .
  class-data GS_PRODUTOR type TY_PRODUTOR .
  class-data GS_FAZENDA type TY_FAZENDA .
  constants:
    lc_stvavr               TYPE c LENGTH 17 value 'EUDR_CONFIG_MATNR' ##NO_TEXT.
  constants:
    lc_cfop_comercializacao TYPE c LENGTH 26 value 'MAGGI_CFOP_COMERCIALIZACAO' ##NO_TEXT.
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
    lc_n_eudr           TYPE c LENGTH 01 value 'N' ##NO_TEXT.
  constants:
    lc_a_hidrido        TYPE c LENGTH 01 value 'A' ##NO_TEXT.
  constants:
    lc_h_hidrido        TYPE c LENGTH 01 value 'H' ##NO_TEXT.
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
    lc_zind             TYPE c LENGTH 04 value 'ZIND' ##NO_TEXT.
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
  constants:
    lc_fob_ov_att1 TYPE c LENGTH 50 value 'Entrada FOB com OV de Industrialização' ##NO_TEXT.
  constants:
    lc_fob_ov_att2 TYPE c LENGTH 50 value ', só permite NF de Entrada com ' ##NO_TEXT.
  constants:
    lc_fob_ov_att3 TYPE c LENGTH 50 value 'CFOP de Comercilazação!' ##NO_TEXT.
  constants:
    lc_europa TYPE c LENGTH 02 value 'EU' ##NO_TEXT.
  class-data AT_DADOS_CFOP type ABAP_BOOL .
  class-data AT_DADOS_ORDEM_VENDA type ABAP_BOOL .
  constants LC_TCODE_EMAIL_GEOJSON type CHAR30 value 'ZMEMO00_GEOJSON' ##NO_TEXT.

  class-methods CHECK_MATERIAL_EUDR
    importing
      value(I_MATNR) type MATNR
    returning
      value(R_EUDR) type CHAR1 .
  class-methods GET_FILIAL_EUDR
    importing
      !I_WERKS type WERKS_D
    returning
      value(R_EUDR) type ZEUDR_STATUS_FILIAL_EUDR .
  class-methods CHECK_REGRA_SOLICITACAO_EUDR
    importing
      !I_TIPO type ZE_TP_SOLICITACAO
    changing
      !C_ZSDT0158 type ZSDT0158 .
  class-methods CHECK_DEPOSITO_EUDR
    importing
      !I_LGORT type LGORT_D optional
    returning
      value(R_DEPOSITO_EUDR) type ZEUDR .
  class-methods PROCESSA_API_SOLICITACAO_EUDR
    importing
      !I_ZSDT0158 type ZSDT0158
    changing
      !C_VALIDA_OV_EUDR type ZSTRUCT_POST_VALIDA_OV_EUDR
    returning
      value(R_ATENDE_EUDR) type CHAR1 .
  class-methods CHECK_OV_PEDIDO_EUDR
    importing
      !I_VBELN type VBELN optional
      !I_EBELN type EBELN optional
      !I_LGORT type LGORT_D optional
      !I_CK_LGORT_PARAM type CHAR01 optional
    returning
      value(R_EUDR) type ZEUDR_ORDEM_PEDIDO .
  class-methods GET_TEXTOS_EUDR_NF_SAIDA
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_TEXTOS) type STRING .
  class-methods CHECK_DOC_FISCAL_EUDR
    importing
      !I_DOCNUM type J_1BDOCNUM optional
      !I_DOCNUM_T type RSIS_T_RANGE optional
    exporting
      !E_DOCNUM_EUDR type ZSDS092_T
    returning
      value(R_EUDR) type ZEUDR .
  class-methods CK_FILIAL_PARTICIPANTE_EUDR
    importing
      !I_WERKS type WERKS_D
    returning
      value(R_PARTICIPANTE) type ZEUDR .
  class-methods CK_FILIAL_CLASSIFICACAO_EUDR
    importing
      !I_WERKS type WERKS_D
    returning
      value(R_CLASSIFICACAO) type ZEUDR_ORDEM_PEDIDO .
  class-methods CHECK_PAIS_DESTINO_EUDR
    importing
      !I_SDT0174 type ZSDT0174_T
    returning
      value(IS_EUDR) type ABAP_BOOL .
  class-methods CHECK_GENERATION_FILE_GEOJSON
    importing
      !I_ID_NOMEACAO type ZID_NOM
    returning
      value(R_OK) type ABAP_BOOL .
  class-methods GET_DUES_EUDR_FROM_NOMEACAO
    importing
      !I_ID_NOMEACAO type ZID_NOM
    returning
      value(R_DUES) type ZSDT0170_T .
  class-methods GET_PROTOCOLOS_EUDR_NOMEACAO
    importing
      !I_ID_NOMEACAO type ZID_NOM
    exporting
      !E_ERROR type ABAP_BOOL
      !E_MSG_ERROR type STRING
    returning
      value(R_PROTOCOLOS_DOCUMENTOS) type ZEPROTOCOLOEUDR_T .
  class-methods CREATE_ARQUIVO_GEOJSON
    importing
      !I_ID_NOMEACAO type ZID_NOM
    exporting
      !E_MSG_ERROR type STRING
    returning
      value(E_FILE_GEOJSON) type STRING .
  class-methods PROCESSA_API_PROTOCOLO_EUDR
    importing
      !I_PROTOCOLO type ZEPROTOCOLOEUDR_T
    returning
      value(R_GEOJSON) type STRING .
  class-methods GET_ARQUIVO_GEOSON
    importing
      !I_ID_NOMEACAO type ZID_NOM
    returning
      value(E_FILE_GEOSON) type STRING .
  class-methods DOWNLOAD_ARQUIVO_GEOJSON
    importing
      !I_ID_NOMEACAO type ZID_NOM .
  class-methods FILL_GEOJSON
    importing
      !I_GEOJSON_EUDR type ZDE_GEOJSON_EUDR_T
    returning
      value(R_GEOJSON) type STRING .
  class-methods GET_NFS_ENT_NOM_VOL_FILIAL
    importing
      !I_DUES type ZSDT0170_T
    exporting
      !E_ERROR type ABAP_BOOL
      !E_MSG_ERROR type STRING
    returning
      value(R_DOCUMENTOS) type J_1B_TT_NFDOC .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA gs_material TYPE zstruct_get_material_eudr .
    CLASS-DATA gs_filial TYPE zstruct_get_filial_eudr .
    CLASS-DATA gs_retorno_filial_eudr TYPE zde_ret_filial_eudr .
    CLASS-DATA gs_retorno_valida_ov_eudr TYPE zde_ret_solicitacao_eudr .
    CLASS-DATA gv_matnr TYPE tvarvc-low .
    CLASS-DATA gs_request_ov_fob TYPE zstruct_post_valida_ov_eudr .
    CLASS-DATA gs_retorno_material_eudr TYPE zde_ret_material_eudr .
    CLASS-DATA gs_valida_entrada_fob_eudr TYPE zstruct_valida_entrada_fob .
    CLASS-DATA gs_retorno_entrada_fob_eudr TYPE zde_ret_entrada_fob_eud .
    CLASS-DATA gs_protocolo_eudr TYPE zstruct_get_protocolo_eudr .
    CLASS-DATA gs_retorno_protocolo_eudr TYPE zde_geojson_eudr_t .
    CLASS-DATA at_zsdt0366 TYPE zsdt0366 .
ENDCLASS.



CLASS ZCL_EUDR_UTILS IMPLEMENTATION.


  METHOD check_material_eudr.

    DATA: lwa_return_erro TYPE zstruct_return_api_eudr.

    DATA: lv_material_sap_in  TYPE matnr,
          lv_material_sap_out TYPE matnr,
          lv_material_api     TYPE matnr.

    CLEAR r_eudr.

    CHECK i_matnr IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = i_matnr
      IMPORTING
        output = lv_material_sap_in.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = i_matnr
      IMPORTING
        output = lv_material_sap_out.


    SELECT SINGLE *
      FROM zsdt0368 INTO @DATA(lwa_zsdt0368)
     WHERE matnr     EQ @lv_material_sap_in
       AND dt_update EQ @sy-datum.

    IF sy-subrc EQ 0.
      r_eudr  = lwa_zsdt0368-atende_eudr.
      RETURN.
    ENDIF.

    gs_material-matnr = lv_material_sap_out.

    TRY.
        zcl_int_ob_get_material_eudr=>zif_integracao_outbound~get_instance( )->execute_request(
          EXPORTING
            i_info_request           = gs_material
          IMPORTING
            e_id_integracao          = DATA(resul_id)
            e_integracao             = DATA(result_json) ).

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE 'Não foi possivel consultar a parametrização EUDR do material no SIGAM!'  TYPE 'E' .
        RETURN.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE 'Não foi possivel consultar a parametrização EUDR do material no SIGAM!'  TYPE 'E' .
        RETURN.
    ENDTRY.

    CLEAR: lwa_zsdt0368.
    lwa_zsdt0368-matnr       = lv_material_sap_in.
    lwa_zsdt0368-dt_update   = sy-datum.

    "// Recupera os dados do JSON
    IF result_json IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = result_json-ds_data_retorno CHANGING data = gs_retorno_material_eudr ).
    ENDIF.

    if gs_retorno_material_eudr-success EQ abap_true AND gs_retorno_material_eudr-data[] IS NOT INITIAL.
      lv_material_api = |{ gs_retorno_material_eudr-data[ 1 ]-codigoprodutosap ALPHA = OUT }|.

      if lv_material_sap_out EQ lv_material_api.
        r_eudr                   = abap_true.
        lwa_zsdt0368-atende_eudr = abap_true.
      endif.
    endif.

    MODIFY zsdt0368 FROM lwa_zsdt0368.
    COMMIT WORK.

  ENDMETHOD.


  METHOD check_deposito_eudr.

    DATA: wa_zmmt0017         TYPE zmmt0017,
          wa_zsdt_depara_depo TYPE zsdt_depara_depo.

    CLEAR: r_deposito_eudr.

    CHECK i_lgort IS NOT INITIAL.

    SELECT SINGLE *
      from zmmt0190 INTO @DATA(lwa_zmmt0190)
     WHERE lgort eq @i_lgort.

    CHECK SY-SUBRC EQ 0.

    r_deposito_eudr = lwa_zmmt0190-eudr.


*    SELECT SINGLE * FROM  zmmt0017 INTO wa_zmmt0017 WHERE lgort EQ i_lgort.
*
*    IF sy-subrc IS INITIAL.
*      SELECT SINGLE * FROM  zmmt0017 INTO wa_zmmt0017 WHERE lgort EQ i_lgort AND eudr EQ lc_s_eudr.
*      IF sy-subrc IS INITIAL.
*        r_deposito_eudr = lc_s_eudr.
*        EXIT.
*      ELSE.
*        SELECT SINGLE * FROM  zmmt0017 INTO wa_zmmt0017 WHERE lgort EQ i_lgort AND eudr EQ lc_n_eudr.
*        IF sy-subrc IS INITIAL.
*          r_deposito_eudr = lc_n_eudr.
*          EXIT.
*        ELSE.
*          EXIT.
*        ENDIF.
*      ENDIF.
*
*    ELSE.
*
*      SELECT SINGLE * FROM  zsdt_depara_depo INTO wa_zsdt_depara_depo WHERE lgort EQ i_lgort OR lgort_t EQ i_lgort.
*
*      IF  sy-subrc IS INITIAL.
*        SELECT SINGLE * FROM  zsdt_depara_depo INTO wa_zsdt_depara_depo WHERE ( lgort EQ i_lgort OR lgort_t EQ i_lgort ) AND eudr EQ lc_s_eudr.
*        IF sy-subrc IS INITIAL.
*          r_deposito_eudr = lc_s_eudr.
*          EXIT.
*        ELSE.
*          SELECT SINGLE * FROM  zsdt_depara_depo INTO wa_zsdt_depara_depo WHERE ( lgort EQ i_lgort OR lgort_t EQ i_lgort ) AND eudr EQ lc_n_eudr.
*          IF sy-subrc IS INITIAL.
*            r_deposito_eudr = lc_n_eudr.
*            EXIT.
*          ELSE.
*            EXIT.
*          ENDIF.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.


  ENDMETHOD.


  METHOD check_doc_fiscal_eudr.

    TYPES: BEGIN OF ty_doc,
             docnum TYPE j_1bnfdoc-docnum,
             direct TYPE j_1bnfdoc-direct,
           END OF ty_doc.

    DATA: lit_documentos_fiscais TYPE rsis_t_range,
          lit_doc                TYPE TABLE OF ty_doc,
          lva_vbeln              TYPE zdoc_rem.


    lit_documentos_fiscais = i_docnum_t.
    IF i_docnum IS NOT INITIAL.
      APPEND VALUE #( sign   = 'I' option = 'EQ' low = i_docnum ) TO lit_documentos_fiscais.
    ENDIF.

    CHECK lit_documentos_fiscais[] IS NOT INITIAL.

    SELECT docnum ,direct
      FROM j_1bnfdoc INTO TABLE @lit_doc
    WHERE docnum IN @lit_documentos_fiscais.

    SORT lit_doc BY docnum.

*--------------------------------------------------------------------------------------------------------*
*   Documentos NF-e
*--------------------------------------------------------------------------------------------------------*

    "Buscar classificação EUDR pelo fluxo de Romaneios
    zcl_les_utils=>get_romaneio_documento_fiscal(
      EXPORTING
        r_docnum   = i_docnum_t
        i_docnum   = i_docnum
      IMPORTING
         e_romaneios = DATA(t_romaneios) " Tabela range genérica
      RECEIVING
        r_romaneio   = DATA(wa_romaneio) ).

    LOOP AT t_romaneios INTO DATA(lwa_romaneio).
      APPEND INITIAL LINE TO e_docnum_eudr ASSIGNING FIELD-SYMBOL(<fs_docnum_eudr>).
      <fs_docnum_eudr>-docnum = lwa_romaneio-docnum.
      <fs_docnum_eudr>-eudr   = lwa_romaneio-eudr.
    ENDLOOP.


    "Verifica se houve mistura do produto depois que saiu da filial
    IF e_docnum_eudr[] IS NOT INITIAL.

      SELECT *
        FROM zlest0249 INTO TABLE @DATA(lit_zlest0249)
         FOR ALL ENTRIES IN @e_docnum_eudr
       WHERE docnum EQ @e_docnum_eudr-docnum.

      SORT lit_zlest0249 BY docnum.

      SELECT *
        FROM zcarta_correcao INTO TABLE @DATA(lit_zcarta_correcao)
         FOR ALL ENTRIES IN @e_docnum_eudr
       WHERE docnum EQ @e_docnum_eudr-docnum.

      DELETE lit_zcarta_correcao WHERE authcode IS INITIAL.
      DELETE lit_zcarta_correcao WHERE novo_terminal IS INITIAL OR ( lgort_d IS INITIAL AND doc_material IS INITIAL ).
      SORT lit_zcarta_correcao BY docnum dt_authcod DESCENDING hr_authcod DESCENDING.

      IF lit_zcarta_correcao[] IS NOT INITIAL.

        SELECT mblnr ,shkzg, lgort
         FROM mseg INTO TABLE @DATA(lit_mseg)
          FOR ALL ENTRIES IN @lit_zcarta_correcao
        WHERE mblnr EQ @lit_zcarta_correcao-doc_material.

        DELETE lit_mseg WHERE shkzg NE 'S'.

        LOOP AT lit_zcarta_correcao ASSIGNING FIELD-SYMBOL(<fs_carta>).
          IF <fs_carta>-lgort_d IS INITIAL.
            READ TABLE lit_mseg INTO DATA(lwa_mseg) WITH KEY mblnr = <fs_carta>-doc_material.
            IF sy-subrc EQ 0.
              <fs_carta>-lgort_d = lwa_mseg-lgort.
            ENDIF.
          ENDIF.
        ENDLOOP.

        SELECT *
          FROM zmmt0190 INTO TABLE @DATA(lit_zmmt0190)
           FOR ALL ENTRIES IN @lit_zcarta_correcao
         WHERE lgort EQ @lit_zcarta_correcao-lgort_d.

      ENDIF.

    ENDIF.

    LOOP AT e_docnum_eudr ASSIGNING <fs_docnum_eudr>.

      DATA(lva_class_eudr_origem) = <fs_docnum_eudr>-eudr.

      READ TABLE lit_doc INTO DATA(lwa_doc) WITH KEY docnum = <fs_docnum_eudr>-docnum BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      CHECK lwa_doc-direct EQ '2'.

      "Houve informativo de mistura de volume da nota fiscal
      READ TABLE lit_zlest0249 WITH KEY docnum = <fs_docnum_eudr>-docnum TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_docnum_eudr>-eudr = 'N'.
        CONTINUE.
      ENDIF.

      "Produto foi misturado no transbordo ou terminal porto.
      CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
        EXPORTING
          i_docnum = <fs_docnum_eudr>-docnum
          i_direct = '2'
        IMPORTING
          e_vbeln  = lva_vbeln.

      IF lva_vbeln IS NOT INITIAL.
        SELECT SINGLE *
          FROM zsdt0023 INTO @DATA(lwa_zsdt0023)
         WHERE vbeln EQ @lva_vbeln.

        IF sy-subrc EQ 0 AND lwa_zsdt0023-lgort_v IS NOT INITIAL.
          SELECT SINGLE *
            FROM zmmt0190 INTO @DATA(lwa_zmmt0190)
           WHERE lgort EQ @lwa_zsdt0023-lgort_v.

          IF sy-subrc NE 0 OR ( sy-subrc EQ 0 AND lwa_zmmt0190-eudr NE 'S' ).
            <fs_docnum_eudr>-eudr = 'N'.
          ENDIF.
        ENDIF.
      ENDIF.


      READ TABLE lit_zcarta_correcao INTO DATA(lwa_carta_correcao) WITH KEY docnum = <fs_docnum_eudr>-docnum.
      IF sy-subrc EQ 0 AND lwa_carta_correcao-lgort_d IS NOT INITIAL.
        READ TABLE lit_zmmt0190 INTO lwa_zmmt0190 WITH KEY lgort = lwa_carta_correcao-lgort_d.
        IF sy-subrc EQ 0.

          IF ( lwa_zmmt0190-eudr EQ 'S' ) AND ( lva_class_eudr_origem EQ 'S' ).
            <fs_docnum_eudr>-eudr = 'S'.
          ELSE.
            <fs_docnum_eudr>-eudr = 'N'.
          ENDIF.

        ELSE.
          <fs_docnum_eudr>-eudr = 'N'.
          CONTINUE.
        ENDIF.
      ENDIF.


    ENDLOOP.


*--------------------------------------------------------------------------------------------------------*
*   Documentos CT-e
*--------------------------------------------------------------------------------------------------------*

    "Buscar classificação EUDR pelo fluxo Aquaviario
    LOOP AT lit_documentos_fiscais ASSIGNING FIELD-SYMBOL(<fs_doc_fiscal>).
      READ TABLE e_docnum_eudr WITH KEY docnum = <fs_doc_fiscal>-low TRANSPORTING NO FIELDS.
      CHECK sy-subrc NE 0. "Só prosseguir se nao foi classificado...

      SELECT SINGLE docnum ,eudr
        FROM zlest0061 INTO @DATA(lwa_zlest0061)
       WHERE docnum EQ @<fs_doc_fiscal>-low.

      CHECK sy-subrc EQ 0.

      APPEND INITIAL LINE TO e_docnum_eudr ASSIGNING <fs_docnum_eudr>.
      <fs_docnum_eudr>-docnum = <fs_doc_fiscal>-low.
      <fs_docnum_eudr>-eudr   = lwa_zlest0061-eudr.

    ENDLOOP.

    IF i_docnum IS NOT INITIAL AND e_docnum_eudr[] IS NOT INITIAL.
      r_eudr = e_docnum_eudr[ 1 ]-eudr.
    ENDIF.


  ENDMETHOD.


  METHOD check_generation_file_geojson.

    r_ok = abap_false.

    zcl_eudr_utils=>get_dues_eudr_from_nomeacao(
      EXPORTING
        i_id_nomeacao = i_id_nomeacao
      RECEIVING
        r_dues        = DATA(r_dues)
    ).

    IF r_dues IS INITIAL.
      MESSAGE |Nomeação não possui DU-e's EUDR!| TYPE 'E'.
      RETURN.
    ENDIF.

    SELECT *
      FROM zsdt0170 AS b
      INTO TABLE @DATA(lt_due_pendente_retificacao)
      FOR ALL ENTRIES IN @r_dues
      WHERE id_due EQ @r_dues-id_due
        AND NOT EXISTS ( SELECT id_due
                            FROM zsdt0170 AS a
                            WHERE a~id_due_ref EQ b~id_due
                              AND a~loekz      EQ @abap_false
                              AND a~status     EQ 1 ).

    IF lt_due_pendente_retificacao IS NOT INITIAL.
      MESSAGE |DU-e EUDR Id: { lt_due_pendente_retificacao[ 1 ]-id_due }, não foi retificada!| TYPE 'E'.
      RETURN.
    ENDIF.

    r_ok = abap_true.

  ENDMETHOD.


  METHOD check_ov_pedido_eudr.
    DATA: v_lgort TYPE lgort_d.

    CLEAR v_lgort.

    IF i_ck_lgort_param IS NOT INITIAL.

      IF i_lgort IS NOT INITIAL.
        zcl_eudr_utils=>check_deposito_eudr(
          EXPORTING
            i_lgort         = i_lgort
          RECEIVING
            r_deposito_eudr = r_eudr ).

        IF r_eudr IS INITIAL.
          r_eudr = zcl_eudr_utils=>lc_n_eudr.
        ENDIF.

      ELSE.
        "r_eudr = 'A'.
        CLEAR r_eudr.
      ENDIF.

      EXIT.

    ENDIF.

    IF i_vbeln IS NOT INITIAL.

      SELECT SINGLE lgort FROM vbap INTO v_lgort WHERE vbeln EQ i_vbeln.

      IF  v_lgort IS NOT INITIAL.
        zcl_eudr_utils=>check_deposito_eudr(
          EXPORTING
            i_lgort         = v_lgort
          RECEIVING
            r_deposito_eudr = r_eudr ).

        IF r_eudr IS INITIAL.
          r_eudr = zcl_eudr_utils=>lc_n_eudr.
          EXIT.
        ENDIF.

      ELSE.
*        r_eudr = 'A'.
        CLEAR r_eudr.
        EXIT.
      ENDIF.

    ENDIF.

    CLEAR v_lgort.

    IF i_ebeln IS NOT INITIAL.

      SELECT SINGLE lgort FROM ekpo INTO v_lgort WHERE ebeln EQ i_ebeln.

      IF  v_lgort IS NOT INITIAL.
        zcl_eudr_utils=>check_deposito_eudr(
          EXPORTING
            i_lgort         = v_lgort
          RECEIVING
            r_deposito_eudr = r_eudr ).

        IF r_eudr IS INITIAL.
          r_eudr = zcl_eudr_utils=>lc_n_eudr.
          EXIT.
        ENDIF.

      ELSE.
*        r_eudr = 'A'.
        CLEAR r_eudr.
        EXIT.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD check_pais_destino_eudr.

    CLEAR: is_eudr.

    CHECK i_sdt0174 IS NOT INITIAL.

    SELECT COUNT( * )
      FROM zpais
      FOR ALL ENTRIES IN @i_sdt0174
      WHERE land1 EQ @i_sdt0174-destino_country
        AND continente EQ @lc_europa.

    CHECK sy-subrc IS INITIAL.

    is_eudr = abap_true.

  ENDMETHOD.


  METHOD check_regra_solicitacao_eudr.

    DATA: lwa_valida_ov_eudr  TYPE  zstruct_post_valida_ov_eudr.

    CLEAR: c_zsdt0158-lgort. "Deposito é determinado por esse metodo

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = |Realizando validações EUDR...|.


*--------------------------------------------------------------------------------------------------------------------------*
*   Montagem Dados Requisição
*--------------------------------------------------------------------------------------------------------------------------*
    CLEAR: lwa_valida_ov_eudr.

    TRY.
        DATA(lwa_zparam_cont_fret) = zcl_util_sd=>get_parametros_solicitacao_ov( i_zsdt0158 = c_zsdt0158 ).
      CATCH cx_root.
    ENDTRY.



    lwa_valida_ov_eudr-document_type    = SWITCH #( i_tipo WHEN lc_formacao_lote OR lc_industrializacao THEN lc_ov
                                                          WHEN lc_pedido                                THEN lc_pd ).

    lwa_valida_ov_eudr-document_sub_type = SWITCH #( i_tipo WHEN lc_formacao_lote OR lc_industrializacao THEN lwa_zparam_cont_fret-auart
                                                            WHEN lc_pedido                               THEN lc_zub ).

    lwa_valida_ov_eudr-code_company        = c_zsdt0158-bukrs.
    lwa_valida_ov_eudr-code_branch         = c_zsdt0158-filial.
    lwa_valida_ov_eudr-identifier          = |SV{ c_zsdt0158-sequencial }|.

    lwa_valida_ov_eudr-code_product        = c_zsdt0158-id_produto.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = lwa_valida_ov_eudr-code_product
      IMPORTING
        output = lwa_valida_ov_eudr-code_product.

    lwa_valida_ov_eudr-year                = c_zsdt0158-safra.

    lwa_valida_ov_eudr-boarding_place      = c_zsdt0158-id_ponto_coleta.
    lwa_valida_ov_eudr-terminal_code       = c_zsdt0158-id_terminal.
    lwa_valida_ov_eudr-transshipment_code  = c_zsdt0158-id_local_destino.

    "Determina se é Processo CIF ou FOB pelo Ponto de Coleta da Solicitação
    TRY.
        zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = lwa_valida_ov_eudr-boarding_place
        )->ck_parceiro_local_negocio(  IMPORTING e_j_1bbranch = DATA(is_branch) ).

        DATA(lc_tipo_processo) = lc_cif.
      CATCH zcx_parceiros.
        lc_tipo_processo = lc_fob.
    ENDTRY.

    lwa_valida_ov_eudr-freight_type = lc_tipo_processo.

*--------------------------------------------------------------------------------------------------------------------------*
*   Inicia Chamadas API
*--------------------------------------------------------------------------------------------------------------------------*

    CASE lc_tipo_processo.
      WHEN lc_fob.

        CLEAR: c_zsdt0158-eudr. "Na Primeira requisição não manda deposito...

        "Verifica se Atende a EUDR
        DATA(lv_atende) = zcl_eudr_utils=>processa_api_solicitacao_eudr( EXPORTING i_zsdt0158 = c_zsdt0158 CHANGING c_valida_ov_eudr = lwa_valida_ov_eudr ). "Realiza 1ª Requisição API

        CASE i_tipo.
          WHEN lc_industrializacao.

            c_zsdt0158-eudr = lc_s_eudr.

          WHEN lc_formacao_lote.

            c_zsdt0158-eudr = lc_a_hidrido.

          WHEN lc_pedido.

            IF lv_atende IS NOT INITIAL.
              c_zsdt0158-eudr = lc_s_eudr.
            ELSE.
              c_zsdt0158-eudr = lc_n_eudr.
            ENDIF.

          WHEN OTHERS.
            MESSAGE 'Tipo solicitação não previsto!' TYPE 'E'.

        ENDCASE.

        "Depois de determinado se atende EUDR, realiza 2ª requisição para validação final com deposito
        zcl_eudr_utils=>processa_api_solicitacao_eudr( EXPORTING i_zsdt0158 = c_zsdt0158 CHANGING c_valida_ov_eudr = lwa_valida_ov_eudr ).

      WHEN lc_cif.

        "// API que verifica se a Filial esta participando do EUDR
        DATA(lv_classificacao_filial) = zcl_eudr_utils=>ck_filial_classificacao_eudr( i_werks = lwa_valida_ov_eudr-code_branch ).

        IF lv_classificacao_filial NE lc_s_eudr AND
           lv_classificacao_filial NE lc_n_eudr AND
           lv_classificacao_filial NE lc_h_hidrido.
          MESSAGE |Classificação: { lv_classificacao_filial } da filial: { lwa_valida_ov_eudr-code_branch } não prevista!| TYPE 'E'.
          RETURN.
        ENDIF.

        IF lv_classificacao_filial NE lc_h_hidrido. "Filiais Hibridas no embarque CIF, o usuario determina a Classificação EUDR de forma manual...
          CLEAR: c_zsdt0158-eudr.
        ENDIF.

        CASE i_tipo.

          WHEN lc_industrializacao.

            c_zsdt0158-eudr = lc_s_eudr.

          WHEN lc_formacao_lote.

            CASE lv_classificacao_filial.
              WHEN lc_s_eudr. "100% EUDR

                c_zsdt0158-eudr = lc_s_eudr.

              WHEN lc_h_hidrido.  "Hibrido

                "// Usuario Defini se Atende ou não EUDR
                IF c_zsdt0158-eudr NE lc_s_eudr AND
                   c_zsdt0158-eudr NE lc_n_eudr.
                  MESSAGE 'Classificação da solicitação deve ser "EUDR" ou "Não EUDR"!' TYPE 'E'.
                  RETURN.
                ENDIF.

              WHEN lc_n_eudr. "Não Atende
                c_zsdt0158-eudr = lc_n_eudr.
            ENDCASE.

          WHEN lc_pedido.

            CASE lv_classificacao_filial.
              WHEN lc_s_eudr. "100% EUDR

                c_zsdt0158-eudr = lc_s_eudr.

              WHEN lc_h_hidrido.  "Hibrido

                "// Usuario Defini se Atende ou não EUDR
                IF c_zsdt0158-eudr NE lc_s_eudr AND
                   c_zsdt0158-eudr NE lc_n_eudr.
                  MESSAGE 'Classificação da solicitação deve ser "EUDR" ou "Não EUDR"!' TYPE 'E'.
                  RETURN.
                ENDIF.

              WHEN lc_n_eudr. "Não Atende
                c_zsdt0158-eudr = lc_n_eudr.
            ENDCASE.


          WHEN OTHERS.
            MESSAGE 'Tipo solicitação não previsto!' TYPE 'E'.
        ENDCASE.

        zcl_eudr_utils=>processa_api_solicitacao_eudr( EXPORTING i_zsdt0158 = c_zsdt0158 CHANGING c_valida_ov_eudr = lwa_valida_ov_eudr ).

    ENDCASE.


    CASE c_zsdt0158-eudr.
      WHEN lc_s_eudr OR lc_n_eudr.

        CASE c_zsdt0158-tp_producao.
          WHEN 'C'.
            DATA(lva_tp_produto) = 'CO'.
          WHEN 'R'.
            lva_tp_produto = 'RR'.
          WHEN OTHERS.
            lva_tp_produto = space.
        ENDCASE.

        TRY.
            zcl_deposito=>zif_deposito~get_instance(
                     )->get_deposito_material_filial(
                     EXPORTING
                       i_matnr          = c_zsdt0158-id_produto
                       i_tp_produto     = CONV #( lva_tp_produto )
                       i_bukrs          = c_zsdt0158-bukrs
                       i_branch         = c_zsdt0158-filial
                       i_eudr           = c_zsdt0158-eudr
                     IMPORTING
                       e_lgort          = c_zsdt0158-lgort  ).

          CATCH zcx_deposito INTO DATA(zcx_deposito). " Classe de Erro de Depósito
            DATA(lva_msg_error) = zcx_deposito->zif_error~get_msg_erro( ).
            MESSAGE lva_msg_error TYPE 'E'.
        ENDTRY.

      WHEN lc_a_hidrido.
        CLEAR: c_zsdt0158-lgort.
      WHEN OTHERS.
        MESSAGE 'Não foi possivel determinar a classificação EUDR da solicitação!' TYPE 'E'.
    ENDCASE.



  ENDMETHOD.


  METHOD ck_filial_classificacao_eudr.

    CHECK i_werks IS NOT INITIAL.

    zcl_eudr_utils=>get_filial_eudr(
      EXPORTING
        i_werks = i_werks
      RECEIVING
        r_eudr  = DATA(ls_filial)
    ).

*"// Classificação da filial
* S - Sim - Atende 100% EUDR
* N - Não - Não atende EUDR
* A - Hibrido

    CHECK ls_filial-atendeeudr IS NOT INITIAL.

    r_classificacao = ls_filial-atendeeudr.

  ENDMETHOD.


  METHOD ck_filial_participante_eudr.

    CHECK i_werks IS NOT INITIAL.

    zcl_eudr_utils=>get_filial_eudr(
      EXPORTING
        i_werks = i_werks
      RECEIVING
        r_eudr  = DATA(ls_filial)
    ).

*"// A filial é participante EUDR
* S - Sim - EUDR
* N - Não - Não EUDR

    CHECK ls_filial-participanteeudr IS NOT INITIAL.

    r_participante = ls_filial-participanteeudr.

  ENDMETHOD.


  METHOD create_arquivo_geojson.

    DATA: lva_file_geojson TYPE string.

    CLEAR: e_msg_error, e_file_geojson, lva_file_geojson.

    CHECK i_id_nomeacao IS NOT INITIAL.

    zcl_eudr_utils=>get_arquivo_geoson(
      EXPORTING
        i_id_nomeacao = i_id_nomeacao
      RECEIVING
        e_file_geoson = lva_file_geojson
    ).

    IF lva_file_geojson IS NOT INITIAL.
      e_file_geojson = lva_file_geojson.
      RETURN.
    ENDIF.

    zcl_eudr_utils=>check_generation_file_geojson(
      EXPORTING
        i_id_nomeacao = i_id_nomeacao
      RECEIVING
        r_ok          = DATA(is_ok)
    ).

    CHECK is_ok EQ abap_true.

*  "// Geração do Arquivo GEOJSON
    zcl_eudr_utils=>get_protocolos_eudr_nomeacao(
      EXPORTING
        i_id_nomeacao           = i_id_nomeacao
      IMPORTING
        e_error                 = DATA(is_error)
        e_msg_error             = e_msg_error
      RECEIVING
        r_protocolos_documentos = DATA(r_protocolos_documentos)
    ).

    IF e_msg_error IS NOT INITIAL.
      MESSAGE e_msg_error TYPE 'E'.
      RETURN.
    ENDIF.

    IF r_protocolos_documentos IS INITIAL.
      MESSAGE 'Procolos EUDR não foram encontrados!' TYPE 'E'.
      RETURN.
    ENDIF.

    zcl_eudr_utils=>processa_api_protocolo_eudr(
      EXPORTING
        i_protocolo = r_protocolos_documentos
      RECEIVING
        r_geojson   = lva_file_geojson
    ).

    CHECK lva_file_geojson IS NOT INITIAL.

    at_zsdt0366 =
    VALUE #(
              id_nomeacao = i_id_nomeacao
              file_json   = lva_file_geojson
              user_create = sy-uname
              date_create = sy-datum
              time_create = sy-uzeit
           ).

    INSERT zsdt0366 FROM at_zsdt0366.

    IF sy-subrc EQ 0.
      e_file_geojson = lva_file_geojson.
      COMMIT WORK.
      MESSAGE 'Arquivo GeoJson criado com sucesso!' TYPE 'S'.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.


  METHOD download_arquivo_geojson.

    DATA: lv_folder TYPE string.

    DATA: l_txt_output TYPE TABLE OF string,
          l_string     TYPE string.

    zcl_eudr_utils=>get_arquivo_geoson(
      EXPORTING
        i_id_nomeacao = i_id_nomeacao
      RECEIVING
        e_file_geoson = DATA(e_geojson)
    ).

    IF e_geojson IS INITIAL.

      zcl_eudr_utils=>get_dues_eudr_from_nomeacao(
        EXPORTING
          i_id_nomeacao = i_id_nomeacao
        RECEIVING
          r_dues        = DATA(r_dues) ).

      IF r_dues[] IS NOT INITIAL.
        e_geojson = zcl_eudr_utils=>create_arquivo_geojson( i_id_nomeacao  = i_id_nomeacao ).
      ENDIF.

      IF e_geojson IS INITIAL.
        MESSAGE 'Arquivo GEOJSON não encontrado!' TYPE 'I'.
        RETURN.
      ENDIF.

    ENDIF.

    APPEND e_geojson TO l_txt_output.

    CALL METHOD cl_gui_frontend_services=>directory_browse
      EXPORTING
        window_title         = 'Download Arquivo GEOJSON'
        initial_folder       = 'C:/'
      CHANGING
        selected_folder      = lv_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

    lv_folder = |{ lv_folder }/GEOJSON_{ sy-datum }_{ sy-uzeit }.txt|.

    TRY.
        cl_gui_frontend_services=>gui_download( EXPORTING filename = CONV #( lv_folder )
                                                          filetype = 'ASC'
                                                CHANGING  data_tab = l_txt_output ).
      CATCH cx_root INTO DATA(e_text).
        MESSAGE e_text->get_text( ) TYPE 'I'.
    ENDTRY.

  ENDMETHOD.


  METHOD get_arquivo_geoson.

    SELECT SINGLE file_json
      FROM zsdt0366
      INTO e_file_geoson
      WHERE id_nomeacao EQ i_id_nomeacao.

  ENDMETHOD.


  METHOD get_dues_eudr_from_nomeacao.

    DATA: r_id_due TYPE RANGE OF zsdt0170-id_due.

    CLEAR: r_dues[].

    SELECT
        'I'    AS sign,
        'EQ'   AS option,
        id_due AS low
      FROM znom_reme_notas AS a
      INTO TABLE @r_id_due
      WHERE a~id_nomeacao_tran = @i_id_nomeacao
        AND EXISTS ( SELECT *
                      FROM zsdt0170 AS b
                       WHERE b~id_due EQ a~id_due
                         AND b~eudr   EQ @lc_sim ).
    SELECT
        'I'    AS sign,
        'EQ'   AS option,
        id_due AS low
      FROM znom_remetente AS a
        APPENDING TABLE @r_id_due
      WHERE id_nomeacao_tran EQ @i_id_nomeacao
        AND EXISTS ( SELECT * FROM zsdt0170 AS b
                        WHERE b~id_due EQ a~id_due
                          AND b~eudr   EQ @lc_sim ).

    CHECK r_id_due IS NOT INITIAL.

    SELECT *
      FROM zsdt0170
      INTO TABLE r_dues
      WHERE id_due IN r_id_due
        AND eudr   EQ lc_sim.

  ENDMETHOD.


  METHOD get_filial_eudr.

    DATA: lwa_return_erro TYPE zstruct_return_api_eudr.

    CLEAR: r_eudr.

    CHECK i_werks IS NOT INITIAL.

    SELECT SINGLE *
      FROM zsdt0367 INTO @DATA(lwa_zsdt0367)
      WHERE branch    EQ @i_werks
        AND dt_update EQ @sy-datum.

    IF sy-subrc EQ 0.
      r_eudr-atendeeudr       = lwa_zsdt0367-atende_eudr.
      r_eudr-participanteeudr = lwa_zsdt0367-participante_eudr.
      RETURN.
    ENDIF.

    gs_filial-idfilialsap = i_werks.

    TRY.
        "// Verifica se a Filial esta participando da EUDR
        zcl_int_ob_get_par_filial_eudr=>zif_integracao_outbound~get_instance( )->execute_request(
        EXPORTING
          i_info_request           = gs_filial
        IMPORTING
          e_id_integracao          = DATA(resul_id)
          e_integracao             = DATA(result_json)
        ).
        "// Recupera os dados do JSON
        IF result_json IS NOT INITIAL.
          /ui2/cl_json=>deserialize( EXPORTING json = result_json-ds_data_retorno CHANGING data = gs_retorno_filial_eudr ).
        ENDIF.

        IF gs_retorno_filial_eudr-success EQ abap_true.
          IF gs_retorno_filial_eudr-data IS NOT INITIAL.
            r_eudr-atendeeudr = gs_retorno_filial_eudr-data[ 1 ]-filialatendeeudr.
            r_eudr-participanteeudr = gs_retorno_filial_eudr-data[ 1 ]-filialparticipanteeudr.

            lwa_zsdt0367-branch             = i_werks.
            lwa_zsdt0367-atende_eudr        = r_eudr-atendeeudr.
            lwa_zsdt0367-participante_eudr  = r_eudr-participanteeudr.
            lwa_zsdt0367-dt_update          = sy-datum.
            MODIFY zsdt0367 FROM lwa_zsdt0367.
            COMMIT WORK.

          ENDIF.
        ENDIF.

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'E' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
        RETURN.
      CATCH zcx_error INTO DATA(zcx_error).

         IF resul_id IS NOT INITIAL.
          SELECT SINGLE ds_data_retorno
            FROM zintegracao_log INTO @DATA(lva_data_return_erro)
            WHERE id_integracao EQ @resul_id.

          IF sy-subrc EQ 0 AND lva_data_return_erro IS NOT INITIAL.
            /ui2/cl_json=>deserialize( EXPORTING json = lva_data_return_erro CHANGING data = lwa_return_erro ).

            IF lwa_return_erro-message IS NOT INITIAL.
              MESSAGE |(WSC SIGAM) - { lwa_return_erro-message } | TYPE 'E'.
            ENDIF.
          ENDIF.
        ENDIF.

        MESSAGE ID zcx_error->msgid TYPE 'E' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
        RETURN.
    ENDTRY.


  ENDMETHOD.


  METHOD get_nfs_ent_nom_vol_filial.

    DATA: lt_remetentes_filial TYPE TABLE OF zemremetente.

    CLEAR: r_documentos[], e_error, e_msg_error.

    CHECK i_dues[] IS NOT INITIAL.

    SELECT docnum_rt
      FROM znom_remetente AS a
      INTO CORRESPONDING FIELDS OF TABLE @lt_remetentes_filial
      FOR ALL ENTRIES IN @i_dues
      WHERE id_due EQ @i_dues-id_due
        AND NOT EXISTS (  SELECT id_due
                            FROM znom_reme_notas AS b
                           WHERE b~id_nomeacao_tran EQ a~id_nomeacao_tran
                             AND b~id_empresa       EQ a~id_empresa
                             AND b~id_filial        EQ a~id_filial
                             AND b~id_material      EQ a~id_material
                             AND b~id_remetente     EQ a~id_remetente
                             AND b~grp_retorno      EQ a~grp_retorno ).

    CHECK lt_remetentes_filial[] IS NOT INITIAL.

    LOOP AT lt_remetentes_filial ASSIGNING FIELD-SYMBOL(<fs_remetentes_filial>).
      <fs_remetentes_filial>-docnum_ret = <fs_remetentes_filial>-docnum_rt.
    ENDLOOP.


    SELECT docnum, docnum_ret
      FROM zsdt_retlote
      INTO TABLE @DATA(lt_nf_retorno_simbolico)
      FOR ALL ENTRIES IN @lt_remetentes_filial
      WHERE docnum_ret EQ @lt_remetentes_filial-docnum_ret.

    IF lt_nf_retorno_simbolico[] IS INITIAL.
      e_msg_error = |Retorno Simbolicos pendendes de geração na nomeação!|.
      e_error = abap_true.
      RETURN.
    ENDIF.

    SELECT docnum, seq_lcto
      FROM zfiwrt0008
      INTO TABLE @DATA(lt_nf_writer_retorno)
      FOR ALL ENTRIES IN @lt_nf_retorno_simbolico
      WHERE docnum EQ @lt_nf_retorno_simbolico-docnum.

    IF lt_nf_writer_retorno[] IS NOT INITIAL.

      SELECT id_boletim, branch, charg, id_agrp, seqlcto_rfl_01, seqlcto_rfl_02, seqlcto_rfl_03
        FROM zsdt0252
        INTO TABLE @DATA(lt_itens_boletim_prod)
        FOR ALL ENTRIES IN @lt_nf_writer_retorno
        WHERE seqlcto_rfl_01 EQ @lt_nf_writer_retorno-seq_lcto OR
              seqlcto_rfl_02 EQ @lt_nf_writer_retorno-seq_lcto OR
              seqlcto_rfl_03 EQ @lt_nf_writer_retorno-seq_lcto.

      IF lt_itens_boletim_prod[] IS NOT INITIAL.

        SELECT id_boletim, branch,  charg, id_agrp, docnum
          FROM zsdt0249
          INTO TABLE @DATA(lt_nfs_itens_boletim_prod)
          FOR ALL ENTRIES IN @lt_itens_boletim_prod
          WHERE id_boletim EQ @lt_itens_boletim_prod-id_boletim
            AND branch     EQ @lt_itens_boletim_prod-branch
            AND charg      EQ @lt_itens_boletim_prod-charg
            AND id_agrp    EQ @lt_itens_boletim_prod-id_agrp.

        IF lt_nfs_itens_boletim_prod[] IS NOT INITIAL.

          SELECT docnum_flote , docnum_eprod
            FROM zsdtvinc_p_flote
            INTO TABLE @DATA(lt_nfs_vinculo)
            FOR ALL ENTRIES IN @lt_nfs_itens_boletim_prod
            WHERE docnum_flote EQ @lt_nfs_itens_boletim_prod-docnum
              AND cancel       EQ @abap_false
              AND vinc_virtual EQ @abap_false.

          IF lt_nfs_vinculo[] IS NOT INITIAL.

            "//  Fluxo 1 - NF do Produtor
            SELECT docnum
              FROM zsdtprod_flote
              INTO TABLE @DATA(lt_nfs_entrada_fluxo_01)
              FOR ALL ENTRIES IN @lt_nfs_vinculo
              WHERE docnum EQ @lt_nfs_vinculo-docnum_eprod
                AND cancel EQ @abap_false.

*          *"// Fluxo 2 - NF do Produtor
*              SELECT docnum
*                FROM zsdtflote_flote
*                INTO TABLE @DATA(lt_nfs_saida_recusa)
*                FOR ALL ENTRIES IN @lt_nfs_vinculo
*                WHERE docnum EQ @lt_nfs_vinculo-docnum_eprod
*                  AND cancel EQ @abap_false.
*
*              IF lt_nfs_saida_recusa[] IS NOT INITIAL.
*                SELECT docnum_flote, docnum_eprod
*                  FROM zsdtvinc_p_flote
*                  INTO TABLE @DATA(lt_nfs_saida_recusa_vinculo)
*                  FOR ALL ENTRIES IN @lt_nfs_saida_recusa
*                  WHERE docnum_flote EQ @lt_nfs_saida_recusa-docnum
*                    AND cancel       EQ @abap_false.
*
*                IF lt_nfs_saida_recusa_vinculo[] IS NOT INITIAL.
*                  SELECT docnum
*                    FROM zsdtprod_flote
*                    INTO TABLE @DATA(lt_nfs_entrada_fluxo_02)
*                    FOR ALL ENTRIES IN @lt_nfs_saida_recusa_vinculo
*                    WHERE docnum EQ @lt_nfs_saida_recusa_vinculo-docnum_eprod
*                      AND cancel EQ @abap_false.
*                ENDIF.
*
*              ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT lt_nf_retorno_simbolico INTO DATA(ls_nf_retorno_simbolico).

      DATA(achou_nf_entrada) = abap_false.

      READ TABLE lt_nf_writer_retorno INTO DATA(lwa_nf_writer_retorno) WITH KEY docnum = ls_nf_retorno_simbolico-docnum.
      CHECK sy-subrc EQ 0.

      DATA(achou_item) = abap_false.
      LOOP AT lt_itens_boletim_prod INTO DATA(lwa_itens_boletim_prod) WHERE seqlcto_rfl_01 = lwa_nf_writer_retorno-seq_lcto OR
                                                                            seqlcto_rfl_02 = lwa_nf_writer_retorno-seq_lcto OR
                                                                            seqlcto_rfl_03 = lwa_nf_writer_retorno-seq_lcto.
        achou_item = abap_true.
        EXIT.
      ENDLOOP.

      CHECK achou_item EQ abap_true.

      LOOP AT lt_nfs_itens_boletim_prod INTO DATA(lwa_nfs_itens_boletim_prod) WHERE id_boletim EQ lwa_itens_boletim_prod-id_boletim
                                                                                AND branch     EQ lwa_itens_boletim_prod-branch
                                                                                AND charg      EQ lwa_itens_boletim_prod-charg
                                                                                AND id_agrp    EQ lwa_itens_boletim_prod-id_agrp.

        LOOP AT lt_nfs_vinculo INTO DATA(lwa_nfs_vinculo) WHERE docnum_flote = lwa_nfs_itens_boletim_prod-docnum.
          LOOP AT lt_nfs_entrada_fluxo_01 INTO DATA(lwa_nfs_entrada_fluxo_01) WHERE docnum = lwa_nfs_vinculo-docnum_eprod.
            APPEND VALUE #( docnum = lwa_nfs_entrada_fluxo_01-docnum ) TO r_documentos.
            achou_nf_entrada = abap_true.
          ENDLOOP.
        ENDLOOP.

      ENDLOOP.

      IF achou_nf_entrada EQ abap_false.
        e_msg_error = |Não foi possivel localizar o romaneio de entrada para a Saida Docnum: { ls_nf_retorno_simbolico-docnum }|.
        e_error = abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.



  ENDMETHOD.


  METHOD get_protocolos_eudr_nomeacao.

    DATA: lt_remetentes_filial TYPE TABLE OF zemremetente.

    CLEAR: r_protocolos_documentos[], e_error, e_msg_error.

    CHECK i_id_nomeacao IS NOT INITIAL.

    zcl_eudr_utils=>get_dues_eudr_from_nomeacao(
      EXPORTING
        i_id_nomeacao = i_id_nomeacao
      RECEIVING
        r_dues        = DATA(r_dues)
    ).

    CHECK r_dues IS NOT INITIAL.

*-----------------------------------------------------------------------------------------------------------------------------*
*   Seleção Notas Produtor - Inicio
*-----------------------------------------------------------------------------------------------------------------------------*

    SELECT docnum
      FROM znom_reme_notas
      INTO TABLE @DATA(lt_notas_entrada)
      FOR ALL ENTRIES IN @r_dues
      WHERE id_due EQ @r_dues-id_due.

*-----------------------------------------------------------------------------------------------------------------------------*
*   Seleção notas do Produtor a partir das Notas da Emissão da Filial - Inicio
*-----------------------------------------------------------------------------------------------------------------------------*
    zcl_eudr_utils=>get_nfs_ent_nom_vol_filial( EXPORTING
                                                   i_dues = r_dues
                                                IMPORTING
                                                   e_error      =  e_error
                                                   e_msg_error  =  e_msg_error
                                                RECEIVING
                                                    r_documentos = DATA(r_nfs_entrada_filial) ).

    CHECK e_error is INITIAL.

    LOOP AT r_nfs_entrada_filial INTO DATA(lwa_nfs_entrada_filial).
      append VALUE #( docnum =  lwa_nfs_entrada_filial-docnum ) to lt_notas_entrada.
    ENDLOOP.

*-----------------------------------------------------------------------------------------------------------------------------*
*   Seleção notas do Produtor a partir das Notas da Emissão da Filial - Fim
*-----------------------------------------------------------------------------------------------------------------------------*

    LOOP AT lt_notas_entrada INTO DATA(ls_notas_entrada).

      zcl_les_utils=>get_romaneio_documento_fiscal(
        EXPORTING
          i_docnum   = ls_notas_entrada-docnum
        RECEIVING
          r_romaneio = DATA(ls_romaneio_entrada)
      ).

      IF ls_romaneio_entrada IS INITIAL.
        e_msg_error = |Romaneio de entrada do documento fiscal: { ls_notas_entrada-docnum } nao foi encontrado!|.
        e_error = abap_true.
        RETURN.
      ENDIF.

      IF ls_romaneio_entrada-protocolo_eudr IS INITIAL.
        e_msg_error = |Protocolo EUDR do romaneio: { ls_romaneio_entrada-nr_romaneio } Filial: { ls_romaneio_entrada-branch } Safra: { ls_romaneio_entrada-nr_safra } Chave: { ls_romaneio_entrada-ch_referencia } nao foi encontrado!|.
        e_error = abap_true.
        RETURN.
      ELSE.
        APPEND VALUE #( protocolo = ls_romaneio_entrada-protocolo_eudr ) TO r_protocolos_documentos.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_textos_eudr_nf_saida.

    SELECT SINGLE docnum, model
      FROM j_1bnfdoc INTO @DATA(lwa_doc)
     WHERE docnum EQ @i_docnum.

    CHECK sy-subrc EQ 0.

    CHECK zcl_eudr_utils=>check_doc_fiscal_eudr( i_docnum = i_docnum ) = 'S'.

    CASE lwa_doc-model.
      WHEN '55'.

        SELECT SINGLE low
          FROM tvarvc INTO  r_textos
         WHERE name = 'EUDR_TEXTO_NF_SAIDA'.

        CHECK sy-subrc EQ 0.

        SELECT SINGLE *
          FROM j_1bnfnad INTO @DATA(lwa_parc_z1)
         WHERE docnum EQ @i_docnum
           AND parvw  EQ 'LR'.

        IF sy-subrc EQ 0.
          r_textos = |*****{ r_textos } { lwa_parc_z1-name1 }*****|.
        ELSE.
          r_textos = |*****{ r_textos }*****|.
        ENDIF.

      WHEN '57'.

        SELECT SINGLE low
          FROM tvarvc INTO  r_textos
         WHERE name = 'EUDR_TEXTO_NF_SAIDA'.

        CHECK sy-subrc EQ 0.

        r_textos = |*****{ r_textos }*****|.

    ENDCASE.





  ENDMETHOD.


  METHOD processa_api_protocolo_eudr.

    TRY.
        zcl_int_ob_get_protocolo_eudr=>zif_integracao_outbound~get_instance( )->execute_request(
          EXPORTING
            i_info_request         = i_protocolo
          IMPORTING
          e_id_integracao          = DATA(resul_id)
          e_integracao             = DATA(result_json)
        ).
      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'E' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
        RETURN.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'E' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
        RETURN.
    ENDTRY.

*"// Recupera os dados do JSON
    IF result_json IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = result_json-ds_data_retorno CHANGING data = gs_retorno_protocolo_eudr ).
    ENDIF.

    zcl_eudr_utils=>fill_geojson(
      EXPORTING
        i_geojson_eudr = gs_retorno_protocolo_eudr
      RECEIVING
        r_geojson      = r_geojson
    ).

  ENDMETHOD.


  METHOD processa_api_solicitacao_eudr.

    DATA: lwa_return_erro TYPE zstruct_return_api_eudr.
    DATA: lv_msg_string TYPE string.

    CLEAR: r_atende_eudr.

    FIELD-SYMBOLS: <fs_has_deposity_regulation> TYPE abap_bool.

    CASE i_zsdt0158-eudr.
      WHEN 'S' OR 'N'.

        CREATE DATA c_valida_ov_eudr-has_deposity_regulation TYPE abap_bool.
        ASSIGN c_valida_ov_eudr-has_deposity_regulation->* TO <fs_has_deposity_regulation>.

        CASE i_zsdt0158-eudr.
          WHEN 'S'.
            <fs_has_deposity_regulation> = abap_true.
          WHEN 'N'.
            <fs_has_deposity_regulation> = abap_false.
        ENDCASE.
      WHEN 'A'.
    ENDCASE.


    TRY.
        zcl_int_ob_valida_ov_eudr=>zif_integracao_outbound~get_instance( )->execute_request(
             EXPORTING
               i_info_request           = c_valida_ov_eudr
             IMPORTING
               e_id_integracao          = DATA(resul_id)
               e_integracao             = DATA(result_json)
             ).
      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'E' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
        RETURN.
      CATCH zcx_error INTO DATA(zcx_error).

        IF resul_id IS NOT INITIAL.
          SELECT SINGLE ds_data_retorno
            FROM zintegracao_log INTO @DATA(lva_data_return_erro)
            WHERE id_integracao EQ @resul_id.

          IF sy-subrc EQ 0 AND lva_data_return_erro IS NOT INITIAL.
            /ui2/cl_json=>deserialize( EXPORTING json = lva_data_return_erro CHANGING data = lwa_return_erro ).

            IF lwa_return_erro-data IS NOT INITIAL.
              MESSAGE |(WSC EUDR) { lwa_return_erro-data } | TYPE 'E'.
            ENDIF.
          ENDIF.
        ENDIF.

        MESSAGE ID zcx_error->msgid TYPE 'E' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
        RETURN.
    ENDTRY.


    "// Recupera os dados do JSON
    /ui2/cl_json=>deserialize( EXPORTING json = result_json-ds_data_retorno CHANGING data = gs_retorno_valida_ov_eudr ).

    IF gs_retorno_valida_ov_eudr IS INITIAL.
      MESSAGE 'Retorno da Consulta ao Serviço EUDR(Middleware) está inválido ou vazio!' TYPE 'E'.
      RETURN.
    ENDIF.

*"// Esta Bloqueado
    IF gs_retorno_valida_ov_eudr-is_blocked IS NOT INITIAL.

      IF lines( gs_retorno_valida_ov_eudr-validations ) > 1.
        LOOP AT gs_retorno_valida_ov_eudr-validations INTO DATA(lwa_validation).
          DATA(tabix) = sy-tabix.
          lv_msg_string = |(WSC EUDR) Mensagem: { tabix } - { lwa_validation-description } |.
          MESSAGE lv_msg_string TYPE 'I'.
        ENDLOOP.
      ENDIF.

      READ TABLE gs_retorno_valida_ov_eudr-validations INTO lwa_validation WITH KEY is_block = abap_true.
      IF SY-SUBRC EQ 0.
        DATA(lv_msg) = lwa_validation-description.
      ELSE.
        lv_msg = gs_retorno_valida_ov_eudr-validations[ 1 ]-description.
      ENDIF.

      MESSAGE |(WSC EUDR) { lv_msg } | TYPE 'E'.
    ENDIF.

*"// Não esta Bloqueado
    IF gs_retorno_valida_ov_eudr-is_regulamentation IS NOT INITIAL.
      r_atende_eudr = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD FILL_GEOJSON.

    FIELD-SYMBOLS: <fs_campo1> TYPE string.
    DATA: ls_geojson_eudr TYPE zstruct_geojson_eudr.
    DATA: lva_cpf   TYPE c LENGTH 11.
    DATA: lva_cnpj  TYPE c LENGTH 14.

    CLEAR: gt_fazenda_final[].

    LOOP AT i_geojson_eudr INTO DATA(ls_retorno_protocolo_eudr).

      "---------------------------------------------------------------------------
      " Populando tabela de Produtores
      "---------------------------------------------------------------------------

      IF ls_retorno_protocolo_eudr-farmer_document IS NOT INITIAL.

        CLEAR: gs_produtor.
        gs_produtor-protocolo       = ls_retorno_protocolo_eudr-code.
        gs_produtor-farmer_document = ls_retorno_protocolo_eudr-farmer_document.
        gs_produtor-ie              = ls_retorno_protocolo_eudr-state_registration.

        APPEND gs_produtor TO gt_produtor.

      ELSE.

        LOOP AT ls_retorno_protocolo_eudr-documents INTO DATA(ls_documents).

          CLEAR: gs_produtor.
          gs_produtor-protocolo       = ls_retorno_protocolo_eudr-code.
          gs_produtor-farmer_document = ls_documents-farmer_document.

          LOOP AT ls_documents-state_registrations INTO DATA(ls_state_registration).
            gs_produtor-ie = ls_state_registration.
            APPEND gs_produtor TO gt_produtor.
          ENDLOOP.

        ENDLOOP.

      ENDIF.

      "---------------------------------------------------------------------------
      " Populando tabela de Fazendas
      "---------------------------------------------------------------------------
      LOOP AT ls_retorno_protocolo_eudr-farm_areas INTO DATA(ls_farm_areas).

        CLEAR: gs_fazenda.

        gs_fazenda-code    = ls_retorno_protocolo_eudr-code.
        gs_fazenda-data    = |{ ls_retorno_protocolo_eudr-date_processed(4) }{ ls_retorno_protocolo_eudr-date_processed+5(2) }{ ls_retorno_protocolo_eudr-date_processed+8(2) }|.
        gs_fazenda-hora    = |{ ls_retorno_protocolo_eudr-date_processed+11(2) }{ ls_retorno_protocolo_eudr-date_processed+14(2) }{ ls_retorno_protocolo_eudr-date_processed+17(2) }|.
        gs_fazenda-farm_id = ls_farm_areas-farm_id.
        gs_fazenda-fazenda = ls_farm_areas-farm_name.
        gs_fazenda-ie      = ls_farm_areas-state_registration.
        gs_fazenda-geojson = ls_farm_areas-areas.

        IF ls_farm_areas-state_registration EQ ls_retorno_protocolo_eudr-state_registration AND
           ls_farm_areas-state_registration IS NOT INITIAL.
          gs_fazenda-farmer_document = ls_retorno_protocolo_eudr-farmer_document.
        ELSE.
          LOOP AT ls_retorno_protocolo_eudr-documents INTO ls_documents.
            IF gs_fazenda-farmer_document IS NOT INITIAL.
              EXIT.
            ENDIF.
            LOOP AT ls_documents-state_registrations INTO ls_state_registration.
              IF ls_state_registration = ls_farm_areas-state_registration.
                gs_fazenda-farmer_document = ls_documents-farmer_document.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
        ENDIF.

        APPEND gs_fazenda TO gt_fazenda.

      ENDLOOP.

    ENDLOOP.

    SORT gt_fazenda BY fazenda.

    LOOP AT gt_fazenda INTO gs_fazenda.
      READ TABLE gt_fazenda_final ASSIGNING FIELD-SYMBOL(<fs_fazenda_final>) WITH KEY farm_id = gs_fazenda-farm_id
                                                                                      ie      = gs_fazenda-ie.
      IF sy-subrc IS INITIAL.
        DATA(_dt_hr_fazenda_final)   = <fs_fazenda_final>-data && <fs_fazenda_final>-hora.
        DATA(_dt_hr_fazenda_current) = gs_fazenda-data && gs_fazenda-hora.

        IF _dt_hr_fazenda_final LT _dt_hr_fazenda_current.
          MOVE-CORRESPONDING gs_fazenda TO <fs_fazenda_final>.
        ENDIF.
      ELSE.
        APPEND gs_fazenda TO gt_fazenda_final.
      ENDIF.
    ENDLOOP.


    LOOP AT gt_produtor ASSIGNING FIELD-SYMBOL(<fs_produtor>).
      TRY .
          zcl_fornecedores=>zif_parceiros~get_instance(
            )->set_parceiro_ie( i_insc_estatual = <fs_produtor>-ie
            )->get_name( IMPORTING e_name = DATA(lva_name_prod)
            )->get_endereco( IMPORTING  e_endereco = DATA(lwa_endereco) ).

          <fs_produtor>-producername = lva_name_prod.
          <fs_produtor>-land1        = lwa_endereco-country.

        CATCH zcx_parceiros INTO DATA(ex_parceiros).
      ENDTRY.
    ENDLOOP.

    ls_geojson_eudr-type = 'FeatureCollection'.

    LOOP AT gt_fazenda_final INTO DATA(ls_fazenda_final).
      LOOP AT ls_fazenda_final-geojson INTO DATA(ls_geojson).

        APPEND INITIAL LINE TO ls_geojson_eudr-features ASSIGNING FIELD-SYMBOL(<fs_features>).

        <fs_features>-type = 'Feature'.
        <fs_features>-properties-productionplace = ls_fazenda_final-fazenda.

        READ TABLE gt_produtor INTO gs_produtor WITH KEY ie              = ls_fazenda_final-ie
                                                         farmer_document = ls_fazenda_final-farmer_document.
        IF sy-subrc IS INITIAL.
          <fs_features>-properties-producername    = gs_produtor-producername.
          <fs_features>-properties-producercountry = gs_produtor-land1.
        ENDIF.

        CREATE DATA <fs_features>-geometry TYPE string.
        ASSIGN <fs_features>-geometry->* TO <fs_campo1>.

        DATA(lvlinha) = lines( ls_fazenda_final-geojson ).

        <fs_campo1> = ls_geojson-geo_json.

      ENDLOOP.

    ENDLOOP.

    /ui2/cl_json=>serialize(
      EXPORTING
        data             = ls_geojson_eudr
        compress         = abap_true
        pretty_name      = abap_true
        assoc_arrays     = abap_true
        ts_as_iso8601    = abap_true
        assoc_arrays_opt = abap_true
        numc_as_string   = abap_true
        conversion_exits = abap_true
        format_output    = abap_true
      RECEIVING
        r_json           = DATA(geojson)
    ).

    REPLACE ALL OCCURRENCES OF '\' IN geojson WITH ''.
    REPLACE ALL OCCURRENCES OF '"{' IN geojson WITH '{'.
    REPLACE ALL OCCURRENCES OF '}"' IN geojson WITH '}'.

    DATA(reader) = cl_sxml_string_reader=>create( cl_abap_codepage=>convert_to( geojson ) ).
    DATA(writer) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
    writer->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    writer->set_option( option = if_sxml_writer=>co_opt_indent ).
    reader->next_node( ).
    reader->skip_node( writer ).

    r_geojson = cl_abap_codepage=>convert_from( CAST cl_sxml_string_writer( writer )->get_output( ) ).

  ENDMETHOD.
ENDCLASS.
