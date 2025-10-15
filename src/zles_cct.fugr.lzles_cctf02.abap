*----------------------------------------------------------------------*
***INCLUDE LZLES_CCTF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP|Request    |Data       |Descrição                              &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN  |DEVK9A2HNK |27/08/2025 |Ajuste Campo DT_RECEPCAO Tab ZLEST0186.&*
*&                                   |Formato vindo como Timestamp. Coverter.&*
*&                                   |Chamado: 189195.                       &*
*&---------------------------------------------------------------------------&*
FORM f_elimina_lfa1_bloq TABLES p_lfa1 STRUCTURE lfa1.

  LOOP AT p_lfa1 INTO DATA(wl_lfa1).
    DATA(_delete) = ''.
    DATA(_tabix)  = sy-tabix.
    TRY.
        zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = wl_lfa1-lifnr
        )->ck_ativo( ).
      CATCH zcx_parceiros INTO DATA(ex_parceiros_k).
        _delete = 'X'.
    ENDTRY.
    IF _delete IS NOT INITIAL.
      DELETE p_lfa1 INDEX _tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM f_elimina_kna1_bloq TABLES p_kna1 STRUCTURE kna1.

  LOOP AT p_kna1 INTO DATA(wl_kna1).
    DATA(_delete) = ''.
    DATA(_tabix)  = sy-tabix.
    TRY.
        zcl_clientes=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = wl_kna1-kunnr
        )->ck_ativo( ).
      CATCH zcx_parceiros INTO DATA(ex_parceiros_d).
        _delete = 'X'.
    ENDTRY.
    IF _delete IS NOT INITIAL.
      DELETE p_kna1 INDEX _tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM f_get_documentos_recepcao USING p_tp_emissor            TYPE c
                            CHANGING p_zlest0146             TYPE zlest0146
                                     p_zlest0147             TYPE zlest0147
                                     p_zlest0168             TYPE zlest0168_t
                                     p_retorno               TYPE zde_retorno_proc.

  RANGES: r_nr_nf            FOR j_1bnfdoc-nfenum,
          r_serie            FOR j_1bnfdoc-series.

  DATA: tg_zsdt0001_e    TYPE TABLE OF zsdt0001 WITH HEADER LINE,
        tg_zsdt0001      TYPE TABLE OF zsdt0001 WITH HEADER LINE,
        tg_doc_rom_e_tmp TYPE TABLE OF j_1bnfdoc WITH HEADER LINE.
  "TG_ZSDT0001_ROM_ENT  TYPE TABLE OF ZSDT0001_ROM_ENT WITH HEADER LINE.

  DATA: v_nr_romaneio  TYPE zsdt0001-nr_romaneio,
        v_candat       TYPE j_1bnfdoc-candat,
        v_c14          TYPE j_1bstcd1,
        v_vbeln        TYPE zdoc_rem,
        wl_zlest0168   TYPE zlest0168,
        v_msg          TYPE string,
        v_nf9          TYPE c LENGTH 9,
        v_nf6          TYPE c LENGTH 6,
        v_docnum_rom_e TYPE j_1bnfdoc-docnum,
        v_refkey_tmp   TYPE j_1bnflin-refkey.

  DATA: it_cfops  TYPE TABLE OF lxhme_range_c10.

  CLEAR: p_retorno, p_zlest0168[].

  IF ( p_tp_emissor NE '1' ) AND
     ( p_tp_emissor NE '2' ).
    MESSAGE s074 INTO v_msg.
    p_retorno-type     = 'E'.
    p_retorno-msgno    = sy-msgno.
    p_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  CASE p_tp_emissor.
    WHEN '1'. "Próprio

      CALL FUNCTION 'ZCCT_GET_DOC_FISCAL'
        EXPORTING
          i_tp_emissor = p_tp_emissor
        CHANGING
          c_zlest0147  = p_zlest0147
          c_retorno    = p_retorno.

      IF p_retorno-type = 'E'.
        RETURN.
      ENDIF.

      IF p_zlest0147-docnum IS INITIAL.
        MESSAGE s075 WITH p_zlest0147-chave_nfe INTO v_msg.
        p_retorno-type     = 'E'.
        p_retorno-msgno    = sy-msgno.
        p_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM j_1bnflin INTO @DATA(_wl_lin_rem_fat)
       WHERE docnum = @p_zlest0147-docnum
         AND reftyp = 'BI'. "Faturamento

      CHECK sy-subrc EQ 0.

      DATA(_get_rom_saida) = abap_false.

      CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
        EXPORTING
          i_docnum = p_zlest0147-docnum
          i_direct = '2'
        IMPORTING
          e_vbeln  = v_vbeln.

      IF v_vbeln IS NOT INITIAL.
        "Busca Romaneio de Saída Formação Lote
        SELECT SINGLE *
          FROM zsdt0001 INTO @DATA(_wl_zsdt0001_s)
         WHERE doc_rem       EQ @v_vbeln
           AND tp_movimento  EQ 'S'.

        IF sy-subrc EQ 0.
          _get_rom_saida = abap_true.
        ENDIF.
      ENDIF.

      IF _get_rom_saida EQ abap_false.
*        MESSAGE S076 WITH P_ZLEST0147-CHAVE_NFE P_ZLEST0147-DOCNUM INTO V_MSG.
*        P_RETORNO-TYPE     = 'E'.
*        P_RETORNO-MSGNO    = SY-MSGNO.
*        P_RETORNO-TEXTO    = V_MSG.
        RETURN.
      ENDIF.

*------------------------------------------------------------------------------------*
*     Rateio Peso p/ NF Produtor
*------------------------------------------------------------------------------------*

      "Buscar Romaneio de Entrada

      "Verifica se é romaneio Completo
      IF _wl_zsdt0001_s-id_referencia IS NOT INITIAL.

        CALL FUNCTION 'Z_MEMO_CFOP_ENTRADAS'
          TABLES
            cfops = it_cfops.

        CHECK it_cfops[] IS NOT INITIAL.

        CLEAR: v_nr_romaneio.

        v_nr_romaneio = _wl_zsdt0001_s-id_referencia.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = v_nr_romaneio
          IMPORTING
            output = v_nr_romaneio.

        "Busca Romaneio Entrada
        CLEAR: tg_zsdt0001_e[].
        SELECT *
          FROM zsdt0001 INTO TABLE tg_zsdt0001_e
         WHERE bukrs        EQ _wl_zsdt0001_s-bukrs
           AND branch       EQ _wl_zsdt0001_s-branch
           AND nr_romaneio  EQ v_nr_romaneio
           AND nr_safra     EQ _wl_zsdt0001_s-nr_safra
           AND tp_movimento EQ 'E'.

        IF ( tg_zsdt0001_e[] IS INITIAL ) AND ( _wl_zsdt0001_s-bukrs EQ '0015' ).
          EXIT.
        ENDIF.

        IF tg_zsdt0001_e[] IS INITIAL.
*          MESSAGE S077 WITH P_ZLEST0147-DOCNUM INTO V_MSG.
*          P_RETORNO-TYPE     = 'E'.
*          P_RETORNO-MSGNO    = SY-MSGNO.
*          P_RETORNO-TEXTO    = V_MSG.
          RETURN.
        ELSEIF ( lines( tg_zsdt0001_e[] ) EQ 1 ).

          READ TABLE tg_zsdt0001_e INDEX 1.

          CHECK tg_zsdt0001_e-ch_referencia IS NOT INITIAL.

          "Se peso Romaneio Saída for igual o Peso Romaneio Entrada
          CHECK ( _wl_zsdt0001_s-peso_liq EQ tg_zsdt0001_e-peso_liq ).

          CLEAR: v_docnum_rom_e.

*          IF ( TG_ZSDT0001_E-DOC_MATERIAL IS NOT INITIAL ) AND
*             ( TG_ZSDT0001_E-NR_SAFRA     IS NOT INITIAL ).
*
*            V_REFKEY_TMP = TG_ZSDT0001_E-DOC_MATERIAL && TG_ZSDT0001_E-NR_SAFRA.
*
*            SELECT SINGLE *
*              FROM J_1BNFLIN AS LI INTO @DATA(WL_LIN_MD_ROM_E)
*             WHERE REFKEY = @V_REFKEY_TMP
*               AND REFTYP = 'MD'
*               AND EXISTS (  SELECT DOCNUM
*                               FROM J_1BNFDOC AS DC
*                              WHERE DC~DOCNUM EQ LI~DOCNUM
*                                AND CANDAT    EQ @V_CANDAT
*                                AND CANCEL    EQ @SPACE ).
*
*            IF SY-SUBRC EQ 0.
*              V_DOCNUM_ROM_E = WL_LIN_MD_ROM_E-DOCNUM.
*            ENDIF.
*
*          ENDIF.

          DATA(_reg_ent_zgr) = abap_false.

          SELECT SINGLE *
            FROM zmmt_ee_zgr INTO @DATA(_wl_ee_zgr)
           WHERE ch_referencia EQ @tg_zsdt0001_e-ch_referencia.

          IF ( sy-subrc EQ 0 ).
            SELECT SINGLE *
              FROM zmmt_ee_zgr_docs AS de INTO @DATA(_wl_ee_zgr_docs)
             WHERE obj_key EQ @_wl_ee_zgr-obj_key
               AND EXISTS (  SELECT docnum
                               FROM j_1bnfdoc AS dc
                              WHERE dc~docnum EQ de~docnum
                                AND candat    EQ @v_candat
                                AND cancel    EQ @space ).

            IF ( sy-subrc EQ 0 ) AND ( _wl_ee_zgr_docs-docnum IS NOT INITIAL ).
              _reg_ent_zgr   = abap_true.
              v_docnum_rom_e = _wl_ee_zgr_docs-docnum.
            ENDIF.
          ENDIF.

          "Localizar Documento Fiscal de Entrada
          IF ( _reg_ent_zgr EQ abap_false ) AND ( v_docnum_rom_e IS INITIAL ).

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

          IF ( v_docnum_rom_e IS INITIAL ).
*            MESSAGE S078 WITH TG_ZSDT0001_E-CH_REFERENCIA INTO V_MSG.
*            P_RETORNO-TYPE     = 'E'.
*            P_RETORNO-MSGNO    = SY-MSGNO.
*            P_RETORNO-TEXTO    = V_MSG.
            RETURN.
          ENDIF.

          SELECT SINGLE *
            FROM j_1bnfdoc INTO @DATA(_wl_doc)
           WHERE docnum   EQ @v_docnum_rom_e
             AND candat   EQ @v_candat
             AND cancel   EQ @space.

          IF ( sy-subrc NE 0 ).
            MESSAGE s072 WITH v_docnum_rom_e INTO v_msg.
            p_retorno-type     = 'E'.
            p_retorno-msgno    = sy-msgno.
            p_retorno-texto    = v_msg.
            RETURN.
          ENDIF.

          "Check CFOP Entrada
          SELECT SINGLE *
            FROM j_1bnflin INTO @DATA(_lin_e)
           WHERE docnum EQ @v_docnum_rom_e
             AND cfop   IN @it_cfops.

          CHECK sy-subrc EQ 0.

          "Popular Tabela Rateio NF
          CLEAR: wl_zlest0168.

          PERFORM f_atrib_chave_docnum USING _wl_doc
                                    CHANGING wl_zlest0168-chave_nfe
                                             wl_zlest0168-chave_nff
                                             p_retorno.

          IF p_retorno-type EQ 'E'.
            RETURN.
          ENDIF.

          IF ( wl_zlest0168-chave_nfe IS INITIAL ) AND
             ( wl_zlest0168-chave_nff IS INITIAL ).
            MESSAGE s073 WITH _wl_doc-docnum INTO v_msg.
            p_retorno-type     = 'E'.
            p_retorno-msgno    = sy-msgno.
            p_retorno-texto    = v_msg.
            RETURN.
          ENDIF.

          wl_zlest0168-docnum       = _wl_doc-docnum.
          wl_zlest0168-peso_aferido = p_zlest0146-peso_aferido_recepcao.

          IF ( wl_zlest0168-peso_aferido <= 0 ) .
            MESSAGE s079 WITH _wl_doc-docnum INTO v_msg.
            p_retorno-type     = 'E'.
            p_retorno-msgno    = sy-msgno.
            p_retorno-texto    = v_msg.
            RETURN.
          ENDIF.

          APPEND wl_zlest0168 TO p_zlest0168.
        ENDIF.
      ENDIF.

      "Quando Possui mais de um Romaneio de Entrada Vinculado ao Romaneio de Saida
*     CLEAR: TG_ZSDT0001_ROM_ENT[].
*     SELECT *
*       FROM ZSDT0001_ROM_ENT INTO TABLE TG_ZSDT0001_ROM_ENT
*      WHERE CH_REFERENCIA EQ _WL_ZSDT0001-CH_REFERENCIA.
*
*     SORT TG_ZSDT0001_ROM_ENT BY ORDEM_VINCULO.
*     CLEAR: V_PESO_AFERIDO.
*     V_PESO_AFERIDO = TG_RECEPCAO_IMP-PESO_AFERIDO_RECEPCAO.
*     DATA(_ERRO_RATEIO) = ABAP_FALSE.
*     LOOP AT TG_ZSDT0001_ROM_ENT.
*       CLEAR: TG_RECEPCAO_IMP_NF_RAT.
*       APPEND TG_RECEPCAO_IMP_NF_RAT.
*     ENDLOOP.
*     IF _ERRO_RATEIO EQ ABAP_TRUE.
*       CONTINUE.
*     ENDIF.

    WHEN '2'. "Terceiro

      CALL FUNCTION 'ZCCT_GET_DOC_FISCAL'
        EXPORTING
          i_tp_emissor = p_tp_emissor
        CHANGING
          c_zlest0147  = p_zlest0147
          c_retorno    = p_retorno.

      IF p_retorno-type = 'E'.
        RETURN.
      ENDIF.

      IF p_zlest0147-docnum IS INITIAL.
        IF p_zlest0147-emissor_cnpj IS NOT INITIAL.
          MESSAGE s083 WITH p_zlest0147-nfnum p_zlest0147-emissor_cnpj INTO v_msg.
        ELSE.
          MESSAGE s084 WITH p_zlest0147-nfnum p_zlest0147-emissor_cpf  INTO v_msg.
        ENDIF.
        p_retorno-type     = 'E'.
        p_retorno-msgno    = sy-msgno.
        p_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

  ENDCASE.


ENDFORM.

FORM f_get_info_parceiro USING p_parid     TYPE j_1bparid
                               p_partyp    TYPE j_1bpartyp
                      CHANGING p_inf_parid TYPE ty_inf_parid.

  DATA: wa_info_c TYPE kna1,
        wa_info_k TYPE lfa1,
        wa_adrc   TYPE adrc.

  CLEAR: p_inf_parid, wa_info_c, wa_info_k, wa_adrc.

  CHECK ( p_parid IS NOT INITIAL ) AND ( p_partyp IS NOT INITIAL ).

  CALL FUNCTION 'Z_PARCEIRO_INFO'
    EXPORTING
      p_parceiro   = p_parid
      p_partype    = p_partyp
      p_endereco   = 'X'
    CHANGING
      wa_info_part = wa_info_k
      wa_info_c    = wa_info_c
      wa_adrc      = wa_adrc.

  CASE p_partyp.
    WHEN 'C'.

      IF wa_info_c-stcd1 IS NOT INITIAL.
        p_inf_parid-stcd1    = wa_info_c-stcd1.
        p_inf_parid-cnpj_cpf = wa_info_c-stcd1.
      ELSEIF wa_info_c-stcd2 IS NOT INITIAL.
        p_inf_parid-stcd2    = wa_info_c-stcd2.
        p_inf_parid-cnpj_cpf = wa_info_c-stcd2.
      ENDIF.

    WHEN 'V' OR 'B'.

      IF wa_info_k-stcd1 IS NOT INITIAL.
        p_inf_parid-stcd1    = wa_info_k-stcd1.
        p_inf_parid-cnpj_cpf = wa_info_k-stcd1.
      ELSEIF wa_info_k-stcd2 IS NOT INITIAL.
        p_inf_parid-stcd2    = wa_info_k-stcd2.
        p_inf_parid-cnpj_cpf = wa_info_k-stcd2.
      ENDIF.

  ENDCASE.

  IF p_inf_parid-cnpj_cpf IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_inf_parid-cnpj_cpf
      IMPORTING
        output = p_inf_parid-cnpj_cpf.
  ENDIF.

  IF ( wa_adrc-region EQ 'RO' ) OR
     ( wa_adrc-region EQ 'AC' ) OR
     ( wa_adrc-region EQ 'AM' ) OR
     ( wa_adrc-region EQ 'RR' ) OR
     ( wa_adrc-region EQ 'PA' ) OR
     ( wa_adrc-region EQ 'AP' ) OR
     ( wa_adrc-region EQ 'TO' ) OR
     ( wa_adrc-region EQ 'MA' ) OR
     ( wa_adrc-region EQ 'PI' ) OR
     ( wa_adrc-region EQ 'CE' ) OR
     ( wa_adrc-region EQ 'RN' ) OR
     ( wa_adrc-region EQ 'PB' ) OR
     ( wa_adrc-region EQ 'PE' ) OR
     ( wa_adrc-region EQ 'AL' ) OR
     ( wa_adrc-region EQ 'SE' ) OR
     ( wa_adrc-region EQ 'BA' ) OR
     ( wa_adrc-region EQ 'MG' ) OR
     ( wa_adrc-region EQ 'ES' ) OR
     ( wa_adrc-region EQ 'RJ' ) OR
     ( wa_adrc-region EQ 'SP' ) OR
     ( wa_adrc-region EQ 'PR' ) OR
     ( wa_adrc-region EQ 'SC' ) OR
     ( wa_adrc-region EQ 'RS' ) OR
     ( wa_adrc-region EQ 'MS' ) OR
     ( wa_adrc-region EQ 'MT' ) OR
     ( wa_adrc-region EQ 'GO' ) OR
     ( wa_adrc-region EQ 'DF' ).
    p_inf_parid-region = wa_adrc-region.
  ENDIF.

ENDFORM.

FORM f_atrib_chave_docnum  USING p_doc               TYPE j_1bnfdoc
                        CHANGING p_chave_nfe         TYPE zde_chave_nfe
                                 p_chave_nff         TYPE zde_chave_nff
                                 p_retorno           TYPE zde_retorno_proc.

  CALL FUNCTION 'ZCCT_MONTA_CHAVE_DOCUMENTO'
    EXPORTING
      i_docnum    = p_doc-docnum
    IMPORTING
      e_chave_nfe = p_chave_nfe
      e_chave_nff = p_chave_nff
      e_retorno   = p_retorno.

ENDFORM.

FORM f_atrib_chave_j_1bnfdoc USING p_doc               TYPE j_1bnfdoc
                          CHANGING p_chave_nfe         TYPE zde_chave_nfe
                                   p_chave_nff         TYPE zde_chave_nff
                                   p_retorno           TYPE zde_retorno_proc.

  CALL FUNCTION 'ZCCT_MONTA_CHAVE_DOCUMENTO'
    EXPORTING
      i_j_1bnfdoc = p_doc
    IMPORTING
      e_chave_nfe = p_chave_nfe
      e_chave_nff = p_chave_nff
      e_retorno   = p_retorno.

ENDFORM.

FORM f_prepare_range_serie TABLES c_rg_serie STRUCTURE rg_serie
                            USING p_serie.


  DATA: v_serie          TYPE j_1bnfdoc-series,
        vl_serie_int_aux TYPE i,
        vl_serie_srt_aux TYPE c LENGTH 3.

  CLEAR: c_rg_serie[], v_serie.

  v_serie = p_serie.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = v_serie
    IMPORTING
      output = v_serie.

  c_rg_serie-sign   = 'I'.
  c_rg_serie-option = 'EQ'.
  c_rg_serie-low    = v_serie.
  c_rg_serie-high   = v_serie.
  APPEND c_rg_serie.

  c_rg_serie-sign   = 'I'.
  c_rg_serie-option = 'EQ'.
  c_rg_serie-low    = v_serie+2(1).
  c_rg_serie-high   = v_serie+2(1).
  APPEND c_rg_serie.

  IF strlen( v_serie ) = 3.
    TRY.
        vl_serie_int_aux = v_serie.
        vl_serie_srt_aux = vl_serie_int_aux.
        CONDENSE vl_serie_srt_aux NO-GAPS.
        IF strlen( vl_serie_srt_aux ) <> strlen( v_serie ).
          c_rg_serie-sign   = 'I'.
          c_rg_serie-option = 'EQ'.
          c_rg_serie-low    = v_serie+1(2).
          c_rg_serie-high   = v_serie+1(2).
          APPEND c_rg_serie.
        ENDIF.
      CATCH cx_sy_conversion_no_number.
    ENDTRY.
  ENDIF.

  IF v_serie <> '000'.
    c_rg_serie-sign   = 'I'.
    c_rg_serie-option = 'EQ'.
    c_rg_serie-low    = v_serie.
    c_rg_serie-high   = v_serie.
    SHIFT c_rg_serie-low  LEFT DELETING LEADING '0'.
    SHIFT c_rg_serie-high LEFT DELETING LEADING '0'.
    APPEND c_rg_serie.
  ENDIF.

  c_rg_serie-sign   = 'I'.
  c_rg_serie-option = 'EQ'.
  c_rg_serie-low    = v_serie.
  c_rg_serie-high   = v_serie.
  SHIFT c_rg_serie-low  LEFT DELETING LEADING space.
  SHIFT c_rg_serie-high LEFT DELETING LEADING space.
  APPEND c_rg_serie.

  SORT c_rg_serie BY sign option low high.
  DELETE ADJACENT DUPLICATES FROM c_rg_serie COMPARING sign option low high.

ENDFORM.

FORM f_proc_ret_cons_pre_acd USING p_estoque_nfe_cct TYPE zde_estoque_nf_cct_js.

  DATA: wl_zlest0186 TYPE zlest0186.

  DATA: v_valor_aux   TYPE c LENGTH 255.

  LOOP AT p_estoque_nfe_cct-estoquenotasfiscais INTO DATA(wl_estoque_nf).

    CLEAR: wl_zlest0186.

    wl_zlest0186-chave               = wl_estoque_nf-numero.
    wl_zlest0186-cnpj_responsavel    = wl_estoque_nf-responsavel.
    wl_zlest0186-codigo_urf          = wl_estoque_nf-urf.
    wl_zlest0186-codigo_ra           = wl_estoque_nf-recinto.
    wl_zlest0186-latitude            = wl_estoque_nf-latitude.
    wl_zlest0186-longitude           = wl_estoque_nf-longitude.
    wl_zlest0186-peso_aferido        = wl_estoque_nf-pesoaferido.


    LOOP AT wl_estoque_nf-itens INTO DATA(wl_item).
      ADD wl_item-saldo TO wl_zlest0186-saldo.
    ENDLOOP.
**<<<------"189195 - NMS - INI------>>>
*    v_valor_aux = wl_estoque_nf-registro(10).
*    REPLACE ALL OCCURRENCES OF '-' IN v_valor_aux WITH ''.
*    wl_zlest0186-dt_recepcao               = v_valor_aux.
* Coverte formato Timestemp em Data, Hora e Resto de Milesegundo
    DATA(lcl_pco_utility) = NEW cl_pco_utility( ).
    lcl_pco_utility->convert_java_timestamp_to_abap( EXPORTING iv_timestamp = wl_estoque_nf-registro IMPORTING ev_date = wl_zlest0186-dt_recepcao ).
**<<<------"189195 - NMS - FIM------>>>
    wl_zlest0186-dt_registro         = sy-datum.
    wl_zlest0186-hr_registro         = sy-uzeit.
    wl_zlest0186-us_registro         = sy-uname.
    MODIFY zlest0186 FROM wl_zlest0186.

  ENDLOOP.

ENDFORM.

FORM f_get_parceiros TABLES p_parceiros STRUCTURE it_parc
                      USING p_cnpj
                            p_cpf.

  DATA: t_kna1            TYPE TABLE OF kna1    WITH HEADER LINE,
        t_lfa1            TYPE TABLE OF lfa1    WITH HEADER LINE,
        t_parc            TYPE TABLE OF ty_parc WITH HEADER LINE,
        wl_j_1bbranch_aux TYPE j_1bbranch.

  CLEAR: t_kna1[], t_lfa1[], t_parc[], p_parceiros[].

  IF p_cnpj IS NOT INITIAL.
    SELECT *
      FROM lfa1 APPENDING TABLE t_lfa1
     WHERE stcd1 EQ p_cnpj.

    SELECT *
       FROM kna1 APPENDING TABLE t_kna1
      WHERE stcd1 EQ p_cnpj.
  ENDIF.

  IF p_cpf IS NOT INITIAL.
    SELECT *
      FROM lfa1 APPENDING TABLE t_lfa1
     WHERE stcd2 EQ p_cpf.

    SELECT *
      FROM kna1 APPENDING TABLE t_kna1
     WHERE stcd2 EQ p_cpf.
  ENDIF.

*-------------------------------------------------------------*
* Eliminar Parceiros Bloqueados
*-------------------------------------------------------------*

  PERFORM f_elimina_lfa1_bloq TABLES t_lfa1.
  LOOP AT t_lfa1 INTO DATA(_wl_lfa1).
    DATA(_tabix) = sy-tabix.
    TRY.
        t_parc-parid  = _wl_lfa1-lifnr.
        t_parc-partyp = 'V'.
        APPEND t_parc.

        "Add Parceiro Filial
        IF _tabix EQ 1.
          zcl_fornecedores=>zif_parceiros~get_instance(
           )->set_parceiro( i_parceiro = _wl_lfa1-lifnr
           )->ck_ativo(
           )->ck_parceiro_local_negocio( ).

          CLEAR: wl_j_1bbranch_aux.
          DATA(r_local_negocio) = zcl_parceiro=>get_parceiro_local_negocio( EXPORTING
                                                                              i_partiner   = _wl_lfa1-lifnr
                                                                            IMPORTING
                                                                              e_j_1bbranch = wl_j_1bbranch_aux ).

          IF ( r_local_negocio = abap_true ) AND ( wl_j_1bbranch_aux IS NOT INITIAL ).
            t_parc-parid  = wl_j_1bbranch_aux-bukrs && wl_j_1bbranch_aux-branch.
            t_parc-partyp = 'B'.
            APPEND t_parc.
          ENDIF.
        ENDIF.
      CATCH zcx_parceiros INTO DATA(ex_parceiros_k).
    ENDTRY.
  ENDLOOP.

  PERFORM f_elimina_kna1_bloq TABLES t_kna1.
  LOOP AT t_kna1 INTO DATA(_wl_kna1).
    t_parc-parid  = _wl_kna1-kunnr.
    t_parc-partyp = 'C'.
    APPEND t_parc.
  ENDLOOP.

  p_parceiros[] = t_parc[].

ENDFORM.

FORM f_get_series TABLES p_series STRUCTURE it_series
                   USING p_serie TYPE j_1bseries.

  DATA: vl_serie_int_aux TYPE i,
        vl_serie_srt_aux TYPE c LENGTH 3,
        v_series         TYPE j_1bnfdoc-series,
        tg_series        TYPE TABLE OF ty_series WITH HEADER LINE.

  CLEAR: v_series, p_series[], tg_series[].

  "V_SERIES = | { P_SERIE ALPHA = IN }|.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_serie
    IMPORTING
      output = v_series.

  tg_series-serie = v_series.
  APPEND tg_series.

  IF strlen( v_series ) = 3.
    TRY.
        vl_serie_int_aux = v_series.
        vl_serie_srt_aux = vl_serie_int_aux.
        CONDENSE vl_serie_srt_aux NO-GAPS.
        IF strlen( vl_serie_srt_aux ) <> strlen( v_series ).
          tg_series-serie = vl_serie_srt_aux.
          APPEND tg_series.
        ENDIF.
      CATCH cx_sy_conversion_no_number.
    ENDTRY.
  ENDIF.

  IF v_series <> '000'.
    tg_series-serie = v_series.
    SHIFT tg_series-serie LEFT DELETING LEADING '0'.
    APPEND tg_series.
  ENDIF.

  SORT tg_series BY serie.
  DELETE ADJACENT DUPLICATES FROM tg_series COMPARING serie.

  p_series[] = tg_series[].

ENDFORM.
