FUNCTION zcct_monta_chave_documento.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"     REFERENCE(I_J_1BNFDOC) TYPE  J_1BNFDOC OPTIONAL
*"     REFERENCE(I_CK_ENTRADA_PROPRIA) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_CHAVE_NFE) TYPE  ZDE_CHAVE_NFE
*"     REFERENCE(E_CHAVE_NFF) TYPE  ZDE_CHAVE_NFF
*"     REFERENCE(E_EMISSOR_CNPJ) TYPE  STCD1
*"     REFERENCE(E_EMISSOR_CPF) TYPE  STCD2
*"     REFERENCE(E_EMISSOR_IE) TYPE  STCD3
*"     REFERENCE(E_REGIO) TYPE  J_1BREGIO
*"     REFERENCE(E_NFYEAR) TYPE  J_1BYEAR
*"     REFERENCE(E_NFMONTH) TYPE  J_1BMONTH
*"     REFERENCE(E_MODEL) TYPE  J_1BMODEL
*"     REFERENCE(E_SERIE) TYPE  J_1BSERIES
*"     REFERENCE(E_NFNUM9) TYPE  J_1BNFNUM9
*"     REFERENCE(E_NFNUM) TYPE  J_1BNFNUMB
*"     REFERENCE(E_DOCNUM9) TYPE  J_1BDOCNUM9
*"     REFERENCE(E_CDV) TYPE  J_1BCHECKDIGIT
*"     REFERENCE(E_RETORNO) TYPE  ZDE_RETORNO_PROC
*"----------------------------------------------------------------------

  DATA: v_inf_parid          TYPE ty_inf_parid,
        v_c14                TYPE c LENGTH 14,
        v_serie              TYPE j_1bnfe_active-serie,
        v_nfnum9             TYPE j_1bnfe_active-nfnum9,
        v_msg                TYPE string,
        wl_zlest0147         TYPE zlest0147,
        wl_zlest0147_nf_prop TYPE zlest0147,
        wl_doc_ent_prop      TYPE j_1bnfdoc,
        wl_doc               TYPE j_1bnfdoc,
        wl_act               TYPE j_1bnfe_active,
        wl_zlest0146         TYPE zlest0146,
        lt_zlest0147         TYPE zlest0147_t,
        lt_zlest0168         TYPE zlest0168_t,
        lit_zsdt0232         TYPE TABLE OF zsdt0232,
        v_chave_nfe_prop     TYPE zlest0147-chave_nfe_prop,
        v_doc_rateio         TYPE char01.

  DATA: lva_numnf            TYPE  j_1bnfnum9,
        lva_aamm_emissao     TYPE  char04,
        lva_serie            TYPE  j_1bseries,
        lva_cnpj             TYPE  stcd1,
        lva_cpf              TYPE  stcd2,
        lva_cuf_ibge         TYPE  zchar02,
        lva_cuf_sigla        TYPE  zchar02,
        lva_modelo           TYPE  j_1bmodel,
        lva_chave_nfe        TYPE zlest0147-chave_nfe,
        lva_chave_nff        TYPE zlest0147-chave_nff,
        lwa_zde_retorno_proc TYPE zde_retorno_proc.

  CLEAR: e_chave_nfe, e_chave_nff, v_serie, v_nfnum9, v_c14, wl_doc, wl_act.


  IF ( i_ck_entrada_propria EQ abap_true ) AND ( i_docnum IS NOT INITIAL ).

    SELECT SINGLE *
      FROM j_1bnfdoc INTO wl_doc_ent_prop
     WHERE docnum EQ i_docnum.

    IF ( sy-subrc EQ 0 ) AND
       ( wl_doc_ent_prop-entrad EQ abap_true ). "Entrada Propria

      "Check se Documento está registrado no CCT
      SELECT SINGLE *
        FROM j_1bnfe_active INTO @DATA(_wl_active_ent_prop)
       WHERE docnum EQ @wl_doc_ent_prop-docnum.

      IF sy-subrc EQ 0.

        CLEAR: v_chave_nfe_prop.

        CONCATENATE _wl_active_ent_prop-regio
                    _wl_active_ent_prop-nfyear
                    _wl_active_ent_prop-nfmonth
                    _wl_active_ent_prop-stcd1
                    _wl_active_ent_prop-model
                    _wl_active_ent_prop-serie
                    _wl_active_ent_prop-nfnum9
                    _wl_active_ent_prop-docnum9
                    _wl_active_ent_prop-cdv INTO v_chave_nfe_prop.

        SELECT SINGLE * INTO wl_zlest0147_nf_prop
          FROM zlest0147 AS a
         WHERE a~chave_nfe_prop EQ v_chave_nfe_prop
           AND EXISTS ( SELECT id_recepcao
                          FROM zlest0146 AS b
                         WHERE b~id_recepcao EQ a~id_recepcao
                           AND b~cancel      EQ abap_false ).

        "Verificar por notas referenciadas no documento de entrada propria
        IF sy-subrc NE 0.

          SELECT SINGLE *
            FROM zsdt0231 INTO @DATA(lwa_zsdt0231)
           WHERE docnum EQ @i_docnum.

          IF sy-subrc EQ 0.
            CLEAR: lit_zsdt0232[].

            SELECT *
              FROM zsdt0232 INTO TABLE lit_zsdt0232
             WHERE obj_key EQ lwa_zsdt0231-obj_key.

            LOOP AT lit_zsdt0232 INTO DATA(lwa_zsdt0232).

              CLEAR: lva_chave_nfe, lva_chave_nff.

              "Nota Fiscal Eletronica
              IF lwa_zsdt0232-refnfe IS NOT INITIAL AND strlen( lwa_zsdt0232-refnfe ) EQ 44.
                lva_chave_nfe = lwa_zsdt0232-refnfe.
              ELSE.
                lva_numnf          = lwa_zsdt0232-nnf.
                lva_aamm_emissao   = lwa_zsdt0232-aamm.
                lva_serie          = lwa_zsdt0232-serie.
                lva_cnpj           = lwa_zsdt0232-cnpj.
                lva_cpf            = lwa_zsdt0232-cpf.
                lva_cuf_ibge       = lwa_zsdt0232-cuf.
                lva_modelo         = lwa_zsdt0232-mod.

                CALL FUNCTION 'ZCCT_MONTA_CHAVE_NFF'
                  EXPORTING
                    i_numnf        = lva_numnf
                    i_aamm_emissao = lva_aamm_emissao
                    i_serie        = lva_serie
                    i_cnpj         = lva_cnpj
                    i_cpf          = lva_cpf
                    i_cuf_ibge     = lva_cuf_ibge
                    i_modelo       = lva_modelo
                  IMPORTING
                    e_chave_nff    = lva_chave_nff
                  CHANGING
                    c_retorno      = lwa_zde_retorno_proc.

                IF lwa_zde_retorno_proc-type = 'E'.
                  CONTINUE.
                ENDIF.
              ENDIF.

              SELECT SINGLE * INTO wl_zlest0147_nf_prop
                FROM zlest0147 AS a
               WHERE a~chave_nfe EQ lva_chave_nfe
                 AND a~chave_nff EQ lva_chave_nff
                 AND EXISTS ( SELECT id_recepcao
                                FROM zlest0146 AS b
                               WHERE b~id_recepcao EQ a~id_recepcao
                                 AND b~cancel      EQ abap_false ).

              IF sy-subrc EQ 0.
                wl_zlest0147_nf_prop-entrada_propria = abap_true.

                "LES - Ajuste Montagem Chave IR210938 - WPP
                IF lva_chave_nff IS NOT INITIAL.
                  e_chave_nff    = lva_chave_nff.

                  e_emissor_cnpj =  lva_cnpj.
                  e_emissor_cpf  =  lva_cpf.
                  e_nfyear       =  lva_aamm_emissao+0(2).
                  e_nfmonth      =  lva_aamm_emissao+2(2).
                  e_model        =  lva_modelo.
                  e_serie        =  lva_serie.
                  e_nfnum9       =  lva_numnf.
                  e_nfnum        =  lva_numnf.
                  RETURN.

                ENDIF.
                "LES - Ajuste Montagem Chave IR210938 - WPP

                EXIT.
              ENDIF.
            ENDLOOP.

          ENDIF.

        ENDIF.
        ""Verificar por notas referenciadas no documento de entrada propria - Fim

        IF ( sy-subrc EQ 0 ) AND ( wl_zlest0147_nf_prop-entrada_propria EQ abap_true ).

          IF ( wl_zlest0147_nf_prop-model NE '55' ).

            CLEAR: wl_doc.
            wl_doc-nfnum   = wl_zlest0147_nf_prop-nfnum.
            wl_doc-model   = wl_zlest0147_nf_prop-model.
            wl_doc-series  = wl_zlest0147_nf_prop-serie.
            wl_doc-docdat  = wl_zlest0147_nf_prop-dt_emissao.
            wl_doc-parid   = wl_doc_ent_prop-parid.
            wl_doc-partyp  = wl_doc_ent_prop-partyp.

          ELSE.
            "Entrada Propria realizada para uma NF-e Eletronica(Estado de SP)

            CLEAR: wl_doc, wl_act.

            "LES - Ajuste Montagem Chave IR210938 - WPP - Ini
            "wl_doc-model        =  wl_zlest0147_nf_prop-model.
            wl_doc-model        =  wl_zlest0147_nf_prop-chave_nfe+20(2).
            "LES - Ajuste Montagem Chave IR210938 - WPP - Fim

            wl_act-regio        =  wl_zlest0147_nf_prop-chave_nfe(2).
            wl_act-nfyear       =  wl_zlest0147_nf_prop-chave_nfe+2(2).
            wl_act-nfmonth      =  wl_zlest0147_nf_prop-chave_nfe+4(2).
            wl_act-stcd1        =  wl_zlest0147_nf_prop-chave_nfe+6(14).
            wl_act-model        =  wl_zlest0147_nf_prop-chave_nfe+20(2).
            wl_act-serie        =  wl_zlest0147_nf_prop-chave_nfe+22(3).
            wl_act-nfnum9       =  wl_zlest0147_nf_prop-chave_nfe+25(9).
            wl_act-docnum9      =  wl_zlest0147_nf_prop-chave_nfe+34(9).
            wl_act-cdv          =  wl_zlest0147_nf_prop-chave_nfe+43(1).

          ENDIF.

        ENDIF.

      ENDIF.


    ENDIF.

  ENDIF.

  IF ( wl_doc IS INITIAL ).

    IF ( i_j_1bnfdoc IS NOT INITIAL ) AND ( i_j_1bnfdoc-model NE '55' ).
      MOVE-CORRESPONDING i_j_1bnfdoc TO wl_doc.
    ELSE.
      IF i_docnum IS INITIAL.
        MESSAGE s106 INTO v_msg.
        e_retorno-type     = 'E'.
        e_retorno-msgno    = sy-msgno.
        e_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM j_1bnfdoc INTO wl_doc
       WHERE docnum EQ i_docnum.

      IF sy-subrc NE 0.
        MESSAGE s107 WITH i_docnum INTO v_msg.
        e_retorno-type     = 'E'.
        e_retorno-msgno    = sy-msgno.
        e_retorno-texto    = v_msg.
        RETURN.
      ENDIF.
    ENDIF.

  ENDIF.

  CHECK wl_doc IS NOT INITIAL.

  IF ( wl_doc-model NE '55' ) AND
     ( wl_doc-model NE '01' ) AND
     ( wl_doc-model NE '04' ).
    MESSAGE s071 WITH wl_doc-docnum INTO v_msg.
    e_retorno-type     = 'E'.
    e_retorno-msgno    = sy-msgno.
    e_retorno-texto    = v_msg.
    RETURN.
  ENDIF.

  CASE wl_doc-model.
    WHEN '55'.

      IF wl_act IS INITIAL.
        SELECT SINGLE *
          FROM j_1bnfe_active INTO @DATA(_wl_active)
         WHERE docnum EQ @wl_doc-docnum.
      ELSE.
        _wl_active = wl_act.
      ENDIF.

      CONCATENATE _wl_active-regio
                  _wl_active-nfyear
                  _wl_active-nfmonth
                  _wl_active-stcd1
                  _wl_active-model
                  _wl_active-serie
                  _wl_active-nfnum9
                  _wl_active-docnum9
                  _wl_active-cdv INTO e_chave_nfe.

      IF ( strlen( e_chave_nfe ) NE 44 ).
        MESSAGE s066 WITH wl_doc-docnum INTO v_msg.
        e_retorno-type     = 'E'.
        e_retorno-msgno    = sy-msgno.
        e_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      e_emissor_cnpj =  _wl_active-stcd1.
      e_regio        =  _wl_active-regio.
      e_nfyear       =  _wl_active-nfyear.
      e_nfmonth      =  _wl_active-nfmonth.
      e_model        =  _wl_active-model.
      e_serie        =  _wl_active-serie.
      e_nfnum9       =  _wl_active-nfnum9.
      e_docnum9      =  _wl_active-docnum9.
      e_cdv          =  _wl_active-cdv.

    WHEN OTHERS.

      CLEAR: v_inf_parid.

      PERFORM f_get_info_parceiro USING wl_doc-parid
                                        wl_doc-partyp
                               CHANGING v_inf_parid.

      IF ( v_inf_parid-cnpj_cpf IS INITIAL ).
        MESSAGE s067 WITH wl_doc-nfnum INTO v_msg.
        e_retorno-type     = 'E'.
        e_retorno-msgno    = sy-msgno.
        e_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      IF ( v_inf_parid-region IS INITIAL ).
        MESSAGE s068 WITH wl_doc-nfnum INTO v_msg.
        e_retorno-type     = 'E'.
        e_retorno-msgno    = sy-msgno.
        e_retorno-texto    = v_msg.
        RETURN.
      ENDIF.

      IF wl_doc-nfnum IS NOT INITIAL.
        lva_numnf  = wl_doc-nfnum.
      ELSEIF ( wl_doc-nfenum IS NOT INITIAL ).
        lva_numnf  = wl_doc-nfenum.
      ENDIF.

      lva_aamm_emissao   = wl_doc-docdat+2(2) && wl_doc-docdat+4(2). "
      lva_serie          = wl_doc-series. "
      lva_cnpj           = v_inf_parid-stcd1. "
      lva_cpf            = v_inf_parid-stcd2. "
      lva_cuf_sigla      = v_inf_parid-region.
      lva_modelo         = wl_doc-model.

      CALL FUNCTION 'ZCCT_MONTA_CHAVE_NFF'
        EXPORTING
          i_numnf        = lva_numnf
          i_aamm_emissao = lva_aamm_emissao
          i_serie        = lva_serie
          i_cnpj         = lva_cnpj
          i_cpf          = lva_cpf
          i_cuf_sigla    = lva_cuf_sigla
          i_modelo       = lva_modelo
        IMPORTING
          e_chave_nff    = e_chave_nff
        CHANGING
          c_retorno      = e_retorno.

      IF e_retorno-type = 'E'.
        RETURN.
      ENDIF.

      e_emissor_cnpj =  v_inf_parid-stcd1.
      e_emissor_cpf  =  v_inf_parid-stcd2.
      e_nfyear       =   wl_doc-docdat+2(2).
      e_nfmonth      =   wl_doc-docdat+4(2).
      e_model        =   wl_doc-model.
      e_serie        =   v_serie.
      e_nfnum9       =   wl_doc-nfenum.
      e_nfnum        =   wl_doc-nfnum.

  ENDCASE.

ENDFUNCTION.
