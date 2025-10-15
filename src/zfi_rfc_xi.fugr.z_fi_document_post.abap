FUNCTION z_fi_document_post .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_DOCUMENTHEADER STRUCTURE  ZFIE_DOCUMENTHEADER
*"      IT_DOCUMENTITEM STRUCTURE  ZFIE_DOCUMENTITEM
*"      IT_RETURN STRUCTURE  ZFIE_RETURN OPTIONAL
*"      IT_RETURNOBJ STRUCTURE  ZFIE_RETURNOBJ OPTIONAL
*"      IT_IMPOSTOS_RETIDOS STRUCTURE  ZIB_CONTABIL_IRT OPTIONAL
*"  EXCEPTIONS
*"      BSCHL_NOT_FOUND
*"----------------------------------------------------------------------

  DATA : BEGIN OF t_mwdat OCCURS 0.
           INCLUDE STRUCTURE rtax1u15.
  DATA : END OF t_mwdat.

  DATA:t_accchg	TYPE TABLE OF	accchg,
       w_accchg	TYPE 	accchg,
       v_buzei  TYPE bseg-buzei.

  DATA: vg_count_iva      TYPE i,
        vg_count_tot      TYPE i,
        lv_count          TYPE i,
        vg_loop_val       TYPE i,
        vg_index          TYPE i,
        vg_tabix          TYPE sy-tabix,
        vg_item_iva       LIKE bapiacar09-itemno_acc,
        lv_tax_rate       TYPE fwste,
        vg_lifnrvat       TYPE lfa1-lifnr,
        vg_wrbtr2         TYPE zfie_documentitem-wrbtr,
        vg_dmbtr          TYPE zfie_documentitem-dmbtr,
        vg_dmbe2          TYPE zfie_documentitem-dmbe2,
        vg_dmbe3          TYPE zfie_documentitem-dmbe3,

        vg_base           TYPE zfie_documentitem-dmbtr,
        vg_dmbtr_base     TYPE zfie_documentitem-dmbtr,
        vg_dmbe2_base     TYPE zfie_documentitem-dmbe2,
        vg_dmbe3_base     TYPE zfie_documentitem-dmbe3,

        vg_base_iva       TYPE bseg-wrbtr,

        it_zmmt_eeiva_zgr TYPE TABLE OF zmmt_eeiva_zgr, "impostos argentina
        wa_zmmt_eeiva_zgr TYPE zmmt_eeiva_zgr,
        vflag_zgr(1).

  TYPES: BEGIN OF ty_t001,
           bukrs TYPE t001-bukrs,
           land1 TYPE t001-land1,
           spras TYPE t001-spras,
         END OF   ty_t001,

         BEGIN OF ty_t005,
           land1 TYPE t005-land1,
           waers TYPE t005-waers,
           curin TYPE t005-curin,
           curha TYPE t005-curha,
         END OF   ty_t005.

  DATA: v_message1 TYPE balm-msgv1,
        v_message2 TYPE balm-msgv2,
        v_message3 TYPE balm-msgv3,
        v_message4 TYPE balm-msgv4,
        vl_docnum  LIKE j_1bnfdoc-docnum,
        wa_t001    TYPE ty_t001,
        wa_t005    TYPE ty_t005,
        wa_lfa1    TYPE lfa1,
        wa_t030k   TYPE t030k.

*> Seleciona informações da chave de lançamento
  SELECT bschl shkzg koart FROM tbsl
    INTO TABLE it_tbsl
     FOR ALL ENTRIES IN it_documentitem
   WHERE ( bschl EQ it_documentitem-bschl ).

*> Erro nenhuma chave de lançamento encontrada
  IF ( sy-subrc NE 0 ).
    MESSAGE ID 'Z01' TYPE 'E' NUMBER '001' RAISING bschl_not_found.
  ENDIF.

*> Altera a transação para que não haja problemas com o estorno do
*> documento. ATENÇÃO : dentro da BAPI o sy-tcode será atribuido
*> diretamente ao campo BKPF-TCODE. Então se o campo não contiver
*> a transação 'FB05' o documento não será considerado como de
*> contabilidade financeira.

  vg_tcode_old          = sy-tcode.

  SORT it_tbsl BY bschl.

  DATA: it_header_aux     TYPE zfie_documentheader OCCURS 0,
        it_item_aux       TYPE zfie_documentitem OCCURS 0,
        wa_zmmt_ee_zgr    TYPE zmmt_ee_zgr,
        it_zmmt_eeimp_zgr TYPE TABLE OF zmmt_eeimp_zgr,
        it_accountwt      TYPE TABLE OF bapiacwt09,
        it_setleaf        TYPE TABLE OF setleaf,
        it_setlinet       TYPE TABLE OF setlinet,
        it_lfbw           TYPE TABLE OF lfbw,
        wl_081            TYPE zglt081,
        tg_088            TYPE TABLE OF zglt088 WITH HEADER LINE,
* Inicio - US80965 - NumenIT - 03.11.2022
        it_bseg_buzid     TYPE TABLE OF bseg.
* Fim - US80965 - NumenIT - 03.11.2022

  DATA: wa_zmmt_eeimp_zgr TYPE zmmt_eeimp_zgr,
        wa_accountwt      TYPE bapiacwt09,
        wa_setleaf        TYPE setleaf,
        wa_setlinet       TYPE setlinet,
        wa_lfbw           TYPE lfbw.

  DATA: validado   TYPE c,
        vl_len     TYPE i,
        vl_index   TYPE i,
        vl_count   TYPE i,
        vl_index_c TYPE c LENGTH 1,
        vl_item    TYPE bapiacwt09-itemno_acc,
        vl_bschl   TYPE zfit0029-bschl,
        lc_dmbtr   TYPE dmbtr.

  CLEAR: it_header_aux,
         it_item_aux,
         validado.

  DATA : vg_len   TYPE i,
         vg_trtyp TYPE zfit0029-trtyp.

* Inicio - US80965 - NumenIT - 03.11.2022
  DATA: vg_index_bseg TYPE i.
* Fim - US80965 - NumenIT - 03.11.2022

  DATA: BEGIN OF wa_item_kidno,
          item               LIKE bapiacar09-itemno_acc,
          kidno              LIKE zfie_documentitem-kidno,
          ds_cpf_treinamento LIKE zde_new_doc_ctb_itm_treino-ds_cpf_treinamento,
          id_lms_treinamento LIKE zde_new_doc_ctb_itm_treino-id_lms_treinamento,
        END OF wa_item_kidno,
        it_item_kidno LIKE STANDARD TABLE OF wa_item_kidno.

  LOOP AT it_documentheader INTO wa_header.
    validado = 'S'.

    "Check Erros
    READ TABLE it_return INTO wa_return WITH KEY seqlan = wa_header-seqlan
                                                 type   = 'E'.
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

    LOOP AT it_documentitem INTO wa_item
                           WHERE ( seqlan EQ wa_header-seqlan ).

      IF wa_item-quantity = 999999  AND ( validado NE 'N' ).
        validado = 'N'.

        CLEAR wa_return.
        wa_return-seqlan     = wa_header-seqlan.
        wa_return-type       = 'E'.
        wa_return-id         = 'Z01'.
        wa_return-number     = '004'.
        wa_return-message    = 'Lançamento faltando partidas, verificar.'.

        APPEND wa_return TO it_return.
      ENDIF.

      IF wa_item-bankl IS NOT INITIAL
        AND wa_item-bankn IS NOT INITIAL
        AND wa_item-bvtyp IS NOT INITIAL.

        DATA: wlifnr TYPE lfbk-lifnr.

        SELECT SINGLE lifnr
          INTO wlifnr
          FROM lfbk
         WHERE lifnr EQ wa_item-hkont
           AND bankl EQ wa_item-bankl
           AND bankn EQ wa_item-bankn
           AND bvtyp EQ wa_item-bvtyp.
        IF sy-subrc NE 0.
          SELECT SINGLE ibupasuplrcotp~supplier
            FROM but0bk
            INNER JOIN ibupasuplrcotp
            ON ibupasuplrcotp~supplier = wa_item-hkont
            INTO wlifnr
            WHERE but0bk~partner EQ ibupasuplrcotp~businesspartner
            AND   but0bk~bankl   EQ wa_item-bankl
            AND   but0bk~bankn   EQ wa_item-bankn
            AND   but0bk~bkvid   EQ wa_item-bvtyp.

        ENDIF.

        IF ( sy-subrc EQ 0 ) AND ( validado NE 'N' ).
          validado = 'S'.
        ELSE.
          validado = 'N'.

          CLEAR wa_return.
          wa_return-seqlan     = wa_header-seqlan.
          wa_return-type       = 'E'.
          wa_return-id         = 'Z01'.
          wa_return-number     = '004'.
          wa_return-message_v1 = wa_item-hkont.
          wa_return-message_v2 = wa_item-bankl.
          wa_return-message_v3 = wa_item-bankn.
          wa_return-message_v4 = wa_item-bvtyp.

          v_message1 = wa_return-message_v1.
          v_message2 = wa_return-message_v2.
          v_message3 = wa_return-message_v3.
          v_message4 = wa_return-message_v4.

          CONCATENATE 'Dados bancários divergentes' ' - '
                      'Cód. Forn: ' v_message1
                      'Agência: ' v_message2
                      'Conta: ' v_message3
                      'Prioridade: ' v_message4 INTO wa_return-message
                      SEPARATED BY space.
          APPEND wa_return TO it_return.

        ENDIF.

      ENDIF.

      "Interface de Adiantamento de Viagem - SoftExpert
      IF wa_header-interface EQ 43 AND wa_item-wf_saldo_finan_a EQ abap_false.

        CLEAR: wa_tbsl.

        READ TABLE it_tbsl INTO wa_tbsl WITH KEY bschl = wa_item-bschl BINARY SEARCH.

        IF wa_tbsl-koart EQ 'K'.
          lc_dmbtr = 0.
          CALL METHOD z_fi_util=>get_saldo_adiantamento_viagem
            EXPORTING
              i_company    = wa_header-bukrs
              i_fornecedor = wa_item-hkont
            IMPORTING
              e_dmbtr      = lc_dmbtr.

          IF lc_dmbtr NE 0.
            validado = 'N'.
            CLEAR wa_return.
            wa_return-seqlan     = wa_header-seqlan.
            wa_return-type       = 'E'.
            wa_return-id         = 'Z01'.
            wa_return-number     = '004'.
            wa_return-message_v1 = wa_header-bukrs.
            wa_return-message_v2 = wa_item-hkont.
            wa_return-message_v3 = wa_item-bschl.
            v_message1 = wa_return-message_v1.
            v_message2 = wa_return-message_v2.
            v_message3 = wa_return-message_v3.
            CONCATENATE 'Fornecedor com saldo em aberto' ' - ' 'Empresa: ' v_message1 'Fornecedor: ' v_message2 'Ch.Lcto: ' v_message3 INTO wa_return-message SEPARATED BY space.
            APPEND wa_return TO it_return.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF validado EQ 'S'.
      LOOP AT it_documentitem INTO wa_item
                             WHERE ( seqlan EQ wa_header-seqlan ).
        APPEND wa_item TO it_item_aux.
      ENDLOOP.
      APPEND wa_header TO it_header_aux.
    ENDIF.

  ENDLOOP.

  it_header[] = it_header_aux[].

  LOOP AT it_header_aux INTO wa_header.

    CLEAR: it_item_kidno.
    CLEAR: vg_item, gd_documentheader, vl_docnum.
    REFRESH: it_accountgl,it_criteria, it_receivable, it_payable, it_currencyamount, it_extension1,
             it_bapiret,it_accounttax.

*> Verifica duplicidade do documento através da chv referencia(AWKEY)
    SELECT mandt bukrs belnr gjahr awkey blart awtyp awsys
            FROM bkpf
            INTO TABLE it_bkpf
           WHERE ( awtyp EQ 'IDOC'             )
             AND ( awkey EQ  wa_header-obj_key )
            ORDER BY awtyp awkey awsys.

    READ TABLE it_bkpf INTO wa_bkpf INDEX 1.

    IF ( sy-subrc EQ 0 ).

      CLEAR wa_return.
      wa_return-seqlan     = wa_header-seqlan.
      "wa_return-type       = 'E'.
      wa_return-type       = 'S'.
      wa_return-id         = 'Z01'.
      wa_return-number     = '002'.
      "   wa_return-message_v1 = wa_bkpf-bukrs.
      wa_return-message_v1 = wa_bkpf-belnr.
      wa_return-message_v2 = wa_bkpf-bukrs.
      wa_return-message_v3 = wa_bkpf-gjahr.

      v_message1 = wa_return-message_v1.
      v_message2 = wa_return-message_v2.
      v_message3 = wa_return-message_v3.


      IF sy-langu = 'E' OR '0200_0201_0202' CS wa_bkpf-bukrs.
        CALL FUNCTION 'MESSAGE_PREPARE'
          EXPORTING
            language = 'E'
            msg_id   = 'Z01'
            msg_no   = '002'
            msg_var1 = v_message1
            msg_var2 = v_message2
            msg_var3 = v_message3
          IMPORTING
            msg_text = wa_return-message.
      ELSE.
        CALL FUNCTION 'MESSAGE_PREPARE'
          EXPORTING
            language = 'P'
            msg_id   = 'Z01'
            msg_no   = '002'
            msg_var1 = v_message1
            msg_var2 = v_message2
            msg_var3 = v_message3
          IMPORTING
            msg_text = wa_return-message.
      ENDIF.

      APPEND wa_return TO it_return.
      CONTINUE.
    ENDIF.

*> Informações fixas - pré definadas para utilização da BAPI
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system = gd_documentheader-obj_sys.

*> OBJ_TYPE has to be replaced by customers object key (Y* or Z*)
    gd_documentheader-obj_type   = 'IDOC'.
    gd_documentheader-username   = sy-uname.
    gd_documentheader-pstng_date = sy-datum.             "BUDAT
    gd_documentheader-bus_act    = 'RFBU'.

    gd_documentheader-obj_key    = wa_header-obj_key.    "AWKEY
    gd_documentheader-header_txt = wa_header-bktxt.
    gd_documentheader-comp_code  = wa_header-bukrs.

    gd_documentheader-doc_date   = wa_header-bldat.
    gd_documentheader-pstng_date = wa_header-budat.
    gd_documentheader-fisc_year  = wa_header-gjahr.
    gd_documentheader-fis_period = wa_header-monat.
    gd_documentheader-doc_type   = wa_header-blart.
    gd_documentheader-ref_doc_no = wa_header-xblnr.


    CLEAR vg_tcode.

    CLEAR:  vg_count_tot, vg_count_iva,vg_lifnrvat.

    CLEAR: vg_wrbtr_base, vg_dmbtr_base,vg_dmbe2_base,vg_dmbe3_base. "totais de IVA
    LOOP AT it_item_aux INTO wa_item WHERE ( seqlan EQ wa_header-seqlan ).
      vg_tabix = sy-tabix.
      ADD 1 TO vg_count_tot.

      READ TABLE it_tbsl INTO wa_tbsl WITH KEY bschl = wa_item-bschl BINARY SEARCH.
      IF wa_tbsl-koart = 'K'.
        vg_lifnrvat = wa_item-hkont.
      ENDIF.

      IF wa_item-tax_code IS NOT INITIAL AND wa_item-vl_bc_iva_wrbtr GT 0.
        CALL FUNCTION 'RECP_FI_TAX_CALCULATE'
          EXPORTING
            ic_bukrs    = wa_header-bukrs
            ic_mwskz    = wa_item-tax_code
            ic_waers    = wa_item-waers
          IMPORTING
            ep_tax_rate = lv_tax_rate
          EXCEPTIONS
            not_found   = 1
            OTHERS      = 2.

        IF sy-subrc <> 0.

        ENDIF.

        "nova regra do IVA, quando tem Base
        "ALRS
        vg_base_iva = wa_item-wrbtr.
        CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
          EXPORTING
            i_bukrs           = wa_header-bukrs
            i_mwskz           = wa_item-tax_code
            i_waers           = wa_item-waers
            i_wrbtr           = vg_base_iva
          TABLES
            t_mwdat           = t_mwdat
          EXCEPTIONS
            bukrs_not_found   = 1
            country_not_found = 2
            mwskz_not_defined = 3
            mwskz_not_valid   = 4
            ktosl_not_found   = 5
            kalsm_not_found   = 6
            parameter_error   = 7
            knumh_not_found   = 8
            kschl_not_found   = 9
            unknown_error     = 10
            account_not_found = 11
            txjcd_not_valid   = 12
            OTHERS            = 13.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF lv_tax_rate NE 0.
          vg_wrbtr2 = wa_item-vl_bc_iva_wrbtr * ( lv_tax_rate / 100 ).
          vg_dmbtr  = wa_item-vl_bc_iva_dmbtr * ( lv_tax_rate / 100 ).
          vg_dmbe2  = wa_item-vl_bc_iva_dmbe2 * ( lv_tax_rate / 100 ).
          vg_dmbe3  = wa_item-dmbe3 * ( lv_tax_rate / 100 ).
        ELSEIF t_mwdat[] IS NOT INITIAL.
          READ TABLE t_mwdat INDEX 1.
          vg_wrbtr2 = wa_item-vl_bc_iva_wrbtr * ( abs( t_mwdat-msatz ) / 100 ).
          vg_dmbtr  = wa_item-vl_bc_iva_dmbtr * ( abs( t_mwdat-msatz ) / 100 ).
          vg_dmbe2  = wa_item-vl_bc_iva_dmbe2 * ( abs( t_mwdat-msatz ) / 100 ).
          vg_dmbe3  = wa_item-dmbe3           * ( abs( t_mwdat-msatz ) / 100 ).
        ENDIF.
        wa_item-wrbtr = wa_item-wrbtr + vg_wrbtr2.
        wa_item-dmbtr = wa_item-dmbtr + vg_dmbtr.
        wa_item-dmbe2 = wa_item-dmbe2 + vg_dmbe2.
        wa_item-dmbe3 = wa_item-dmbe3 + vg_dmbe3.
        MODIFY it_item_aux FROM wa_item INDEX vg_tabix TRANSPORTING wrbtr dmbtr dmbe2 dmbe3.

        ADD vg_wrbtr2 TO vg_wrbtr_base.
        ADD vg_dmbtr  TO vg_dmbtr_base.
        ADD vg_dmbe2  TO vg_dmbe2_base.
        ADD vg_dmbe3  TO vg_dmbe3_base.

      ENDIF.
    ENDLOOP.

    "IVA - Argentina (Já vem calculado )
    REFRESH it_zmmt_eeiva_zgr.
    SELECT *
      FROM zmmt_eeiva_zgr
      INTO TABLE it_zmmt_eeiva_zgr
      WHERE obj_key EQ wa_header-obj_key
      AND   mwskz   NE ''.

    CLEAR  vflag_zgr.
    LOOP AT it_zmmt_eeiva_zgr INTO wa_zmmt_eeiva_zgr.
      vflag_zgr = 'X'.
      LOOP AT it_item_aux INTO wa_item WHERE ( seqlan EQ wa_header-seqlan ).

        READ TABLE it_tbsl INTO wa_tbsl WITH KEY bschl = wa_item-bschl BINARY SEARCH.
        IF wa_tbsl-koart NE 'K'.
          EXIT.
        ENDIF.
      ENDLOOP.

      "
      CALL FUNCTION 'RECP_FI_TAX_CALCULATE'
        EXPORTING
          ic_bukrs    = wa_header-bukrs
          ic_mwskz    = wa_zmmt_eeiva_zgr-mwskz
          ic_waers    = wa_item-waers
        IMPORTING
          ep_tax_rate = lv_tax_rate
        EXCEPTIONS
          not_found   = 1
          OTHERS      = 2.

      IF sy-subrc <> 0.

      ENDIF.

      ADD wa_zmmt_eeiva_zgr-vlr_imp        TO vg_wrbtr_base.
      ADD wa_zmmt_eeiva_zgr-vlr_imp_dmbtr  TO vg_dmbtr_base.
      ADD wa_zmmt_eeiva_zgr-vlr_imp_dmbe2  TO vg_dmbe2_base.

      vg_base_iva = wa_zmmt_eeiva_zgr-vlr_base.
      CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
        EXPORTING
          i_bukrs           = wa_header-bukrs
          i_mwskz           = wa_zmmt_eeiva_zgr-mwskz
          i_waers           = wa_item-waers
          i_wrbtr           = vg_base_iva
        TABLES
          t_mwdat           = t_mwdat
        EXCEPTIONS
          bukrs_not_found   = 1
          country_not_found = 2
          mwskz_not_defined = 3
          mwskz_not_valid   = 4
          ktosl_not_found   = 5
          kalsm_not_found   = 6
          parameter_error   = 7
          knumh_not_found   = 8
          kschl_not_found   = 9
          unknown_error     = 10
          account_not_found = 11
          txjcd_not_valid   = 12
          OTHERS            = 13.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF t_mwdat[] IS NOT INITIAL.
        CLEAR t_mwdat.
        ADD 1 TO vg_count_tot.

        READ TABLE t_mwdat INDEX 1. "INTO s_mwdat
        IF  lv_tax_rate NE 0.
          wa_accounttax-itemno_acc   = vg_count_tot.
          wa_accounttax-gl_account   = t_mwdat-hkont.
          wa_accounttax-tax_code     = wa_zmmt_eeiva_zgr-mwskz.
          wa_accounttax-tax_rate     = lv_tax_rate.
          wa_accounttax-cond_key     = t_mwdat-kschl.
          wa_accounttax-acct_key     = t_mwdat-ktosl.
          APPEND wa_accounttax TO it_accounttax.
        ELSE.
          LOOP AT t_mwdat.
            lv_tax_rate = abs( t_mwdat-msatz ).
            wa_accounttax-itemno_acc   = vg_count_tot.
            wa_accounttax-gl_account   = t_mwdat-hkont.
            wa_accounttax-tax_code     = wa_zmmt_eeiva_zgr-mwskz.
            wa_accounttax-tax_rate     = t_mwdat-msatz.
            wa_accounttax-cond_key     = t_mwdat-kschl.
            wa_accounttax-acct_key     = t_mwdat-ktosl.
            APPEND wa_accounttax TO it_accounttax.
            ADD 1 TO vg_count_tot.
          ENDLOOP.
          SUBTRACT 1 FROM vg_count_tot.
        ENDIF.
      ENDIF.
      DO 3 TIMES.
        CLEAR: vg_wrbtr, wa_currencyamount.

        wa_currencyamount-itemno_acc    = vg_count_tot.

        CASE sy-index.
          WHEN 1.
            wa_currencyamount-curr_type = '00'.
            vg_wrbtr                    = wa_zmmt_eeiva_zgr-vlr_imp.
            vg_base                     = wa_zmmt_eeiva_zgr-vlr_base.
            wa_currencyamount-currency  = wa_item-waers.

          WHEN 2.
            wa_currencyamount-curr_type = '10'.
            vg_wrbtr                    = wa_zmmt_eeiva_zgr-vlr_imp_dmbtr.
            vg_base                     = wa_zmmt_eeiva_zgr-vlr_base_dmbtr.
            wa_currencyamount-currency  = wa_item-waers_i.

          WHEN 3.
            wa_currencyamount-curr_type = '40'.
            vg_wrbtr                    = wa_zmmt_eeiva_zgr-vlr_imp_dmbe2.
            vg_base                     = wa_zmmt_eeiva_zgr-vlr_base_dmbe2..
            wa_currencyamount-currency  = wa_item-waers_f.

        ENDCASE.
        IF lv_tax_rate NE 0.
          CHECK ( NOT vg_wrbtr IS INITIAL ) .
        ELSE.
          CHECK ( NOT vg_base  IS INITIAL ) .
        ENDIF.

        IF ( wa_tbsl-shkzg EQ 'H' ).
          IF lv_tax_rate = 0.
            wa_currencyamount-amt_doccur   = 0.
            wa_currencyamount-tax_amt  = 0.
            wa_currencyamount-amt_base = vg_base.
          ELSE.
            wa_currencyamount-amt_doccur   = vg_wrbtr * -1.
            wa_currencyamount-tax_amt  = ( ( vg_wrbtr * -1 ) * lv_tax_rate ) / 100.
            wa_currencyamount-amt_base = vg_base * -1.
          ENDIF.
        ELSE.
          IF lv_tax_rate = 0.
            wa_currencyamount-amt_doccur   = 0.
            wa_currencyamount-tax_amt  = 0.
            wa_currencyamount-amt_base = vg_base * -1.
          ELSE.
            wa_currencyamount-amt_doccur   = vg_wrbtr.
            wa_currencyamount-tax_amt  = ( vg_wrbtr * lv_tax_rate ) / 100.
            wa_currencyamount-amt_base = vg_base.
          ENDIF.

        ENDIF.


        APPEND wa_currencyamount TO it_currencyamount.

      ENDDO.
    ENDLOOP.
    "IVA -  Argentina
    "
    LOOP AT it_item_aux INTO wa_item WHERE ( seqlan EQ wa_header-seqlan ).
      vg_tabix = sy-tabix.
      READ TABLE it_tbsl INTO wa_tbsl WITH KEY bschl = wa_item-bschl BINARY SEARCH.
      IF wa_tbsl-koart = 'K'.
        wa_item-wrbtr = wa_item-wrbtr + vg_wrbtr_base.
        wa_item-dmbtr = wa_item-dmbtr + vg_dmbtr_base.
        wa_item-dmbe2 = wa_item-dmbe2 + vg_dmbe2_base.
        wa_item-dmbe3 = wa_item-dmbe3 + vg_dmbe3_base.
        "
        IF wa_item-wrbtr LT 0.
          MULTIPLY wa_item-wrbtr BY -1.
        ENDIF.
        "
        IF wa_item-dmbtr LT 0.
          MULTIPLY wa_item-dmbtr BY -1.
        ENDIF.
        "
        IF wa_item-dmbe2 LT 0.
          MULTIPLY wa_item-dmbe2 BY -1.
        ENDIF.

        IF wa_item-dmbe3 LT 0.
          MULTIPLY wa_item-dmbe3 BY -1.
        ENDIF.
        MODIFY it_item_aux FROM wa_item INDEX vg_tabix TRANSPORTING wrbtr dmbtr dmbe2 dmbe3.
        IF  vflag_zgr IS INITIAL.
          EXIT.
        ENDIF.
*      ELSEIF  VFLAG_ZGR = 'X'.
*        WA_ITEM-WRBTR = WA_ITEM-WRBTR + VG_WRBTR_BASE.
*        WA_ITEM-DMBTR = WA_ITEM-DMBTR + VG_DMBTR_BASE.
*        WA_ITEM-DMBE2 = WA_ITEM-DMBE2 + VG_DMBE2_BASE.
*        WA_ITEM-DMBE3 = WA_ITEM-DMBE3 + VG_DMBE3_BASE.
*        MODIFY IT_ITEM_AUX FROM WA_ITEM INDEX VG_TABIX TRANSPORTING WRBTR DMBTR DMBE2 DMBE3.
      ENDIF.
    ENDLOOP.

    REFRESH : it_accountwt.

    LOOP AT it_impostos_retidos INTO DATA(wa_impostos_retidos) WHERE obj_key EQ wa_header-obj_key.
      wa_accountwt-itemno_acc  = wa_impostos_retidos-itemno_acc.
      wa_accountwt-wt_type     = wa_impostos_retidos-wt_type.
      wa_accountwt-wt_code     = wa_impostos_retidos-wt_code.
      wa_accountwt-bas_amt_tc  = wa_impostos_retidos-bas_amt_tc.
      wa_accountwt-man_amt_tc  = wa_impostos_retidos-man_amt_tc.
      wa_accountwt-bas_amt_ind = wa_impostos_retidos-bas_amt_ind.
      wa_accountwt-man_amt_ind = wa_impostos_retidos-man_amt_ind.
      APPEND wa_accountwt TO it_accountwt.
    ENDLOOP.

    LOOP AT it_item_aux INTO wa_item WHERE ( seqlan EQ wa_header-seqlan ).

      vg_item = vg_item + 1.

      CLEAR wa_item_kidno.
      wa_item_kidno-item  = vg_item.
      wa_item_kidno-kidno = wa_item-kidno.
      wa_item_kidno-ds_cpf_treinamento = wa_item-ds_cpf_treinamento.
      wa_item_kidno-id_lms_treinamento = wa_item-id_lms_treinamento.
      APPEND wa_item_kidno TO it_item_kidno.

      READ TABLE it_tbsl INTO wa_tbsl WITH KEY bschl = wa_item-bschl BINARY SEARCH.
      IF ( sy-subrc NE 0 ).
        CLEAR wa_tbsl.
      ENDIF.

*> Analisa o tipo de conta e grava no registro respectivo.
      "Preenche o tipo de movimento
      CLEAR: vg_trtyp, vg_len.
      IF wa_header-bktxt(5) EQ 'CARGA'.

        vg_trtyp = wa_item-bewar.
        vg_len =  strlen( vg_item ) - 3.

        CONCATENATE vg_item+vg_len(3) vg_trtyp INTO wa_extension1-field1.

        APPEND wa_extension1 TO it_extension1.
      ENDIF.

      IF ( wa_tbsl-koart = 'S' ) OR ( wa_tbsl-koart = 'A' ) .
        IF wa_tbsl-koart = 'A'. "ALRS
          gd_documentheader-bus_act    = 'RMWE'.
        ENDIF.
        PERFORM f_fill_account.
      ENDIF.
      IF  wa_tbsl-koart = 'K'.
        PERFORM f_fill_vendor.
        vg_tcode = 'F-43'.
      ENDIF.
      IF wa_tbsl-koart = 'D'.
        PERFORM f_fill_customer.
        vg_tcode = 'F-22'.
      ENDIF.


*> Grava o montante da linha contábil, analisando o cód crédito/débito

      SELECT SINGLE bukrs land1 spras
          FROM t001
          INTO wa_t001
          WHERE bukrs = wa_header-bukrs.

      SELECT SINGLE land1 waers curin curha
        FROM t005
        INTO wa_t005
        WHERE land1 = wa_t001-land1.

      vg_loop_val = 1.
      CLEAR: vg_wrbtr_base, vg_dmbtr_base,vg_dmbe2_base,vg_dmbe3_base.

      IF wa_item-tax_code IS NOT INITIAL
        AND ( wa_header-bktxt(5) NE 'COMEX' OR wa_header-bukrs = '0100' ).  "desdobrar valores e adicionar a partida do IVA
        CALL FUNCTION 'RECP_FI_TAX_CALCULATE'
          EXPORTING
            ic_bukrs    = wa_header-bukrs
            ic_mwskz    = wa_item-tax_code
            ic_waers    = wa_item-waers
          IMPORTING
            ep_tax_rate = lv_tax_rate
          EXCEPTIONS
            not_found   = 1
            OTHERS      = 2.

        IF sy-subrc <> 0.

        ENDIF.

        "ALRS
        vg_base_iva = wa_item-wrbtr.
        CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
          EXPORTING
            i_bukrs           = wa_header-bukrs
            i_mwskz           = wa_item-tax_code
            i_waers           = wa_item-waers
            i_wrbtr           = vg_base_iva
          TABLES
            t_mwdat           = t_mwdat
          EXCEPTIONS
            bukrs_not_found   = 1
            country_not_found = 2
            mwskz_not_defined = 3
            mwskz_not_valid   = 4
            ktosl_not_found   = 5
            kalsm_not_found   = 6
            parameter_error   = 7
            knumh_not_found   = 8
            kschl_not_found   = 9
            unknown_error     = 10
            account_not_found = 11
            txjcd_not_valid   = 12
            OTHERS            = 13.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF lv_tax_rate NE 0.
          vg_wrbtr2 = wa_item-wrbtr.
          vg_dmbtr = wa_item-dmbtr.
          vg_dmbe2 = wa_item-dmbe2.
          vg_dmbe3 = wa_item-dmbe3.
          "Valor na conta principal
          wa_item-wrbtr = wa_item-wrbtr / ( 1 + ( lv_tax_rate / 100 ) ).
          wa_item-dmbtr = wa_item-dmbtr / ( 1 + ( lv_tax_rate / 100 ) ).
          wa_item-dmbe2 = wa_item-dmbe2 / ( 1 + ( lv_tax_rate / 100 ) ).
          wa_item-dmbe3 = wa_item-dmbe3 / ( 1 + ( lv_tax_rate / 100 ) ).
          "
          vg_wrbtr_base = wa_item-wrbtr.
          vg_dmbtr_base = wa_item-dmbtr.
          vg_dmbe2_base = wa_item-dmbe2.
          vg_dmbe3_base = wa_item-dmbe3.
          "valor na conta do IVA
          vg_wrbtr2 = vg_wrbtr2 - wa_item-wrbtr.
          vg_dmbtr = vg_dmbtr - wa_item-dmbtr.
          vg_dmbe2 = vg_dmbe2 - wa_item-dmbe2.
          vg_dmbe3 = vg_dmbe3 - wa_item-dmbe3.
        ELSEIF t_mwdat[] IS NOT INITIAL.
          READ TABLE t_mwdat INDEX 1.
          vg_wrbtr_base = wa_item-wrbtr.
          vg_dmbtr_base = wa_item-dmbtr.
          vg_dmbe2_base = wa_item-dmbe2.
          vg_dmbe3_base = wa_item-dmbe3.

          vg_wrbtr2 = wa_item-wrbtr * ( abs( t_mwdat-msatz ) / 100 ).
          vg_dmbtr = wa_item-dmbtr *  ( abs( t_mwdat-msatz ) / 100 ).
          vg_dmbe2 = wa_item-dmbe2 *  ( abs( t_mwdat-msatz ) / 100 ).
          vg_dmbe3 = wa_item-dmbe3 *  ( abs( t_mwdat-msatz ) / 100 ).
        ENDIF.

        IF t_mwdat[] IS NOT INITIAL.
          CLEAR t_mwdat.
          ADD 1 TO  vg_count_iva.
          READ TABLE t_mwdat INDEX 1. "INTO s_mwdat
          IF  lv_tax_rate NE 0.
            vg_loop_val = 2.
            wa_accounttax-itemno_acc   = vg_count_iva + vg_count_tot.
            wa_accounttax-gl_account   = t_mwdat-hkont.
            wa_accounttax-tax_code     = wa_item-tax_code.
            wa_accounttax-tax_rate     = lv_tax_rate.
            wa_accounttax-cond_key     = t_mwdat-kschl.
            wa_accounttax-acct_key     = t_mwdat-ktosl.
            APPEND wa_accounttax TO it_accounttax.
          ELSE.
            lv_count = lines( t_mwdat ).
            IF lv_count = 1.
              vg_loop_val = 2.
            ELSE.
              vg_loop_val = 3.
            ENDIF.
            LOOP AT t_mwdat.
              lv_tax_rate = abs( t_mwdat-msatz ).
              wa_accounttax-itemno_acc   = vg_count_iva + vg_count_tot.
              wa_accounttax-gl_account   = t_mwdat-hkont.
              wa_accounttax-tax_code     = wa_item-tax_code.
              wa_accounttax-tax_rate     = t_mwdat-msatz.
              wa_accounttax-cond_key     = t_mwdat-kschl.
              wa_accounttax-acct_key     = t_mwdat-ktosl.
              APPEND wa_accounttax TO it_accounttax.
              ADD 1 TO vg_count_iva.
            ENDLOOP.
            IF vg_loop_val = 3.
              SUBTRACT 2 FROM vg_count_iva.
            ELSE.
              SUBTRACT 1 FROM vg_count_iva.
            ENDIF.
          ENDIF.
          "
          vl_len =  strlen( wa_accounttax-itemno_acc ) - 3.
          CLEAR  wa_extension1.
          wa_extension1-field1 = 'DIVISAO'.
          CONCATENATE  wa_accounttax-itemno_acc+vl_len(3) wa_item-gsber INTO wa_extension1-field2.
          APPEND wa_extension1 TO it_extension1.
        ENDIF.
      ENDIF.

      DO vg_loop_val TIMES.
        vg_index = sy-index.
        DO 4 TIMES.
          CLEAR wa_currencyamount.

          wa_currencyamount-itemno_acc    = vg_item.

          IF wa_item-kurrf IS NOT INITIAL. "Ajuste, incluindo a taxa - AO.
            wa_currencyamount-exch_rate     = wa_item-kurrf.
          ENDIF.

          IF vg_index >= 2. "Substitui por valor do IVA
            wa_currencyamount-itemno_acc    = vg_count_iva + vg_count_tot.
            IF vg_index = 3.
              wa_currencyamount-itemno_acc    = vg_count_iva + vg_count_tot + 1.
            ENDIF.
            IF lv_tax_rate  GT 0."troca valor senao mantem para ser base do imposto
              wa_item-wrbtr = vg_wrbtr2.
              wa_item-dmbtr = vg_dmbtr.
              wa_item-dmbe2 = vg_dmbe2.
              wa_item-dmbe3 = vg_dmbe3.
            ENDIF.
          ENDIF.

          "Encerramento de Período REPORT ZFIR0047
          "Moeda Interna
          IF wa_header-interface EQ 36.
            "Acatar valores passados
            CASE sy-index.
              WHEN 1.
                wa_currencyamount-curr_type  = '00'.
                vg_wrbtr                     = wa_item-wrbtr.
                vg_base                      = vg_wrbtr_base.
                wa_currencyamount-currency   = wa_item-waers.
                IF vg_wrbtr EQ 0.
                  wa_currencyamount-amt_doccur = vg_wrbtr.
                  APPEND wa_currencyamount TO it_currencyamount.
                ENDIF.
              WHEN 2.
                wa_currencyamount-curr_type = '10'.
                vg_wrbtr                     = wa_item-dmbtr.
                vg_base                      = vg_dmbtr_base.
                wa_currencyamount-currency   = wa_item-waers_i.
                IF vg_wrbtr EQ 0.
                  wa_currencyamount-amt_doccur = vg_wrbtr.
                  APPEND wa_currencyamount TO it_currencyamount.
                ENDIF.
              WHEN 3.
                wa_currencyamount-curr_type = '40'.
                vg_wrbtr                     = wa_item-dmbe2.
                vg_base                      = vg_dmbe2_base.
                wa_currencyamount-currency   = wa_item-waers_f.
                IF vg_wrbtr EQ 0.
                  wa_currencyamount-amt_doccur = vg_wrbtr.
                  APPEND wa_currencyamount TO it_currencyamount.
                ENDIF.
              WHEN 4.
                wa_currencyamount-curr_type = '30'.
                vg_wrbtr                     = wa_item-dmbe3.
                vg_base                      = vg_dmbe3_base.
                wa_currencyamount-currency   = wa_item-waers_g.
                IF vg_wrbtr EQ 0.
                  wa_currencyamount-amt_doccur = vg_wrbtr.
                  APPEND wa_currencyamount TO it_currencyamount.
                ENDIF.
            ENDCASE.
          ELSE.
            CASE sy-index.
              WHEN 1.
                wa_currencyamount-curr_type = '00'.
                vg_wrbtr                    = wa_item-wrbtr.
                vg_base                     = vg_wrbtr_base.
                wa_currencyamount-currency  = wa_item-waers.
                IF wa_item-waers_i = 'X' AND vg_wrbtr = 0 . "uma moeda
                  wa_currencyamount-currency   = wa_item-waers. "moeda doc zera
                  wa_currencyamount-amt_doccur = 0.
                  APPEND wa_currencyamount TO it_currencyamount.
                  CONTINUE.
                ENDIF.
              WHEN 2.
                wa_currencyamount-curr_type = '10'.
                vg_wrbtr                    = wa_item-dmbtr.
                vg_base                     = vg_dmbtr_base.
                wa_currencyamount-currency  = wa_item-waers_i.
                IF wa_item-waers_i = 'X'.
                  wa_currencyamount-currency   = wa_t005-waers. "moeda empresa zera
                ENDIF.
                IF wa_item-waers_i = 'X' AND vg_wrbtr = 0 . "uma moeda
                  wa_currencyamount-currency   = wa_t005-waers. "moeda empresa zera
                  wa_currencyamount-amt_doccur = 0.
                  APPEND wa_currencyamount TO it_currencyamount.
                  CONTINUE.
                ENDIF.
              WHEN 3.
                wa_currencyamount-curr_type = '40'.
                vg_wrbtr                    = wa_item-dmbe2.
                vg_base                     = vg_dmbe2_base.
                wa_currencyamount-currency  = wa_item-waers_f.
                IF wa_item-waers_i = 'X' AND vg_wrbtr = 0 . "uma moeda
                  wa_currencyamount-currency   = wa_t005-curha. "moeda forte zera
                  wa_currencyamount-amt_doccur = 0.
                  APPEND wa_currencyamount TO it_currencyamount.
                  CONTINUE.
                ENDIF.
              WHEN 4.
                wa_currencyamount-curr_type = '30'.
                vg_wrbtr                    = wa_item-dmbe3.
                vg_base                     = vg_dmbe3_base.
                wa_currencyamount-currency  = wa_item-waers_g.
                IF wa_item-waers_i = 'X' AND vg_wrbtr = 0 . "uma moeda
                  wa_currencyamount-currency   = wa_t005-curin.
                  wa_currencyamount-amt_doccur = 0.
                  APPEND wa_currencyamount TO it_currencyamount.
                  CONTINUE.
                ENDIF.
            ENDCASE.
          ENDIF.

          CHECK ( NOT vg_wrbtr IS INITIAL ) .

          IF ( vl_docnum IS INITIAL ).
            vl_docnum = wa_item-xref1.
          ENDIF.

          IF ( wa_tbsl-shkzg EQ 'H' ).
            IF lv_tax_rate = 0 AND vg_index = 2.
              wa_currencyamount-amt_doccur   = 0.
            ELSE.
              wa_currencyamount-amt_doccur   = vg_wrbtr * -1.
            ENDIF.
            IF vg_index = 2.
              wa_currencyamount-tax_amt  = ( ( vg_wrbtr * -1 ) * lv_tax_rate ) / 100.
              wa_currencyamount-amt_base = vg_base * -1.
            ENDIF.
          ELSE.
            IF lv_tax_rate = 0 AND vg_index = 2.
              wa_currencyamount-amt_doccur   = 0.
            ELSE.
              wa_currencyamount-amt_doccur   = vg_wrbtr.
            ENDIF.
            IF vg_index = 2.
              wa_currencyamount-tax_amt  = ( vg_wrbtr * lv_tax_rate ) / 100.
              wa_currencyamount-amt_base = vg_base.
            ENDIF.
          ENDIF.

          IF vg_loop_val = 3 AND vg_index >= 2.
            READ TABLE it_accounttax INTO wa_accounttax WITH KEY itemno_acc = wa_currencyamount-itemno_acc.
            IF lv_tax_rate = 0 AND vg_index >= 2.
              wa_currencyamount-amt_doccur   = 0.
            ELSE.
              IF wa_accounttax-tax_rate < 0.
                wa_currencyamount-amt_doccur   = vg_wrbtr * -1.
              ELSE.
                wa_currencyamount-amt_doccur   = vg_wrbtr.
              ENDIF.
            ENDIF.
            IF vg_index >= 2.
              IF wa_accounttax-tax_rate < 0.
                wa_currencyamount-tax_amt  = ( ( vg_wrbtr * -1 ) * lv_tax_rate ) / 100.
                wa_currencyamount-amt_base = vg_base * -1.
              ELSE.
                wa_currencyamount-tax_amt  = ( ( vg_wrbtr ) * lv_tax_rate ) / 100.
                wa_currencyamount-amt_base = vg_base.
              ENDIF.
            ENDIF.
          ENDIF.
          APPEND wa_currencyamount TO it_currencyamount.

        ENDDO.

      ENDDO.
      IF wa_item-tax_code IS NOT INITIAL.
        IF vg_loop_val = 3.
          ADD 1 TO  vg_count_iva.
        ENDIF.
      ENDIF.

    ENDLOOP.

    "Se Form Amaggi Argentina
    SELECT SINGLE *
      FROM setleaf
      INTO wa_setleaf
     WHERE setname = 'MAGGI_EMPRESA_EXTERIOR'
       AND valfrom = wa_header-bukrs.

    IF sy-subrc IS INITIAL.

      vl_len =  strlen( wa_header-obj_key ) - 6.

      SELECT SINGLE *
        FROM zmmt_ee_zgr
        INTO wa_zmmt_ee_zgr
       WHERE obj_key = wa_header-obj_key+2(vl_len).

      SELECT *
        FROM zmmt_eeimp_zgr
        INTO TABLE it_zmmt_eeimp_zgr
       WHERE obj_key = wa_header-obj_key+2(vl_len)
        AND  wi_tax_base NE 0. "ALRS

      SELECT *
        FROM setleaf
        INTO TABLE it_setleaf
       WHERE setname = 'MAGGI_ZFIY0017' .

      SELECT *
        FROM setlinet
        INTO TABLE it_setlinet
       WHERE setname = 'MAGGI_ZFIY0017' .

      SORT : it_setlinet BY setname lineid,
             it_setleaf  BY setname lineid.

      LOOP AT it_setleaf INTO wa_setleaf.
        READ TABLE it_setlinet INTO wa_setlinet WITH KEY setname = wa_setleaf-setname  lineid = wa_setleaf-lineid.

        DELETE it_zmmt_eeimp_zgr WHERE wi_tax_code = wa_setleaf-valfrom AND wt_withcd =  wa_setlinet-descript.

      ENDLOOP.

      vl_index = 1.

      IF it_zmmt_eeimp_zgr[] IS NOT INITIAL.
        DO 3 TIMES.

          CLEAR vl_item.

          vl_index_c = vl_index.

          "CONCATENATE '000' VL_INDEX_C INTO VL_ITEM.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = vl_index_c
            IMPORTING
              output = vl_item.

          IF vl_index = 3.

            SELECT *
              INTO TABLE it_lfbw
              FROM lfbw
             WHERE lifnr = wa_zmmt_ee_zgr-lifnr
               AND bukrs = wa_zmmt_ee_zgr-comp_code.

            LOOP AT it_lfbw INTO wa_lfbw.
              CLEAR : wa_accountwt.
              "Se não tiver os impostos desconsidera o registro.
              IF wa_lfbw-witht IS NOT INITIAL.
                wa_accountwt-itemno_acc  = vl_item.
                wa_accountwt-wt_type     = wa_lfbw-witht.
                wa_accountwt-bas_amt_lc  = 0.
                wa_accountwt-bas_amt_tc  = 0.

                APPEND wa_accountwt TO it_accountwt.
              ENDIF.

            ENDLOOP.

          ELSE.
            LOOP AT it_zmmt_eeimp_zgr  INTO wa_zmmt_eeimp_zgr .
              CLEAR wa_accountwt.
              "Se não tiver os impostos desconsidera o registro.
              IF wa_zmmt_eeimp_zgr-wt_withcd IS INITIAL AND wa_zmmt_eeimp_zgr-wi_tax_code IS INITIAL.
                CONTINUE.
              ENDIF.
              wa_accountwt-itemno_acc  = vl_item.
              wa_accountwt-wt_type     = wa_zmmt_eeimp_zgr-wt_withcd.
              wa_accountwt-wt_code     = wa_zmmt_eeimp_zgr-wi_tax_code.
              wa_accountwt-bas_amt_lc  = wa_zmmt_eeimp_zgr-wi_tax_base.
              wa_accountwt-bas_amt_tc  = wa_zmmt_eeimp_zgr-wi_tax_base.

              APPEND wa_accountwt TO it_accountwt.

            ENDLOOP.
          ENDIF.

          vl_index = vl_index + 1.
        ENDDO.
      ENDIF.

      DELETE it_accountwt WHERE ( wt_type IS INITIAL OR wt_type EQ space ) AND ( wt_code IS INITIAL OR wt_code EQ space ).

    ENDIF.
    "Fim Amaggi Argentina

    "Inicio Busca dados IRF ZGL059.
    IF ( it_accountwt[] IS INITIAL      ) AND
       ( wa_header-obj_key(5) = 'ZGL17' ) AND
       ( strlen( wa_header-obj_key ) >= 15 ).

      CLEAR: wl_081, tg_088[].
      SELECT SINGLE *
        FROM zglt081 INTO wl_081
       WHERE bukrs     = wa_header-bukrs
         AND doc_lcto  = wa_header-obj_key+5(10)
         AND part_forn = 'X'
         AND bschl     = '31'.

      IF sy-subrc = 0.

        SELECT *
          FROM zglt088 INTO TABLE tg_088
         WHERE seq_lcto = wl_081-seq_lcto
           AND seqitem  = wl_081-seqitem.

        IF tg_088[] IS NOT INITIAL.

          READ TABLE it_payable INTO wa_payable WITH KEY vendor_no = wl_081-hkont.

          IF sy-subrc = 0.

            LOOP AT tg_088.

              CLEAR wa_accountwt.

              wa_accountwt-itemno_acc  = wa_payable-itemno_acc.
              wa_accountwt-wt_type     = tg_088-witht.
              wa_accountwt-wt_code     = tg_088-wt_withcd.
              wa_accountwt-bas_amt_tc  = tg_088-wi_tax_base.
              wa_accountwt-man_amt_tc  = tg_088-wi_tax_amt.

              IF tg_088-wt_basman IS NOT INITIAL.
                wa_accountwt-bas_amt_ind = tg_088-wt_basman.
              ENDIF.

              IF tg_088-wt_taxman IS NOT INITIAL.
                wa_accountwt-man_amt_ind = tg_088-wt_taxman.
              ENDIF.

              APPEND wa_accountwt TO it_accountwt.

            ENDLOOP.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.
    "Fim (Inicio Busca dados IRF ZGL059.)

    IF gd_documentheader-header_txt(5) EQ 'COMEX' AND wa_header-bukrs NE '0100'.
      LOOP AT it_accountgl INTO wa_accountgl.
        CLEAR wa_accountgl-tax_code.
        MODIFY it_accountgl FROM wa_accountgl INDEX sy-tabix TRANSPORTING tax_code.
      ENDLOOP.

      LOOP AT it_receivable INTO wa_receivable.
        CLEAR wa_receivable-tax_code.
        MODIFY it_receivable FROM wa_receivable INDEX sy-tabix TRANSPORTING tax_code.
      ENDLOOP.

      LOOP AT it_payable INTO wa_payable.
        CLEAR wa_payable-tax_code.
        MODIFY it_payable FROM wa_payable INDEX sy-tabix TRANSPORTING tax_code.
      ENDLOOP.

    ENDIF.

    CLEAR wa_returnobj.
    wa_returnobj-seqlan   = wa_header-seqlan.
    sy-tcode              = 'FB05'.

    DATA: wa_zib_contabil_chv LIKE zib_contabil_chv.
    wa_zib_contabil_chv-mandt   = sy-mandt.
    wa_zib_contabil_chv-obj_key = gd_documentheader-obj_key.
    INSERT INTO zib_contabil_chv VALUES wa_zib_contabil_chv.

    IF ( sy-subrc EQ 0 ).

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader    = gd_documentheader
        IMPORTING
          obj_type          = wa_returnobj-obj_type
          obj_key           = wa_returnobj-obj_key
          obj_sys           = wa_returnobj-obj_sys
        TABLES
          criteria          = it_criteria
          accountgl         = it_accountgl
          accountreceivable = it_receivable
          accountpayable    = it_payable
          currencyamount    = it_currencyamount
          accounttax        = it_accounttax
          extension1        = it_extension1
          return            = it_bapiret
          accountwt         = it_accountwt.

      READ TABLE it_bapiret INTO wa_bapiret WITH KEY type = 'E'.
      IF ( sy-subrc EQ 0 ).
        DELETE FROM zib_contabil_chv
         WHERE obj_key EQ wa_zib_contabil_chv-obj_key.
      ELSE.
*** 19.10.2023 - CSB - Inicio
* A = termination message (abort)
        READ TABLE it_bapiret INTO wa_bapiret WITH KEY type = 'A'.
        IF ( sy-subrc EQ 0 ).
          DELETE FROM zib_contabil_chv
           WHERE obj_key EQ wa_zib_contabil_chv-obj_key.
        ENDIF.
*** 19.10.2023 - CSB - Fim
      ENDIF.
      CLEAR wa_bapiret.

    ELSE.

      SELECT SINGLE *
        INTO wa_zib_contabil_chv
        FROM zib_contabil_chv
       WHERE obj_key EQ gd_documentheader-obj_key.

      wa_bapiret-type	= 'E'.
      wa_bapiret-id   = 'ZPK'.
      wa_bapiret-number = '003'.
      CONCATENATE 'CHAVE DUPLICADA' ': - Este documento já foi enviado ao SAP' INTO wa_bapiret-message.
      CONCATENATE wa_bapiret-message ' Doc: ' wa_zib_contabil_chv-belnr ' Empresa: ' wa_zib_contabil_chv-bukrs ' Ano: ' wa_zib_contabil_chv-gjahr
             INTO wa_bapiret-message.
      wa_bapiret-message_v1 = wa_returnobj-obj_type.
      wa_bapiret-message_v2 = wa_returnobj-obj_key.
      wa_bapiret-message_v3 = wa_returnobj-obj_sys.
      wa_bapiret-message_v4 = 'Para enviar novamente deve ser incrementado 1 no ID_ENVIADO_SAP'.
      APPEND wa_bapiret TO it_bapiret.
    ENDIF.

    READ TABLE it_bapiret INTO wa_bapiret WITH KEY type = 'E'.
    IF ( sy-subrc NE 0 ).
* CSB - Inicio -  * a = termination message (abort)
      READ TABLE it_bapiret INTO wa_bapiret WITH KEY type = 'A'.
      IF ( sy-subrc NE 0 ).
* CSB - Fim.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        COMMIT WORK.

        CLEAR: vg_belnr, vg_bukrs, vg_gjahr.

        UPDATE bkpf SET tcode = 'FB05'
                  WHERE awtyp EQ wa_returnobj-obj_type
                    AND awkey EQ wa_returnobj-obj_key.

        COMMIT WORK.

        SELECT awtyp awkey awsys belnr bukrs gjahr
          FROM bkpf UP TO 1 ROWS
          INTO (vg_awtyp, vg_awkey, vg_awsys,
                vg_belnr, vg_bukrs, vg_gjahr)
         WHERE ( awtyp EQ wa_returnobj-obj_type )
           AND ( awkey EQ wa_returnobj-obj_key  )
          ORDER BY awtyp awkey awsys.
        ENDSELECT.

        wa_returnobj-bukrs = vg_bukrs.
        wa_returnobj-gjahr = vg_gjahr.
        wa_returnobj-belnr = vg_belnr.

* Inicio - US80965 - NumenIT - 03.11.2022

        IF vg_bukrs = '0200'.

          DATA:lt_bseg_aux TYPE TABLE OF bseg,
               lw_bseg     TYPE bseg.

* ---> S4 Migration - 15/06/2023 - MA
*        select *
*          into table @data(IT_BSEG_AUX)
*          from BSEG
*          where BUKRS = @VG_BUKRS and
*                BELNR = @VG_BELNR and
*                GJAHR = @VG_GJAHR.

          DATA: lt_bseg     TYPE fagl_t_bseg,
                it_bseg_aux TYPE TABLE OF bseg.

          CALL FUNCTION 'FAGL_GET_BSEG'
            EXPORTING
              i_bukrs   = vg_bukrs
              i_belnr   = vg_belnr
              i_gjahr   = vg_gjahr
            IMPORTING
              et_bseg   = lt_bseg
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.


          IF sy-subrc = 0 AND lines( lt_bseg ) > 0.
            MOVE-CORRESPONDING lt_bseg TO it_bseg_aux.
            sy-dbcnt = lines( lt_bseg ).
          ELSE.
            sy-subrc = 4.
            sy-dbcnt = 0.
          ENDIF.
*<--- S4 Migration - 15/06/2023 - MA

          IF sy-subrc IS INITIAL.

            lt_bseg_aux[] = it_bseg_aux[].

*          LOOP AT it_bseg_aux INTO DATA(wa_bseg_aux).
*            IF wa_bseg_aux-buzid = 'T'.
*              APPEND wa_bseg_aux TO it_bseg_buzid.
*              CLEAR wa_bseg_aux.
*            ENDIF.
*          ENDLOOP.

*          DELETE it_bseg_aux WHERE buzid <> ''.
            DELETE it_bseg_aux WHERE mwskz = ''.

            SORT it_bseg_buzid BY belnr gjahr buzei.
            SORT it_bseg_aux   BY belnr gjahr buzei.

            LOOP AT it_bseg_aux INTO DATA(wa_bseg_aux).
*            vg_index_bseg = vg_index_bseg + 1.
*            READ TABLE it_bseg_buzid INTO DATA(wa_bseg_buzid) INDEX vg_index_bseg.
*            IF sy-subrc IS INITIAL.

*              IF wa_bseg_aux-mwskz = wa_bseg_buzid-mwskz.

              v_buzei = wa_bseg_aux-buzei.
              IF wa_bseg_aux-buzid = 'T'.

                CLEAR:lw_bseg.
                READ TABLE lt_bseg_aux INTO lw_bseg WITH KEY mwskz = wa_bseg_aux-mwskz
                                                             txgrp = wa_bseg_aux-txgrp.

                REFRESH:t_accchg.
                w_accchg-fdname = 'SGTXT'.
                w_accchg-newval = lw_bseg-sgtxt.
                APPEND w_accchg TO t_accchg.
                CLEAR:w_accchg.

              ELSE.
                REFRESH:t_accchg.
                w_accchg-fdname = 'SGTXT'.
                w_accchg-newval = wa_bseg_aux-sgtxt.
                APPEND w_accchg TO t_accchg.
                CLEAR:w_accchg.
              ENDIF.

              CALL FUNCTION 'FI_DOCUMENT_CHANGE'
                EXPORTING
                  i_buzei  = v_buzei
                  i_bukrs  = vg_bukrs
                  i_belnr  = vg_belnr
                  i_gjahr  = vg_gjahr
                TABLES
                  t_accchg = t_accchg.

*                UPDATE bseg
*                   SET sgtxt = wa_bseg_aux-sgtxt
*                 WHERE belnr = vg_belnr
*                   AND bukrs = vg_bukrs
*                   AND gjahr = vg_gjahr
*                   AND buzei = wa_bseg_buzid-buzei.
              IF sy-subrc IS INITIAL.
                COMMIT WORK.
              ENDIF.
*              ENDIF.

*            ENDIF.
            ENDLOOP.

            CLEAR vg_index_bseg.
          ENDIF.

        ENDIF.

* Fim - US80965 - NumenIT - 03.11.2022

        LOOP AT it_item_kidno INTO wa_item_kidno.

*        REFRESH:t_accchg.
*        w_accchg-fdname = 'KIDNO'.
*        w_accchg-newval = wa_item_kidno-kidno.
*        APPEND w_accchg TO t_accchg.
*        CLEAR:w_accchg.
*
*        v_buzei = wa_item_kidno-item.
*        CALL FUNCTION 'FI_DOCUMENT_CHANGE'
*          EXPORTING
*            i_buzei  = v_buzei
*            i_bukrs  = vg_bukrs
*            i_belnr  = vg_belnr
*            i_gjahr  = vg_gjahr
*          TABLES
*            t_accchg = t_accchg.
*
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.

          UPDATE bseg
             SET kidno = wa_item_kidno-kidno
           WHERE belnr = vg_belnr
             AND bukrs = vg_bukrs
             AND gjahr = vg_gjahr
             AND buzei = wa_item_kidno-item.

          IF wa_item_kidno-id_lms_treinamento IS NOT INITIAL.
* ---> S4 Migration - 15/06/2023 - MA
*          SELECT SINGLE * INTO @DATA(wa_bseg)
*            FROM bseg
*           WHERE belnr EQ @vg_belnr
*             AND bukrs EQ @vg_bukrs
*             AND gjahr EQ @vg_gjahr
*             AND buzei EQ @wa_item_kidno-item.

            DATA: "LT_BSEG type FAGL_T_BSEG,
                  wa_bseg TYPE bseg.

            CALL FUNCTION 'FAGL_GET_BSEG'
              EXPORTING
                i_bukrs   = vg_belnr
                i_belnr   = vg_bukrs
                i_gjahr   = vg_gjahr
              IMPORTING
                et_bseg   = lt_bseg
              EXCEPTIONS
                not_found = 1
                OTHERS    = 2.

            DELETE lt_bseg WHERE buzei NE wa_item_kidno-item.

            READ TABLE lt_bseg INTO DATA(ls_bseg) INDEX 1.
            IF sy-subrc = 0.
              MOVE-CORRESPONDING ls_bseg TO wa_bseg.
            ENDIF.
*<--- S4 Migration - 15/06/2023 - MA


            IF sy-subrc IS INITIAL.
              TRY .
                  CALL FUNCTION 'ZMF_ZMMT0105_CREATE_FROM_BSEG'
                    EXPORTING
                      i_bseg               = wa_bseg
                      i_ds_cpf_treinamento = wa_item_kidno-ds_cpf_treinamento
                      i_id_lms_treinamento = wa_item_kidno-id_lms_treinamento.
                CATCH zcx_integracao.
              ENDTRY.
            ENDIF.
          ENDIF.
        ENDLOOP.

        APPEND wa_returnobj TO it_returnobj.

        UPDATE zib_contabil_chv
           SET belnr = vg_belnr
               bukrs = vg_bukrs
               gjahr = vg_gjahr
         WHERE obj_key EQ wa_returnobj-obj_key.

*> Elimina mensagem standard de sucesso e formata com os dados do
*> documento gerado no R/3 para que o SIGAM possa fazer referência
        DELETE it_bapiret WHERE ( type EQ 'S' ).
        CLEAR wa_bapiret.
        wa_bapiret-type   = 'S'.
        wa_bapiret-id     = 'Z01'.
        wa_bapiret-number = '003'.
        wa_bapiret-message_v1 = vg_belnr.
        wa_bapiret-message_v2 = vg_bukrs.
        wa_bapiret-message_v3 = vg_gjahr.

        IF sy-langu = 'E' OR '0200_0201_0202' CS vg_bukrs.
          CALL FUNCTION 'MESSAGE_PREPARE'
            EXPORTING
              language = 'E'
              msg_id   = 'Z01'
              msg_no   = '003'
              msg_var1 = wa_bapiret-message_v1
              msg_var2 = wa_bapiret-message_v2
              msg_var3 = wa_bapiret-message_v3
            IMPORTING
              msg_text = wa_bapiret-message.
        ELSE.
          CALL FUNCTION 'MESSAGE_PREPARE'
            EXPORTING
              language = 'P'
              msg_id   = 'Z01'
              msg_no   = '003'
              msg_var1 = wa_bapiret-message_v1
              msg_var2 = wa_bapiret-message_v2
              msg_var3 = wa_bapiret-message_v3
            IMPORTING
              msg_text = wa_bapiret-message.
        ENDIF.
        APPEND wa_bapiret TO it_bapiret.

*> Uso exclusivo para disparar Workflown de bloqueio de pagamentos
        IF ( vg_tcode EQ 'F-43' ) AND ( NOT vg_belnr IS INITIAL ).

          CALL FUNCTION 'Z_FI_APROVAR_PAGAMENTOS'
            EXPORTING
              belnr = vg_belnr
              bukrs = vg_bukrs
              gjahr = vg_gjahr.

        ENDIF.

      ENDIF.
    ENDIF. "19.10.2023 - CSB

    PERFORM show_messages TABLES it_return.
*> Fim

  ENDLOOP.

  sy-tcode              = vg_tcode_old.

ENDFUNCTION.
