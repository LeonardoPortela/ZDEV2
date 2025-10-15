class ZCL_INTEGRA_VIP definition
  public
  final
  create public .

public section.

  constants AT_API_INVOICE type STRING value 'invoice_posting' ##NO_TEXT.
  constants AT_API_INVOICE_REVERSAL type STRING value 'invoice_reversal' ##NO_TEXT.
  constants AT_API_INVOICE_CANCEL type STRING value 'invoice_cancel' ##NO_TEXT.

  methods SET_EXCHANGE_RATES .
  methods SET_COUNTERPARTIES
    importing
      !I_LFA1 type LFA1 optional
      !I_KNA1 type KNA1 optional
      !I_CREATION type CHAR1 .
  methods SET_COUNTERPARTIES_BANK
    importing
      !I_LFA1 type LFA1
      !T_LFBK type VMDS_LFBK_T
      !I_CREATION type CHAR1 .
  methods SEND_XML_TO_API
    importing
      !I_XML type STRING
      !I_METODO type STRING optional
      !I_LFA1 type LFA1 optional
      !I_KNA1 type KNA1 optional
      !I_CREATION type CHAR1 optional .
  methods SET_INVOICE_XML_TO_SAP
    importing
      !I_XML type STRING
    returning
      value(R_MESSAGE) type STRING .
  methods SET_INVOICE_ZIB_CONTABIL
    importing
      value(I_ZFIT0164) type ZFIT0164 optional .
  methods SET_INVOICE_REVERSE
    importing
      value(I_ZFIT0164) type ZFIT0164 .
  methods SET_RETORNA_STATUS .
  methods GET_SIMPLE_PAYMENT
    importing
      !I_TRANSNO type ZFIT0164-TRANSNO optional .
  methods CONVERT_INVOICE_XML_TO_ABAP
    importing
      !I_XML type STRING
    exporting
      !R_MSG_RETURN type STRING .
  methods LIMPA_TABELA_MENSAGENS .
  methods GET_EXCHANGE_RATES
    importing
      !I_DATE type SY-DATUM
      !I_FCURR type FCURR_CURR
      !I_TCURR type TCURR_CURR
    returning
      value(R_UKURS) type UKURS_CURR .
  methods SEND_CONTRAPARTIES .
  methods ARREDONDA_CONTABIL
    changing
      value(I_ZIB_CONTABIL) type ZFIS_VIP_ZIB_CONTABIL optional .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_INTEGRA_VIP IMPLEMENTATION.


  METHOD convert_invoice_xml_to_abap.

    DATA: lva_xml_xstring    TYPE xstring,
          v_value            TYPE string,
          l_data             TYPE REF TO data,
          it_return          TYPE STANDARD TABLE OF bapiret2,
          it_xml_table       TYPE STANDARD TABLE OF smum_xmltb,
          it_xml_info        TYPE TABLE OF smum_xmltb,
          it_header          TYPE TABLE OF zfit_vip_invoice,
          l_columns          TYPE REF TO cl_salv_columns_table,
          l_aggregations     TYPE REF TO cl_salv_aggregations,
          it_fieldcat_header TYPE lvc_t_fcat,
          l_salv_table       TYPE REF TO cl_salv_table.

    FIELD-SYMBOLS: <f_table>  TYPE STANDARD TABLE,
                   <fs_value> TYPE any.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = i_xml
      IMPORTING
        buffer = lva_xml_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc = 0.

      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lva_xml_xstring
        TABLES
          xml_table = it_xml_table[]  " XML Table structure used for retreive and output XML doc
          return    = it_return[].    " XML Table structure used for retreive and output XML doc

      IF ( line_exists( it_return[ type = 'E' ] ) ). "Erro

        TRY.
            r_msg_return = it_return[ 1 ]-message.
            RETURN.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      ELSE. "Sucesso

        it_xml_info[] = CORRESPONDING #( it_xml_table[] ).

        LOOP AT it_xml_info ASSIGNING FIELD-SYMBOL(<fs_info>).
          TRANSLATE <fs_info>-cname TO UPPER CASE.
        ENDLOOP.

      ENDIF.

      " CRIA UMA ESTRUTURA COM O MESMO LAYOUT DA TABELA DE SAÍDA
      CREATE DATA l_data LIKE it_xml_info[].
      ASSIGN l_data->* TO <f_table>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      " Monta a estrutura dinâmica no objeto l_salv_table
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              list_display = abap_false
            IMPORTING
              r_salv_table = l_salv_table
            CHANGING
              t_table      = <f_table> ).
        CATCH cx_salv_msg.
      ENDTRY.

      " Recupera as colunas e dados internos
      l_columns      = l_salv_table->get_columns( ).
      l_aggregations = l_salv_table->get_aggregations( ).

      " Monta o fieldcat
      it_fieldcat_header = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
        r_columns      = l_columns
        r_aggregations = l_aggregations ).

      APPEND INITIAL LINE TO it_header ASSIGNING FIELD-SYMBOL(<fs_header>).

      LOOP AT it_fieldcat_header ASSIGNING FIELD-SYMBOL(<fs_fcat>).

        CHECK <fs_fcat>-fieldname <> 'INVOICEDETAILS'.

        ASSIGN COMPONENT <fs_fcat>-fieldname OF STRUCTURE <fs_header> TO <fs_value>.
        CHECK <fs_value> IS ASSIGNED.

        CLEAR v_value.

        READ TABLE it_xml_info INTO DATA(w_xml_info) WITH KEY cname = 'INVOICE'.

        IF ( sy-subrc = 0 ).

          DATA(lva_hier) = w_xml_info-hier.
          lva_hier = ( lva_hier + 1 ).

          READ TABLE it_xml_info[] INTO w_xml_info WITH KEY cname = <fs_fcat>-fieldname
                                                            hier  = lva_hier.
          IF ( sy-subrc = 0 ).
            <fs_value> = w_xml_info-cvalue.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_exchange_rates.

    DATA: gva_exch_date  TYPE sy-datum,
          lva_date       TYPE sy-datum,
          lva_gdatu_base TYPE char10,
          lva_gdatu      TYPE tcurr-gdatu.

    lva_date = i_date.

    WHILE r_ukurs IS INITIAL.

      CALL FUNCTION 'READ_EXCHANGE_RATE'
        EXPORTING
          client           = sy-mandt
          date             = lva_date
          foreign_currency = i_fcurr
          local_currency   = i_tcurr
          exact_date       = abap_true
        IMPORTING
          exchange_rate    = r_ukurs
          valid_from_date  = gva_exch_date
        EXCEPTIONS
          no_rate_found    = 1
          no_factors_found = 2
          no_spread_found  = 3
          derived_2_times  = 4
          overflow         = 5
          zero_rate        = 6
          OTHERS           = 7.

      IF ( sy-subrc <> 0 ).

        lva_gdatu_base = |{ lva_date+6(2) }.{ lva_date+4(2) }.{ lva_date+0(4) }|.

        CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
          EXPORTING
            input  = lva_gdatu_base    " Entered date (screen)
          IMPORTING
            output = lva_gdatu.  " Technical date in invert.

        SELECT SINGLE * FROM tcurr
          INTO @DATA(lwa_tcurr)
          WHERE kurst = 'B'
            AND fcurr = @i_fcurr
            AND tcurr = @i_tcurr
            AND gdatu = @lva_gdatu.
        IF ( sy-subrc = 0 ).
          r_ukurs = lwa_tcurr-ukurs.
        ENDIF.

      ENDIF.

      lva_date = ( lva_date - 1 ).

    ENDWHILE.

  ENDMETHOD.


  METHOD get_simple_payment.

    DATA: gva_xml_result        TYPE xstring,
          gva_xml_string        TYPE string,
          lva_pagamento_parcial TYPE abap_bool,
          lit_data              TYPE TABLE OF setleaf,
          lva_data              TYPE zfit0164-augbl,
          lwa_zfit0164_retcomp  TYPE zfit0164_retcomp,
          lwa_retcomp           TYPE zfit0164_retcomp,
          lwa_zfit0164_aux      TYPE zfit0164,
          lva_count             TYPE i.

    DATA: rg_augbl TYPE zrsdsselopts.
    DATA: lva_gjahr TYPE bsak-gjahr.
    DATA: lva_gjahr_parc TYPE bsak-gjahr.

    DATA: lva_valor_w TYPE bsis-wrbtr,
          lva_valor_d TYPE bsis-dmbe2.

    DATA: lva_wrbtr TYPE bsis-wrbtr,
          lva_dmbe2 TYPE bsis-dmbe2.

    DATA: lva_wrbtr_sum TYPE bsis-wrbtr,
          lva_dmbe2_sum TYPE bsis-dmbe2.

    DATA: lva_transno TYPE zfit0164-transno.


*** Issue - 64595 - Inicio - CBRAND
    SELECT *
      FROM setleaf INTO TABLE lit_data
     WHERE setname = 'MAGGI_SUICA_VIP_ENV'.

    READ TABLE lit_data INTO DATA(lwa_data) INDEX 1.
    CONCATENATE lwa_data-valfrom+6(4) lwa_data-valfrom+3(2) lwa_data-valfrom+0(2) INTO lva_data.
*** Issue - 64595 - Fim - CBRAND

    IF i_transno IS INITIAL.

      SELECT * FROM zfit0164
        INTO TABLE @DATA(it_zfit0164)
        WHERE status = '1' "Posted, not paid
         AND ck_compensado IN ( '', 'P' ).
      "AND transno EQ @lva_transno.

*** BUG - 91927 - Inicio - CBRAND
    ELSE.
      SELECT * FROM zfit0164
        INTO TABLE it_zfit0164
      WHERE status = '1' "Posted, not paid
        AND transno = i_transno. "lva_transno."

      SELECT * FROM zfit0164_retcomp
        INTO TABLE @DATA(it_zfit0164_retcomp)
      WHERE transno = @i_transno.

      lva_count = lines( it_zfit0164_retcomp ).

      IF lva_count > 1.
        lva_pagamento_parcial = abap_true.
      ENDIF.

    ENDIF.
*** BUG - 91927 - FIM - CBRAND

    IF ( sy-subrc = 0 ).

      LOOP AT it_zfit0164[] INTO DATA(lwa_zfit0164).

        DATA(lva_gjhar) = CONV gjahr( lwa_zfit0164-actdate+0(4) ).
        lva_gjahr_parc = CONV gjahr( lwa_zfit0164-actdate+0(4) ).
        DATA(lva_obj_key) = CONV awkey( |{ lwa_zfit0164-transno }{ lva_gjhar }| ).

        SELECT SINGLE * FROM zib_contabil
          INTO @DATA(lwa_zib)
          WHERE obj_key = @lva_obj_key
            AND bschl IN ( '31', '21', '39', '29',
                           '01', '11', '09', '19'  ).

        SELECT SINGLE * FROM zib_contabil_chv
        INTO @DATA(lwa_zib_chv)
        WHERE obj_key = @lva_obj_key.

        DATA(lva_chv_belnr) = lwa_zib_chv-belnr. " Pega o documento original

        IF lwa_zib_chv IS NOT INITIAL.

          CASE lwa_zib-bschl.
            WHEN '31' OR '21' OR '39' OR '29'. "FORNECEDOR

              IF lwa_zfit0164-ck_compensado = 'P'.
                lwa_zib_chv-belnr = lwa_zfit0164-augbl. " " Se for parcial, buscar com base no último documento de compensação:
                lva_gjahr = lwa_zfit0164-augdt+0(4).
                SELECT SINGLE * FROM bsak
                  INTO @DATA(lwa_bsak)
                  WHERE bukrs = @lwa_zib_chv-bukrs
                    AND belnr = @lwa_zib_chv-belnr
                    AND gjahr = @lva_gjahr
                    AND augbl <> @lwa_zfit0164-augbl.

*** BUG - 102751 - CBRAND - Inicio
**** Documento com moeda usd e compensação em outra moeda, pagamento parcial.
                IF lwa_bsak IS INITIAL.
* Busco o documento original.

                  SELECT SINGLE * FROM zib_contabil_chv
                    INTO @DATA(lwa_zib_chv_or)
                    WHERE obj_key = @lva_obj_key.

                  SELECT SINGLE * FROM bsak
                      INTO @DATA(lwa_bsak_origem)
                      WHERE bukrs = @lwa_zib_chv_or-bukrs
                        AND belnr = @lwa_zib_chv_or-belnr
                        AND gjahr = @lwa_zib_chv_or-gjahr.

                  IF lwa_bsak_origem-augbl IS NOT INITIAL.

                    lva_gjahr = lwa_zfit0164-augdt+0(4).

                    FREE rg_augbl.
                    APPEND  VALUE #( sign = 'I'
                                     option = 'EQ'
                                     low = lwa_bsak_origem-augbl
                                   ) TO rg_augbl.

                    SELECT * FROM zfit0164_retcomp
                       INTO TABLE @DATA(lit_zfit0164_retcomp)
                       WHERE transno = @lwa_zfit0164-transno.

* Uso o documento que esta na 0164 para buscar a proxima compensação.
                    LOOP AT lit_zfit0164_retcomp INTO DATA(lwa_zfit0164_origem).

                      APPEND  VALUE #( sign = 'I'
                                       option = 'EQ'
                                       low = lwa_zfit0164_origem-augbl
                                     ) TO rg_augbl.

                    ENDLOOP.

                    SELECT SINGLE * FROM bsak
                      INTO lwa_bsak
                      WHERE bukrs = lwa_zib_chv_or-bukrs
                        AND belnr = lwa_bsak_origem-augbl
                        AND gjahr = lva_gjahr
                        AND augbl NOT IN rg_augbl.

                    IF lwa_bsak IS NOT INITIAL.

                      SELECT SINGLE waers FROM bsak
                        INTO  @DATA(lva_waers)
                      WHERE belnr = @lwa_bsak-augbl
                        AND bukrs = @lwa_bsak-bukrs
                        AND gjahr = @lwa_bsak-gjahr.

                      IF lva_waers = lwa_zfit0164-currency.
                        CLEAR lwa_bsak.
                      ELSE.
                        DATA(lva_dif_moeda) = 'S'.
                      ENDIF.
                    ENDIF.

                  ENDIF.
                ENDIF.
*** BUG - 102751 - CBRAND - Fim
              ELSE.
                SELECT SINGLE * FROM bsak
                  INTO lwa_bsak
                  WHERE bukrs = lwa_zib_chv-bukrs
                    AND belnr = lwa_zib_chv-belnr
                    AND gjahr = lwa_zib_chv-gjahr.
              ENDIF.

              IF lwa_bsak IS NOT INITIAL.
*              IF ( sy-subrc = 0 ).

                lva_gjahr = lwa_bsak-augdt+0(4).
*               Busca pagamento parcial
                SELECT SINGLE * FROM bsak
                  INTO @DATA(lwa_bsak_parc)
                  WHERE bukrs = @lwa_bsak-bukrs
                    AND belnr = @lwa_bsak-augbl
                    AND gjahr = @lva_gjahr.

                IF ( sy-subrc = 0 ).
                  SELECT SINGLE * FROM bsik
                    INTO @DATA(lwa_bsik)
                    WHERE bukrs = @lwa_bsak_parc-bukrs
                      AND belnr = @lwa_bsak_parc-belnr
                      AND gjahr = @lwa_bsak_parc-gjahr.
                  IF ( sy-subrc = 0 ). "Se encontrar valor, documento é pgto parcial
                    lva_pagamento_parcial = abap_true.
                  ENDIF.
                ENDIF.

                SELECT SINGLE * FROM bkpf
                  INTO  @DATA(lwa_bkpf)
                  WHERE bukrs = @lwa_bsak-bukrs
                    AND belnr = @lwa_bsak-augbl
                    AND gjahr = @lva_gjahr. "@lwa_bsak-gjahr. mudei dia 23.06.2022 CSB

                SELECT  * FROM bsis
                  INTO TABLE @DATA(lit_bsis)
                  WHERE bukrs = @lwa_bsak-bukrs
                    AND belnr = @lwa_bsak-augbl
                    AND gjahr = @lva_gjahr."@lwa_bsak-gjahr. mudei dia 23.06.2022 CSB

                IF lit_bsis IS NOT INITIAL.

                  LOOP AT lit_bsis INTO DATA(lwa_bsis).

                    SELECT SINGLE * FROM skb1
                      INTO  @DATA(lwa_skb1_aux)
                        WHERE bukrs  = @lwa_bsis-bukrs
                        AND saknr  = @lwa_bsis-hkont
                        AND fdlev  = 'F0'
                        AND fstag  = 'YB04'.

                    IF lwa_skb1_aux IS NOT INITIAL.
                      EXIT.
                    ENDIF.

                  ENDLOOP.
                ENDIF.

* BUG - 95332 - Inicio - CBRAND
                IF lva_pagamento_parcial = abap_true.
                  SELECT  * FROM bsis
                    INTO TABLE @DATA(lit_bsis_aux)
                    WHERE bukrs = @lwa_bsak-bukrs
                      AND belnr = @lva_chv_belnr   "lwa_bsak-belnr Buscar o documento original, mais de uma linha 19.05.2023
                      AND gjahr = @lva_gjahr_parc. "lva_gjahr. CSB 12.12.2022

                  IF lit_bsis_aux IS NOT INITIAL.

                    " Duas linhas de pagamento: BELNR: 0100097430 - 22AMGI0002555N
                    " READ TABLE lit_bsis_aux INTO DATA(lwa_bsis_aux) INDEX  1.

                    LOOP AT lit_bsis_aux INTO DATA(lwa_bsis_aux).

                      IF lwa_bsis_aux-shkzg = 'H'.
                        lva_wrbtr_sum  = lva_wrbtr_sum - lwa_bsis_aux-wrbtr.
                        lva_dmbe2_sum  = lva_dmbe2_sum - lwa_bsis_aux-dmbe2.
                      ELSE.
                        lva_wrbtr_sum  = lva_wrbtr_sum + lwa_bsis_aux-wrbtr.
                        lva_dmbe2_sum  = lva_dmbe2_sum + lwa_bsis_aux-dmbe2.
                      ENDIF.
                    ENDLOOP.


                    lva_wrbtr  = abs( lva_wrbtr_sum ).
                    lva_dmbe2  = abs( lva_dmbe2_sum ).

                    CLEAR: lwa_bsis-wrbtr,
                           lwa_bsis-dmbe2.

                    IF lwa_bsik-shkzg = 'H'. "15.122202 - Caso que recebeu a mais ou pagou a mais. Compensação maior que o valor do faturamento.
                      lwa_bsis-wrbtr = lva_wrbtr - lwa_bsik-wrbtr.  "lwa_bsis_aux-wrbtr - lwa_bsik-wrbtr.
                      lwa_bsis-dmbe2 = lva_dmbe2 - lwa_bsik-dmbe2.  "lwa_bsis_aux-dmbe2 - lwa_bsik-dmbe2.
                    ELSE.
                      lwa_bsis-wrbtr = lva_wrbtr + lwa_bsik-wrbtr.   "lwa_bsis_aux-wrbtr + lwa_bsik-wrbtr.
                      lwa_bsis-dmbe2 = lva_dmbe2 + lwa_bsik-dmbe2.   "lwa_bsis_aux-dmbe2 + lwa_bsik-dmbe2.
                    ENDIF.


                    SELECT * FROM zfit0164_retcomp
                      INTO TABLE @DATA(it_retcomp)
                    WHERE transno = @lwa_zfit0164-transno
                       AND augbl <> @lwa_bsak-augbl . " Mais de uma perna de lanlamento iguais 18.05.2023

                    SORT it_retcomp BY transno augbl.
                    DELETE ADJACENT DUPLICATES FROM it_retcomp COMPARING transno augbl.

                    IF it_retcomp[] IS NOT INITIAL.
                      LOOP AT it_retcomp INTO lwa_retcomp.
                        lwa_bsis-wrbtr =  lwa_bsis-wrbtr  - abs( lwa_retcomp-dmbtr ). "26.04.2023
                        lwa_bsis-dmbe2 =  lwa_bsis-dmbe2  - abs( lwa_retcomp-dmbe2 ). "26.04.2023
                      ENDLOOP.
                    ENDIF.
                  ENDIF.
                ELSE. "14.12.2022 ( Pagamento total com Despesa - Mas ocorrreu parcial antes )
                  IF lit_bsis IS NOT INITIAL.

                    CLEAR:  lva_valor_w , lva_valor_d.

                    LOOP AT lit_bsis INTO DATA(lwa_bsis_t).
                      IF lwa_bsis_t-shkzg = 'H'. "+
                        lva_valor_w = lva_valor_w +   lwa_bsis_t-wrbtr .
                        lva_valor_d = lva_valor_d +   lwa_bsis_t-dmbe2 .
                      ELSE. "-
                        lva_valor_w = lva_valor_w - lwa_bsis_t-wrbtr.
                        lva_valor_d = lva_valor_d - lwa_bsis_t-dmbe2.
                      ENDIF.
                    ENDLOOP.
                    lwa_bsis-wrbtr = lva_valor_w .
                    lwa_bsis-dmbe2 = lva_valor_d.
                  ENDIF.
                ENDIF.
* BUG - 95332 - Fim - CBRAND

                lwa_zfit0164-augbl = lwa_bsak-augbl.
                lwa_zfit0164-augdt = lwa_bsak-augdt.

                IF lva_pagamento_parcial = abap_true OR lwa_zfit0164-ck_compensado = 'P'.
                  lwa_zfit0164-dmbtr = lwa_bsis-wrbtr.
                  lwa_zfit0164-dmbe2 = lwa_bsis-dmbe2.
** Valor de compensação quando outra moeda.
                  IF lva_dif_moeda IS NOT INITIAL.
                    lwa_zfit0164-dmbtr = lwa_bsak-pswbt.
                  ENDIF.

                ELSE.
                  lwa_zfit0164-dmbtr = lwa_zfit0164-detail_currencyamount.
                  lwa_zfit0164-dmbe2 = lwa_zfit0164-detail_currencyamount.
                ENDIF.
              ENDIF.

            WHEN '01' OR '11' OR '09' OR '19'. "CLIENTE

              IF lwa_zfit0164-ck_compensado = 'P'.
**** Pagamento parcial mesma moeda.
                lwa_zib_chv-belnr = lwa_zfit0164-augbl. " " Se for parcial, buscar com base no último documento de compensação:
                lva_gjahr = lwa_zfit0164-augdt+0(4).

                SELECT SINGLE * FROM bsad
                INTO @DATA(lwa_bsad)
                WHERE bukrs = @lwa_zib_chv-bukrs
                  AND belnr = @lwa_zib_chv-belnr
                  AND gjahr = @lva_gjahr
                  AND augbl <> @lwa_zfit0164-augbl.

*** BUG - 102751 - CBRAND - Inicio
**** Documento com moeda usd e compensação em outra moeda, pagamento parcial.
                IF lwa_bsad IS INITIAL.
* Busco o documento original.
                  CLEAR: lwa_zib_chv_or , lit_zfit0164_retcomp, lwa_zfit0164_origem.
                  SELECT SINGLE * FROM zib_contabil_chv
                    INTO lwa_zib_chv_or
                    WHERE obj_key = lva_obj_key.

                  SELECT SINGLE * FROM bsad
                      INTO @DATA(lwa_bsad_origem)
                      WHERE bukrs = @lwa_zib_chv_or-bukrs
                        AND belnr = @lwa_zib_chv_or-belnr
                        AND gjahr = @lwa_zib_chv_or-gjahr.

                  IF lwa_bsad_origem-augbl IS NOT INITIAL.

                    lva_gjahr = lwa_zfit0164-augdt+0(4).

                    FREE rg_augbl.
                    APPEND  VALUE #( sign = 'I'
                                     option = 'EQ'
                                     low = lwa_bsad_origem-augbl
                                   ) TO rg_augbl.

                    SELECT * FROM zfit0164_retcomp
                       INTO TABLE lit_zfit0164_retcomp
                       WHERE transno = lwa_zfit0164-transno.

* Uso o documento que esta na 0164 para buscar a proxima compensação.
                    LOOP AT lit_zfit0164_retcomp INTO lwa_zfit0164_origem.

                      APPEND  VALUE #( sign = 'I'
                                       option = 'EQ'
                                       low = lwa_zfit0164_origem-augbl
                                     ) TO rg_augbl.

                    ENDLOOP.

                    SELECT SINGLE * FROM bsad
                      INTO lwa_bsad
                      WHERE bukrs = lwa_zib_chv_or-bukrs
                        AND belnr = lwa_bsad_origem-augbl
                        AND gjahr = lva_gjahr
                        AND augbl NOT IN rg_augbl.

                    IF lwa_bsad IS NOT INITIAL.
                      CLEAR lva_waers.
                      SELECT SINGLE waers FROM bsad
                        INTO  lva_waers
                      WHERE belnr = lwa_bsad-augbl
                        AND bukrs = lwa_bsad-bukrs
                        AND gjahr = lwa_bsad-gjahr.

                      IF lva_waers = lwa_zfit0164-currency.
                        CLEAR lwa_bsad.
                      ELSE.
                        lva_dif_moeda = 'S'.
                      ENDIF.
                    ENDIF.

                  ENDIF.
                ENDIF.
              ELSE.
*** BUG - 102751 - CBRAND - Fim
**** Pagamento não parcial
                SELECT SINGLE * FROM bsad
                  INTO lwa_bsad
                  WHERE bukrs = lwa_zib_chv-bukrs
                    AND belnr = lwa_zib_chv-belnr
                    AND gjahr = lwa_zib_chv-gjahr.
              ENDIF.

              IF lwa_bsad IS NOT INITIAL.
                " IF ( sy-subrc = 0 ).

                lva_gjahr = lwa_bsad-augdt+0(4).

*               Busca pagamento parcial
                SELECT SINGLE * FROM bsad
                  INTO @DATA(lwa_bsad_parc)
                  WHERE bukrs = @lwa_bsad-bukrs
                    AND belnr = @lwa_bsad-augbl
                    AND gjahr = @lva_gjahr."@lwa_bsad-gjahr.

                IF ( sy-subrc = 0 ).
                  SELECT SINGLE * FROM bsid
                    INTO  @DATA(lwa_bsid)
                    WHERE bukrs = @lwa_bsad_parc-bukrs
                    AND belnr = @lwa_bsad_parc-belnr
                    AND gjahr = @lwa_bsad_parc-gjahr.
                  IF ( sy-subrc = 0 ). "Se encontrar valor, documento é pgto parcial
                    lva_pagamento_parcial = abap_true.
                  ENDIF.
                ENDIF.

                SELECT SINGLE * FROM bkpf
                  INTO lwa_bkpf
                  WHERE bukrs = lwa_bsad-bukrs
                    AND belnr = lwa_bsad-augbl
                    AND gjahr = lva_gjahr. "@lwa_bsad-gjahr. mudei dia 23.06.2022 CSB .

* Somente para buscar a conta
                SELECT  * FROM bsis
                  INTO TABLE lit_bsis
                  WHERE bukrs = lwa_bsad-bukrs
                    AND belnr = lwa_bsad-augbl
                    AND gjahr = lva_gjahr.

                IF lit_bsis IS NOT INITIAL.

                  LOOP AT lit_bsis INTO lwa_bsis.

                    SELECT SINGLE * FROM skb1
                      INTO  lwa_skb1_aux
                        WHERE bukrs  = lwa_bsis-bukrs
                        AND saknr  = lwa_bsis-hkont
                        AND fdlev  = 'F0'
                        AND fstag  = 'YB04'.

                    IF lwa_skb1_aux IS NOT INITIAL.
                      EXIT.
                    ENDIF.

                  ENDLOOP.
                ENDIF.

* BUG - 95332 - Inicio - CBRAND
                IF lva_pagamento_parcial = abap_true.
* Documento 2021 - Com baixa em 2022 - lva_gjahr_parc - CSB 12.12.2022
                  SELECT  * FROM bsis
                    INTO TABLE lit_bsis_aux
                    WHERE bukrs = lwa_bsad-bukrs
                      AND belnr = lva_chv_belnr     "lwa_zib_chv-belnr "lwa_bsad-belnr
                      AND gjahr = lva_gjahr_parc.  "lva_gjahr. CSB 12.12.2022

                  IF lit_bsis_aux IS NOT INITIAL.

                    CLEAR: lwa_bsis_aux, lva_wrbtr, lva_dmbe2 ,  lva_wrbtr_sum, lva_dmbe2_sum .

                    " READ TABLE lit_bsis_aux INTO lwa_bsis_aux INDEX  1.

** 29.05.2023 - Mais de uma chave 40
                    LOOP AT lit_bsis_aux INTO lwa_bsis_aux.
                      IF lwa_bsis_aux-shkzg = 'H'.
                        lva_wrbtr_sum  = lva_wrbtr_sum - lwa_bsis_aux-wrbtr.
                        lva_dmbe2_sum  = lva_dmbe2_sum - lwa_bsis_aux-dmbe2.
                      ELSE.
                        lva_wrbtr_sum  = lva_wrbtr_sum + lwa_bsis_aux-wrbtr.
                        lva_dmbe2_sum  = lva_dmbe2_sum + lwa_bsis_aux-dmbe2.
                      ENDIF.

                    ENDLOOP.


                    lva_dmbe2  = abs( lva_dmbe2_sum  ).
                    lva_wrbtr  = abs( lva_wrbtr_sum ).

                    CLEAR: lwa_bsis-wrbtr,
                           lwa_bsis-dmbe2.

                    "lwa_bsis-wrbtr = lwa_bsis_aux-wrbtr - lwa_bsak-wrbtr. "CSB 12.12.2022
                    "lwa_bsis-dmbe2 = lwa_bsis_aux-dmbe2 - lwa_bsak-dmbe2. "CSB 12.12.2022

                    IF lwa_bsid-shkzg = 'H'.  "15.122202 - Caso que recebeu a mais ou pagou a mais. Compensação maior que o valor do faturamento.
                      lwa_bsis-wrbtr = lva_wrbtr + lwa_bsid-wrbtr. "lwa_bsis_aux-wrbtr + lwa_bsid-wrbtr.
                      lwa_bsis-dmbe2 = lva_dmbe2 + lwa_bsid-dmbe2. "lwa_bsis_aux-dmbe2 + lwa_bsid-dmbe2.
                    ELSE.
                      lwa_bsis-wrbtr = lva_wrbtr  - lwa_bsid-wrbtr. "lwa_bsis_aux-wrbtr - lwa_bsid-wrbtr.
                      lwa_bsis-dmbe2 = lva_dmbe2  - lwa_bsid-dmbe2. "lwa_bsis_aux-dmbe2 - lwa_bsid-dmbe2.
                    ENDIF.

                    SELECT * FROM zfit0164_retcomp
                      INTO TABLE it_retcomp
                    WHERE transno = lwa_zfit0164-transno
                       AND augbl <> lwa_bsad-augbl.

                    SORT it_retcomp BY transno augbl.

                    DELETE ADJACENT DUPLICATES FROM it_retcomp COMPARING transno augbl.

                    IF it_retcomp[] IS NOT INITIAL.
                      LOOP AT it_retcomp INTO lwa_retcomp.
                        lwa_bsis-wrbtr =  lwa_bsis-wrbtr  - abs( lwa_retcomp-dmbtr ). "26.04.2023
                        lwa_bsis-dmbe2 =  lwa_bsis-dmbe2  - abs( lwa_retcomp-dmbe2 ). "26.04.2023
                      ENDLOOP.
                    ENDIF.
                  ENDIF.
**** BUG - 156557 - 28.10.2024 - CBRAND - Inicio
*** Aqui preciso devolver o sinal de negativo ( Removido para efetuar os calculos acima ) lva_dmbe2  = abs( lva_dmbe2_sum  ).
                  IF lva_wrbtr_sum < 0 and lwa_zfit0164-detail_transtype = '1'.
                    lwa_bsis-wrbtr = lwa_bsis-wrbtr * -1.
                    lwa_bsis-dmbe2 = lwa_bsis-dmbe2 * -1.
                  ENDIF.
**** BUG - 156557 - 28.10.2024 - CBRAND  - Fim
                ELSE. "14.12.2022 ( Pagamento total com Despesa - Mas ocorrreu parcial antes )
                  IF lit_bsis IS NOT INITIAL.

                    CLEAR:  lva_valor_w , lva_valor_d, lwa_bsis_t.

                    LOOP AT lit_bsis INTO lwa_bsis_t.
                      IF lwa_bsis_t-shkzg = 'H'. "+
                        lva_valor_w = lva_valor_w - lwa_bsis_t-wrbtr .
                        lva_valor_d = lva_valor_d - lwa_bsis_t-dmbe2 .
                      ELSE. "-
                        lva_valor_w = lva_valor_w + lwa_bsis_t-wrbtr.
                        lva_valor_d = lva_valor_d + lwa_bsis_t-dmbe2.
                      ENDIF.
                    ENDLOOP.
                    lwa_bsis-wrbtr = lva_valor_w .
                    lwa_bsis-dmbe2 = lva_valor_d.
                  ENDIF.
                ENDIF.
* BUG - 95332 - Fim - CBRAND

                lwa_zfit0164-augbl = lwa_bsad-augbl.
                lwa_zfit0164-augdt = lwa_bsad-augdt.

                IF lva_pagamento_parcial = abap_true OR lwa_zfit0164-ck_compensado = 'P'.
                  lwa_zfit0164-dmbtr = lwa_bsis-wrbtr.
                  lwa_zfit0164-dmbe2 = lwa_bsis-dmbe2.

** Valor de compensação quando outra moeda.
                  IF lva_dif_moeda IS NOT INITIAL.
                    lwa_zfit0164-dmbtr = lwa_bsad-pswbt.
                    CLEAR: lva_dif_moeda.
                  ENDIF.
                ELSE.
                  lwa_zfit0164-dmbtr = lwa_zfit0164-detail_currencyamount.
                  lwa_zfit0164-dmbe2 = lwa_zfit0164-detail_currencyamount.
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDIF.

        " -> Buscar conta de banco
        IF ( lwa_bsis IS NOT INITIAL ).

          SELECT SINGLE * FROM skb1
            INTO @DATA(lwa_skb1)
            WHERE bukrs  = @lwa_bsis-bukrs
              AND saknr  = @lwa_bsis-hkont
              AND fdlev  = 'F0'
             AND fstag  = 'YB04'.

****** Inicio - BUG 102761 - CBRAND.
          IF ( sy-subrc <> 0 ).

            CASE lwa_zib-bschl.
              WHEN '31' OR '21' OR '39' OR '29'. "FORNECEDOR
                SELECT SINGLE * FROM bsak
                  INTO @DATA(lwa_bsak_hk)
                 WHERE bukrs  = @lwa_bsis-bukrs
                   AND augbl <> @lwa_zfit0164-augbl
                   AND belnr =   @lwa_zfit0164-augbl.

                lwa_zfit0164-augbl = lwa_bsak_hk-augbl.

                SELECT  * FROM bsis
                  INTO TABLE @DATA(lit_bsis_hk)
                  WHERE bukrs = @lwa_bsak_hk-bukrs
                    AND belnr = @lwa_bsak_hk-augbl.

                IF sy-subrc = 0.
                  CLEAR: lwa_skb1.
                  LOOP AT lit_bsis_hk INTO DATA(lwa_bsis_hk).
                    SELECT SINGLE * FROM skb1
                        INTO lwa_skb1
                        WHERE bukrs  = lwa_bsis_hk-bukrs
                          AND saknr  = lwa_bsis_hk-hkont
                          AND fdlev  = 'F0'
                         AND fstag  = 'YB04'.

                    IF sy-subrc = 0.
                      lwa_bsis-hkont =  lwa_bsis_hk-hkont.
                    ENDIF.

                  ENDLOOP.
                ENDIF.
              WHEN '01' OR '11' OR '09' OR '19'. "CLIENTE
                SELECT SINGLE * FROM bsad
                  INTO @DATA(lwa_bsad_hk)
                 WHERE bukrs  = @lwa_bsis-bukrs
                   AND augbl <> @lwa_zfit0164-augbl
                   AND belnr =   @lwa_zfit0164-augbl.

                lwa_zfit0164-augbl = lwa_bsad_hk-augbl.
                CLEAR: lit_bsis_hk, lwa_bsis_hk.
                SELECT  * FROM bsis
                  INTO TABLE @lit_bsis_hk
                  WHERE bukrs = @lwa_bsad_hk-bukrs
                    AND belnr = @lwa_bsad_hk-augbl.

                IF sy-subrc = 0.
                  CLEAR: lwa_skb1.
                  LOOP AT lit_bsis_hk INTO lwa_bsis_hk.
                    SELECT SINGLE * FROM skb1
                        INTO lwa_skb1
                        WHERE bukrs  = lwa_bsis_hk-bukrs
                          AND saknr  = lwa_bsis_hk-hkont
                          AND fdlev  = 'F0'
                         AND fstag  = 'YB04'.

                    IF sy-subrc = 0.
                      lwa_bsis-hkont =  lwa_bsis_hk-hkont.
                    ENDIF.

                  ENDLOOP.
                ENDIF.
            ENDCASE.
          ENDIF.
****** FIM - BUG 102761 - CBRAND.
          IF lwa_skb1 IS NOT INITIAL.

            IF lwa_zfit0164-augdt >= lva_data. "CSB - 16.02.2021

**** 26.04.2023 - Ajuste Negativo - CBRAND
              " -> Valor negativo
              IF lva_pagamento_parcial = abap_true OR lwa_zfit0164-ck_compensado = 'P'.
                IF ( lwa_zfit0164-detail_transtype = '2' AND lwa_zfit0164-currencyamount < 0 ) .
                  lwa_zfit0164-dmbtr = ( lwa_zfit0164-dmbtr * -1 ).
                  lwa_zfit0164-dmbe2 = ( lwa_zfit0164-dmbe2 * -1 ).
                ENDIF.
              ENDIF.
**** 26.04.2023 - Ajuste Negativo - CBRAND

              lwa_zfit0164-hkont = lwa_bsis-hkont.
              lwa_zfit0164-kursf = lwa_bkpf-kursf.

              DATA(lva_date) = |{ lwa_zfit0164-augdt+0(4) }-{ lwa_zfit0164-augdt+4(2) }-{ lwa_zfit0164-augdt+6(2) }|.
              DATA(lva_ex_date) = |{ lwa_bkpf-wwert+0(4) }-{ lwa_bkpf-wwert+4(2) }-{ lwa_bkpf-wwert+6(2) }|.
              SHIFT lwa_bsis-hkont LEFT DELETING LEADING '0'.

              DATA(lwa_simple_payment) = VALUE zfis_vip_simple_payment(
                  invoicetransno     = lwa_zfit0164-transno
                  entrydate          = lva_date
                  actdate            = lva_date
                  externalrefid      = lwa_zfit0164-augbl
                  bankcode           = lwa_bsis-hkont
                  bankxcrate         = lwa_bkpf-kursf
                  bankcurr           = COND #( WHEN lwa_bsak IS NOT INITIAL THEN lwa_bsak-waers ELSE lwa_bsad-waers )
                  currencyamount     = COND #( WHEN ( lva_pagamento_parcial = abap_true OR lwa_zfit0164-ck_compensado = 'P') THEN lwa_zfit0164-dmbtr ELSE lwa_zfit0164-currencyamount )
                  currency           = COND #( WHEN lwa_bsak IS NOT INITIAL THEN lwa_bsak-waers ELSE lwa_bsad-waers )
                  basecurrencyamount = COND #( WHEN ( lva_pagamento_parcial = abap_true OR lwa_zfit0164-ck_compensado = 'P') THEN lwa_zfit0164-dmbtr ELSE lwa_zfit0164-currencyamount )
                  exchangeratedate   = lva_ex_date
                  paymode            = 'WT'    ).

              CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
                CHANGING
                  value = lwa_simple_payment-currencyamount.

              CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
                CHANGING
                  value = lwa_simple_payment-basecurrencyamount.


              IF ( lwa_simple_payment IS NOT INITIAL ) AND ( lwa_simple_payment-currencyamount <> 0 ) . "CBRAND 18.05.2023

                CALL TRANSFORMATION zfi_vip_simple_payment
                SOURCE simplepayment = lwa_simple_payment
                  RESULT XML gva_xml_result.

                CALL FUNCTION 'CRM_IC_XML_XSTRING2STRING'
                  EXPORTING
                    inxstring = gva_xml_result
                  IMPORTING
                    outstring = gva_xml_string.

                IF ( gva_xml_string IS NOT INITIAL ).
                  me->send_xml_to_api( i_xml = gva_xml_string i_metodo = 'GET_SIMPLE_PAYMENT' ).

*                  IF ( lva_pagamento_parcial = abap_true ).
*                    lwa_zfit0164-ck_compensado = 'P'.
*                  ELSE.
*                    lwa_zfit0164-ck_compensado = 'T'.
*                  ENDIF.
*                  MOVE-CORRESPONDING lwa_zfit0164 TO lwa_zfit0164_retcomp.
*                  MODIFY zfit0164_retcomp  FROM lwa_zfit0164_retcomp.
*                  MODIFY zfit0164 FROM lwa_zfit0164.
                ENDIF.

              ENDIF.
* CBRAND 18.05.2023 - Inicio
              IF ( lva_pagamento_parcial = abap_true ).
                lwa_zfit0164-ck_compensado = 'P'.
              ELSE.
                lwa_zfit0164-ck_compensado = 'T'.
              ENDIF.
              MOVE-CORRESPONDING lwa_zfit0164 TO lwa_zfit0164_retcomp.
              MODIFY zfit0164_retcomp  FROM lwa_zfit0164_retcomp.
              MODIFY zfit0164 FROM lwa_zfit0164.
* CBRAND 18.05.2023 - Fim
            ENDIF.

          ENDIF.

        ENDIF.

        CLEAR: lva_gjhar, lva_obj_key, lwa_bsad, lwa_bsak, lwa_bsis, lwa_simple_payment, "lwa_bsid,
                lwa_skb1,lwa_zib_chv, lva_date, lva_ex_date, lwa_bkpf, lva_pagamento_parcial, lva_wrbtr, lva_dmbe2, lva_wrbtr_sum, lva_dmbe2_sum, lva_dmbe2_sum,  lwa_skb1_aux, lit_bsis.

      ENDLOOP.
    ENDIF.

* Codigo Antigo - 31.10.2022 - Deixei aqui pq removi comentários e códigos antigos comentados
*    METHOD get_simple_payment.
*
*    DATA: gva_xml_result        TYPE xstring,
*          gva_xml_string        TYPE string,
*          lva_pagamento_parcial TYPE abap_bool,
*          lit_data              TYPE TABLE OF setleaf,
*          lva_data              TYPE zfit0164-augbl,
*          lwa_zfit0164_retcomp  TYPE zfit0164_retcomp,
*          lwa_zfit0164_aux      TYPE zfit0164,
*          lva_count             TYPE i.
*
*
**** Melhorias inicio
*    DATA: lva_gjahr TYPE bsak-gjahr.
**** Melhorias Fim
*
**** Issue - 64595 - Inicio - CBRAND
*    SELECT *
*      FROM setleaf INTO TABLE lit_data
*     WHERE setname = 'MAGGI_SUICA_VIP_ENV'.
*
*    READ TABLE lit_data INTO DATA(lwa_data) INDEX 1.
*    CONCATENATE lwa_data-valfrom+6(4) lwa_data-valfrom+3(2) lwa_data-valfrom+0(2) INTO lva_data.
**** Issue - 64595 - Fim - CBRAND
*
*    IF i_transno IS INITIAL.
*
*      SELECT * FROM zfit0164
*        INTO TABLE @DATA(it_zfit0164)
*        WHERE status = '1' "Posted, not paid
*        AND ck_compensado IN ( '', 'P' ).
*      "AND augbl = ''.
**** BUG - 91927 - Inicio - CBRAND
*    ELSE.
*      SELECT * FROM zfit0164
*        INTO TABLE it_zfit0164
*      WHERE status = '1' "Posted, not paid
*        "AND ck_compensado IN ( '', 'P' )
*        AND transno = i_transno.
*
*      SELECT * FROM zfit0164_retcomp
*        INTO TABLE @DATA(it_zfit0164_retcomp)
*      WHERE transno = @i_transno.
*
*      lva_count = lines( it_zfit0164_retcomp ).
*
*      IF lva_count > 1.
*        lva_pagamento_parcial = abap_true.
*      ENDIF.
*
*    ENDIF.
**** BUG - 91927 - FIM - CBRAND
*
*    IF ( sy-subrc = 0 ).
*
*      LOOP AT it_zfit0164[] INTO DATA(lwa_zfit0164).
*
*        DATA(lva_gjhar) = CONV gjahr( lwa_zfit0164-actdate+0(4) ).
*        DATA(lva_obj_key) = CONV awkey( |{ lwa_zfit0164-transno }{ lva_gjhar }| ).
*
*        SELECT SINGLE * FROM zib_contabil
*          INTO @DATA(lwa_zib)
*          WHERE obj_key = @lva_obj_key
*            AND bschl IN ( '31', '21', '39', '29',
*                           '01', '11', '09', '19'  ).
*
*        SELECT SINGLE * FROM zib_contabil_chv
*        INTO @DATA(lwa_zib_chv)
*        WHERE obj_key = @lva_obj_key.
*
*        IF lwa_zib_chv IS NOT INITIAL.
*
*          CASE lwa_zib-bschl.
*            WHEN '31' OR '21' OR '39' OR '29'.
*
*              IF lwa_zfit0164-ck_compensado = 'P'.
*                lwa_zib_chv-belnr = lwa_zfit0164-augbl. " " Se for parcial, buscar com base no último documento de compensação:
*                lva_gjahr = lwa_zfit0164-augdt+0(4).
*                SELECT SINGLE * FROM bsak
*                  INTO @DATA(lwa_bsak)
*                  WHERE bukrs = @lwa_zib_chv-bukrs
*                    AND belnr = @lwa_zib_chv-belnr
*                    AND gjahr = @lva_gjahr
*                    AND augbl <> @lwa_zfit0164-augbl.
*              ELSE.
*                SELECT SINGLE * FROM bsak
*                  INTO lwa_bsak
*                  WHERE bukrs = lwa_zib_chv-bukrs
*                    AND belnr = lwa_zib_chv-belnr
*                    AND gjahr = lwa_zib_chv-gjahr.
*              ENDIF.
*
*
*              IF ( sy-subrc = 0 ).
*
*                lva_gjahr = lwa_bsak-augdt+0(4).
*
**               Busca pagamento parcial
*                SELECT SINGLE * FROM bsak
*                  INTO @DATA(lwa_bsak_parc)
*                  WHERE bukrs = @lwa_bsak-bukrs
*                    AND belnr = @lwa_bsak-augbl
*                    AND gjahr = @lva_gjahr.
*
**** CSB - 22.12.2021 - Inicio (Comentei)
**                IF ( sy-subrc = 0 ).
**                  SELECT SINGLE * FROM bsid
**                    INTO @DATA(lwa_bsid)
**                    WHERE bukrs = @lwa_bsak_parc-bukrs
**                    AND belnr = @lwa_bsak_parc-belnr
**                    AND gjahr = @lwa_bsak_parc-gjahr.
**                  IF ( sy-subrc = 0 ). "Se encontrar valor, documento é pgto parcial
**                    lva_pagamento_parcial = abap_true.
**                  ENDIF.
**                ENDIF.
**** CSB - 22.12.2021 - Fim (Comentei)
**** CSB - 22.12.2021 - Inicio
*                IF ( sy-subrc = 0 ).
*                  SELECT SINGLE * FROM bsik
*                    INTO @DATA(lwa_bsik)
*                    WHERE bukrs = @lwa_bsak_parc-bukrs
*                      AND belnr = @lwa_bsak_parc-belnr
*                      AND gjahr = @lwa_bsak_parc-gjahr.
*                  IF ( sy-subrc = 0 ). "Se encontrar valor, documento é pgto parcial
*                    lva_pagamento_parcial = abap_true.
*                  ENDIF.
*                ENDIF.
**** CSB - 22.12.2021 - Fim
*
*                SELECT SINGLE * FROM bkpf
*                  INTO  @DATA(lwa_bkpf)
*                  WHERE bukrs = @lwa_bsak-bukrs
*                    AND belnr = @lwa_bsak-augbl
*                    AND gjahr = @lva_gjahr. "@lwa_bsak-gjahr. mudei dia 23.06.2022 CSB
**** CSB - 21.12.2021 - Inicio
**                SELECT SINGLE * FROM bsis
**                  INTO @DATA(lwa_bsis)
**                  WHERE bukrs = @lwa_bsak-bukrs
**                    AND belnr = @lwa_bsak-augbl
**                    AND gjahr = @lwa_bsak-gjahr.
**** CSB - 21.12.2021 - fim
**** CSB - 21.12.2021 - Inicio
**** Comentado dia 31.10.2022 - BUG 95332 - CBRAND Inicio
**                SELECT  * FROM bsis
**                  INTO TABLE @DATA(lit_bsis)
**                  WHERE bukrs = @lwa_bsak-bukrs
**                    AND belnr = @lwa_bsak-augbl
**                    AND gjahr = @lva_gjahr."@lwa_bsak-gjahr. mudei dia 23.06.2022 CSB
**
**                IF lit_bsis IS NOT INITIAL.
**
**                  LOOP AT lit_bsis INTO DATA(lwa_bsis).
**
**                    SELECT SINGLE * FROM skb1
**                      INTO  @DATA(lwa_skb1_aux)
**                        WHERE bukrs  = @lwa_bsis-bukrs
**                        AND saknr  = @lwa_bsis-hkont
**                        AND fdlev  = 'F0'
**                        AND fstag  = 'YB04'.
**
**                    IF lwa_skb1_aux IS NOT INITIAL.
**                      EXIT.
**                    ENDIF.
**
**                  ENDLOOP.
**                ENDIF.
** Fim - 31.10.2022
**** CSB - 21.12.2021 - fim
*
*
** BUG - 95332 - Inicio - CBRAND
*                SELECT  * FROM bsis
*                INTO TABLE @DATA(lit_bsis)
*                WHERE bukrs = @lwa_bsak-bukrs
*                  AND belnr = @lwa_bsak-belnr
*                  AND gjahr = @lva_gjahr.
*
*                IF lit_bsis IS NOT INITIAL.
*
*                  SELECT * FROM zfit0164_retcomp
*                    INTO TABLE @DATA(it_retcomp)
*                  WHERE transno = lwa_zfit0164-transno.
*
*                  IF sy-subrc = 0.
*                    READ TABLE lit_bsis INTO lwa_bsis INDEX 1.
*
*                    lwa_bsis-wrbtr = lwa_bsis-wrbtr - lwa_bsik-wrbtr.
*                    lwa_bsis-dmbe2 = lwa_bsis-dmbe2 - lwa_bsik-dmbe2.
*
*                    LOOP AT it_retcomp INTO lwa_retcomp.
*                      lwa_bsis-wrbtr = lwa_bsis-wrbtr  - lwa_retcomp-dmbtr.
*                      lwa_bsis-dmbe2 =  lwa_bsis-dmbe2 - lwa_retcomp-dmbe2.
*                    ENDLOOP.
*
*                  ENDIF.
*                ENDIF.
*
*                lwa_zfit0164-augbl = lwa_bsak-augbl.
*                lwa_zfit0164-augdt = lwa_bsak-augdt.
*
**** CSB - 21.12.2021 - inicio.
*                IF lva_pagamento_parcial = abap_true OR lwa_zfit0164-ck_compensado = 'P'.
*                  lwa_zfit0164-dmbtr = lwa_bsis-wrbtr.
*                  lwa_zfit0164-dmbe2 = lwa_bsis-dmbe2.
**** CSB - 21.12.2021 - Fim.
*                ELSE.
*                  lwa_zfit0164-dmbtr = lwa_zfit0164-detail_currencyamount.
*                  lwa_zfit0164-dmbe2 = lwa_zfit0164-detail_currencyamount.
*                ENDIF.
**** BUG - 69477 - Inicio
**                lwa_zfit0164-dmbtr = lwa_bsis-wrbtr.
**                lwa_zfit0164-dmbe2 = lwa_bsis-dmbe2.
**** BUG - 69477 - CSB - Fim
*              ENDIF.
*
*            WHEN '01' OR '11' OR '09' OR '19'.
*
*
*              IF lwa_zfit0164-ck_compensado = 'P'.
*                lwa_zib_chv-belnr = lwa_zfit0164-augbl. " " Se for parcial, buscar com base no último documento de compensação:
*                lva_gjahr = lwa_zfit0164-augdt+0(4).
*
*                SELECT SINGLE * FROM bsad
*                INTO @DATA(lwa_bsad)
*                WHERE bukrs = @lwa_zib_chv-bukrs
*                  AND belnr = @lwa_zib_chv-belnr
*                  AND gjahr = @lva_gjahr
*                  AND augbl <> @lwa_zfit0164-augbl.
*
*              ELSE.
*
*                SELECT SINGLE * FROM bsad
*                  INTO lwa_bsad
*                  WHERE bukrs = lwa_zib_chv-bukrs
*                    AND belnr = lwa_zib_chv-belnr
*                    AND gjahr = lwa_zib_chv-gjahr.
*              ENDIF.
*
*              IF ( sy-subrc = 0 ).
*
*                lva_gjahr = lwa_bsad-augdt+0(4).
*
**               Busca pagamento parcial
*                SELECT SINGLE * FROM bsad
*                  INTO @DATA(lwa_bsad_parc)
*                  WHERE bukrs = @lwa_bsad-bukrs
*                    AND belnr = @lwa_bsad-augbl
*                    AND gjahr = @lva_gjahr."@lwa_bsad-gjahr.
*
*                IF ( sy-subrc = 0 ).
*                  SELECT SINGLE * FROM bsid
*                    INTO  @DATA(lwa_bsid)
*                    WHERE bukrs = @lwa_bsad_parc-bukrs
*                    AND belnr = @lwa_bsad_parc-belnr
*                    AND gjahr = @lwa_bsad_parc-gjahr.
*                  IF ( sy-subrc = 0 ). "Se encontrar valor, documento é pgto parcial
*                    lva_pagamento_parcial = abap_true.
*                  ENDIF.
*                ENDIF.
*
*                SELECT SINGLE * FROM bkpf
*                  INTO lwa_bkpf
*                  WHERE bukrs = lwa_bsad-bukrs
*                    AND belnr = lwa_bsad-augbl
*                    AND gjahr = lwa_bsad-gjahr.
*
*
**** CSB - 21.12.2021 - Inicio
**                SELECT SINGLE * FROM bsis
**                  INTO lwa_bsis
**                  WHERE bukrs = lwa_bsad-bukrs
**                    AND belnr = lwa_bsad-augbl
**                    AND gjahr = lwa_bsad-gjahr.
**** CSB - 21.12.2021 - Fim
*
*
**** CSB - 21.12.2021 - Inicio
*                SELECT  * FROM bsis
*                  INTO TABLE lit_bsis
*                  WHERE bukrs = lwa_bsad-bukrs
*                    AND belnr = lwa_bsad-augbl
*                    AND gjahr = lva_gjahr. "lwa_bsad-gjahr. mudei dia 23.06.2022 CSB
*
*                IF lit_bsis IS NOT INITIAL.
*
*                  LOOP AT lit_bsis INTO lwa_bsis.
*
*                    SELECT SINGLE * FROM skb1
*                      INTO  lwa_skb1_aux
*                        WHERE bukrs  = lwa_bsis-bukrs
*                        AND saknr  = lwa_bsis-hkont
*                        AND fdlev  = 'F0'
*                        AND fstag  = 'YB04'.
*
*                    IF lwa_skb1_aux IS NOT INITIAL.
*                      EXIT.
*                    ENDIF.
*
*                  ENDLOOP.
*                ENDIF.
**** CSB - 21.12.2021 - Fim
*
*                lwa_zfit0164-augbl = lwa_bsad-augbl.
*                lwa_zfit0164-augdt = lwa_bsad-augdt.
**** CSB - 21.12.2021 - inicio.
*                IF lva_pagamento_parcial = abap_true OR lwa_zfit0164-ck_compensado = 'P'.
*
*                  lwa_zfit0164-dmbtr = lwa_bsis-wrbtr.
*                  lwa_zfit0164-dmbe2 = lwa_bsis-dmbe2.
*                ELSE.
**** CSB - 21.12.2021 - fim
*                  lwa_zfit0164-dmbtr = lwa_zfit0164-detail_currencyamount.
*                  lwa_zfit0164-dmbe2 = lwa_zfit0164-detail_currencyamount.
*                ENDIF.
**** BUG - 69477 - Inicio
**                lwa_zfit0164-dmbtr = lwa_bsis-wrbtr.
**                lwa_zfit0164-dmbe2 = lwa_bsis-dmbe2.
**** BUG - 69477 - Fim
*              ENDIF.
*          ENDCASE.
*        ENDIF.
*
*        " -> Buscar conta de banco
*        IF ( lwa_bsis IS NOT INITIAL ).
*
*          SELECT SINGLE * FROM skb1
*            INTO @DATA(lwa_skb1)
*            WHERE bukrs  = @lwa_bsis-bukrs
*              AND saknr  = @lwa_bsis-hkont
*              AND fdlev  = 'F0'
*              AND fstag  = 'YB04'.
*
*          IF ( sy-subrc = 0 ).
*
*            IF lwa_zfit0164-augdt >= lva_data. "CSB - 16.02.2021
*
*              lwa_zfit0164-hkont = lwa_bsis-hkont.
*              lwa_zfit0164-kursf = lwa_bkpf-kursf.
*
*              DATA(lva_date) = |{ lwa_zfit0164-augdt+0(4) }-{ lwa_zfit0164-augdt+4(2) }-{ lwa_zfit0164-augdt+6(2) }|.
*              DATA(lva_ex_date) = |{ lwa_bkpf-wwert+0(4) }-{ lwa_bkpf-wwert+4(2) }-{ lwa_bkpf-wwert+6(2) }|.
*              SHIFT lwa_bsis-hkont LEFT DELETING LEADING '0'.
*
*              DATA(lwa_simple_payment) = VALUE zfis_vip_simple_payment(
*                  invoicetransno     = lwa_zfit0164-transno
*                  entrydate          = lva_date
*                  actdate            = lva_date
*                  externalrefid      = lwa_zfit0164-augbl
*                  bankcode           = lwa_bsis-hkont
*                  bankxcrate         = lwa_bkpf-kursf
*                  bankcurr           = COND #( WHEN lwa_bsak IS NOT INITIAL THEN lwa_bsak-waers ELSE lwa_bsad-waers )
*                  currencyamount     = COND #( WHEN ( lva_pagamento_parcial = abap_true OR lwa_zfit0164-ck_compensado = 'P') THEN lwa_zfit0164-dmbtr ELSE lwa_zfit0164-currencyamount )
*                  currency           = COND #( WHEN lwa_bsak IS NOT INITIAL THEN lwa_bsak-waers ELSE lwa_bsad-waers )
*                  basecurrencyamount = COND #( WHEN ( lva_pagamento_parcial = abap_true OR lwa_zfit0164-ck_compensado = 'P') THEN lwa_zfit0164-dmbtr ELSE lwa_zfit0164-currencyamount )
*                  exchangeratedate   = lva_ex_date
*                  paymode            = 'WT'    ).
*
*              CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*                CHANGING
*                  value = lwa_simple_payment-currencyamount.
*
*              CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*                CHANGING
*                  value = lwa_simple_payment-basecurrencyamount.
*
*
*              IF ( lwa_simple_payment IS NOT INITIAL ).
*
*                CALL TRANSFORMATION zfi_vip_simple_payment
*                SOURCE simplepayment = lwa_simple_payment
*                  RESULT XML gva_xml_result.
*
*                CALL FUNCTION 'CRM_IC_XML_XSTRING2STRING'
*                  EXPORTING
*                    inxstring = gva_xml_result
*                  IMPORTING
*                    outstring = gva_xml_string.
*
*                IF ( gva_xml_string IS NOT INITIAL ).
*                  me->send_xml_to_api( i_xml = gva_xml_string i_metodo = 'GET_SIMPLE_PAYMENT' ).
*
*                  IF ( lva_pagamento_parcial = abap_true ).
*                    lwa_zfit0164-ck_compensado = 'P'.
*                  ELSE.
*                    lwa_zfit0164-ck_compensado = 'T'.
*                  ENDIF.
*                  MOVE-CORRESPONDING lwa_zfit0164 TO lwa_zfit0164_retcomp.
*                  MODIFY zfit0164_retcomp  FROM lwa_zfit0164_retcomp.
*                  MODIFY zfit0164 FROM lwa_zfit0164.
*                ENDIF.
*
*              ENDIF.
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*
*        CLEAR: lva_gjhar, lva_obj_key, lwa_bsad, lwa_bsak, lwa_bsis, lwa_simple_payment, "lwa_bsid,
*               lwa_skb1, lwa_zib_chv, lva_date, lva_ex_date, lwa_bkpf, lva_pagamento_parcial.
*
*      ENDLOOP.
*    ENDIF.
*
*  ENDMETHOD.
  ENDMETHOD.


  METHOD limpa_tabela_mensagens.

    DATA(lva_data_base) = CONV begda( sy-datum ).

    CALL FUNCTION 'MONTH_PLUS_DETERMINE'
      EXPORTING
        months  = -6
        olddate = lva_data_base
      IMPORTING
        newdate = lva_data_base.
    IF ( sy-subrc = 0 ).
      DELETE FROM zfit0163 WHERE data_envio <= lva_data_base.
    ENDIF.

  ENDMETHOD.


  METHOD send_contraparties.

    DATA: lt_lfbk TYPE vmds_lfbk_t.
    DATA(lva_data_busca) = sy-datum.


*    Busca fornecedores cadastrados ou modificados no dia
    SELECT * FROM lfa1
      INTO TABLE @DATA(lt_lfa1)
      WHERE scacd = '0200'
      "brsch = '0200'
        AND erdat = @lva_data_busca.

    IF ( lt_lfa1[] IS NOT INITIAL ).

*      Verifica se registro já foi enviado
      SELECT * FROM zfit0163
        INTO TABLE @DATA(lt_zfit0163)
        FOR ALL ENTRIES IN @lt_lfa1
        WHERE data_envio = @sy-datum
          AND lifnr = @lt_lfa1-lifnr.


      LOOP AT lt_lfa1[] ASSIGNING FIELD-SYMBOL(<lfs_lfa1>).

        SELECT  * FROM lfbk
        INTO TABLE lt_lfbk
        WHERE lifnr = <lfs_lfa1>-lifnr.

        READ TABLE lt_zfit0163[] ASSIGNING FIELD-SYMBOL(<lfs_zfit0163>) WITH KEY lifnr = <lfs_lfa1>-lifnr.

*        Modificação de fornecedor já enviada
***        IF ( sy-subrc = 0 ) AND ( <lfs_lfa1>-uptim = <lfs_zfit0163>-hora_cadastro ).
***          CONTINUE.

*        Criação de fornecedor já enviada
        IF ( sy-subrc = 0 ) AND ( <lfs_zfit0163>-criacao = abap_true ). " AND ( <lfs_lfa1>-updat IS INITIAL ).
          CONTINUE.

*        Modificação de registro
***        ELSEIF ( sy-subrc = 0 ) AND ( <lfs_zfit0163>-hora_cadastro <> <lfs_lfa1>-uptim ).
***          me->set_counterparties( EXPORTING i_lfa1 = <lfs_lfa1> i_creation = abap_false ).
***          me->set_counterparties_bank( EXPORTING i_lfa1 =  <lfs_lfa1> t_lfbk = lt_lfbk i_creation = abap_false  ).

*        Criação de registro não enviada
        ELSEIF ( sy-subrc <> 0 ). " AND ( <lfs_lfa1>-updat = lva_data_busca ).
*** COmentado dia 17.05.2024 - Inicio - CBRAND - BUG - 140984
**          Não enviar se o código de cliente não estiver criando ainda.
*          IF ( <lfs_lfa1>-kunnr IS INITIAL ).
*            CONTINUE.
*          ENDIF.
*** COmentado dia 17.05.2024 - Fim - CBRAND
          me->set_counterparties( EXPORTING i_lfa1 = <lfs_lfa1> i_creation = abap_true ).
          me->set_counterparties_bank( EXPORTING i_lfa1 =  <lfs_lfa1> t_lfbk = lt_lfbk i_creation = abap_true ).

        ENDIF.

        CLEAR: lt_lfbk[].

      ENDLOOP.


    ENDIF.

    CLEAR: lt_zfit0163[].


*    Busca clientes cadastrados ou modificados no dia
    SELECT * FROM kna1
      INTO TABLE @DATA(lt_kna1)
      WHERE brsch = '0200'
         AND ( erdat = @lva_data_busca OR updat = @lva_data_busca ).

    IF ( lt_kna1[] IS NOT INITIAL ).

*      Verifica se registro já foi enviado
      SELECT * FROM zfit0163
        INTO TABLE lt_zfit0163
        FOR ALL ENTRIES IN lt_kna1
        WHERE data_envio = sy-datum
          AND kunnr = lt_kna1-kunnr.

      LOOP AT lt_kna1[] ASSIGNING FIELD-SYMBOL(<lfs_kna1>).

        READ TABLE lt_zfit0163[] ASSIGNING <lfs_zfit0163> WITH KEY kunnr = <lfs_kna1>-kunnr.

*        Modificação de fornecedor já enviada
        IF ( sy-subrc = 0 ) AND ( <lfs_kna1>-uptim = <lfs_zfit0163>-hora_cadastro ).
          CONTINUE.

*        Criação de fornecedor já enviada
        ELSEIF ( sy-subrc = 0 ) AND ( <lfs_zfit0163>-criacao = abap_true ) AND ( <lfs_kna1>-updat IS INITIAL ).
          CONTINUE.

*        Modificação de registro
        ELSEIF ( sy-subrc = 0 ) AND ( <lfs_zfit0163>-hora_cadastro <> <lfs_kna1>-uptim ).
          me->set_counterparties( EXPORTING i_kna1 = <lfs_kna1> i_creation = abap_false ).

*        Criação de registro não enviada
        ELSEIF ( sy-subrc <> 0 ) AND ( <lfs_kna1>-erdat = lva_data_busca ).
          me->set_counterparties( EXPORTING i_kna1 = <lfs_kna1> i_creation = abap_true ).

        ENDIF.

      ENDLOOP.


    ENDIF.



  ENDMETHOD.


  METHOD send_xml_to_api.

    DATA: va_url       TYPE string,
          va_url_token TYPE string,
          va_retorno   TYPE string,
          va_reason    TYPE string.

    SELECT SINGLE z~url z~url_token
      FROM zauth_webservice AS z
      INTO (va_url, va_url_token)
      WHERE z~service = 'VIP_SAP_INTEGRATION'.

    CHECK ( sy-subrc = 0 ).

    DATA(obj_webservice) = NEW zcl_webservice( ).

    cl_http_client=>create_by_url(
      EXPORTING
        url                = va_url
      IMPORTING
        client             = DATA(e_http)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).

    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'POST'.
    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = '~server_protocol'
        value = 'HTTP/1.1'.

    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/xml; charset=UTF-8'.

    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = 'Accept'
        value = 'application/xml'.

    zcl_webservice=>zif_webservice~add_token_opus_http_cliente(
      EXPORTING
        i_url_destino              = va_url
        i_url_token                = va_url_token
      CHANGING
        i_http                     = e_http
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5   ).

    IF ( sy-subrc = 0 ).

      obj_webservice->zif_webservice~consultar(
      EXPORTING
        i_http                     = e_http
        i_xml                      = i_xml
      IMPORTING
        e_reason                   = va_reason
      RECEIVING
        e_resultado                = va_retorno
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
      IF ( sy-subrc = 0 ).

        REPLACE ALL OCCURRENCES OF '<string xmlns="http://schemas.microsoft.com/2003/10/Serialization/">' IN va_retorno WITH ''.
        REPLACE ALL OCCURRENCES OF '</string>' IN va_retorno WITH ''.
        REPLACE ALL OCCURRENCES OF 'queue/message/' IN va_retorno WITH ''.

        DATA(wa_message_log) = VALUE zfit0163(
            servico    = i_metodo
            data_envio = sy-datum
            message_id = va_retorno
            hora_envio = sy-uzeit
            lifnr      = COND #( WHEN i_lfa1 IS NOT INITIAL THEN i_lfa1-lifnr ELSE '' )
            kunnr      = COND #( WHEN i_kna1 IS NOT INITIAL THEN i_kna1-kunnr ELSE '' )
            criacao    = COND #( WHEN i_creation IS NOT INITIAL THEN i_creation ELSE '' )
            hora_cadastro = sy-uzeit   ).

        MODIFY zfit0163 FROM wa_message_log.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD set_counterparties.

    DATA: gwa_company_node TYPE zfis_vip_counterparties,
          gva_xml_result   TYPE xstring,
          gva_xml_string   TYPE string,
          gva_tag          TYPE string.

** 01.11.2023 -  CSB - Inicio
* Comentado dia 17.05.2024 - Para usar o campo I_LFA1-SCACD - Inicio
*    SELECT SINGLE * FROM ibpsupplier
*      INTO @DATA(wa_ibpsuplier)
*      WHERE supplier = @i_lfa1-lifnr.
*
*    SELECT SINGLE ind_sector
*      FROM but0is
*    INTO @DATA(lva_isec)
*      WHERE partner EQ @wa_ibpsuplier-businesspartner
*      AND ind_sector = '0200'.
* Comentado dia 17.05.2024 - Para usar o campo I_LFA1-SCACD - Fim
    "IF ( i_lfa1 IS NOT INITIAL ) AND ( i_lfa1-brsch = '0200' ). "-> Fornecedor
    IF ( i_lfa1 IS NOT INITIAL ) AND i_lfa1-scacd = '0200'. "( lva_isec = '0200' ). "-> Fornecedor
** 01.11.2023 -  CSB - Fim

      SELECT SINGLE * FROM adrc
        INTO @DATA(lwa_lfa1_adrc)
        WHERE addrnumber = @i_lfa1-adrnr
          AND date_to    >= @sy-datum.

      IF ( sy-subrc = 0 ).

        SELECT SINGLE * FROM lfb1
          INTO @DATA(lwa_lfb1)
          WHERE lifnr = @i_lfa1-lifnr.

        IF ( i_lfa1-kunnr IS NOT INITIAL ).
          SELECT SINGLE * FROM knb1 INTO @DATA(lwa_knb1_chk)
            WHERE kunnr = @i_lfa1-kunnr.
        ENDIF.

        gwa_company_node-companytype  = 'T'.
        gwa_company_node-companycode  = ''.
        gwa_company_node-shortname    = lwa_lfa1_adrc-sort1.
        gwa_company_node-fullname     = |{ i_lfa1-name1 } { i_lfa1-name2 }|.
        gwa_company_node-currency     = 'USD'.
        gwa_company_node-isinactive   = COND #(
          WHEN i_lfa1-sperr = 'X' THEN 'True' ELSE 'False' ).
        gwa_company_node-apaccount    = |{ lwa_lfb1-akont ALPHA = OUT }|.
        gwa_company_node-araccount    = |{ lwa_knb1_chk-akont ALPHA = OUT }|.
        gwa_company_node-vatno        = i_lfa1-stceg.
        gwa_company_node-address1     = |{ i_lfa1-stras }|.
        gwa_company_node-address2     = i_lfa1-ort02.
        gwa_company_node-address3     = |{ lwa_lfa1_adrc-post_code1 }-{ i_lfa1-ort01 }|.
        gwa_company_node-countrycode  = lwa_lfa1_adrc-country.
        gwa_company_node-externalref  = |{ i_lfa1-lifnr ALPHA = OUT }|.
        gwa_company_node-referencecode  = |{ i_lfa1-kunnr ALPHA = OUT }|.

        APPEND INITIAL LINE TO gwa_company_node-vendoruserproperties[] ASSIGNING FIELD-SYMBOL(<fs_user_prop>).
        <fs_user_prop>-fieldname = 'Counterparty type SAP'.
        <fs_user_prop>-fieldid   = 1.
        <fs_user_prop>-value     = 'F'.

      ENDIF.

    ELSEIF ( i_kna1 IS NOT INITIAL ). "-> Cliente

      SELECT SINGLE * FROM adrc
        INTO @DATA(lwa_kna1_adrc)
        WHERE addrnumber = @i_kna1-adrnr
          AND date_to    >= @sy-datum.

** 01.11.2023 -  CSB - Inicio
      SELECT SINGLE * FROM ibupacustomer
        INTO @DATA(wa_ibupacustomer)
        WHERE customer = @i_kna1-kunnr.

      SELECT SINGLE ind_sector
        FROM but0is
      INTO @DATA(lva_isec_c)
        WHERE partner EQ @wa_ibupacustomer-businesspartner
        AND ind_sector = '0200'.

      IF ( sy-subrc = 0 ) AND ( lva_isec_c = '0200' ).
        "IF ( sy-subrc = 0 ) AND ( i_kna1-brsch = '0200' ).
** 01.11.2023 -  CSB - Fim

        SELECT SINGLE * FROM knb1
          INTO @DATA(lwa_knb1)
          WHERE kunnr = @i_kna1-kunnr.

        IF ( i_kna1-lifnr IS NOT INITIAL ).
          SELECT SINGLE * FROM lfb1 INTO @DATA(lwa_lfb1_chk)
          WHERE lifnr = @i_kna1-lifnr.
        ENDIF.

        gwa_company_node-companytype  = 'T'.
        gwa_company_node-companycode  = ''.
        gwa_company_node-shortname    = lwa_kna1_adrc-sort1.
        gwa_company_node-fullname     = |{ i_kna1-name1 } { i_kna1-name2 }|.
        gwa_company_node-currency     = 'USD'.
        gwa_company_node-isinactive   = COND #(
          WHEN i_kna1-sperr = 'X' THEN 'True' ELSE 'False' ).
        gwa_company_node-araccount    = |{ lwa_knb1-akont ALPHA = OUT }|.
        gwa_company_node-apaccount    = |{ lwa_lfb1_chk-akont ALPHA = OUT }|.
        gwa_company_node-vatno        = i_kna1-stceg.
        gwa_company_node-address1     = |{ i_kna1-stras }|.
        gwa_company_node-address2     = i_kna1-ort02.
        gwa_company_node-address3     = |{ lwa_kna1_adrc-post_code1 }-{ i_kna1-ort01 }|.
        gwa_company_node-countrycode  = i_kna1-land1.
        gwa_company_node-externalref  = |{ i_kna1-lifnr ALPHA = OUT }|.
        gwa_company_node-referencecode  = |{ i_kna1-kunnr ALPHA = OUT }|.

        APPEND INITIAL LINE TO gwa_company_node-vendoruserproperties[] ASSIGNING <fs_user_prop>.
        <fs_user_prop>-fieldname = 'Counterparty type SAP'.
        <fs_user_prop>-fieldid   = 1.
        <fs_user_prop>-value     = 'C'.

      ENDIF.

    ENDIF.

    IF ( gwa_company_node IS NOT INITIAL ).

      CALL TRANSFORMATION zfi_vip_counterparties
      SOURCE company_node = gwa_company_node
        RESULT XML gva_xml_result.

      CALL FUNCTION 'CRM_IC_XML_XSTRING2STRING'
        EXPORTING
          inxstring = gva_xml_result
        IMPORTING
          outstring = gva_xml_string.

      IF ( i_creation = abap_true ).

        CONCATENATE '<company xmlns:ns0="http://schemas.veson.com/2005/ImosData"'
                    'xmlns:imos="http://schemas.veson.com/2005/Imos"'
                    'xmlns:imosmsg="http://schemas.veson.com/2005/ImosMsg" imosmsg:action="create">'
         INTO gva_tag SEPARATED BY space.

      ELSE.

        CONCATENATE '<company xmlns:ns0="http://schemas.veson.com/2005/ImosData"'
                    'xmlns:imos="http://schemas.veson.com/2005/Imos"'
                    'xmlns:imosmsg="http://schemas.veson.com/2005/ImosMsg" imosmsg:action="update">'
         INTO gva_tag SEPARATED BY space.

      ENDIF.

      REPLACE ALL OCCURRENCES OF '<company>' IN gva_xml_string WITH gva_tag.

      IF ( gva_xml_string IS NOT INITIAL ).
        me->send_xml_to_api( i_xml  = gva_xml_string i_metodo = 'SET_COUNTERPARTIES'
                             i_lfa1 = COND #( WHEN i_lfa1 IS NOT INITIAL THEN i_lfa1 ELSE '' )
                             i_kna1 = COND #( WHEN i_kna1 IS NOT INITIAL THEN i_kna1 ELSE '' )
                             i_creation = COND #( WHEN i_creation IS NOT INITIAL THEN i_creation ELSE '' ) ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD set_counterparties_bank.

    DATA: gwa_company_bank TYPE zfis_vip_counterparties_bank,
          gva_xml_result   TYPE xstring,
          gva_xml_string   TYPE string,
          gva_tag          TYPE string.

** 01.11.2023 -  CSB - Inicio
* Comentado dia 17.05.2024 - Para usar o campo I_LFA1-SCACD - Inicio
*    SELECT SINGLE * FROM ibpsupplier
*      INTO @DATA(wa_ibpsuplier)
*      WHERE supplier = @i_lfa1-lifnr.
*
*    SELECT SINGLE ind_sector
*      FROM but0is
*    INTO @DATA(lva_isec)
*      WHERE partner EQ @wa_ibpsuplier-businesspartner
*      AND ind_sector = '0200'.
* Comentado dia 17.05.2024 - Para usar o campo I_LFA1-SCACD - Fim

    "CHECK ( i_lfa1 IS NOT INITIAL AND t_lfbk[] IS NOT INITIAL ) AND ( i_lfa1-brsch = '0200' ).

    CHECK ( i_lfa1 IS NOT INITIAL AND t_lfbk[] IS NOT INITIAL ) AND i_lfa1-scacd = '0200'. "AND ( lva_isec = '0200' ).
** 01.11.2023 -  CSB - Fim

    gwa_company_bank-externalref  = |{ i_lfa1-lifnr ALPHA = OUT }|.


    LOOP AT t_lfbk[] INTO DATA(wa_lfbk).

      gwa_company_bank-seq  = wa_lfbk-bvtyp.
      gwa_company_bank-beneficiarybankcompanyno  = ' '.
      gwa_company_bank-beneficiarybankcountrycode = wa_lfbk-banks.

      SELECT SINGLE * FROM bnka INTO @DATA(wa_bnka)
         WHERE banks = @wa_lfbk-banks
           AND bankl = @wa_lfbk-bankl.

      IF ( sy-subrc = 0 ).
        gwa_company_bank-beneficiarybankname     = wa_bnka-banka. "wa_bnka-brnch.
        gwa_company_bank-beneficiarybankaddress1 = wa_bnka-stras.
        gwa_company_bank-beneficiarybankaddress2 = wa_bnka-ort01.
        gwa_company_bank-beneficiarybankfullname = |{ i_lfa1-name1 } { i_lfa1-name2 }|. "wa_bnka-banka.
        gwa_company_bank-beneficiarybankswiftcode = wa_bnka-swift.
      ENDIF.

      SELECT SINGLE * FROM tiban INTO @DATA(wa_tiban)
        WHERE banks = @wa_lfbk-banks
          AND bankl = @wa_lfbk-bankl
          AND bankn = @wa_lfbk-bankn.

      IF ( sy-subrc = 0 ).
        gwa_company_bank-beneficiarybankiban = wa_tiban-iban.
      ENDIF.

      gwa_company_bank-beneficiarybankact = wa_lfbk-bankn.
      gwa_company_bank-beneficiarybankabano = ' '.


      IF ( wa_lfbk-bkref IS NOT INITIAL ).  " Banco correspondente

        SELECT SINGLE * FROM lfbk INTO @DATA(lwa_correspondent)
          WHERE lifnr = @wa_lfbk-lifnr AND
                bvtyp = @wa_lfbk-bkref.

        IF ( sy-subrc = 0 ).

          SELECT SINGLE * FROM bnka INTO @DATA(lwa_bnka_corr)
            WHERE banks = @lwa_correspondent-banks
              AND bankl = @lwa_correspondent-bankl.

          gwa_company_bank-correspondentbankcompanyno   = ''.
          gwa_company_bank-correspondentbankcountrycode = lwa_correspondent-banks.
          gwa_company_bank-correspondentbankname        = lwa_bnka_corr-banka.
          gwa_company_bank-correspondentbankaddress1    = lwa_bnka_corr-stras.
          gwa_company_bank-correspondentbankaddress2    = lwa_bnka_corr-ort01.
          gwa_company_bank-correspondentbankswiftcode   = lwa_bnka_corr-swift.
          gwa_company_bank-correspondentbankfullname    = lwa_bnka_corr-banka.

        ENDIF.

      ENDIF.

      IF ( gwa_company_bank IS NOT INITIAL ).

        CALL TRANSFORMATION zfi_vip_counterparties_bank
        SOURCE company_node = gwa_company_bank
          RESULT XML gva_xml_result.

        CALL FUNCTION 'CRM_IC_XML_XSTRING2STRING'
          EXPORTING
            inxstring = gva_xml_result
          IMPORTING
            outstring = gva_xml_string.

        IF ( i_creation = abap_true ).

          CONCATENATE '<companyBank xmlns:ns0="http://schemas.veson.com/2005/ImosData"'
                      'xmlns:imos="http://schemas.veson.com/2005/Imos"'
                      'xmlns:imosmsg="http://schemas.veson.com/2005/ImosMsg" imosmsg:action="create">'
           INTO gva_tag SEPARATED BY space.

        ELSE.

          CONCATENATE '<companyBank xmlns:ns0="http://schemas.veson.com/2005/ImosData"'
                      'xmlns:imos="http://schemas.veson.com/2005/Imos"'
                      'xmlns:imosmsg="http://schemas.veson.com/2005/ImosMsg" imosmsg:action="update">'
           INTO gva_tag SEPARATED BY space.

        ENDIF.

        REPLACE ALL OCCURRENCES OF '<companyBank>' IN gva_xml_string WITH gva_tag.

        IF ( gva_xml_string IS NOT INITIAL ).
          me->send_xml_to_api( i_xml = gva_xml_string i_metodo = 'SET_COUNTERPARTIES_BANK' ).
        ENDIF.

      ENDIF.

      CLEAR: wa_bnka, wa_tiban, gva_xml_string, gva_xml_result.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_exchange_rates.

    TYPES: ty_rg_curr       TYPE RANGE OF tcurr-fcurr.

    DATA: gva_exch_rate     TYPE tcurr-ukurs,
          gva_exch_date     TYPE sy-datum,
          gva_xml_result    TYPE xstring,
          gva_xml_string    TYPE string,
          git_exchange_rate TYPE zfit_vip_exchange_rate.

    "Moedas enviadas:
    DATA(rg_curr) = VALUE ty_rg_curr(
      ( sign   = 'I' option = 'EQ' low = 'USD' high = 'EUR' )
      ( sign   = 'I' option = 'EQ' low = 'USD' high = 'CHF' )
      ( sign   = 'I' option = 'EQ' low = 'USD' high = 'GBP' ) ).


    LOOP AT rg_curr[] INTO DATA(lwa_curr).

      CALL FUNCTION 'READ_EXCHANGE_RATE'
        EXPORTING
          client           = sy-mandt
          date             = sy-datum
          foreign_currency = lwa_curr-high
          local_currency   = lwa_curr-low
          exact_date       = abap_true
        IMPORTING
          exchange_rate    = gva_exch_rate
          valid_from_date  = gva_exch_date
        EXCEPTIONS
          no_rate_found    = 1
          no_factors_found = 2
          no_spread_found  = 3
          derived_2_times  = 4
          overflow         = 5
          zero_rate        = 6
          OTHERS           = 7.

      IF ( sy-subrc = 0 ).

        APPEND INITIAL LINE TO git_exchange_rate[] ASSIGNING FIELD-SYMBOL(<lfs_exchange_rate>).

        <lfs_exchange_rate>-basecurrency  = lwa_curr-low.
        <lfs_exchange_rate>-currency      = lwa_curr-high.
        <lfs_exchange_rate>-rate          = gva_exch_rate.
        <lfs_exchange_rate>-effectivedate = |{ gva_exch_date+0(4) }-{ gva_exch_date+4(2) }-{ gva_exch_date+6(2) }T00:00:00|.

        REPLACE ALL OCCURRENCES OF '-' IN <lfs_exchange_rate>-rate WITH ''.

      ELSE.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    ENDLOOP.

    IF ( git_exchange_rate[] IS NOT INITIAL ).

      CALL TRANSFORMATION zfi_vip_exchange_rates
      SOURCE exchange_rates = git_exchange_rate[]
        RESULT XML gva_xml_result.

      CALL FUNCTION 'CRM_IC_XML_XSTRING2STRING'
        EXPORTING
          inxstring = gva_xml_result
        IMPORTING
          outstring = gva_xml_string.

      IF ( gva_xml_string IS NOT INITIAL ).

        me->send_xml_to_api( i_xml = gva_xml_string i_metodo = 'SET_EXCHANGE_RATES' ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD set_invoice_reverse.

    DATA(lva_gjhar) = CONV gjahr( i_zfit0164-actdate+0(4) ).

    IF ( i_zfit0164-status = 'R' ).
      DATA(lva_obj_key) = CONV awkey( |{ i_zfit0164-transno+0(13) }N{ lva_gjhar }| ).
    ELSE.
      lva_obj_key = CONV awkey( |{ i_zfit0164-transno }{ lva_gjhar }| ).
    ENDIF.

    SELECT SINGLE * FROM zib_contabil_chv
      INTO @DATA(lwa_zib_chv)
      WHERE obj_key = @lva_obj_key.

    IF ( lwa_zib_chv IS NOT INITIAL ).

      SUBMIT z_fb08_zgl042 WITH p_obj = lva_obj_key
      AND RETURN.

      WAIT UP TO 2 SECONDS.

      SELECT SINGLE stblg FROM bkpf
        INTO @DATA(lwa_check_estorno)
        WHERE bukrs = '0200'
          AND belnr = @lwa_zib_chv-belnr
          AND gjahr = @lva_gjhar.

      IF ( lwa_check_estorno IS NOT INITIAL ).

        UPDATE zfit0164
           SET ck_estornado     = abap_true
               ck_zib_contabil  = abap_true
               belnr_estorno    = lwa_check_estorno
         WHERE transno    = i_zfit0164-transno
           AND entrydate  = i_zfit0164-entrydate.

      ENDIF.

    ELSE.

      SELECT * FROM zfit0164
        INTO TABLE @DATA(it_check_0164)
        WHERE transno = @i_zfit0164-transno
          AND status  IN ( '1', '0' ).
      IF ( sy-subrc = 0 ).
        UPDATE zfit0164
           SET ck_estornado     = abap_true
               ck_zib_contabil  = abap_true
               "BELNR_ESTORNO    = ''
         WHERE transno    = i_zfit0164-transno
           AND entrydate  = i_zfit0164-entrydate.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  method set_invoice_xml_to_sap.

    data: "GWA_INVOICE     TYPE ZFIT_VIP_INVOICE,
      it_invoices     type zfit_t_vip_invoice.

    if ( i_xml is not initial ).

      call function 'ZFI_VIP_INVOICE_XML_CONVERT'
        exporting
          i_xml      = i_xml            " Invoice XML
        tables
          t_conteudo = it_invoices[].  " VIP - Tabela Invoices

      loop at it_invoices[] into data(gwa_invoice).


        if ( gwa_invoice-transno is not initial ).

          replace all occurrences of '-' in gwa_invoice-entrydate with ''.
          replace all occurrences of '-' in gwa_invoice-invoicedate with ''.
          replace all occurrences of '-' in gwa_invoice-actdate with ''.
          replace all occurrences of '-' in gwa_invoice-duedate with ''.
          replace all occurrences of '-' in gwa_invoice-exchangeratedate with ''.

          data(lwa_zfit0164) = value zfit0164(
              transno             = gwa_invoice-transno
              entrydate           = conv #( gwa_invoice-entrydate )
              status              = gwa_invoice-status
              vendorexternalref   = gwa_invoice-vendorexternalref
              vendorreferencecode = gwa_invoice-vendorreferencecode
              invoiceno           = gwa_invoice-invoiceno
              invoicedate         = conv #( gwa_invoice-invoicedate )
              actdate             = conv #( gwa_invoice-actdate )
              duedate             = conv #( gwa_invoice-duedate )
              exchangeratedate    = conv #( gwa_invoice-exchangeratedate )
              exchangerate        = gwa_invoice-exchangerate
              taxa_imos           = gwa_invoice-exchangerate "US - 145673 - 06 decimal places
              oprbillsource       = gwa_invoice-oprbillsource
              currencyamount      = conv #( gwa_invoice-currencyamount )
              currency            = gwa_invoice-currency
              basecurrencyamount  = conv #( gwa_invoice-basecurrencyamount ) ).

          loop at gwa_invoice-invoicedetails[] into data(lwa_invoice_details).

            "REPLACE ALL OCCURRENCES OF '-' IN LWA_INVOICE_DETAILS-CURRENCYAMOUNT WITH ''.
            "REPLACE ALL OCCURRENCES OF '-' IN  LWA_INVOICE_DETAILS-BASECURRENCYAMOUNT WITH ''.

            lwa_zfit0164-detail_transtype           = conv #( lwa_invoice_details-transtype ).
            lwa_zfit0164-seqitem                    = conv #( lwa_invoice_details-seqno ).
            lwa_zfit0164-detail_companycode         = conv #( lwa_invoice_details-companycode ).
            lwa_zfit0164-detail_oprbillcode         = conv #( lwa_invoice_details-oprbillcode ).
            lwa_zfit0164-detail_lobcode             = conv #( lwa_invoice_details-lobcode ).
            lwa_zfit0164-detail_ledgercode          = conv #( lwa_invoice_details-ledgercode(6) ).
            lwa_zfit0164-detail_aparcode            = conv #( lwa_invoice_details-aparcode ).
            lwa_zfit0164-detail_memo                = conv #( lwa_invoice_details-memo ).
            lwa_zfit0164-detail_currencyamount      = conv #( lwa_invoice_details-currencyamount ).
            lwa_zfit0164-detail_basecurrencyamount  = conv #( lwa_invoice_details-basecurrencyamount ).
            lwa_zfit0164-detail_voyageno            = conv #( lwa_invoice_details-voyageno ).
            lwa_zfit0164-detail_vesselcode          = conv #( lwa_invoice_details-vesselcode ).
            lwa_zfit0164-dt_envio_sap               = sy-datum.
            lwa_zfit0164-hr_envio_sap               = sy-uzeit.


            if ( lwa_zfit0164 is not initial ).

              if ( lwa_zfit0164-vendorexternalref is initial and lwa_zfit0164-vendorreferencecode is initial ) and
                 ( lwa_zfit0164-oprbillsource <> 'MACR' ).
                "Não grava registro sem CLIENTE/FORNECEDOR
                r_message = |{ gwa_invoice-transno } / { gwa_invoice-entrydate } - XML without VENDOREXTERNALREF or VENDORREFERENCECODE|.
              else.
                modify zfit0164 from lwa_zfit0164.

                r_message = |{ gwa_invoice-transno } / { gwa_invoice-entrydate } - Success - Invoice on queue to input!|.

              endif.

              clear:  lwa_zfit0164-detail_transtype, lwa_zfit0164-detail_companycode, lwa_zfit0164-detail_oprbillcode, lwa_zfit0164-detail_lobcode,
                      lwa_zfit0164-detail_ledgercode, lwa_zfit0164-detail_aparcode, lwa_zfit0164-detail_currencyamount, lwa_zfit0164-detail_basecurrencyamount,
                      lwa_zfit0164-detail_voyageno, lwa_zfit0164-detail_vesselcode.
            endif.

          endloop.

        endif.

      endloop.

    else.
      r_message = 'XML request is empty!'.
    endif.

  endmethod.


  METHOD set_invoice_zib_contabil.

    TYPES: ty_rg_transno TYPE RANGE OF zfit0164-transno.

    DATA: it_zib_contabil    TYPE TABLE OF zib_contabil,
          lwa_zfit0164       TYPE zfit0164,
          lva_wrbtr_cliente  TYPE tslxx12,
          lva_wrbtr_forn     TYPE tslxx12,
          lva_wrbtr_soma     TYPE tslxx12,
          lva_seq_item       TYPE num6 VALUE 000000,
          lva_valor_negativo TYPE abap_bool.

    " 1 - Busca todas as Invoices que não foram lançadas na ZIB_CONTABIL
    SELECT * FROM zfit0164
      INTO TABLE @DATA(lt_zfit0164)
      WHERE ck_zib_contabil = ''
        AND ck_estornado    = ''.
    SORT lt_zfit0164[] BY transno entrydate status seqitem ASCENDING.

    " -> Buscar chaves de lançamento na tabela ZFIT0165 cadastrada pela transação "ZFI0121 - Cadastro Chaves Lcto - VIP"
    SELECT * FROM zfit0165
      INTO TABLE @DATA(lt_zfit0165).

    IF ( lt_zfit0164[] IS NOT INITIAL ).

      " -> Criar range de TRANSNO ( Transaction Number on IMOS ) para processá-las 1 a 1:
      DATA(rg_transno) = VALUE ty_rg_transno( FOR _line IN lt_zfit0164[] (
        sign    = 'I'
        option  = 'EQ'
        low     = _line-transno ) ).
      SORT rg_transno[] BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM rg_transno[] COMPARING low.

      LOOP AT rg_transno[] INTO DATA(wa_transno).

        LOOP AT lt_zfit0164[] ASSIGNING FIELD-SYMBOL(<fs_zfit0164>) WHERE transno = wa_transno-low.

          " Estornar documento caso STATUS seja = X ou R
          IF ( <fs_zfit0164>-status = 'X' OR <fs_zfit0164>-status = 'R' ).
            me->set_invoice_reverse( i_zfit0164 = <fs_zfit0164> ).
            CONTINUE.
          ENDIF.

          DATA(lva_oprbillsource) = <fs_zfit0164>-oprbillsource.
          DATA(lva_gjhar) = CONV gjahr( <fs_zfit0164>-actdate+0(4) ).
          DATA(lva_monat) = CONV monat( <fs_zfit0164>-actdate+4(2) ).
          DATA(lva_budat) = |{ <fs_zfit0164>-actdate+6(2) }.{ <fs_zfit0164>-actdate+4(2) }.{ <fs_zfit0164>-actdate+0(4) }|.
          DATA(lva_bldat) = |{ <fs_zfit0164>-invoicedate+6(2) }.{ <fs_zfit0164>-invoicedate+4(2) }.{ <fs_zfit0164>-invoicedate+0(4) }|.
          DATA(lva_zfbdt) = |{ <fs_zfit0164>-duedate+6(2) }.{ <fs_zfit0164>-duedate+4(2) }.{ <fs_zfit0164>-duedate+0(4) }|.

          IF ( <fs_zfit0164>-detail_basecurrencyamount < 0 ).
            <fs_zfit0164>-detail_currencyamount = ( <fs_zfit0164>-detail_currencyamount * -1 ).
            lva_valor_negativo = abap_true.
          ENDIF.

          IF ( lva_oprbillsource = 'MACR' ).
*            IF ( <FS_ZFIT0164>-DETAIL_LEDGERCODE+0(1) <> '3' ).
*              CONTINUE.
*            ELSE.
            TRY .
                DATA(lwa_zfit0165) = lt_zfit0165[ source_code = lva_oprbillsource conta_contabil = <fs_zfit0164>-detail_ledgercode ].
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.
*            ENDIF.
          ELSE.
            "Buscar chave de lançamento de acordo com tipo do movimento ( 1 = FOrn 2 = Cliente )
            IF ( <fs_zfit0164>-detail_transtype = '1' ) AND ( lva_valor_negativo = abap_true ).
              LOOP AT lt_zfit0165[] INTO lwa_zfit0165 WHERE source_code = lva_oprbillsource AND chv_cliente IS NOT INITIAL.
              ENDLOOP.
            ELSEIF ( <fs_zfit0164>-detail_transtype = '1' ) AND ( lva_valor_negativo <> abap_true ).
              LOOP AT lt_zfit0165[] INTO lwa_zfit0165 WHERE source_code = lva_oprbillsource AND chv_forn IS NOT INITIAL.
              ENDLOOP.
            ELSEIF ( <fs_zfit0164>-detail_transtype = '2' ) AND ( lva_valor_negativo <> abap_true ).
              LOOP AT lt_zfit0165[] INTO lwa_zfit0165 WHERE source_code = lva_oprbillsource AND chv_cliente IS NOT INITIAL.
              ENDLOOP.
            ELSEIF ( <fs_zfit0164>-detail_transtype = '2' ) AND ( lva_valor_negativo = abap_true ).
              LOOP AT lt_zfit0165[] INTO lwa_zfit0165 WHERE source_code = lva_oprbillsource AND chv_forn IS NOT INITIAL.
              ENDLOOP.
            ENDIF.

          ENDIF.

          lva_seq_item = ( lva_seq_item + 1 ).

          DATA(lwa_zib_contabil_1) = VALUE zib_contabil(
            obj_key            = |{ <fs_zfit0164>-transno }{ lva_gjhar }|
            seqitem            = CONV #( lva_seq_item )
            bschl              = lwa_zfit0165-chv_contrap
            umskz              = COND #( WHEN lwa_zfit0165-raz_especial IS NOT INITIAL THEN lwa_zfit0165-raz_especial ELSE '' )
            gsber              = 'S201'
            bukrs              = '0200'
            interface          = '3'
            bktxt              = 'IMOS'
            bldat              = lva_bldat
            budat              = lva_budat
            gjahr              = lva_gjhar
            zfbdt              = lva_zfbdt
            monat              = lva_monat
            blart              = 'SI'
            xblnr              = CONV #( <fs_zfit0164>-invoiceno )
            hkont              = |0000{ <fs_zfit0164>-detail_ledgercode }|
            wrbtr              = CONV #( <fs_zfit0164>-detail_currencyamount )
            waers              = CONV #( <fs_zfit0164>-currency )
            sgtxt              = <fs_zfit0164>-detail_memo
            bupla              = 'S201'
            zuonr              = CONV #( <fs_zfit0164>-invoiceno )
            prctr              = CONV #( <fs_zfit0164>-detail_lobcode )
            waers_i            = 'CHF'
            rg_atualizado      = 'N'
        ).

          IF ( <fs_zfit0164>-currency = 'EUR' ) OR ( <fs_zfit0164>-currency = 'GBP' ).
            DATA(lva_eur) = me->get_exchange_rates(
              i_date  = <fs_zfit0164>-actdate
              i_fcurr = <fs_zfit0164>-currency
              i_tcurr = 'CHF' ).

            DATA(lva_usd) = me->get_exchange_rates(
              i_date  = <fs_zfit0164>-actdate
              i_fcurr = 'USD'
              i_tcurr = 'CHF' ).

*** US - 145673 - Inicio - CBRAND - 26.07.2024
*            if ( lva_eur is not initial ) and ( lva_usd is not initial ).
*              lwa_zib_contabil_1-dmbtr    = ( lwa_zib_contabil_1-wrbtr * lva_eur ).
*              lwa_zib_contabil_1-waers_f  = 'USD'.
*              lwa_zib_contabil_1-dmbe2    = ( lwa_zib_contabil_1-dmbtr / lva_usd ).
*            endif.
            IF ( lva_eur IS NOT INITIAL ) AND ( lva_usd IS NOT INITIAL ).
              lwa_zib_contabil_1-dmbtr    = ( lwa_zib_contabil_1-wrbtr * lva_eur ).
              lwa_zib_contabil_1-waers_f  = 'USD'.
              lwa_zib_contabil_1-dmbe2    = ( lwa_zib_contabil_1-wrbtr / <fs_zfit0164>-taxa_imos ).
            ENDIF.
          ENDIF.
*** US - 145673 - Fim - CBRAND - 26.07.2024

          IF ( lva_oprbillsource = 'MACR' ).

            IF ( lva_valor_negativo = abap_true ).

              CASE lwa_zib_contabil_1-bschl.
*                WHEN '31'.
*                  lwa_zib_contabil_1-bschl = '21'.
*                WHEN '01'.
*                  lwa_zib_contabil_1-bschl = '11'.
                WHEN '50'.
                  lwa_zib_contabil_1-bschl = '40'.
                WHEN '40'.
                  lwa_zib_contabil_1-bschl = '50'.
              ENDCASE.

            ENDIF.

            IF ( <fs_zfit0164>-detail_ledgercode = '211502' ).
              IF ( lva_valor_negativo = abap_true ).
                lwa_zib_contabil_1-bschl = '31'.
              ELSE.
                lwa_zib_contabil_1-bschl = '21'.
              ENDIF.

              lwa_zib_contabil_1-hkont = |0000{ <fs_zfit0164>-vendorexternalref }|.

            ELSEIF ( <fs_zfit0164>-detail_ledgercode = '113302' ).

              IF ( lva_valor_negativo = abap_true ).
                lwa_zib_contabil_1-bschl = '01'.
              ELSE.
                lwa_zib_contabil_1-bschl = '11'.
              ENDIF.

              lwa_zib_contabil_1-hkont = |0000{ <fs_zfit0164>-vendorreferencecode }|.

            ENDIF.

          ELSEIF ( lva_oprbillsource = 'FFAS' ).

            IF ( <fs_zfit0164>-currencyamount < 0 ).
              lwa_zib_contabil_1-bschl = COND #(
                WHEN lva_valor_negativo = abap_true THEN '40'
                ELSE '50'  ).
            ENDIF.

          ENDIF.

          IF ( lwa_zib_contabil_1-wrbtr < 0  ).
            lwa_zib_contabil_1-wrbtr = ( lwa_zib_contabil_1-wrbtr * -1 ).
          ENDIF.

          APPEND lwa_zib_contabil_1 TO it_zib_contabil[].
          <fs_zfit0164>-ck_zib_contabil = abap_true.
          lwa_zfit0164 = <fs_zfit0164>.

          MODIFY lt_zfit0164 FROM lwa_zfit0164 TRANSPORTING ck_zib_contabil
            WHERE transno   = lwa_zfit0164-transno   AND
                  entrydate = lwa_zfit0164-entrydate AND
                  status    = lwa_zfit0164-status.

          lva_wrbtr_soma = ( lva_wrbtr_soma + <fs_zfit0164>-detail_basecurrencyamount ).


          CLEAR: lwa_zfit0165, lva_valor_negativo.

        ENDLOOP.

        IF ( lva_oprbillsource <> 'MACR' ) AND ( <fs_zfit0164>-status <> 'X' ) AND ( <fs_zfit0164>-status <> 'R' ).

          lva_seq_item = ( lva_seq_item + 1 ).
** Se for rodar novamente a ZIB se atentar ao detail_currencyamount na 164 que estará positivo ( Antes era negativo )
          IF ( <fs_zfit0164>-detail_basecurrencyamount < 0 ).
            <fs_zfit0164>-detail_currencyamount = ( <fs_zfit0164>-detail_currencyamount * -1 ).
            lva_valor_negativo = abap_true.
          ENDIF.

          "Buscar chave de lançamento de acordo com tipo do movimento ( 1 = FOrn 2 = Cliente )
          IF ( lva_oprbillsource = 'FFAS' ).

            LOOP AT lt_zfit0165[] INTO lwa_zfit0165 WHERE source_code = lva_oprbillsource AND chv_cliente IS NOT INITIAL.
            ENDLOOP.

          ELSE.

            IF ( <fs_zfit0164>-detail_transtype = '1' ) AND ( lva_wrbtr_soma < 0 ). "( lva_valor_negativo = abap_true ).
              LOOP AT lt_zfit0165[] INTO lwa_zfit0165 WHERE source_code = lva_oprbillsource AND chv_cliente IS NOT INITIAL.
              ENDLOOP.
            ELSEIF ( <fs_zfit0164>-detail_transtype = '2' ) AND ( lva_wrbtr_soma < 0 ). "( lva_valor_negativo = abap_true ).
              LOOP AT lt_zfit0165[] INTO lwa_zfit0165 WHERE source_code = lva_oprbillsource AND chv_forn IS NOT INITIAL.
              ENDLOOP.
            ELSEIF ( <fs_zfit0164>-detail_transtype = '1' ) AND ( lva_wrbtr_soma > 0 ). "( lva_valor_negativo <> abap_true ).
              LOOP AT lt_zfit0165[] INTO lwa_zfit0165 WHERE source_code = lva_oprbillsource AND chv_forn IS NOT INITIAL.
              ENDLOOP.
            ELSEIF ( <fs_zfit0164>-detail_transtype = '2' ) AND ( lva_wrbtr_soma > 0 ). "( lva_valor_negativo <> abap_true ).
              LOOP AT lt_zfit0165[] INTO lwa_zfit0165 WHERE source_code = lva_oprbillsource AND chv_cliente IS NOT INITIAL.
              ENDLOOP.
            ENDIF.

          ENDIF.

          " Criar segunda linha ZIB_CONTABIL:
          DATA(lwa_zib_contabil_2) = VALUE zib_contabil(
              obj_key            = |{ <fs_zfit0164>-transno }{ lva_gjhar }|
              seqitem            = CONV #( lva_seq_item )
              bschl              = COND #( WHEN lwa_zfit0165-chv_forn IS NOT INITIAL THEN lwa_zfit0165-chv_forn
                                           ELSE lwa_zfit0165-chv_cliente )
                                    "COND #( WHEN <fs_zfit0164>-detail_transtype = '1' THEN lwa_zfit0165-chv_forn
                                    "       ELSE lwa_zfit0165-chv_cliente )
              umskz              = COND #( WHEN lwa_zfit0165-raz_especial IS NOT INITIAL THEN lwa_zfit0165-raz_especial ELSE '' )
              gsber              = 'S201'
              bukrs              = '0200'
              interface          = '3'
              bktxt              = 'IMOS'
              bldat              = lva_bldat
              budat              = lva_budat
              gjahr              = lva_gjhar
              monat              = lva_monat
              blart              = 'SI'
              xblnr              = CONV #( <fs_zfit0164>-invoiceno )
              zfbdt              = lva_zfbdt
              "hkont              = COND #( WHEN lwa_zfit0165-chv_forn IS NOT INITIAL THEN <fs_zfit0164>-vendorexternalref
              "                             ELSE <fs_zfit0164>-vendorreferencecode )
                                    "COND #( WHEN <fs_zfit0164>-detail_transtype = '1' THEN CONV #( <fs_zfit0164>-vendorexternalref )
                                    "        ELSE CONV #( <fs_zfit0164>-vendorreferencecode ) )           "CONV #( <fs_zfit0164>-detail_ledgercode )
              wrbtr              = COND #( WHEN <fs_zfit0164>-currency = 'USD' THEN <fs_zfit0164>-basecurrencyamount
                                           ELSE <fs_zfit0164>-currencyamount  ) " lva_wrbtr_soma )
              waers              = CONV #( <fs_zfit0164>-currency )
              sgtxt              = <fs_zfit0164>-detail_memo
              bupla              = 'S201'
              zuonr              = CONV #( <fs_zfit0164>-invoiceno )
              prctr              = CONV #( <fs_zfit0164>-detail_lobcode )
              waers_i            = 'CHF'
              rg_atualizado      = 'N'
          ).

          IF ( lwa_zfit0165-chv_forn IS NOT INITIAL ).
            lwa_zib_contabil_2-hkont = |0000{ <fs_zfit0164>-vendorexternalref }|.
          ELSE.
            lwa_zib_contabil_2-hkont = |0000{ <fs_zfit0164>-vendorreferencecode }|.
          ENDIF.

          IF ( lwa_zib_contabil_2-wrbtr < 0 ).
            lwa_zib_contabil_2-wrbtr = ( lwa_zib_contabil_2-wrbtr * -1 ).
          ENDIF.


          IF ( <fs_zfit0164>-currency = 'EUR' ) OR ( <fs_zfit0164>-currency = 'GBP' ).
            CLEAR: lva_eur, lva_usd.
            lva_eur = me->get_exchange_rates(
              i_date  = <fs_zfit0164>-actdate
              i_fcurr = <fs_zfit0164>-currency
              i_tcurr = 'CHF' ).

            lva_usd = me->get_exchange_rates(
              i_date  = <fs_zfit0164>-actdate
              i_fcurr = 'USD'
              i_tcurr = 'CHF' ).

*** US - 145673 - Inicio - CBRAND
*            if ( lva_eur is not initial ) and ( lva_usd is not initial ).
*              lwa_zib_contabil_2-dmbtr    = ( lwa_zib_contabil_2-wrbtr * lva_eur ).
*              lwa_zib_contabil_2-waers_f  = 'USD'.
*              lwa_zib_contabil_2-dmbe2    = ( lwa_zib_contabil_2-dmbtr / lva_usd ).
*            endif.

            IF ( lva_eur IS NOT INITIAL ) AND ( lva_usd IS NOT INITIAL ).
              lwa_zib_contabil_2-dmbtr    = ( lwa_zib_contabil_2-wrbtr * lva_eur ).
              lwa_zib_contabil_2-waers_f  = 'USD'.
*** BUG - 149066 - Inicio - CBRAND
              " BUG - 147662 - CSB - Inicio
*              lwa_zib_contabil_2-dmbe2    = ( lwa_zib_contabil_2-dmbtr / <fs_zfit0164>-taxa_imos ).
              " BUG - 147662 - CSB - Fim
               lwa_zib_contabil_2-dmbe2    = ( lwa_zib_contabil_2-wrbtr / <fs_zfit0164>-taxa_imos ).
*** BUG - 149066 - Fim - CBRAND
            ENDIF.
          ENDIF.
*** US - 145673 - Fim - CBRAND

          APPEND lwa_zib_contabil_2 TO it_zib_contabil[].

        ENDIF.

        CLEAR: lwa_zfit0164, lwa_zfit0165, lva_oprbillsource, lva_gjhar, lva_monat, lva_budat, lva_bldat, lva_zfbdt,
               lwa_zib_contabil_1, lwa_zib_contabil_2, lva_wrbtr_cliente, lva_wrbtr_forn,
               lva_oprbillsource, lva_valor_negativo, lva_wrbtr_soma, lva_eur, lva_usd.
*        lwa_zib_contabil_cli, lva_ledgercode_cliente, lva_ledgercode_forn,
      ENDLOOP.

      IF ( it_zib_contabil[] IS NOT INITIAL ).

*** BUG - 142990 - Inicio -  Ajustes de arredondamento.
        me->arredonda_contabil( CHANGING i_zib_contabil =  it_zib_contabil[]  ).
*** BUG - 142990 - Fim -  Ajustes de arredondamento.


        "-> Validar primeiro se chave já possui documento gerado na ZIB_CONTABIL_CHV
        SELECT obj_key, belnr
          FROM zib_contabil_chv
          INTO TABLE @DATA(it_chk_zib_chv)
          FOR ALL ENTRIES IN @it_zib_contabil
          WHERE obj_key = @it_zib_contabil-obj_key
            AND bukrs   = @it_zib_contabil-bukrs
            AND gjahr   = @it_zib_contabil-gjahr.
        IF ( sy-subrc = 0 ).
          LOOP AT it_chk_zib_chv[] INTO DATA(lwa_chk_zib_chv).
            DELETE it_zib_contabil[] WHERE obj_key = lwa_chk_zib_chv.
          ENDLOOP.
        ENDIF.

        IF ( it_zib_contabil[] IS NOT INITIAL ).
          MODIFY zib_contabil FROM TABLE it_zib_contabil[].

          "Atualizar Tabela ZFIT0164 após lançamento na ZIB_CONTABIL:
          IF ( sy-subrc = 0 ).
            MODIFY zfit0164 FROM TABLE lt_zfit0164.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  method set_retorna_status.

    data: gva_xml_result type xstring,
          gva_xml_string type string.
    data: wa_zfit0036 type zfit0036. " US 164319-28-02-2025-#164319-RJF

    select * from zfit0164
      into table @data(lt_zift0164)
      where ck_zib_contabil = 'X' and
            ck_status_enviado = ''.

    if ( lt_zift0164[] is not initial ).
      loop at lt_zift0164[] into data(lwa_zfit0164).

        data(lva_gjhar)  = conv gjahr( lwa_zfit0164-actdate+0(4) ).
        data(lva_monat)  = conv monat( lwa_zfit0164-actdate+4(2) ).
        data(lva_objkey) = |{ lwa_zfit0164-transno }{ lva_gjhar }|.

        select single * from zib_contabil_chv
          into @data(lwa_zib_chv)
          where obj_key = @lva_objkey.
        if ( sy-subrc = 0 ) or ( lwa_zfit0164-status = 'R' ).

          select single * from zib_contabil
            into @data(lwa_zib_contabil)
            where obj_key = @lva_objkey.

          data(lwa_status) = value zfis_vip_invoice_status(
            transno                   = lwa_zfit0164-transno
            externalrefid             = cond #(
              when lwa_zfit0164-belnr_estorno is not initial then lwa_zfit0164-belnr_estorno else lwa_zib_chv-belnr )
            interfacestatus           = 'S'
            interfaceerrordescription = ''
            entrydate                 = lwa_zfit0164-entrydate
            invoiceno                 = lwa_zfit0164-invoiceno
            invoicedate               = lwa_zfit0164-invoicedate
            actdate                   = lwa_zfit0164-actdate
        ).

        else.

          select single * from zib_contabil_err
            into @data(lwa_zib_err)
            where obj_key = @lva_objkey.

          if ( sy-subrc = 0 ).

            lwa_status = value zfis_vip_invoice_status(
              transno                   = lwa_zfit0164-transno
              externalrefid             = ''
              interfacestatus           = 'B'
              interfaceerrordescription = lwa_zib_err-message
              entrydate                 = lwa_zfit0164-entrydate
              invoiceno                 = lwa_zfit0164-invoiceno
              invoicedate               = lwa_zfit0164-invoicedate
              actdate                   = lwa_zfit0164-actdate
            ).

          endif.

        endif.

        if ( lwa_status is not initial ).

          call transformation zfi_vip_invoice_status
            source invoicestatus = lwa_status
            result xml gva_xml_result.

          call function 'CRM_IC_XML_XSTRING2STRING'
            exporting
              inxstring = gva_xml_result
            importing
              outstring = gva_xml_string.

          if ( gva_xml_string is not initial ).
            me->send_xml_to_api( i_xml = gva_xml_string i_metodo = 'SET_RETORNA_STATUS' ).

            update zfit0164
               set ck_status_enviado  = abap_true
             where transno    = lwa_zfit0164-transno
               and entrydate  = lwa_zfit0164-entrydate
               and status     = lwa_zfit0164-status.

*-US 164319-28-02-2025-#164319-RJF-Inicio
            select single * from zib_contabil
              into @data(wa_zib_contabilx)
              where obj_key = @lva_objkey.

            wa_zfit0036-obj_key           = wa_zib_contabilx-obj_key.
            wa_zfit0036-bukrs             = wa_zib_contabilx-bukrs.
            wa_zfit0036-invoice           = lwa_zfit0164-invoiceno.
            wa_zfit0036-in_performance    = 'N'.
            wa_zfit0036-invoice_terc      = lwa_zfit0164-invoiceno.
            wa_zfit0036-navio             = lwa_zfit0164-detail_vesselcode.


            modify zfit0036 from wa_zfit0036.
            if sy-subrc is initial.
              commit work and wait.
            endif.
*-US 164319-28-02-2025-#164319-RJF-Fim

          endif.

        endif.

      endloop.
    endif.

  endmethod.


  METHOD arredonda_contabil.

    DATA:
      vlr_toler        TYPE zglt036-vlr_moeda_int,
      vlr_tot_int      TYPE zglt036-vlr_moeda_int,
      vlr_tot_int_s    TYPE zglt036-vlr_moeda_int,
      vlr_tot_doc      TYPE zglt036-vlr_moeda_doc,
      vlr_tot_gru      TYPE zglt036-vlr_moeda_grupo,
      vlr_tot_for      TYPE zglt036-vlr_moeda_forte,
      vlr_tot_for_s    TYPE zglt036-vlr_moeda_forte,
      git_zib_contabil TYPE TABLE OF zib_contabil.

    git_zib_contabil[] = i_zib_contabil[].

    SORT git_zib_contabil BY obj_key.
    DELETE ADJACENT DUPLICATES FROM git_zib_contabil COMPARING obj_key.

    LOOP AT git_zib_contabil INTO DATA(gwa_zib_contabil).
      CLEAR: vlr_tot_doc,
             vlr_tot_int,
             vlr_tot_for.

      LOOP AT i_zib_contabil[] INTO DATA(lwa_zib_contabil) WHERE obj_key EQ gwa_zib_contabil-obj_key .

        SELECT SINGLE shkzg FROM tbsl
          INTO @DATA(lva_shkzg)
        WHERE bschl EQ @lwa_zib_contabil-bschl.

        IF lva_shkzg NE 'S'.
          IF lwa_zib_contabil-dmbtr   >= 0. "Moeda inerna
            lwa_zib_contabil-dmbtr    = lwa_zib_contabil-dmbtr   * -1.
          ENDIF.
          IF lwa_zib_contabil-dmbe2 >= 0.
            lwa_zib_contabil-dmbe2  = lwa_zib_contabil-dmbe2 * -1.
          ENDIF.

          IF lwa_zib_contabil-wrbtr >= 0.
            lwa_zib_contabil-wrbtr    = lwa_zib_contabil-wrbtr   * -1.
          ENDIF.
        ENDIF.

        ADD lwa_zib_contabil-wrbtr TO vlr_tot_doc.
        ADD lwa_zib_contabil-dmbtr TO vlr_tot_int.
        ADD lwa_zib_contabil-dmbe2 TO vlr_tot_for.
        CLEAR: lwa_zib_contabil, lva_shkzg.
      ENDLOOP.

      vlr_toler = 2 / 100.

      LOOP AT i_zib_contabil ASSIGNING FIELD-SYMBOL(<lwa_zib_contabil>) WHERE obj_key EQ gwa_zib_contabil-obj_key . .
        CLEAR: lva_shkzg.

        SELECT SINGLE shkzg FROM tbsl
          INTO lva_shkzg
          WHERE bschl EQ <lwa_zib_contabil>-bschl.


        "FORTE VLR_TOT_FOR

        IF vlr_tot_for GT 0 AND vlr_tot_for LE vlr_toler.
          IF  <lwa_zib_contabil>-bschl EQ '50' AND lva_shkzg EQ 'H'.  "Moeda Forte Maior  que tolerancia
            <lwa_zib_contabil>-dmbe2 = <lwa_zib_contabil>-dmbe2 + abs( vlr_tot_for ).
            CLEAR vlr_tot_for.
          ENDIF.
          IF  <lwa_zib_contabil>-bschl EQ '40' AND lva_shkzg EQ 'S'.  "Moeda Forte Maior  que tolerancia
            <lwa_zib_contabil>-dmbe2 = <lwa_zib_contabil>-dmbe2  - abs( vlr_tot_for ).
            CLEAR vlr_tot_for.
          ENDIF.
        ENDIF.
        IF vlr_tot_for LT 0  AND vlr_tot_for GE ( vlr_toler * ( -1 ) ).
          IF <lwa_zib_contabil>-bschl EQ '50' AND lva_shkzg EQ 'H'."Moeda Forte menor que tolerancia
            <lwa_zib_contabil>-dmbe2 = <lwa_zib_contabil>-dmbe2  - abs( vlr_tot_for ).
            CLEAR vlr_tot_for.
          ENDIF.
          IF <lwa_zib_contabil>-bschl EQ '40' AND lva_shkzg EQ 'S'.""Moeda Forte menor que tolerancia - RJF
            <lwa_zib_contabil>-dmbe2 = <lwa_zib_contabil>-dmbe2  + abs( vlr_tot_for ).
            CLEAR vlr_tot_for.
          ENDIF.
        ENDIF.

        " "INTERNO
        IF vlr_tot_int GT 0 AND vlr_tot_int LE vlr_toler.
          IF  <lwa_zib_contabil>-bschl EQ '50' AND lva_shkzg EQ 'H'.  "Moeda INTERNA  Maior  que tolerancia
            <lwa_zib_contabil>-dmbtr = <lwa_zib_contabil>-dmbtr + abs( vlr_tot_int ).
            CLEAR vlr_tot_int.
          ENDIF.
          IF  <lwa_zib_contabil>-bschl EQ '40' AND lva_shkzg EQ 'S'.  "Moeda INTERNA  Maior  que tolerancia
            <lwa_zib_contabil>-dmbtr = <lwa_zib_contabil>-dmbtr - abs( vlr_tot_int ).
            CLEAR vlr_tot_int.
          ENDIF.
        ENDIF.
        IF vlr_tot_int LT 0  AND vlr_tot_int GE ( vlr_toler * ( -1 ) ) .
          IF <lwa_zib_contabil>-bschl EQ '50' AND lva_shkzg EQ 'H'."Moeda INTERNA menor que tolerancia
            <lwa_zib_contabil>-dmbtr = <lwa_zib_contabil>-dmbtr - abs( vlr_tot_int ).
            CLEAR vlr_tot_int.
          ENDIF.
          IF <lwa_zib_contabil>-bschl EQ '40' AND lva_shkzg EQ 'S'. "Moeda INTERNA menor que tolerancia
            <lwa_zib_contabil>-dmbtr = <lwa_zib_contabil>-dmbtr + abs( vlr_tot_int ).
            CLEAR vlr_tot_int.
          ENDIF.
        ENDIF.


*        IF vlr_tot_doc = 0.
*
*          "INTERNO
*          IF abs( vlr_tot_int ) GT 0
*         AND abs( vlr_tot_int ) LE vlr_toler. "Arredondar
*
*            IF vlr_tot_int GT 0.
*              IF lva_shkzg = 'S'. " EQ D
*                vlr_tot_int_s = abs( vlr_tot_int ) .
*                SUBTRACT vlr_tot_int_s FROM <lwa_zib_contabil>-dmbtr.
*                EXIT.
*              ENDIF.
*            ELSE.
*              IF lva_shkzg NE 'S'. " EQ C
*                vlr_tot_int_s = abs( vlr_tot_int ) .
*                SUBTRACT vlr_tot_int_s FROM <lwa_zib_contabil>-dmbtr.
*                EXIT.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*
*          "FORTE
*          IF abs( vlr_tot_for ) GT 0
*         AND abs( vlr_tot_for ) LE vlr_toler. "Arredondar
*            IF vlr_tot_for GT 0.
*              IF lva_shkzg = 'S'. " EQ D
*                SUBTRACT vlr_tot_for_s FROM <lwa_zib_contabil>-dmbe2.
*                EXIT.
*              ENDIF.
*            ELSE.
*              IF lva_shkzg NE 'S'. " EQ C
*                vlr_tot_for_s = abs( vlr_tot_for ).
*                SUBTRACT  vlr_tot_for_s   FROM <lwa_zib_contabil>-dmbe2.
*                EXIT.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*
*          CLEAR: vlr_tot_for_s, vlr_tot_int_s.
*
*        ENDIF.
      ENDLOOP.
      CLEAR: gwa_zib_contabil.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
