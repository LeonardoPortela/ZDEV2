CLEAR: wa_socio.
TRY.
    DATA(cpf_socio)    = wa_parceiros-cpf.
    DATA(estado_civil) = wa_parceiros-ftext.
    DATA(partner2)     = wa_parceiros-partner2.
  CATCH cx_sy_itab_line_not_found.
ENDTRY.
BREAK PQUEVEDO.
IF ( cpf_socio IS NOT INITIAL ).

  IF ( partner2 IS NOT INITIAL ).
      SELECT SINGLE partner, name_last, marst
             FROM but000
             INTO @DATA(wa_but000_socio)
             WHERE partner EQ @partner2.

      wa_socio-nome = wa_but000_socio-name_last.

      SELECT partner, taxtype, taxnum
             FROM dfkkbptaxnum
             INTO TABLE @DATA(it_dfkkbptaxnum_socio)
             WHERE partner EQ @partner2
             AND   taxtype IN ('BR2', 'BR4').


     READ TABLE it_dfkkbptaxnum_socio INTO DATA(wa_dfkkbptaxnum_socio) WITH
                 KEY partner = partner2
                     taxtype = 'BR4'.
      IF sy-subrc EQ 0.
       wa_socio-rg = wa_dfkkbptaxnum_socio-taxnum.
      ENDIF.

      CLEAR wa_dfkkbptaxnum_socio.

      READ TABLE it_dfkkbptaxnum_socio INTO wa_dfkkbptaxnum_socio WITH
                 KEY partner = partner2
                     taxtype = 'BR2'.
      IF sy-subrc EQ 0.
       WRITE wa_dfkkbptaxnum_socio-taxnum
       USING EDIT MASK '___.___.___-__'  TO wa_socio-cpf.
      ENDIF.

     SELECT partner, addrnumber
             FROM but020
             INTO TABLE @DATA(it_but020_socio)
             WHERE partner EQ @partner2.

      IF NOT it_but020_socio[] IS INITIAL.
          SORT it_but020_socio BY partner.

          SELECT addrnumber, city1, city2, street, house_num1, region
                 FROM adrc
                 INTO TABLE @DATA(it_adrc_socio)
                 FOR ALL ENTRIES IN @it_but020_socio
                 WHERE addrnumber EQ @it_but020_socio-addrnumber.
      ENDIF.

      SORT it_adrc_socio BY addrnumber.

      READ TABLE it_but020_socio INTO DATA(wa_but020_socio) WITH KEY partner = partner2 BINARY SEARCH.
      IF sy-subrc EQ 0.
         READ TABLE it_adrc_socio INTO DATA(wa_adrc_socio) WITH KEY addrnumber = wa_but020_socio-addrnumber BINARY SEARCH.
         IF sy-subrc EQ 0.
            wa_socio-cidade    = wa_adrc_socio-city1.
            wa_socio-rua       = | { wa_adrc_socio-street } , { wa_adrc_socio-house_num1 }|.
            wa_socio-estado    = wa_adrc_socio-region.
         ENDIF.
      ENDIF.

  ELSE.
    "-> Busca código de cliente
      REPLACE ALL OCCURRENCES OF '.' IN cpf_socio WITH ''.
      REPLACE ALL OCCURRENCES OF '-' IN cpf_socio WITH ''.

      SELECT SINGLE
          kunnr,
          name1 AS nome,
          ort01 AS cidade,
          stcd2 AS cpf,
          stcd4 AS rg,
          stras AS rua,
          regio AS estado,
          katr1
        FROM kna1
        INTO @DATA(wa_dados_socio)
        WHERE stcd2 = @cpf_socio.

        IF ( sy-subrc = 0 ).
          wa_socio-nome = wa_dados_socio-nome.
          wa_socio-rg = wa_dados_socio-rg.
          wa_socio-cpf = wa_parceiros-cpf.
          wa_socio-rua = wa_dados_socio-rua.
          wa_socio-cidade = wa_dados_socio-cidade.
          wa_socio-estado = wa_dados_socio-estado.
        endif.
  ENDIF.

    wa_socio-stkzn = 'X'.

    wa_socio-situacao = COND #(
      WHEN estado_civil = '2' THEN 'casado(a)'
      WHEN estado_civil = '4' THEN 'divorciado(a)'
      WHEN estado_civil = '5' THEN 'separado(a)'
      WHEN estado_civil = '1' THEN 'solteiro(a)'
      WHEN estado_civil = '6' THEN 'união estável'
      WHEN estado_civil = '3' THEN 'viúvo(a)' ).

    IF wa_socio-situacao IS INITIAL.
    wa_socio-situacao = COND #(
      WHEN wa_dados_socio-katr1 = 'CS' THEN 'casado(a)'
      WHEN wa_dados_socio-katr1 = 'DV' THEN 'divorciado(a)'
      WHEN wa_dados_socio-katr1 = 'SP' THEN 'separado(a)'
      WHEN wa_dados_socio-katr1 = 'ST' THEN 'solteiro(a)'
      WHEN wa_dados_socio-katr1 = 'UE' THEN 'união estável'
      WHEN wa_dados_socio-katr1 = 'VV' THEN 'viúvo(a)' ).
    ENDIF.

    IF NOT partner2 IS INITIAL.

     SELECT partner1, partner2 UP TO 1 ROWS
            FROM but050
            INTO @DATA(wa_but050)
            WHERE partner1 EQ @partner2
            AND   reltyp   EQ 'BUR004'.
      ENDSELECT.

    IF NOT wa_but050 IS INITIAL.
      SELECT SINGLE partner, name_last, marst
             FROM but000
             INTO @DATA(wa_but000)
             WHERE partner EQ @wa_but050-partner2.

      SELECT partner, taxtype, taxnum
             FROM dfkkbptaxnum
             INTO TABLE @DATA(it_dfkkbptaxnum)
             WHERE partner EQ @wa_but050-partner2
             AND   taxtype IN ('BR2', 'BR4').

      wa_socio-s_c    = 'C'.
      wa_socio-nome_c = wa_but000-name_last.
      READ TABLE it_dfkkbptaxnum INTO DATA(wa_dfkkbptaxnum) WITH
                 KEY partner = wa_but050-partner2
                     taxtype = 'BR4'.
      IF sy-subrc EQ 0.
       wa_socio-rg_c = wa_dfkkbptaxnum-taxnum.
      ENDIF.

      READ TABLE it_dfkkbptaxnum INTO wa_dfkkbptaxnum WITH
                 KEY partner = wa_but050-partner2
                     taxtype = 'BR2'.
      IF sy-subrc EQ 0.
       "wa_socio-cpf_c = wa_dfkkbptaxnum-taxnum.
       WRITE wa_dfkkbptaxnum-taxnum
       USING EDIT MASK '___.___.___-__'  TO wa_socio-cpf_c.
      ENDIF.

     ENDIF.

     "Acrescentar na tabela pra sair na assinatura:
      LOOP AT it_zsds017[] ASSIGNING FIELD-SYMBOL(<w1_17>)
          WHERE namev_i = wa_parceiros-namev.
          SPLIT wa_but000-name_last AT ' '
          INTO <w1_17>-namev_p <w1_17>-name1_p.
      ENDLOOP.

      "Acrescentar na tabela pra sair na assinatura:
      LOOP AT it_zsds017_ass ASSIGNING FIELD-SYMBOL(<wa1_17>)
          WHERE namev_i = wa_parceiros-namev.
          SPLIT wa_but000-name_last AT ' '
          INTO <wa1_17>-namev_p <wa1_17>-name1_p.
      ENDLOOP.


    ELSE.


    "-> Busca conjuge
    SELECT SINGLE * FROM knvk
      INTO @DATA(wa_conjuge)
    WHERE kunnr = @wa_dados_socio-kunnr
      AND pafkt = '11'.

    IF ( wa_conjuge IS NOT INITIAL ).

      SELECT SINGLE * FROM adr3
        INTO @DATA(wa_adr3_conj)
        WHERE persnumber = @wa_conjuge-prsnr.

      wa_socio-s_c = 'C'.
      wa_socio-nome_c = |{ wa_conjuge-namev } { wa_conjuge-name1 }|.
      wa_socio-rg_c = wa_conjuge-telf1.
      wa_socio-cpf_c = wa_adr3_conj-fax_number.

      "Acrescentar na tabela pra sair na assinatura:
      LOOP AT it_zsds017[] ASSIGNING FIELD-SYMBOL(<w_17>)
          WHERE namev_i = wa_parceiros-namev.
        <w_17>-namev_p = wa_conjuge-namev.
        <w_17>-name1_p = wa_conjuge-name1.
      ENDLOOP.

      "Acrescentar na tabela pra sair na assinatura:
      LOOP AT it_zsds017_ass ASSIGNING FIELD-SYMBOL(<wa_17>)
          WHERE namev_i = wa_parceiros-namev.
        <wa_17>-namev_p = wa_conjuge-namev.
        <wa_17>-name1_p = wa_conjuge-name1.
      ENDLOOP.

    ENDIF.

    ENDIF.

    CLEAR: cpf_socio, wa_dados_socio,
           wa_conjuge, partner2, estado_civil.

ENDIF.
