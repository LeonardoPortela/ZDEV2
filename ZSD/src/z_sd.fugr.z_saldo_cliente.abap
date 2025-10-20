FUNCTION z_saldo_cliente.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(V_KUNNR) LIKE  LIKP-KUNNR
*"     REFERENCE(V_VKORG) LIKE  LIKP-VKORG
*"     REFERENCE(V_GJAHR) LIKE  KNC1-GJAHR
*"     REFERENCE(V_VGBEL) LIKE  LIPS-VGBEL OPTIONAL
*"     REFERENCE(V_LFIMG) LIKE  LIPS-LFIMG OPTIONAL
*"  EXPORTING
*"     REFERENCE(V_SALDO) TYPE  VBAK-NETWR
*"     REFERENCE(V_TOTAL) TYPE  NETWR_AK
*"     REFERENCE(V_SALDO_RESIDUAL) TYPE  NETWR_AK
*"     REFERENCE(V_LIMITE) TYPE  KLIMG
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_tcurr,
           kurst TYPE tcurr-kurst,
           fcurr TYPE tcurr-fcurr,
           tcurr TYPE tcurr-tcurr,
           gdatu TYPE tcurr-gdatu,
           ukurs TYPE tcurr-ukurs,
         END OF ty_tcurr.

  DATA: wa_knc1   TYPE knc1,
        it_knc1   TYPE TABLE OF knc1,
        wa_knc3   TYPE knc3,
        it_knc3   TYPE TABLE OF knc3,
        "wa_knka   TYPE knka, "#EC CI_USAGE_OK[2227014]
        wa_ukmbp_cms_sgm   TYPE ukmbp_cms_sgm,
        wa_UKMCRED_SGM0C   TYPE UKMCRED_SGM0C,
        wa_kna1   TYPE kna1,
        wa_vbak   TYPE vbak,
        wa_konv   TYPE konv,
        v_konzs   TYPE kna1-konzs,
        v_saldo_c TYPE knc1-umsav,
        v_saldo_a TYPE knc1-umsav.

  DATA vfator TYPE i.
  DATA: lva_moeda_limite TYPE UKMCRED_SGM0C-currency.

  RANGES: r_gdatu FOR tcurr-gdatu,
          r_fcurr FOR tcurr-fcurr.

  DATA: wl_date_aux  TYPE datum,
        wl_input(10),
        wa_tcurr     TYPE ty_tcurr,
        lc_fator     TYPE i,
        t_tcurr_a    TYPE TABLE OF ty_tcurr,
        it_bsid      TYPE TABLE OF bsid WITH HEADER LINE.

  MOVE 'IBT' TO r_gdatu.

  wl_date_aux = sy-datum - 30.

  WRITE wl_date_aux TO wl_input.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = wl_input
    IMPORTING
      output = r_gdatu-high.

  wl_date_aux = sy-datum.

  WRITE wl_date_aux TO wl_input.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = wl_input
    IMPORTING
      output = r_gdatu-low.

  APPEND r_gdatu.

  SELECT kurst fcurr tcurr gdatu ukurs
     FROM tcurr
     INTO TABLE t_tcurr_a
    WHERE kurst EQ 'B'
      AND fcurr EQ 'USD'
      AND tcurr EQ 'BRL'
      AND gdatu IN r_gdatu.

  SORT t_tcurr_a BY gdatu ASCENDING fcurr ASCENDING.

  CHECK t_tcurr_a[] IS NOT INITIAL.

  CLEAR: v_saldo_c, v_saldo_a,  v_konzs .

  SELECT SINGLE *
  FROM  kna1
  INTO wa_kna1
  WHERE kunnr = v_kunnr.

  v_konzs =  wa_kna1-konzs.

  CLEAR:  wa_ukmbp_cms_sgm, wa_konv, lva_moeda_limite.

  IF v_konzs IS NOT INITIAL. "grupo economico
    SELECT SINGLE * "#EC CI_USAGE_OK[2227014]
     FROM  ukmbp_cms_sgm as a
      INNER JOIN kna1
      ON kna1~konzs = v_konzs
      inner join IBUPACUSTOMER as b
      on kna1~kunnr = b~customer
     INTO  CORRESPONDING FIELDS OF wa_ukmbp_cms_sgm
     WHERE a~partner EQ b~businesspartner
     AND   a~credit_limit GT 0.
    "
    SELECT  *
      FROM kna1
      INTO TABLE @DATA(it_kna1)
      WHERE konzs = @v_konzs.
  ELSE.
    SELECT SINGLE * "#EC CI_USAGE_OK[2227014]
    FROM  ukmbp_cms_sgm
      inner join IBUPACUSTOMER as b
      on  b~customer = v_kunnr
    INTO CORRESPONDING FIELDS OF wa_ukmbp_cms_sgm
    WHERE partner = b~businesspartner.
    "
    SELECT  *
     FROM kna1
     INTO TABLE it_kna1
     WHERE kunnr = v_kunnr.
  ENDIF.

  if wa_ukmbp_cms_sgm-credit_sgmnt is NOT INITIAL.
    SELECT SINGLE currency
      from UKMCRED_SGM0C INTO lva_moeda_limite
      where credit_sgmnt = wa_ukmbp_cms_sgm-credit_sgmnt.
  endif.

  "
  v_limite = wa_ukmbp_cms_sgm-credit_limit.
  "
  CLEAR: v_saldo_a, v_saldo_c.
  "
  LOOP AT it_kna1 INTO wa_kna1.
    REFRESH it_bsid.
    CALL FUNCTION 'Z_FI_GL_PART_ABERTO'
      EXPORTING
        i_company            = v_vkorg
        i_forne              = abap_false
        i_cliente            = abap_true
        i_parid              = wa_kna1-kunnr
*       I_DATA_VENC_INI      = P_INICIAL
*       I_DATA_VENC_FINAL    = P_FINAL
        i_nao_razao_especial = abap_false
      TABLES
        it_bsxd              = it_bsid.

    LOOP AT it_bsid.
      IF it_bsid-shkzg EQ 'H'.
        lc_fator = -1.
      ELSE.
        lc_fator = 1.
      ENDIF.
      "
      IF lva_moeda_limite = 'USD'.
        v_saldo_a   = it_bsid-dmbe2 * lc_fator.
      ELSE.
        v_saldo_a   = it_bsid-dmbtr * lc_fator.
      ENDIF.
      v_saldo_c = v_saldo_c + v_saldo_a.
    ENDLOOP.
  ENDLOOP.

  CLEAR v_saldo_a.

  CLEAR v_total.

  IF v_vgbel IS NOT INITIAL.
    vfator = 1.
    SELECT SINGLE *
      FROM vbak
      INTO wa_vbak
      WHERE vbeln = v_vgbel.

    IF sy-subrc = 0.

*--> S4 Migration - 07/07/2023 - JP
*      SELECT SINGLE *
*        FROM KONV
*        INTO WA_KONV
*      WHERE KNUMV  = WA_VBAK-KNUMV
*      AND   KSCHL  = 'PR00'.

        SELECT SINGLE *
          FROM v_konv
          INTO @DATA(ls_konv)
        WHERE knumv  = @wa_vbak-knumv
          AND kschl  = 'PR00'.

        MOVE-CORRESPONDING ls_konv TO wa_konv.

*--> S4 Migration - 07/07/2023 - JP

        IF wa_konv-kmein = 'TO' or wa_konv-kmein = 'M3'.
          vfator = 1000.
        ENDIF.
      ENDIF.

      v_total = ( v_lfimg / vfator ) * wa_konv-kbetr.

      IF lva_moeda_limite = 'USD'.
        READ TABLE  t_tcurr_a INTO wa_tcurr INDEX 1.
        v_total = v_total / wa_tcurr-ukurs.
      ENDIF.


    ENDIF.

    v_saldo = wa_ukmbp_cms_sgm-credit_limit - ( v_saldo_c + v_saldo_a  ).
    v_saldo_residual =  wa_ukmbp_cms_sgm-credit_limit - ( v_saldo_c + v_saldo_a  ) -  v_total.

  ENDFUNCTION.
