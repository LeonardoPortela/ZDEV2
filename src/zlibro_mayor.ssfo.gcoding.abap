DATA: wl_budat LIKE LINE OF rg_budat,
      wl_gjahr TYPE bkpf-gjahr.

wl_gjahr = budat_low(4).
*BREAK-POINT.
IF budat_low IS NOT INITIAL.
  IF budat_high IS INITIAL.
    wl_budat-sign = 'I'.
    wl_budat-option = 'EQ'.
    wl_budat-low  = budat_low.
    APPEND: wl_budat TO rg_budat.
    CLEAR: wl_budat.
  ELSE.
    wl_budat-sign = 'I'.
    wl_budat-option = 'BT'.
    wl_budat-low  = budat_low.
    wl_budat-high = budat_high.
    APPEND wl_budat TO rg_budat.
    CLEAR: wl_budat.
  ENDIF.
ENDIF.

SELECT bukrs gjahr budat belnr
  FROM bkpf
    INTO TABLE tg_bkpf
      WHERE bukrs EQ bukrs
        AND budat IN rg_budat
        AND gjahr EQ wl_gjahr.

IF sy-subrc IS INITIAL.
  SELECT ryear docnr rbukrs rldnr rcntr racct hsl drcrk buzei
    FROM faglflexa
      INTO TABLE tg_faglflexa
      FOR ALL ENTRIES IN tg_bkpf
        WHERE ryear  EQ tg_bkpf-gjahr
          AND docnr  EQ tg_bkpf-belnr
          AND rbukrs EQ tg_bkpf-bukrs
          AND rldnr  EQ '0L'.

  IF sy-subrc IS INITIAL.
    SELECT ktopl saknr txt50
      FROM skat
        INTO TABLE tg_skat
        FOR ALL ENTRIES IN tg_faglflexa
          WHERE saknr EQ tg_faglflexa-racct
            AND ktopl EQ '0050'
            AND spras EQ 'S'. "'ES'.

    SELECT ktopl saknr ktoks
      FROM ska1
        INTO TABLE tg_ska1
        FOR ALL ENTRIES IN tg_faglflexa
          WHERE saknr EQ tg_faglflexa-racct
            AND ktopl EQ '0050'.

    IF sy-subrc IS INITIAL.
      SELECT ktopl ktoks
        FROM t077s
          INTO TABLE tg_t077s
           FOR ALL ENTRIES IN tg_ska1
             WHERE ktoks EQ tg_ska1-ktoks
               AND ktopl EQ '0050'.
*               AND spras EQ sy-langu. "'ES'.

      IF sy-subrc IS INITIAL.
        SELECT ktopl ktoks txt30
          FROM t077z
            INTO TABLE tg_t077z
            FOR ALL ENTRIES IN tg_t077s
              WHERE ktoks EQ tg_t077s-ktoks
                AND ktopl EQ '0050'
                 AND spras EQ 'S'. "'ES'.
      ENDIF.
    ENDIF.
  ENDIF.
*  BREAK-POINT.
  SELECT bukrs belnr gjahr buzei sgtxt
    FROM bsis
      INTO TABLE tg_bsis
      FOR ALL ENTRIES IN tg_bkpf
        WHERE bukrs EQ bukrs
          AND belnr EQ tg_bkpf-belnr
          AND gjahr EQ tg_bkpf-gjahr.

  IF sy-subrc IS INITIAL.
    SELECT bukrs belnr gjahr buzei sgtxt
      FROM bsas
       INTO TABLE tg_bsas
       FOR ALL ENTRIES IN tg_bsis
         WHERE bukrs EQ tg_bsis-bukrs
           AND belnr EQ tg_bsis-belnr
           AND gjahr EQ tg_bsis-gjahr.
  ENDIF.
ENDIF.

SELECT SINGLE bukrs butxt adrnr
  FROM t001
    INTO wg_t001
      WHERE bukrs EQ bukrs.

CALL FUNCTION 'ISSR_MD_ADDRESS_COMPANY_GET'
  EXPORTING
    i_adrnr  = wg_t001-adrnr
  IMPORTING
    e_s_sadr = wg_endereco.

CALL FUNCTION 'NUMBER_GET_NEXT'
  EXPORTING
    nr_range_nr             = '01'
    object                  = 'ZLIB_MAY'
  IMPORTING
    number                  = p_number
  EXCEPTIONS
    interval_not_found      = 1
    number_range_not_intern = 2
    object_not_found        = 3
    quantity_is_0           = 4
    quantity_is_not_1       = 5
    interval_overflow       = 6
    buffer_overflow         = 7
    OTHERS                  = 8.

LOOP AT tg_faglflexa INTO wg_faglflexa.
  READ TABLE tg_bkpf INTO wg_bkpf
    WITH KEY gjahr = wg_faglflexa-ryear
             belnr = wg_faglflexa-docnr.

  IF sy-subrc IS INITIAL.
    READ TABLE tg_bsis INTO wg_bsis
      WITH KEY "BUKRS = WG_BKPF-BUKRS
               belnr = wg_bkpf-belnr
               gjahr = wg_bkpf-gjahr
               buzei = wg_faglflexa-buzei.


    IF sy-subrc IS NOT INITIAL.
      READ TABLE tg_bsas INTO wg_bsas
        WITH KEY "BUKRS = WG_BKPF-BUKRS
                 belnr = wg_bkpf-belnr
                 gjahr = wg_bkpf-gjahr
                 buzei = wg_faglflexa-buzei.
      wg_saida-texto = wg_bsas-sgtxt.
    ENDIF.

    wg_saida-texto = wg_bsas-sgtxt.

    READ TABLE tg_skat INTO wg_skat
      WITH KEY saknr  = wg_faglflexa-racct
               ktopl  = '0050'.

    READ TABLE tg_ska1 INTO wg_ska1
      WITH KEY saknr  = wg_faglflexa-racct
               ktopl  = '0050'.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_t077s INTO wg_t077s
        WITH KEY  ktopl	=	'0050'
                  ktoks	=	wg_ska1-ktoks.

      IF sy-subrc IS INITIAL.
        READ TABLE tg_t077z INTO wg_t077z
          WITH KEY ktopl = '0050'
                   ktoks = wg_t077s-ktoks.

        wg_saida-concepto = wg_t077z-txt30.
      ENDIF.

    ENDIF.

    wg_saida-n_cta   = wg_faglflexa-racct.
    wg_saida-nombre  = wg_skat-txt50.
    wg_saida-fecha   = wg_bkpf-budat.
    wg_saida-n_asto  = '000001'.
    wg_saida-n_compr = '00000000'.
    wg_saida-tc      = 'AP'.

    IF wg_faglflexa-drcrk EQ 'S'.
      wg_saida-debitos = wg_faglflexa-hsl.
    ELSEIF wg_faglflexa-drcrk EQ 'H'.
      wg_saida-creditos = wg_faglflexa-hsl.
    ENDIF.

    APPEND wg_saida TO tg_saida.
    CLEAR: wg_bkpf, wg_skat, wg_ska1, wg_t077s, wg_t077z,
           wg_bsas, wg_bsis, wg_saida.
  ENDIF.
ENDLOOP.

tg_aux[] = tg_saida[].

SORT: tg_aux   BY n_cta,
      tg_saida BY n_cta.

DELETE ADJACENT DUPLICATES FROM tg_aux
                      COMPARING n_cta.
