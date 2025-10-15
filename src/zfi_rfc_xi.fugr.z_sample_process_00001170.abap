FUNCTION z_sample_process_00001170 .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_COMPANY) LIKE  BKPF-BUKRS OPTIONAL
*"     VALUE(I_RANGE) LIKE  T003-NUMKR OPTIONAL
*"     VALUE(I_YEAR) LIKE  BKPF-GJAHR OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_NO_BUFFER) LIKE  BOOLE-BOOLE
*"     VALUE(E_RANGE) LIKE  T003-NUMKR
*"----------------------------------------------------------------------
  CHECK sy-mandt = '160'  OR
        sy-mandt = '300'.

  DATA: vl_augbl       LIKE bseg-augbl,
        vl_bukrs       LIKE bseg-bukrs,
        vl_belnr       LIKE bseg-belnr,
        vl_gjahr       LIKE bseg-gjahr,
        vl_blart       LIKE bkpf-blart,
        vl_tcode       LIKE bkpf-tcode,
        vl_interval    LIKE nriv,
        vl_field       TYPE string.

**** Eduardo Ruttkowski Tavares - 26.01.2010 >>> INI
*  IF SY-TCODE = 'MR22'.
*
*    DATA: WA_BSEG TYPE BSEG.
*
*    FIELD-SYMBOLS: <BSEGT> TYPE ANY TABLE,
*                   <BSEGH> TYPE ANY.
*
*    ASSIGN ('(SAPLFACI)xbseg[]') TO <BSEGT>.
*    ASSIGN ('(SAPLFACI)xbseg')   TO <BSEGH>.
*
*    LOOP AT <BSEGT> INTO WA_BSEG.
*      IF WA_BSEG = <BSEGH>.
*        BREAK-POINT.
*      ENDIF.
*    ENDLOOP.
*
*
*  ENDIF.
**** Eduardo Ruttkowski Tavares - 26.01.2010 >>> INI

  CHECK ( 'FB01 FB05 FBZ1 FBZ2 FBZ4 FBRA FB1K FB1D FB08' CS sy-tcode ).

  CALL FUNCTION 'NUMBER_GET_INFO'
    EXPORTING
      nr_range_nr        = i_range
      object             = 'RF_BELEG'
      subobject          = i_company
      toyear             = i_year
    IMPORTING
      interval           = vl_interval
    EXCEPTIONS
      interval_not_found = 1
      object_not_found   = 2
      OTHERS             = 3.

  CHECK ( sy-subrc EQ 0 ).

  vl_augbl = vl_interval-nrlevel + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vl_augbl
    IMPORTING
      output = vl_augbl.

  CLEAR vg_testeabap_debug.

  IF ( sy-tcode EQ 'FB08' ).

    FIELD-SYMBOLS: <wa_bkpf> TYPE bkpf.
    vl_field = '(SAPMF05A)BKPF'.
    ASSIGN (vl_field) TO <wa_bkpf>.

    vl_belnr = <wa_bkpf>-stblg.
    vl_bukrs = <wa_bkpf>-bukrs.
    vl_gjahr = <wa_bkpf>-stjah.

    SELECT SINGLE t1~blart t1~tcode
       FROM bkpf AS t1
       INTO (vl_blart, vl_tcode)
      WHERE ( bukrs EQ vl_bukrs )
        AND ( belnr EQ vl_belnr )
        AND ( gjahr EQ vl_gjahr ).

    CHECK ( sy-subrc EQ 0 ).

*>  Para títulos manuais (FB01) e gerados p/SIGAM via interface(FB05)
    IF ( 'FB01 FB05' CS vl_tcode ).
*> Filtra somente os tipos de documento para retorno
*> através de interface outbound (SIGAM/FOLHA) (FB08)
      SELECT SINGLE blart FROM zfit0005
                          INTO vl_blart
                         WHERE ( blart EQ vl_blart ).
    ELSE.
*> No caso de docto de compensação não é necessário (FBRA)
      sy-subrc = 0.
    ENDIF.

    CHECK ( sy-subrc EQ 0 ).

* As informações do lançamento são gravadas na tabela Z
* e posteriormente pelo report de start são enviadas para o SIGAM
    wa_dadosrfc-bukrs   = vl_bukrs.
    wa_dadosrfc-belnr   = vl_belnr.
    wa_dadosrfc-gjahr   = vl_gjahr.
    wa_dadosrfc-bukrs_e = i_company.
    wa_dadosrfc-belnr_e = vl_augbl.
    wa_dadosrfc-gjahr_e = i_year.
    wa_dadosrfc-tcode_e = vl_tcode.
  ELSE.
* As informações do lançamento são gravadas na tabela Z
* e posteriormente pelo report de start são enviadas para o SIGAM
    wa_dadosrfc-bukrs = i_company.
    wa_dadosrfc-belnr = vl_augbl.
    wa_dadosrfc-gjahr = i_year.
    wa_dadosrfc-tcode = sy-tcode.
  ENDIF.
  wa_dadosrfc-data = sy-datum.
  wa_dadosrfc-hora = sy-uzeit.
  INSERT into zfit0006 values wa_dadosrfc.
ENDFUNCTION.
