METHOD z_recalcula_icms .

  FIELD-SYMBOLS:
    <f_mwskz> TYPE any,
    <f_lifnr> TYPE any,
    <f_txjcd> TYPE any.

  TYPES: BEGIN OF ty_inloc,
           mwskz TYPE mwskz,
           base  TYPE char2.
  TYPES: END   OF ty_inloc.


  DATA: t_grupo    TYPE TABLE OF rgsb4,
        r_mwskz    TYPE RANGE OF mwskz,
        w_mwskz    LIKE LINE  OF r_mwskz,
        t_inloc    TYPE TABLE OF ty_inloc,
        w_inloc    TYPE ty_inloc,
        l_vlr_bx30 TYPE mty_taxamount,
        l_vlr_bx31 TYPE mty_taxamount,
        l_vlr_bx32 TYPE mty_taxamount,
        l_vlr_base TYPE mty_taxamount,
        l_vlr_amt  TYPE mty_taxamount,
        l_xuff     TYPE lfa1-regio,
        l_xufc     TYPE txjcd,
        l_xalf     TYPE j_1btxrate.

*-CS2020001318 - 07.04.2021 - JT - inicio
  FREE: l_xuff,
        l_xufc,
        t_grupo,
        r_mwskz,
        t_inloc.

*-------------------------------------------------------
* set result
*-------------------------------------------------------
  ev_result = iv_result.

*-------------------------------------------------------
* check transacoes
*-------------------------------------------------------
  CHECK sy-tcode = 'ME21N' OR
        sy-tcode = 'ME22N' OR
        sy-tcode = 'ME23N' OR
        sy-tcode = 'MIRO'  OR
        sy-tcode = 'MR8M'  OR
        sy-xprog = 'SAPLME11'  OR
        sy-xprog = 'SAPCNVE' . "ALRS


*-------------------------------------------------------
* set MAGGI_IVADUPLABASE
*-------------------------------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_IVADUPLABASE'
    TABLES
      set_values    = t_grupo
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT t_grupo INTO DATA(w_grupo).
    w_mwskz-sign     = 'I'.
    w_mwskz-option   = 'EQ'.
    w_mwskz-low      = w_grupo-from.
    APPEND w_mwskz  TO r_mwskz.
  ENDLOOP.

*-------------------------------------------------------
* set MAGGI_IVA_NLOC
*-------------------------------------------------------
  FREE: t_grupo.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class           = '0000'
      setnr           = 'MAGGI_IVA_NLOC'
      no_descriptions = abap_false
    TABLES
      set_values      = t_grupo
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT t_grupo INTO w_grupo.
    w_inloc-mwskz    = w_grupo-from.
    w_inloc-base     = w_grupo-title.
    APPEND w_inloc  TO t_inloc.
  ENDLOOP.

*-------------------------------------------------------
* assign MWSKZ
*-------------------------------------------------------
  ASSIGN ('(SAPLV61A)KOMK-MWSKZ') TO <f_mwskz>.

  CHECK sy-subrc = 0.

  CHECK <f_mwskz> IN r_mwskz[] AND r_mwskz[] IS NOT INITIAL.

*-------------------------------------------------------
* Recalculo DIFAL
*-------------------------------------------------------
  l_vlr_base = ms_tax_result-icms_cbas.
  l_vlr_amt  = ms_tax_result-icms_amt.

  READ TABLE t_inloc INTO w_inloc WITH KEY mwskz = <f_mwskz>.

  IF sy-subrc = 0 AND w_inloc-base = 'BD'.
    ASSIGN ('(SAPLV61A)KOMK-LIFNR') TO <f_lifnr>.
    CHECK sy-subrc = 0.

    ASSIGN ('(SAPLV61A)KOMK-TXJCD') TO <f_txjcd>.
    CHECK sy-subrc = 0.

    SELECT regio
      INTO l_xuff
      FROM lfa1
        UP TO 1 ROWS
     WHERE lifnr = <f_lifnr>.
    ENDSELECT.

    CHECK sy-subrc = 0.

    l_xufc = <f_txjcd>.
    l_xufc = l_xufc(2).

    SELECT rate
      INTO l_xalf
      FROM j_1btxic1
        UP TO 1 ROWS
     WHERE land1    = 'BR'
       AND shipfrom = l_xuff
       AND shipto   = l_xufc.
    ENDSELECT.

    CHECK sy-subrc = 0.

    l_vlr_base = ms_tax_result-icms_oth.
    l_vlr_amt  = l_vlr_base * ( l_xalf / 100 ).
  ENDIF.
  "
  IF sy-subrc = 0 AND w_inloc-base = 'BE'.
    l_vlr_base = l_vlr_base + ms_tax_result-icms_exc.

  ENDIF.


*-------------------------------------------------------
* BX30 = NF_ICMS_COMP_BASE
* BX31 = NF_ICMS_COMP_AMOUNT'.
* BX32 = NF_ICMS_COMP_RATE
*-------------------------------------------------------
  CASE iv_cond_code.

    WHEN 'NF_ICMS_COMP_BASE'.
      l_vlr_bx30 = ( l_vlr_base - l_vlr_amt )
                 / ( 1 - ms_tax_result-st_rate ).
      ev_result  = l_vlr_bx30.

    WHEN 'NF_ICMS_COMP_AMOUNT'.
      l_vlr_bx30 = ( l_vlr_base - l_vlr_amt )
                 / ( 1 - ms_tax_result-st_rate ).
      l_vlr_bx31 = ( l_vlr_bx30 * ms_tax_result-st_rate )
                 -   l_vlr_amt.
      ev_result  = l_vlr_bx31.

    WHEN 'NF_ICMS_COMP_RATE'.
      l_vlr_bx30 = ( l_vlr_base - l_vlr_amt )
                 / ( 1 - ms_tax_result-st_rate ).
      l_vlr_bx31 = ( l_vlr_bx30 * ms_tax_result-st_rate )
                 -   l_vlr_amt.
      l_vlr_bx32 = ( l_vlr_bx31 / l_vlr_bx30 ) * 100.
      ev_result  = l_vlr_bx32   * 10.

  ENDCASE.
*-CS2020001318 - 07.04.2021 - JT - fim

ENDMETHOD.
