FUNCTION z_transferir_imobilizado.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IMOBILIZADO) TYPE  ANLN1
*"     REFERENCE(EQUNR) TYPE  EQUNR
*"     REFERENCE(KOSTL) TYPE  KOSTL
*"     REFERENCE(SHTXT) TYPE  ORT01_ANLA
*"     REFERENCE(BUKRS) TYPE  BUKRS
*"     REFERENCE(GSBER) TYPE  GSBER
*"     REFERENCE(BUKRSW) TYPE  BUKRS
*"----------------------------------------------------------------------

*-US 142094-08-10-2024-#142094-RJF-inicio
  DATA:
    wa_realestate  TYPE bapi1022_feglg007,
    wa_realestatex TYPE bapi1022_feglg007x,
    wa_return      TYPE bapiret2.

  wa_realestate-tax_office = equnr.
  wa_realestate-tax_no = kostl.
  wa_realestate-municipality = shtxt.
  wa_realestate-lndreg_vol = bukrs.
  wa_realestate-lndreg_map_no  = gsber.
*  wa_realestate-lndreg_plot_no = gsber.

  wa_realestatex-tax_office = abap_on.
  wa_realestatex-tax_no = abap_on.
  wa_realestatex-municipality = abap_on.
  wa_realestatex-lndreg_vol = abap_on.
  wa_realestatex-lndreg_map_no  = abap_on.
*  wa_realestatex-lndreg_plot_no = abap_on.

  CALL FUNCTION 'BAPI_FIXEDASSET_CHANGE'
    EXPORTING
      companycode = bukrsw
      asset       = imobilizado
      subnumber   = '0000'
      realestate  = wa_realestate
      realestatex = wa_realestatex
    IMPORTING
      return      = wa_return.

  IF sy-subrc IS INITIAL.
    IF wa_return-type NE 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
  ENDIF.
*-US 142094-08-10-2024-#142094-RJF-fim




**  TYPES: BEGIN OF ty_msg,
**           tcode   TYPE bdc_tcode,
**           dyname  TYPE bdc_module,
**           dynumb  TYPE bdc_dynnr,
**           msgtyp  TYPE bdc_mart,
**           msgspra TYPE bdc_spras,
**           msgid   TYPE bdc_mid,
**           msgnr   TYPE bdc_mnr,
**           msgv1   TYPE bdc_vtext1,
**           msgv2   TYPE bdc_vtext1,
**           msgv3   TYPE bdc_vtext1,
**           msgv4   TYPE bdc_vtext1,
**           env     TYPE bdc_akt,
**           fldname TYPE fnam_____4,
**         END OF ty_msg.
**
**  DATA: it_msg     TYPE TABLE OF ty_msg,
**        wa_msg     TYPE ty_msg,
**        ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
**        t_messtab  TYPE TABLE OF bdcmsgcoll.
**
**  DATA: wa_bdcdata LIKE LINE OF ti_bdcdata,
**        wl_mode(1),
**        l_tcode    TYPE sy-tcode      VALUE 'AS02'.
**  CLEAR wa_bdcdata.
**
**
**
**  wa_bdcdata-program = 'SAPLAIST'.
**  wa_bdcdata-dynpro = '0100'.
**  wa_bdcdata-dynbegin = 'X'.
**  wa_bdcdata-fnam =  ''          .
**  wa_bdcdata-fval =  ''.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_CURSOR'.
**  wa_bdcdata-fval =	'ANLA-BUKRS'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_OKCODE'.
**  wa_bdcdata-fval =	'/00'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-ANLN1'.
**  wa_bdcdata-fval =	imobilizado.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-ANLN2'.
**  wa_bdcdata-fval =	'0'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-BUKRS'.
**  wa_bdcdata-fval =	bukrs.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**  wa_bdcdata-program = 'SAPLAIST'.
**  wa_bdcdata-dynpro = '1000'.
**  wa_bdcdata-dynbegin = 'X'.
**  wa_bdcdata-fnam = ''.
**  wa_bdcdata-fval =  ''.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''.
**  wa_bdcdata-dynpro = ''.
**  wa_bdcdata-dynbegin = ''.
**  wa_bdcdata-fnam = 'BDC_OKCODE'.
**  wa_bdcdata-fval = '=TAB05'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''.
**  wa_bdcdata-dynpro = ''.
**  wa_bdcdata-dynbegin = ''.
**  wa_bdcdata-fnam = 'BDC_SUBSCR'.
**  wa_bdcdata-fval = 'SAPLAIST 0099KOPF'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''.
**  wa_bdcdata-dynpro = ''.
**  wa_bdcdata-dynbegin = ''.
**  wa_bdcdata-fnam = 'BDC_SUBSCR'.
**  wa_bdcdata-fval = 'SAPLATAB 0100TABSTRIP'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''.
**  wa_bdcdata-dynpro = ''.
**  wa_bdcdata-dynbegin = ''.
**  wa_bdcdata-fnam = 'BDC_SUBSCR'.
**  wa_bdcdata-fval = 'SAPLATAB 0200SUBSC'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''.
**  wa_bdcdata-dynpro = ''.
**  wa_bdcdata-dynbegin = ''.
**  wa_bdcdata-fnam = 'BDC_SUBSCR'.
**  wa_bdcdata-fval = 'SAPLAIST 1140AREA1'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''.
**  wa_bdcdata-dynpro = ''.
**  wa_bdcdata-dynbegin = ''.
**  wa_bdcdata-fnam = 'BDC_SUBSCR'.
**  wa_bdcdata-fval = 'SAPLAIST 1141AREA2'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''.
**  wa_bdcdata-dynpro = ''.
**  wa_bdcdata-dynbegin = ''.
**  wa_bdcdata-fnam = 'BDC_SUBSCR'.
**  wa_bdcdata-fval = 'SAPLAIST 1142AREA3'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''.
**  wa_bdcdata-dynpro = ''.
**  wa_bdcdata-dynbegin = ''.
**  wa_bdcdata-fnam = 'BDC_SUBSCR'.
**  wa_bdcdata-fval = 'SAPLATAB 0300AREA4'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''.
**  wa_bdcdata-dynpro = ''.
**  wa_bdcdata-dynbegin = ''.
**  wa_bdcdata-fnam = 'BDC_SUBSCR'.
**  wa_bdcdata-fval = 'SAPLATAB 0300AREA5'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''.
**  wa_bdcdata-dynpro = ''.
**  wa_bdcdata-dynbegin = ''.
**  wa_bdcdata-fnam = 'BDC_SUBSCR'.
**  wa_bdcdata-fval = 'SAPLATAB 0300AREA6'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''.
**  wa_bdcdata-dynpro = ''.
**  wa_bdcdata-dynbegin = ''.
**  wa_bdcdata-fnam = 'BDC_SUBSCR'.
**  wa_bdcdata-fval = 'SAPLATAB 0300AREA7'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**  wa_bdcdata-program = 'SAPLAIST'.
**  wa_bdcdata-dynpro = '1000'.
**  wa_bdcdata-dynbegin = 'X'.
**  wa_bdcdata-fnam =  ''          .
**  wa_bdcdata-fval =  ''.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_OKCODE'.
**  wa_bdcdata-fval =	'/00'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_SUBSCR'.
**  wa_bdcdata-fval =	'SAPLAIST 0099KOPF'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_SUBSCR'.
**  wa_bdcdata-fval =	'SAPLATAB 0100TABSTRIP'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_SUBSCR'.
**  wa_bdcdata-fval =	'SAPLATAB 0202SUBSC'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_SUBSCR'.
**  wa_bdcdata-fval =	'SAPLAIST 1301AREA1'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_SUBSCR'.
**  wa_bdcdata-fval =	'SAPLAIST 1302AREA2'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_CURSOR'.
**  wa_bdcdata-fval =	'ANLA-FLURK'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-FIAMT'.
**  wa_bdcdata-fval =	equnr.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-EHWNR'.
**  wa_bdcdata-fval =	kostl.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-STADT'.
**  wa_bdcdata-fval =	shtxt.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-GRBND'.
**  wa_bdcdata-fval =	bukrs.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-FLURK'.
**  wa_bdcdata-fval =	gsber.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = 'SAPLAIST'.
**  wa_bdcdata-dynpro = '1000'.
**  wa_bdcdata-dynbegin = 'X'.
**  wa_bdcdata-fnam =  ''          .
**  wa_bdcdata-fval =  ''.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_OKCODE'.
**  wa_bdcdata-fval =	'=BUCH'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_SUBSCR'.
**  wa_bdcdata-fval =	'SAPLAIST 0099KOPF'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_SUBSCR'.
**  wa_bdcdata-fval =  'SAPLATAB 0100TABSTRIP'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_SUBSCR'.
**  wa_bdcdata-fval =	'SAPLATAB 0202SUBSC'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_SUBSCR'.
**  wa_bdcdata-fval =	'SAPLAIST 1301AREA1'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_CURSOR'.
**  wa_bdcdata-fval =	'ANLA-VMGLI'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'BDC_SUBSCR'.
**  wa_bdcdata-fval =	'SAPLAIST 1302AREA2'.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-FIAMT'.
**  wa_bdcdata-fval =	equnr.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-EHWNR'.
**  wa_bdcdata-fval =	kostl.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-STADT'.
**  wa_bdcdata-fval =	shtxt.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-GRBND'.
**  wa_bdcdata-fval =	bukrs.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**
**  wa_bdcdata-program = ''        .
**  wa_bdcdata-dynpro = ''    .
**  wa_bdcdata-dynbegin = '' .
**  wa_bdcdata-fnam =  'ANLA-FLURK'.
**  wa_bdcdata-fval =	gsber.
**
**  APPEND wa_bdcdata TO ti_bdcdata.
**  CLEAR wa_bdcdata.
**
**  REFRESH it_msg.
**
**  wl_mode = 'N'.
**
**  CALL TRANSACTION l_tcode USING ti_bdcdata
**        MODE wl_mode
**        MESSAGES INTO it_msg.

ENDFUNCTION.
