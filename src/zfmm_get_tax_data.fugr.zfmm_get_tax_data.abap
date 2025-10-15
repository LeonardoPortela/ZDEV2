FUNCTION ZFMM_GET_TAX_DATA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EKKO) TYPE  EKKO
*"     REFERENCE(I_EKPO) TYPE  EKPO
*"     REFERENCE(I_LFA1) TYPE  LFA1
*"  EXPORTING
*"     REFERENCE(E_TAXCOM) TYPE  TAXCOM
*"  TABLES
*"      T_KOMV STRUCTURE  KOMV
*"----------------------------------------------------------------------

*Local Data declaration
  DATA: my_taxcom   TYPE j_1b_taxcom,
        l_taxcom    TYPE taxcom,
        l_wa_t001   TYPE t001,
        l_wa_komv   TYPE komv,
        l_gv_amount TYPE netwr,
        l_unitprice TYPE netwr,
        l_ipiamount TYPE netwr,
        l_total_ipi TYPE netwr,
        l_total     TYPE netwr,
        l_taxamount TYPE netwr.

  SELECT SINGLE * FROM t001
                  INTO l_wa_t001
                  WHERE bukrs EQ i_ekko-bukrs.

* Fill tax fields relevant for PO
  my_taxcom-txreg_sf = i_lfa1-txjcd.
  my_taxcom-txreg_st = i_ekpo-txjcd.
  my_taxcom-taxbs = i_lfa1-taxbs.
  my_taxcom-ipisp = i_lfa1-ipisp.
  my_taxcom-brsch = i_lfa1-brsch.
  my_taxcom-mtuse = i_ekpo-j_1bmatuse.
  my_taxcom-mtorg = i_ekpo-j_1bmatorg.
  my_taxcom-ownpr = i_ekpo-j_1bownpro.
  my_taxcom-steuc = i_ekpo-j_1bnbm.
  my_taxcom-matkl = i_ekpo-matkl.
  my_taxcom-vrkme = i_ekpo-meins.
  my_taxcom-mgame = i_ekpo-menge.

* ISS Calculation with 2-level tax jurisdiction code
* To get the ISS Tax Condtions, need to pass the Following Parameters
  my_taxcom-loc_se = i_lfa1-txjcd.
  my_taxcom-loc_sr = i_lfa1-txjcd.
* if you don't pass the above parameters and if your PO has ISS Tax conditions,
* you don't see them in the Result


* Location of service provider = Tax Jur. Code of
* vendor:
  my_taxcom-loc_pr = i_lfa1-txjcd.
* Location of service = Tax Jur. Code of delivery address

*Populate fields based on country
  CALL FUNCTION 'J_1B_SAVE_TAX_FIELDS'
    EXPORTING
      i_taxcom = my_taxcom.

  CLEAR l_taxcom.
  l_taxcom-bukrs = i_ekpo-bukrs.
  l_taxcom-budat = i_ekko-bedat.
  l_taxcom-waers = i_ekko-waers.
  l_taxcom-kposn = i_ekpo-ebelp.
  l_taxcom-mwskz = i_ekpo-mwskz.
  l_taxcom-txjcd = i_ekpo-txjcd.
  l_taxcom-shkzg = 'H'.
  l_taxcom-xmwst = 'X'.
  IF i_ekko-bstyp EQ 'F'.
    l_taxcom-wrbtr = i_ekpo-netwr.
  ELSE.
    l_taxcom-wrbtr = i_ekpo-zwert.
  ENDIF.
  l_taxcom-lifnr = i_ekko-lifnr.
  l_taxcom-land1 = i_ekko-lands.
  l_taxcom-ekorg = i_ekko-ekorg.
  l_taxcom-hwaer = l_wa_t001-waers.
  l_taxcom-llief = i_ekko-llief.
  l_taxcom-bldat = i_ekko-bedat.
  l_taxcom-matnr = i_ekpo-matnr.
  l_taxcom-werks = i_ekpo-werks.
  l_taxcom-bwtar = i_ekpo-bwtar.
  l_taxcom-matkl = i_ekpo-matkl.
  l_taxcom-meins = i_ekpo-meins.
  IF i_ekko-bstyp EQ 'F'.
    l_taxcom-mglme = i_ekpo-menge.
  ELSE.
    IF i_ekko-bstyp EQ 'K' AND i_ekpo-abmng GT 0.
      l_taxcom-mglme = i_ekpo-abmng.
    ELSE.
      l_taxcom-mglme = i_ekpo-ktmng.
    ENDIF.
  ENDIF.
  IF l_taxcom-mglme EQ 0.
    l_taxcom-mglme = 1000.
  ENDIF.
  l_taxcom-mtart = i_ekpo-mtart.

*Calculation of TAX

  CALL FUNCTION 'CALCULATE_TAX_ITEM'
    EXPORTING
      i_taxcom            = l_taxcom
    IMPORTING
      e_taxcom            = e_taxcom
    TABLES
      t_xkomv             = t_komv
    EXCEPTIONS
      mwskz_not_defined   = 1
      mwskz_not_found     = 2
      mwskz_not_valid     = 3
      steuerbetrag_falsch = 4
      country_not_found   = 5
      txjcd_not_valid     = 6
      OTHERS              = 7.

ENDFUNCTION.
