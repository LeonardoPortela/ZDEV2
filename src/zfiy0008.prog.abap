************************************************************************
* DOCUMENTATION BOX
************************************************************************
* PROGRAMA      : ZFIY0008
* MODULO SAP    : fi
* TITULO        : Libro iva venta y compras.
* TIPO          : R
* COPIA         : J_1AF217
************************************************************************
* MODIFICACIONES
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************
*& Report  J_1AF217                                                    *
************************************************************************
* CITI Report                                               NEW VERSION
* RG 781/2000 AFIP/CITI                                          7/2000
*
* The report is used to create a list/file based concerning the law RG *
* 781/2000.
* This report is a suplement of the report J_1AF117.
* The list/files consist of only one section which includes all related
* documents.
* VAT totals are printed at the end of the list. Further totals are no *
* longer required for this report. The amounts are printed in local    *
* currency.
* Program Description:
* This Report has been converted from the classic list into ALV lists.
*
* The main REUSE function modules used in this program are :
*       1.REUSE_FIELDCATALOG_MERGE
*       2.REUSE_ALV_LIST_DISPLAY
*
* Note - 731898 If for a document class and print character combination
*               in the customiztion if it is marked as Not Relevant for
*               official document numbering then the report should not
*               enter any error log. Msg 228379 2004
************************************************************************

REPORT  ZFIY0008   NO STANDARD PAGE HEADING  LINE-SIZE 130
                                             LINE-COUNT 65
                                             MESSAGE-ID 8a.

INCLUDE ZFIY0008_top.
INCLUDE ZFIY0008_scr.
INCLUDE ZFIY0008_form.
* ALV Changes starts
*-------------------------------------------------------------------------*
*           INITIALIZATION.
*-------------------------------------------------------------------------*
INITIALIZATION.

  gv_repid = sy-repid.

* for heirarchical list display
  gs_keyinfo-header01  = gc_serialno.
  gs_keyinfo-item01    = gc_serialno.
  gs_keyinfo-header02  = gc_bukrs.
  gs_keyinfo-item02    = gc_bukrs.

* Function module to get the DDIC text for Layout
  CALL FUNCTION 'PAK_GET_SHORTTEXT_DTEL'
    EXPORTING
      i_elem_key   = gc_layout
      i_language   = sy-langu
    IMPORTING
      e_short_text = gv_frame_tit.

  MOVE: gv_frame_tit TO vari_tit.

* Subroutine to initialize variant
  PERFORM variant_init_alv.
*-------------------------------------------------------------------------*
*           AT SELECTION-SCREEN
*-------------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.
* Subroutine to get the possible values of variant
  PERFORM reuse_alv_get_variant USING gv_repid
                             CHANGING p_varia.

*  Checking parameters
AT SELECTION-SCREEN.

  PERFORM variant_check_alv.                      "ALV Addition
  PERFORM check_selection_screen_1.

AT SELECTION-SCREEN ON: s_file1.
  PERFORM check_selection_screen_2.

*-------------------------------------------------------------------------*
*           START-OF-SELECTION.
*-------------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM read_company_data.

  SELECT * FROM j_1ataxid INTO TABLE xtaxid WHERE kalsm = t005-kalsm.

  n_vatam = p_vatam * ( - 1 ).

* Accounting document header
GET bkpf.

* Check if SD document reversed
  IF bkpf-awtyp = 'VBRK'.
    PERFORM check_if_canceled_sd.
  ENDIF.
* Check if MM document reversed
  IF bkpf-awtyp = 'RMRP'.
    PERFORM check_if_canceled_mm.
  ENDIF.

*  CHECK: BKPF-STBLG EQ SPACE.                "1052854

  CLEAR: doctab, hlp_vatam.

  MOVE-CORRESPONDING bkpf TO doctab.

  doctab-calye  = bkpf-budat+2(2).
  doctab-calmo  = bkpf-budat+4(2).

  doctab-brnch  = bkpf-xblnr(4).
*  DOCTAB-OFFNUM = BKPF-XBLNR+5(8).
  doctab-offnum = bkpf-xblnr.
  xprtchr       = bkpf-xblnr+4(1).
  doctab-stblg  = bkpf-stblg.                               "1061683

  PERFORM read_customs_data.

* Note 1061683 Start
  IF bkpf-stblg NE space.
    APPEND bkpf TO t_bkpf.

    SELECT SINGLE monat INTO rev_monat
    FROM bkpf
    WHERE bukrs = bkpf-bukrs
    AND belnr = bkpf-stblg
    AND gjahr = bkpf-stjah.
    IF sy-subrc EQ 0.
      IF bkpf-monat NE rev_monat.
        MOVE: 'X' TO doctab-xfile.
      ENDIF.
    ENDIF.
  ENDIF.
* Note 1061683 End

* Accounting document segment
GET bseg.
*  IF  bseg-mwskz IS INITIAL
*  AND bseg-bschl EQ '50'.
    APPEND bseg TO t_bseg.
*  ENDIF.


  IF bseg-mwskz NE '**' AND
     bseg-mwskz NE space.
    CHECK bseg-mwskz IN s_mwskz.
  ENDIF.

  IF bseg-koart EQ 'D'.
* deselect intercompany posting 'K' and 'D' lines
    IF bseg-ktosl NE 'BUV'.
      doctab-koart = bseg-koart.
      doctab-accno = bseg-kunnr.
    ENDIF.

    IF bseg-umskz NE space AND
       NOT bseg-umskz IN s_spcgld.
      reject_document = 'X'.
    ELSEIF bseg-umsks EQ 'A'.
*      if doctab-offnum is initial.                        NOTE 500034
      PERFORM read_request.
*      endif.                                              NOTE 500034
    ENDIF.
  ELSEIF bseg-koart EQ 'K'.
* deselect intercompany posting 'K' and 'D' lines
    IF bseg-ktosl NE 'BUV'.
      doctab-koart = bseg-koart.
      doctab-accno = bseg-lifnr.
    ENDIF.

    IF NOT custom-date IS INITIAL.
      doctab-bldat = custom-date.
    ENDIF.
    IF NOT custom-number IS INITIAL.
      doctab-offnum = custom-number.
    ENDIF.
    IF bseg-umskz NE space AND
       NOT bseg-umskz IN s_spcglk.
      reject_document = 'X'.
    ELSEIF bseg-umsks EQ 'A'.
*      if doctab-offnum is initial.                       NOTE 500034
      PERFORM read_request.
*      endif.                                             NOTE 500034
    ENDIF.
  ENDIF.

  IF bseg-xcpdd NE space.
    IF bseg-koart = 'K'.
      CALL FUNCTION 'VENDOR_READ'
        EXPORTING
          i_bukrs = space
          i_lifnr = bseg-lifnr
        IMPORTING
          e_lfa1  = lfa1.
    ELSE.
      CALL FUNCTION 'CUSTOMER_READ'
        EXPORTING
          i_bukrs = space
          i_kunnr = bseg-kunnr
        IMPORTING
          e_kna1  = kna1.
    ENDIF.
    IF ( bseg-koart = 'K' AND lfa1-xcpdk NE space ) OR
       ( bseg-koart = 'D' AND kna1-xcpdk NE space ).
      doctab-xcpdd = bseg-xcpdd.
    ENDIF.
  ENDIF.
* Tax data document segment
GET bset.

  CHECK bset-mwskz IN s_mwskz.
 APPEND bset TO t_bset.
  PERFORM fill_taxes.
*APPEND doctab.


GET bkpf LATE.
  IF NOT p_comp IS INITIAL.
    CHECK: doctab-accno    NE space,
           reject_document EQ space,
* Change 25.08.2011 - Valor mínimo AFIP - Sonda
*          hlp_vatam       NE 0.
           ( hlp_vatam     GE p_vatam OR
             hlp_vatam     LE n_vatam ).
* End change 25.08.2011 ......

    PERFORM read_document_type.

    PERFORM read_document_class.

    PERFORM read_official_doc_type.

    doctab-j_1aoftp = xotdet-j_1aoftp.

    doctab-vatam =  hlp_vatam.
*** Begin of Note 987617 ***
    doctab-waers = t001-waers.

*** End of Note 987617 ***

*---Compras
    APPEND doctab.

  ELSE.
*---Ventas

  ENDIF.



END-OF-SELECTION.

* Note 1046429 Start
*  CALL FUNCTION 'J_1A_EXIT_J_1AF217'
*    TABLES
*      doctab = doctab.
* Note 1046429 End

  PERFORM print_doctab.
