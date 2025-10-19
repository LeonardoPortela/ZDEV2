*----------------------------------------------------------------------*
*              Print of an order confirmation by SAPscript
*----------------------------------------------------------------------*
REPORT zrvador01 LINE-COUNT 100 MESSAGE-ID vn.

*---> S4 Migration - 12/07/2023 - PS
INCLUDE: /mignow/i_constants_sd.
*<--- S4 Migration - 12/07/2023 - PS

TABLES: komk,                          "Communicationarea for conditions
        komp,                          "Communicationarea for conditions
        komvd,                         "Communicationarea for conditions
        vbco3,                         "Communicationarea for view
        vbdka,                         "Headerview
        vbdpa,                         "Itemview
        vbdpau,                        "Subitemnumbers
        conf_out,                      "Configuration data
        sadr,                          "Addresses
        tvag,                          "Reason for rejection
        vedka,                         "Servicecontract head data
        vedpa,                         "Servicecontract position data
        vedkn,                         "Servicecontract head notice data
        vedpn,                         "Servicecontract pos. notice data
        riserls,                       "Serialnumbers
        komser,                        "Serialnumbers for print
        tvbur,                         "Sales office
        tvko,                          "Sales organisation
        adrs,                          "Communicationarea for Address
        fpltdr,                        "billing schedules
        wtad_addis_in_so_print,        "additional
        wtad_buying_print_extra_text.  "texts belonging to additional
INCLUDE zrvadtabl.
*INCLUDE rvadtabl.
INCLUDE zrvdirekt.
*INCLUDE rvdirekt.
INCLUDE zvedadata.
*INCLUDE vedadata.

*ENHANCEMENT-POINT RVADOR01_01 SPOTS ES_RVADOR01 STATIC.
* data for access to central address maintenance
INCLUDE zsdzavdat.
*INCLUDE sdzavdat.

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TYPE-POOLS: addi.

DATA price_print_mode(1) TYPE c.       "Print-mode
DATA: retcode   LIKE sy-subrc.         "Returncode
DATA: repeat(1) TYPE c.
DATA: xscreen(1) TYPE c.               "Output on printer or screen
DATA: BEGIN OF steu,                   "Controldata for output
        vdkex(1) TYPE c,
        vdpex(1) TYPE c,
        kbkex(1) TYPE c,
        kbpex(1) TYPE c,
      END OF steu.


DATA: BEGIN OF tvbdpa OCCURS 0.        "Internal table for items
        INCLUDE STRUCTURE vbdpa.
DATA: END OF tvbdpa.

DATA: BEGIN OF tkomv OCCURS 50.
        INCLUDE STRUCTURE komv.
DATA: END OF tkomv.

DATA: BEGIN OF tkomvd OCCURS 50.
        INCLUDE STRUCTURE komvd.
DATA: END OF tkomvd.

DATA: BEGIN OF tvbdpau OCCURS 5.
        INCLUDE STRUCTURE vbdpau.
DATA: END   OF tvbdpau.

DATA: BEGIN OF tkomcon OCCURS 50.
        INCLUDE STRUCTURE conf_out.
DATA: END   OF tkomcon.

DATA: BEGIN OF tkomservh OCCURS 1.
        INCLUDE STRUCTURE vedka.
DATA: END   OF tkomservh.

DATA: BEGIN OF tkomservp OCCURS 5.
        INCLUDE STRUCTURE vedpa.
DATA: END   OF tkomservp.

DATA: BEGIN OF tkomservhn OCCURS 5.
        INCLUDE STRUCTURE vedkn.
DATA: END   OF tkomservhn.

DATA: BEGIN OF tkomservpn OCCURS 5.
        INCLUDE STRUCTURE vedpn.
DATA: END   OF tkomservpn.

DATA: BEGIN OF tkomser OCCURS 5.
        INCLUDE STRUCTURE riserls.
DATA: END   OF tkomser.

DATA: BEGIN OF tkomser_print OCCURS 5.
        INCLUDE STRUCTURE komser.
DATA: END   OF tkomser_print.

DATA: BEGIN OF tfpltdr OCCURS 5.
        INCLUDE STRUCTURE fpltdr.
DATA: END   OF tfpltdr.

DATA: taddi_print TYPE addi_so_print_itab WITH HEADER LINE.

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

DATA: pr_kappl(01)   TYPE c VALUE 'V'. "Application for pricing

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*O/p Control - SD email enhancement

DATA: gv_address         TYPE adsmtp.
DATA: gv_pdf             TYPE fpcontent.
DATA: go_badi_sls_email  TYPE REF TO badi_sd_sls_email.
DATA: gv_email_enh_flag  TYPE boolean.
gv_email_enh_flag = abap_false.
DATA: otf_data TYPE TABLE OF itcoo.

*End o/p control
FORM entry USING return_code TYPE i
                 us_screen TYPE c.

  CLEAR retcode.
  xscreen = us_screen.
  PERFORM processing.
  IF retcode NE 0.
    return_code = 1.
  ELSE.
    return_code = 0.
  ENDIF.

ENDFORM.                    "ENTRY

*---------------------------------------------------------------------*
*       FORM PROCESSING                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM processing.

  PERFORM get_data.
  CHECK retcode = 0.

* O/p control
* Email Output Control Enhancements - check only if type is External Send
  IF nast-nacha EQ '5'.
    PERFORM check_badi_email.
  ENDIF.
  IF gv_email_enh_flag EQ abap_false.
    PERFORM form_open USING xscreen vbdka-land1.
    CHECK retcode = 0.
  ELSE.
    PERFORM form_open_new USING xscreen vbdka-land1.
    CHECK retcode = 0.
  ENDIF.
* O/p control check end

  IF gv_email_enh_flag EQ abap_false.
    PERFORM form_title_print.
    CHECK retcode = 0.
  ENDIF.
  PERFORM validity_print.
  CHECK retcode = 0.
  PERFORM header_data_print.
  CHECK retcode = 0.
  PERFORM header_serv_print.
  CHECK retcode = 0.
  PERFORM header_notice_print.
  CHECK retcode = 0.
  PERFORM header_inter_print.
  CHECK retcode = 0.
  PERFORM header_text_print.
  CHECK retcode = 0.
  PERFORM item_print.
  CHECK retcode = 0.
  PERFORM end_print.
  CHECK retcode = 0.

* O/p control new form close call
  IF gv_email_enh_flag EQ abap_false.
    PERFORM form_close.
    CHECK retcode = 0.
  ELSE.
    PERFORM form_close_new.
    CHECK retcode = 0.
    PERFORM archive_data.
  ENDIF.
* End o/p control change

  gv_email_enh_flag = abap_false.

ENDFORM.                    "PROCESSING

***********************************************************************
*       S U B R O U T I N E S                                         *
***********************************************************************

*---------------------------------------------------------------------*
*       FORM ALTERNATIVE_ITEM                                         *
*---------------------------------------------------------------------*
*       A text is printed, if the item is an alternative item.        *
*---------------------------------------------------------------------*

FORM alternative_item.

  CHECK vbdpa-grpos CN '0'.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ALTERNATIVE_ITEM'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "ALTERNATIVE_ITEM

*---------------------------------------------------------------------*
*       FORM CHECK_REPEAT                                             *
*---------------------------------------------------------------------*
*       A text is printed, if it is a repeat print for the document.  *
*---------------------------------------------------------------------*

FORM check_repeat.

  CLEAR repeat.
  SELECT * INTO *nast FROM nast WHERE kappl = nast-kappl
                                AND   objky = nast-objky
                                AND   kschl = nast-kschl
                                AND   spras = nast-spras
                                AND   parnr = nast-parnr
                                AND   parvw = nast-parvw
                                AND   nacha BETWEEN '1' AND '4'.
    CHECK *nast-vstat = '1'.
    repeat = 'X'.
    EXIT.
  ENDSELECT.

ENDFORM.                    "CHECK_REPEAT

*---------------------------------------------------------------------*
*       FORM DELIVERY_DATE                                            *
*---------------------------------------------------------------------*
*       If the delivery date in the item is different to the header   *
*       date and there are no scheduled quantities, the delivery date *
*       is printed in the item block.                                 *
*---------------------------------------------------------------------*

FORM delivery_date.

  IF vbdka-lfdat =  space AND
     vbdpa-lfdat NE space AND
     vbdpa-etenr_da = space.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'ITEM_DELIVERY_DATE'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ENDIF.

ENDFORM.                    "DELIVERY_DATE

*---------------------------------------------------------------------*
*       FORM DIFFERENT_CONSIGNEE                                      *
*---------------------------------------------------------------------*
*       If the consignee in the item is different to the header con-  *
*       signee, it is printed by this routine.                        *
*---------------------------------------------------------------------*

FORM different_consignee.

  CHECK vbdka-name1_we NE vbdpa-name1_we
    OR  vbdka-name2_we NE vbdpa-name2_we
    OR  vbdka-name3_we NE vbdpa-name3_we
    OR  vbdka-name4_we NE vbdpa-name4_we
    OR  vbdka-stras_we NE vbdpa-stras_we
    OR  vbdka-pfach_we NE vbdpa-pfach_we
    OR  vbdka-pstlz_we NE vbdpa-pstlz_we
    OR  vbdka-pstl2_we NE vbdpa-pstl2_we
    OR  vbdka-ort01_we NE vbdpa-ort01_we
    OR  vbdka-pfort_we NE vbdpa-pfort_we
    OR  vbdka-land1_we NE vbdpa-land1_we.
  CHECK vbdpa-name1_we NE space
    OR  vbdpa-name2_we NE space
    OR  vbdpa-name3_we NE space
    OR  vbdpa-name4_we NE space
    OR  vbdpa-stras_we NE space
    OR  vbdpa-pfach_we NE space
    OR  vbdpa-pstlz_we NE space
    OR  vbdpa-pstl2_we NE space
    OR  vbdpa-ort01_we NE space
    OR  vbdpa-pfort_we NE space
    OR  vbdpa-land1_we NE space.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_CONSIGNEE'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "DIFFERENT_CONSIGNEE

*---------------------------------------------------------------------*
*       FORM DIFFERENT_REFERENCE_NO                                   *
*---------------------------------------------------------------------*
*       If the reference number in the item is different to the header*
*       reference number, it is printed by this routine.              *
*---------------------------------------------------------------------*

FORM different_reference_no.

  CHECK vbdpa-vbeln_vang NE vbdka-vbeln_vang
    OR  vbdpa-vbtyp_vang NE vbdka-vbtyp_vang.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_REFERENCE_NO'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "DIFFERENT_REFERENCE_NO

*---------------------------------------------------------------------*
*       FORM DIFFERENT_TERMS                                          *
*---------------------------------------------------------------------*
*       If the terms in the item are different to the header terms,   *
*       they are printed by this routine.                             *
*---------------------------------------------------------------------*
FORM different_terms.

  DATA: us_vposn   LIKE vedpa-vposn.
  DATA: us_text(1) TYPE c.             "Flag for Noticetext was printed

  IF vbdpa-zterm NE vbdka-zterm AND
     vbdpa-zterm NE space.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'ITEM_TERMS_OF_PAYMENT'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ENDIF.
  IF vbdpa-inco1 NE space.
    IF vbdpa-inco1 NE vbdka-inco1 OR
       vbdpa-inco2 NE vbdka-inco2.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_TERMS_OF_DELIVERY'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    ENDIF.
  ENDIF.

* Print different validity-data for the position
  READ TABLE tkomservp WITH KEY vbdpa-posnr.
  IF sy-subrc EQ 0.
    vedpa = tkomservp.
    IF vedpa-vbegdat NE space       AND
       vedpa-venddat NE space       AND
       NOT vedpa-vbegdat IS INITIAL AND
       NOT vedpa-venddat IS INITIAL.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_TERMS_OF_SERV1'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    ELSEIF vedpa-vbegdat NE space AND
           NOT vedpa-vbegdat IS INITIAL.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_TERMS_OF_SERV2'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    ELSE.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_TERMS_OF_SERV3'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    ENDIF.
  ENDIF.

* Notice-rules for the positions.
  MOVE vbdpa-posnr TO us_vposn.
  CLEAR us_text.
  LOOP AT tkomservpn WHERE vposn = us_vposn.
    vedpn = tkomservpn.
    IF us_text IS INITIAL.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_TERMS_OF_NOTTXT'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
      us_text = charx.
    ENDIF.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'ITEM_TERMS_OF_NOTICE'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ENDLOOP.
  IF NOT us_text IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'EMPTY_LINE'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ENDIF.

ENDFORM.                    "DIFFERENT_TERMS

*---------------------------------------------------------------------*
*       FORM END_PRINT                                                *
*---------------------------------------------------------------------*
*                                                                     *
*---------------------------------------------------------------------*

FORM end_print.

  PERFORM get_header_prices.

  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      command = 'PROTECT'.

  PERFORM header_price_print.

  IF NOT price_print_mode EQ chara AND
     xscreen IS INITIAL.
* Pricing data init
    CALL FUNCTION 'RV_PRICE_PRINT_GET_BUFFER'
      EXPORTING
        i_init   = charx
      TABLES
        t_tkomv  = tkomv
        t_tkomvd = tkomvd.

  ENDIF.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'END_VALUES'.
  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      command = 'ENDPROTECT'.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'SUPPLEMENT_TEXT'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "END_PRINT

*---------------------------------------------------------------------*
*       FORM FORM_CLOSE                                               *
*---------------------------------------------------------------------*
*       End of printing the form                                      *
*---------------------------------------------------------------------*

FORM form_close.

  DATA: da_clear_vbeln(1) TYPE c,
        lt_otfdata        TYPE TABLE OF itcoo.

* bei Druckansicht im Anlegen gibt es noch keine Belegnummer - für die
* Anzeige temporäre Belegnummer übergeben und danach zurücknehmen, damit
* Folgeverarbeitung noch funktioniert
  IF vbdka-vbeln IS INITIAL.
    da_clear_vbeln = charx.
    vbdka-vbeln = '$000000001'.
  ENDIF.

  IF cl_ops_switch_check=>sd_sfws_sc3( ) EQ 'X'.
    IF xscreen EQ 'W'.
      CALL FUNCTION 'CLOSE_FORM'
        TABLES
          otfdata = lt_otfdata
        EXCEPTIONS
          OTHERS  = 1.
      PERFORM convert_otf_to_pdf
         TABLES lt_otfdata.
    ELSE.
      CALL FUNCTION 'CLOSE_FORM'
        EXCEPTIONS
          OTHERS = 1.
    ENDIF.
  ELSE.
    CALL FUNCTION 'CLOSE_FORM'
      EXCEPTIONS
        OTHERS = 1.
  ENDIF.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
    retcode = 1.
  ENDIF.
  SET COUNTRY space.

  IF da_clear_vbeln EQ charx.
    CLEAR vbdka-vbeln.
  ENDIF.

ENDFORM.                    "FORM_CLOSE

*---------------------------------------------------------------------*
*       FORM FORM_OPEN                                                *
*---------------------------------------------------------------------*
*       Start of printing the form                                    *
*---------------------------------------------------------------------*
*  -->  US_SCREEN  Output on screen                                   *
*                  ' ' = printer                                      *
*                  'X' = screen                                       *
*  -->  US_COUNTRY County for telecommunication and SET COUNTRY       *
*---------------------------------------------------------------------*

FORM form_open USING us_screen TYPE c
                     us_country TYPE c.

* Send confirmation to user who send the document.
  IF  nast-nacha EQ '2'.
    nast-usnam = vbdka-ernam.
*  get fax country key
    IF nast-teltx IS INITIAL AND nast-manue NE 'X'.
      PERFORM get_fax_land USING nast-tland.
    ENDIF.
  ENDIF.

  INCLUDE zrvadopfo.
*  INCLUDE rvadopfo.

ENDFORM.                    "FORM_OPEN

*---------------------------------------------------------------------*
*       FORM FORM_TITLE_PRINT                                         *
*---------------------------------------------------------------------*
*       Printing of the form title depending of the field VBTYP       *
*---------------------------------------------------------------------*

FORM form_title_print.

  CASE vbdka-vbtyp.
    WHEN 'A'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TITLE_A'
          window  = 'TITLE'
        EXCEPTIONS
          element = 1
          window  = 2.

*ENHANCEMENT-SECTION     FORM_TITLE_PRINT_01 SPOTS ES_RVADOR01.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.


*END-ENHANCEMENT-SECTION.


    WHEN 'B'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TITLE_B'
          window  = 'TITLE'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    WHEN 'C'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TITLE_C'
          window  = 'TITLE'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    WHEN 'E'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TITLE_E'
          window  = 'TITLE'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    WHEN 'F'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TITLE_F'
          window  = 'TITLE'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    WHEN 'G'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TITLE_F'
          window  = 'TITLE'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    WHEN 'H'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TITLE_H'
          window  = 'TITLE'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    WHEN 'K'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TITLE_K'
          window  = 'TITLE'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    WHEN 'L'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TITLE_L'
          window  = 'TITLE'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    WHEN OTHERS.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TITLE_OTHERS'
          window  = 'TITLE'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
  ENDCASE.
  IF repeat NE space.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'REPEAT'
        window  = 'REPEAT'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ENDIF.

ENDFORM.                    "FORM_TITLE_PRINT

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       General provision of data for the form                        *
*---------------------------------------------------------------------*

FORM get_data.

  DATA: us_veda_vbeln     LIKE veda-vbeln.
  DATA: us_veda_posnr_low LIKE veda-vposn.

  DATA: da_mess LIKE vbfs OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'RV_PRICE_PRINT_GET_MODE'
    IMPORTING
      e_print_mode = price_print_mode.

  IF price_print_mode EQ chara.
    CALL FUNCTION 'RV_PRICE_PRINT_REFRESH'
      TABLES
        tkomv = tkomv.
  ENDIF.

  CLEAR komk.
  CLEAR komp.

  vbco3-mandt = sy-mandt.
  vbco3-spras = nast-spras.
  vbco3-vbeln = nast-objky.
  vbco3-kunde = nast-parnr.
  vbco3-parvw = nast-parvw.

  SUBMIT zsdr0138
          WITH p_vbeln EQ vbco3-vbeln AND RETURN.

  LEAVE LIST-PROCESSING.


  CALL FUNCTION 'RV_DOCUMENT_PRINT_VIEW'
    EXPORTING
      comwa                       = vbco3
    IMPORTING
      kopf                        = vbdka
    TABLES
      pos                         = tvbdpa
      mess                        = da_mess
    EXCEPTIONS
      fehler_bei_datenbeschaffung = 1.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
    retcode = 1.
    EXIT.
  ELSE.
    LOOP AT da_mess.
      sy-msgid = da_mess-msgid.
      sy-msgno = da_mess-msgno.
      sy-msgty = da_mess-msgty.
      sy-msgv1 = da_mess-msgv1.
      sy-msgv2 = da_mess-msgv2.
      sy-msgv3 = da_mess-msgv3.
      sy-msgv4 = da_mess-msgv4.
      PERFORM protocol_update.
    ENDLOOP.
  ENDIF.

* fill address key --> necessary for emails
  addr_key-addrnumber = vbdka-adrnr.
  addr_key-persnumber = vbdka-adrnp.
  addr_key-addr_type  = vbdka-address_type.

* Fetch servicecontract-data and notice-data for head and position.
  us_veda_vbeln     = vbdka-vbeln.
  us_veda_posnr_low = posnr_low.
  CALL FUNCTION 'SD_VEDA_GET_PRINT_DATA'
    EXPORTING
      i_document_number = us_veda_vbeln
      i_language        = nast-spras
      i_posnr_low       = us_veda_posnr_low
    TABLES
      print_data_pos    = tkomservp
      print_data_head   = tkomservh
      print_notice_pos  = tkomservpn
      print_notice_head = tkomservhn.

  PERFORM get_controll_data.

  PERFORM sender.
  PERFORM check_repeat.
  PERFORM tvbdpau_create.

ENDFORM.                    "GET_DATA

*---------------------------------------------------------------------*
*       FORM GET_ITEM_BILLING_SCHEDULES                               *
*---------------------------------------------------------------------*
*       In this routine the billing schedules are fetched from the    *
*       database.                                                     *
*---------------------------------------------------------------------*

FORM get_item_billing_schedules.

  REFRESH tfpltdr.
  CHECK NOT vbdpa-fplnr IS INITIAL.

  CALL FUNCTION 'BILLING_SCHED_PRINTVIEW_READ'
    EXPORTING
      i_fplnr    = vbdpa-fplnr
      i_language = nast-spras
      i_vbeln    = vbdka-vbeln
    TABLES
      zfpltdr    = tfpltdr.

ENDFORM.                    "GET_ITEM_BILLING_SCHEDULES

*&---------------------------------------------------------------------*
*&      Form  ITEM_BILLING_SCHEDULES_PRINT
*&---------------------------------------------------------------------*
*       This routine prints the billing shedules of a salesdocument    *
*       position.                                                      *
*----------------------------------------------------------------------*
FORM  item_billing_schedules_print.

  DATA: first_line(1) TYPE c.

  first_line = charx.
  LOOP AT tfpltdr.
    fpltdr = tfpltdr.
*   Output of the following printlines
    IF NOT fpltdr-perio IS INITIAL.
*     periodische Fakturen
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_BILLING_SCHEDULE_PERIODIC'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
*     bei periodischen nur eine Zeile
      EXIT.
    ELSEIF fpltdr-fareg CA '14'.
*     prozentuale Teilfakturierung
      IF NOT first_line IS INITIAL.
        CLEAR first_line.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_BILLING_SCHEDULE_PERCENT_HEADER'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ELSE.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_BILLING_SCHEDULE_PERCENT'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ENDIF.
    ELSEIF fpltdr-fareg CA '235'.
*     wertmäßige  Teilfakturierung
      IF NOT first_line IS INITIAL.
        CLEAR first_line.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_BILLING_SCHEDULE_VALUE_HEADER'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ELSE.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_BILLING_SCHEDULE_VALUE'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ENDIF.
    ELSEIF fpltdr-fareg CA '3'.
*     Schlußrechnung
    ENDIF.
  ENDLOOP.
ENDFORM.                    "ITEM_BILLING_SCHEDULES_PRINT
*eject

*&---------------------------------------------------------------------*
*&      FORM  GET_ITEM_ADDIS
*&---------------------------------------------------------------------*
*       Additionals data are fetched from database
*----------------------------------------------------------------------*
FORM get_item_addis.

  CLEAR: taddi_print.

  CALL FUNCTION 'WTAD_ADDIS_IN_SO_PRINT'
    EXPORTING
      fi_vbeln              = vbdka-vbeln
      fi_posnr              = vbdpa-posnr
*     FI_LANGUAGE           = SY-LANGU
    TABLES
      fet_addis_in_so_print = taddi_print
    EXCEPTIONS
      addis_not_active      = 1
      no_addis_for_so_item  = 2
      OTHERS                = 3.

ENDFORM.                               " GET_ITEM_ADDIS

*---------------------------------------------------------------------*
*       FORM GET_ITEM_CHARACTERISTICS                                 *
*---------------------------------------------------------------------*
*       In this routine the configuration data item is fetched from   *
*       the database.                                                 *
*---------------------------------------------------------------------*

FORM get_item_characteristics.

  DATA da_t_cabn LIKE cabn OCCURS 10 WITH HEADER LINE.
  DATA: BEGIN OF da_key,
          mandt LIKE cabn-mandt,
          atinn LIKE cabn-atinn,
        END   OF da_key.

  REFRESH tkomcon.
  CHECK NOT vbdpa-cuobj IS INITIAL AND
            vbdpa-attyp NE var_typ.

  CALL FUNCTION 'VC_I_GET_CONFIGURATION'
    EXPORTING
      instance      = vbdpa-cuobj
      language      = nast-spras
      print_sales   = charx
    TABLES
      configuration = tkomcon
    EXCEPTIONS
      OTHERS        = 4.

  RANGES : da_in_cabn FOR da_t_cabn-atinn.
* Beschreibung der Merkmale wegen Objektmerkmalen auf sdcom-vkond holen
  CLEAR da_in_cabn. REFRESH da_in_cabn.
  LOOP AT tkomcon.
    da_in_cabn-option = 'EQ'.
    da_in_cabn-sign   = 'I'.
    da_in_cabn-low    = tkomcon-atinn.
    APPEND da_in_cabn.
  ENDLOOP.

  CLEAR da_t_cabn. REFRESH da_t_cabn.
  CALL FUNCTION 'CLSE_SELECT_CABN'
*    EXPORTING
*         KEY_DATE                     = SY-DATUM
*         BYPASSING_BUFFER             = ' '
*         WITH_PREPARED_PATTERN        = ' '
*         I_AENNR                      = ' '
*    IMPORTING
*         AMBIGUOUS_OBJ_CHARACTERISTIC =
    TABLES
      in_cabn        = da_in_cabn
      t_cabn         = da_t_cabn
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

* Preisfindungsmerkmale / Merkmale auf VCSD_UPDATE herausnehmen
  SORT da_t_cabn.
  LOOP AT tkomcon.
    da_key-mandt = sy-mandt.
    da_key-atinn = tkomcon-atinn.
    READ TABLE da_t_cabn WITH KEY da_key BINARY SEARCH.
    IF sy-subrc <> 0 OR
       ( ( da_t_cabn-attab = 'SDCOM' AND
          da_t_cabn-atfel = 'VKOND'       ) OR
        ( da_t_cabn-attab = 'VCSD_UPDATE' ) ) .
      DELETE tkomcon.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "GET_ITEM_CHARACTERISTICS

*---------------------------------------------------------------------*
*       FORM GET_ITEM_PRICES                                          *
*---------------------------------------------------------------------*
*       In this routine the price data for the item is fetched from   *
*       the database.                                                 *
*---------------------------------------------------------------------*

FORM get_item_prices.

  CLEAR: komp,
         tkomv.

  IF komk-knumv NE vbdka-knumv OR
     komk-knumv IS INITIAL.
    CLEAR komk.
    komk-mandt = sy-mandt.
    komk-kalsm = vbdka-kalsm.
    komk-kappl = pr_kappl.
    komk-waerk = vbdka-waerk.
    komk-knumv = vbdka-knumv.
    komk-knuma = vbdka-knuma.
    komk-vbtyp = vbdka-vbtyp.
    komk-land1 = vbdka-land1.
    komk-vkorg = vbdka-vkorg.
    komk-vtweg = vbdka-vtweg.
    komk-spart = vbdka-spart.
    komk-bukrs = vbdka-bukrs_vf.
    komk-hwaer = vbdka-waers.
    komk-prsdt = vbdka-erdat.
    komk-kurst = vbdka-kurst.
    komk-kurrf = vbdka-kurrf.
    komk-kurrf_dat = vbdka-kurrf_dat.
  ENDIF.
  komp-kposn = vbdpa-posnr.
  komp-kursk = vbdpa-kursk.
  komp-kursk_dat = vbdpa-kursk_dat.
  komp-werks = vbdpa-werks.
  IF vbdka-vbtyp CA 'HKNOT6'.
    IF vbdpa-shkzg CA ' A'.
      komp-shkzg = 'X'.
    ENDIF.
  ELSE.
    IF vbdpa-shkzg CA 'BX'.
      komp-shkzg = 'X'.
    ENDIF.
  ENDIF.

  IF price_print_mode EQ chara.
    CALL FUNCTION 'RV_PRICE_PRINT_ITEM'
      EXPORTING
        comm_head_i = komk
        comm_item_i = komp
        language    = nast-spras
      IMPORTING
        comm_head_e = komk
        comm_item_e = komp
      TABLES
        tkomv       = tkomv
        tkomvd      = tkomvd.
  ELSE.
    CALL FUNCTION 'RV_PRICE_PRINT_ITEM_BUFFER'
      EXPORTING
        comm_head_i = komk
        comm_item_i = komp
        language    = nast-spras
      IMPORTING
        comm_head_e = komk
        comm_item_e = komp
      TABLES
        tkomv       = tkomv
        tkomvd      = tkomvd.
  ENDIF.

ENDFORM.                    "GET_ITEM_PRICES

*---------------------------------------------------------------------*
*       FORM GET_HEADER_PRICES                                        *
*---------------------------------------------------------------------*
*       In this routine the price data for the header is fetched from *
*       the database.                                                 *
*---------------------------------------------------------------------*

FORM get_header_prices.

  LOOP AT tvbdpa.

    CALL FUNCTION 'SD_TAX_CODE_MAINTAIN'
      EXPORTING
        key_knumv           = vbdka-knumv
        key_kposn           = tvbdpa-posnr
        i_application       = ' '
        i_pricing_procedure = vbdka-kalsm
      TABLES
        xkomv               = tkomv.


  ENDLOOP.

  IF price_print_mode EQ chara.
    CALL FUNCTION 'RV_PRICE_PRINT_HEAD'
      EXPORTING
        comm_head_i = komk
        language    = nast-spras
      IMPORTING
        comm_head_e = komk
      TABLES
        tkomv       = tkomv
        tkomvd      = tkomvd.
  ELSE.
    CALL FUNCTION 'RV_PRICE_PRINT_HEAD_BUFFER'
      EXPORTING
        comm_head_i = komk
        language    = nast-spras
      IMPORTING
        comm_head_e = komk
      TABLES
        tkomv       = tkomv
        tkomvd      = tkomvd.
  ENDIF.

ENDFORM.                    "GET_HEADER_PRICES

*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_PRINT
*&---------------------------------------------------------------------*
*       Printing of header data like terms, weights ....               *
*----------------------------------------------------------------------*

FORM header_data_print.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'HEADER_DATA'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                               " HEADER_DATA_PRINT

*---------------------------------------------------------------------*
*       FORM HEADER_PRICE_PRINT                                       *
*---------------------------------------------------------------------*
*       Printout of the header prices                                 *
*---------------------------------------------------------------------*

FORM header_price_print.

  LOOP AT tkomvd.

    AT FIRST.
      IF komk-supos NE 0.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_SUM'.
      ELSE.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'UNDER_LINE'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ENDIF.
    ENDAT.

    komvd = tkomvd.
    IF komvd-koaid = 'D'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TAX_LINE'.
    ELSE.
      IF NOT komvd-kntyp EQ 'f'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'SUM_LINE'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE tkomvd LINES sy-tfill.
  IF sy-tfill = 0.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'UNDER_LINE'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ENDIF.

ENDFORM.                    "HEADER_PRICE_PRINT

*---------------------------------------------------------------------*
*       FORM HEADER_TEXT_PRINT                                        *
*---------------------------------------------------------------------*
*       Printout of the headertexts                                   *
*---------------------------------------------------------------------*

FORM header_text_print.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'HEADER_TEXT'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "HEADER_TEXT_PRINT

*---------------------------------------------------------------------*
*       FORM ITEM_BILLING_CORRECTION_HEADER                          *
*---------------------------------------------------------------------*
*       In the case of a billing correction, the header of the item   *
*       debit memo / credit memo position, is printed by this routine *
*---------------------------------------------------------------------*

FORM item_billing_correction_header USING us_ganf TYPE c
                                          us_lanf TYPE c.


  CHECK vbdka-vbklt EQ vbklt_rech_korr.

  IF vbdka-vbtyp = vbtyp_ganf.
*   Gutschriftsanforderung
    IF vbdpa-shkzg = charx.
      IF us_ganf IS INITIAL.
        MOVE charx TO us_ganf.
        MOVE space TO us_lanf.

        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'CORRECTION_TEXT_K'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ENDIF.
    ELSE.
      IF us_lanf IS INITIAL.
        MOVE charx TO us_lanf.
        MOVE space TO us_ganf.

        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'CORRECTION_TEXT_L'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF vbdka-vbtyp = vbtyp_lanf.
*   Lastschriftssanforderung
    IF vbdpa-shkzg = space.
      IF us_lanf IS INITIAL.
        MOVE charx TO us_lanf.
        MOVE space TO us_ganf.

        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'CORRECTION_TEXT_L'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ENDIF.
    ELSE.
      IF us_ganf IS INITIAL.
        MOVE charx TO us_ganf.
        MOVE space TO us_lanf.

        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'CORRECTION_TEXT_K'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "ITEM_BILLING_CORRECTION_HEADER
*&---------------------------------------------------------------------*
*&      Form  ITEM_ADDIS_PRINT
*&---------------------------------------------------------------------*
*       Printout of item additionals
*----------------------------------------------------------------------*
FORM item_addis_print.

  LOOP AT taddi_print.
    MOVE-CORRESPONDING taddi_print TO wtad_addis_in_so_print.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'ITEM_ADDI_SO_INFO'
      EXCEPTIONS
        OTHERS  = 1.
    LOOP AT taddi_print-addi_so_extra_text_info
            INTO wtad_buying_print_extra_text.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_ADDI_EXTRA_TEXT'
        EXCEPTIONS
          OTHERS  = 1.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                               " ITEM_ADDIS_PRINT
*---------------------------------------------------------------------*
*       FORM ITEM_CHARACERISTICS_PRINT                                *
*---------------------------------------------------------------------*
*       Printout of the item characteristics -> configuration         *
*---------------------------------------------------------------------*

FORM item_characteristics_print.

  LOOP AT tkomcon.
    conf_out = tkomcon.
    IF sy-tabix = 1.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_LINE_CONFIGURATION_HEADER'
        EXCEPTIONS
          OTHERS  = 1.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    ELSE.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_LINE_CONFIGURATION'
        EXCEPTIONS
          OTHERS  = 1.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "ITEM_CHARACTERISTICS_PRINT

*---------------------------------------------------------------------*
*       FORM ITEM_DELIVERY_CONFIRMATION                               *
*---------------------------------------------------------------------*
*       If the delivery date is not confirmed, a text is printed      *
*---------------------------------------------------------------------*

FORM item_delivery_confirmation.

  CHECK vbdka-vbtyp NE vbtyp_ganf AND vbdka-vbtyp NE vbtyp_lanf.
  CHECK vbdpa-lfdat = space.
  CHECK vbdpa-kwmeng NE 0.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_DELIVERY_CONFIRMATION'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "ITEM_DELIVERY_CONFIRMATION
*---------------------------------------------------------------------*
*       FORM ITEM_AGREED_DELIVERY_TIME                                *
*---------------------------------------------------------------------*
*       If an agreed delivery time and the corresponding text is      *
*       available on item level, the text is printed                  *
*---------------------------------------------------------------------*

FORM item_agreed_delivery_time.

  CHECK vbdka-vbtyp EQ 'B' OR vbdka-vbtyp EQ 'G'.
  CHECK vbdpa-delco NE space AND vbdpa-delco_bez NE space.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_AGREED_DELIVERY_TIME'
    EXCEPTIONS
      element = 1
      window  = 2.

  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "ITEM_AGREED_DELIVERY_TIME

*---------------------------------------------------------------------*
*       FORM ITEM_PRICE_PRINT                                         *
*---------------------------------------------------------------------*
*       Printout of the item prices                                   *
*---------------------------------------------------------------------*

FORM item_price_print.

  LOOP AT tkomvd.
    komvd = tkomvd.
    IF sy-tabix = 1 AND
     ( komvd-koaid = charb OR
       komvd-kschl = space ).
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_LINE_PRICE_QUANTITY'.
    ELSE.
      IF komvd-kntyp NE 'f'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_LINE_PRICE_TEXT'.
      ELSE.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_LINE_REBATE_IN_KIND'.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "ITEM_PRICE_PRINT

*---------------------------------------------------------------------*
*       FORM ITEM_PRINT                                               *
*---------------------------------------------------------------------*
*       Printout of the items                                         *
*---------------------------------------------------------------------*

FORM item_print.

  DATA: da_subrc LIKE sy-subrc,
        da_dragr LIKE tvag-dragr.
  DATA: da_ganf(1) TYPE c,      "Print flag for billing correction
        da_lanf(1) TYPE c.      "Print flag for billing correction
  DATA: lt_posnr TYPE TABLE OF posnr.
  DATA: da_posnr TYPE posnr.


  CALL FUNCTION 'WRITE_FORM'           "First header
    EXPORTING
      element = 'ITEM_HEADER'
    EXCEPTIONS
      OTHERS  = 1.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.
  CALL FUNCTION 'WRITE_FORM'           "Activate header
    EXPORTING
      element = 'ITEM_HEADER'
      type    = 'TOP'
    EXCEPTIONS
      OTHERS  = 1.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

  LOOP AT tvbdpa WHERE lfrel = 'X'.
    APPEND tvbdpa-posnr TO lt_posnr.
  ENDLOOP.

  LOOP AT tvbdpa.
    vbdpa = tvbdpa.

*ENHANCEMENT-POINT ITEM_PRINT_01 SPOTS ES_RVADOR01.


    IF vbdpa-dragr EQ space.           "Print rejected item?
      IF vbdpa-posnr_neu NE space.     "Item
        PERFORM item_billing_correction_header USING da_ganf da_lanf.
        PERFORM get_item_serials.
        PERFORM get_item_characteristics.
        PERFORM get_item_billing_schedules.
        PERFORM get_item_prices.
        PERFORM get_item_addis.
        CALL FUNCTION 'CONTROL_FORM'
          EXPORTING
            command = 'ENDPROTECT'.
        CALL FUNCTION 'CONTROL_FORM'
          EXPORTING
            command = 'PROTECT'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_LINE'.
        PERFORM item_rejected.
        PERFORM item_price_print.
        CALL FUNCTION 'CONTROL_FORM'
          EXPORTING
            command = 'ENDPROTECT'.
        PERFORM item_text_print.
        PERFORM item_serials_print.
        PERFORM item_characteristics_print.
        PERFORM item_addis_print.
        PERFORM item_reference_billing.
        PERFORM alternative_item.
        PERFORM delivery_date.
        PERFORM item_delivery_confirmation.
        PERFORM item_agreed_delivery_time.
        PERFORM item_billing_schedules_print.
        PERFORM different_reference_no.
        PERFORM different_terms.
        PERFORM different_consignee.
        LOOP AT lt_posnr INTO da_posnr.
          IF da_posnr = vbdpa-posnr.
            PERFORM schedule_header.
            EXIT.
          ENDIF.
        ENDLOOP.
        PERFORM main_item.
      ELSE.
        PERFORM schedule_print.
      ENDIF.
*ENHANCEMENT-POINT ITEM_PRINT_03 SPOTS ES_RVADOR01 STATIC.

*ENHANCEMENT-POINT ITEM_PRINT_02 SPOTS ES_RVADOR01.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'WRITE_FORM'           "Deactivate Header
    EXPORTING
      element  = 'ITEM_HEADER'
      function = 'DELETE'
      type     = 'TOP'
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "ITEM_PRINT
*---------------------------------------------------------------------*
*       FORM ITEM_REFERENCE_BILLING                                  *
*---------------------------------------------------------------------*
*       If the reference number of the billing is printed by this     *
*       routine. In case (debit memo / credit memo)                   *
*---------------------------------------------------------------------*

FORM item_reference_billing.

  CHECK vbdka-vbklt EQ vbklt_rech_korr.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_REFERENCE_BILLING'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "ITEM_REFERENCE_BILLING


*---------------------------------------------------------------------*
*       FORM ITEM_REJECTED                                            *
*---------------------------------------------------------------------*
*       A text is printed, if the item is rejected                    *
*---------------------------------------------------------------------*

FORM item_rejected.

  CHECK NOT vbdpa-abgru IS INITIAL.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_REJECTED'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "ITEM_REJECTED

*---------------------------------------------------------------------*
*       FORM MAIN_ITEM                                                *
*---------------------------------------------------------------------*
*       A text is printed, if the item is a main item                 *
*---------------------------------------------------------------------*

FORM main_item.

  LOOP AT tvbdpau INTO vbdpau
                  WHERE posnr EQ vbdpa-posnr.
    IF vbdpau-uposb IS INITIAL.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ONE_SUBITEM'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    ELSE.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'SEVERAL_SUBITEMS'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "MAIN_ITEM

*---------------------------------------------------------------------*
*       FORM ITEM_TEXT_PRINT                                          *
*---------------------------------------------------------------------*
*       Printout of the item texts                                    *
*---------------------------------------------------------------------*

FORM item_text_print.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_TEXT'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "ITEM_TEXT_PRINT

*---------------------------------------------------------------------*
*       FORM PROTOCOL_UPDATE                                          *
*---------------------------------------------------------------------*
*       The messages are collected for the processing protocol.       *
*---------------------------------------------------------------------*

FORM protocol_update.

  CHECK xscreen = space.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      msg_arbgb = syst-msgid
      msg_nr    = syst-msgno
      msg_ty    = syst-msgty
      msg_v1    = syst-msgv1
      msg_v2    = syst-msgv2
      msg_v3    = syst-msgv3
      msg_v4    = syst-msgv4
    EXCEPTIONS
      OTHERS    = 1.

ENDFORM.                    "PROTOCOL_UPDATE

*---------------------------------------------------------------------*
*       FORM SCHEDULE_HEADER                                          *
*---------------------------------------------------------------------*
*       If there are schedules in the item, then here is printed the  *
*       header for the schedules.                                     *
*---------------------------------------------------------------------*

FORM schedule_header.

  CHECK vbdpa-etenr_da NE space.
  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      command = 'PROTECT'.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_SCHEDULE_HEADER'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "SCHEDULE_HEADER

*---------------------------------------------------------------------*
*       FORM SCHEDULE_PRINT                                           *
*---------------------------------------------------------------------*
*       This routine prints the schedules for an item.                *
*---------------------------------------------------------------------*

FORM schedule_print.

  CHECK vbdpa-lfrel EQ 'X'.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_SCHEDULE_PRINT'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                    "SCHEDULE_PRINT

*---------------------------------------------------------------------*
*       FORM SENDER                                                   *
*---------------------------------------------------------------------*
*       This routine determines the address of the sender (Table VKO) *
*---------------------------------------------------------------------*

FORM sender.

  SELECT SINGLE * FROM tvko  WHERE vkorg = vbdka-vkorg.
  IF sy-subrc NE 0.
    syst-msgid = 'VN'.
    syst-msgno = '203'.
    syst-msgty = 'E'.
    syst-msgv1 = 'TVKO'.
    syst-msgv2 = syst-subrc.
    PERFORM protocol_update.
    EXIT.
  ENDIF.

  CLEAR gv_fb_addr_get_selection.
  gv_fb_addr_get_selection-addrnumber = tvko-adrnr.         "SADR40A
  CALL FUNCTION 'ADDR_GET'
    EXPORTING
      address_selection = gv_fb_addr_get_selection
      address_group     = 'CA01'
    IMPORTING
      sadr              = sadr
    EXCEPTIONS
      OTHERS            = 01.
  IF sy-subrc NE 0.
    CLEAR sadr.
  ENDIF.                                                    "SADR40A
  vbdka-sland = sadr-land1.
  IF sy-subrc NE 0.
    syst-msgid = 'VN'.
    syst-msgno = '203'.
    syst-msgty = 'E'.
    syst-msgv1 = 'SADR'.
    syst-msgv2 = syst-subrc.
    PERFORM protocol_update.
  ENDIF.
*  SELECT SINGLE * FROM TVBUR  WHERE VKBUR = VBDKA-VKBUR.
*  IF SY-SUBRC NE 0.
*    SYST-MSGID = 'VN'.
*    SYST-MSGNO = '203'.
*    SYST-MSGTY = 'E'.
*    SYST-MSGV1 = 'TVBUR'.
*    SYST-MSGV2 = SYST-SUBRC.
*    PERFORM PROTOCOL_UPDATE.
*  ENDIF.

ENDFORM.                    "SENDER

*---------------------------------------------------------------------*
*       FORM TVBDPAU_CREATE                                           *
*---------------------------------------------------------------------*
*       This routine is creating a table which includes the subitem-  *
*       numbers                                                       *
*---------------------------------------------------------------------*

FORM tvbdpau_create.

  CLEAR tvbdpau.
  REFRESH tvbdpau.
  LOOP AT tvbdpa.
    IF tvbdpa-uepos IS INITIAL OR
       tvbdpa-uepos NE tvbdpau-posnr.
* Append work area to internal table TVBDPAU
      IF tvbdpau-uposv > 0.
        APPEND tvbdpau.
        CLEAR tvbdpau.
      ENDIF.
* Start filling new work area
      tvbdpau-posnr = tvbdpa-posnr.

      IF NOT tvbdpa-uepos IS INITIAL AND
         tvbdpa-uepos NE tvbdpau-posnr.
        tvbdpau-posnr = tvbdpa-uepos.
        tvbdpau-uepvw = tvbdpa-uepvw.
        tvbdpau-uposv = tvbdpa-posnr.
      ENDIF.

    ELSE.
      IF tvbdpau-uposv IS INITIAL OR
         tvbdpau-uposv > tvbdpa-posnr.
        tvbdpau-uposv = tvbdpa-posnr.
      ENDIF.
      IF tvbdpau-uposb < tvbdpa-posnr AND
         tvbdpau-uposv < tvbdpa-posnr.
        tvbdpau-uposb = tvbdpa-posnr.
      ENDIF.
      tvbdpau-uepvw = tvbdpa-uepvw.    "UPOS-Verwendung
    ENDIF.
  ENDLOOP.
  IF tvbdpau-uposv > 0.
    APPEND tvbdpau.
  ENDIF.
  SORT tvbdpau.

ENDFORM.                    "TVBDPAU_CREATE

*---------------------------------------------------------------------*
*       FORM VALIDITY_PRINT                                           *
*---------------------------------------------------------------------*
*       This routine is printing the period of validity for offers    *
*       and contracts                                                 *
*---------------------------------------------------------------------*

FORM validity_print.

  CHECK steu-vdkex EQ space.
  CASE vbdka-vbtyp.
    WHEN 'B'.
      IF vbdka-angdt CN '0' OR
         vbdka-bnddt CN '0'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'VALIDITY_OFFER'
            window  = 'VALIDITY'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ENDIF.
    WHEN 'E'.
      IF vbdka-guebg CN '0' OR
         vbdka-gueen CN '0'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'VALIDITY_CONTRACT'
            window  = 'VALIDITY'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ENDIF.
    WHEN 'F'.
      IF vbdka-guebg CN '0' OR
         vbdka-gueen CN '0'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'VALIDITY_CONTRACT'
            window  = 'VALIDITY'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ENDIF.
    WHEN 'G'.
      IF vbdka-guebg CN '0' OR
         vbdka-gueen CN '0'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'VALIDITY_CONTRACT'
            window  = 'VALIDITY'
          EXCEPTIONS
            element = 1
            window  = 2.
        IF sy-subrc NE 0.
          PERFORM protocol_update.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    "VALIDITY_PRINT

*&---------------------------------------------------------------------*
*&      Form  HEADER_NOTICE_PRINT
*&---------------------------------------------------------------------*
*       This routine prints the notice-rules of the contract-header.   *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM header_notice_print.

  DATA: us_text(1) TYPE c.             "Kz. falls Text für Kündigungsbed.

* Kündigungsbedingungen auf Kopfebene.
  CLEAR us_text.
  LOOP AT tkomservhn.
    vedkn = tkomservhn.
    IF us_text IS INITIAL.
*     For the first time a headertext is printed.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'HEADER_TERMS_OF_NOTTXT'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
      us_text = charx.
    ENDIF.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER_TERMS_OF_NOTICE'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ENDLOOP.
* If notice-rules exists a empty line is printed.
  IF NOT us_text IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'EMPTY_LINE'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ENDIF.

ENDFORM.                               " HEADER_NOTICE_PRINT
*eject

*&---------------------------------------------------------------------*
*&      Form  GET_ITEM_SERIALS
*&---------------------------------------------------------------------*
*       This routine give back the serialnumbers of salesdocument      *
*       position. The numbers are processed as print-lines in the      *
*       table KOMSER_PRINT.                                            *
*----------------------------------------------------------------------*
*  -->  US_VBELN  Salesdocument
*  -->  US_POSNR  Position of the salesdocument
*----------------------------------------------------------------------*
FORM get_item_serials.

  DATA: key_data LIKE rserob,
        sernos   LIKE rserob OCCURS 0 WITH HEADER LINE.

  key_data-taser = 'SER02'.
  key_data-sdaufnr = vbdka-vbeln.
  key_data-posnr = vbdpa-posnr.
  IF key_data-sdaufnr IS INITIAL AND NOT
     key_data-posnr IS INITIAL.
* beim Anlegen ist Belegnummer leer - deshalb Dummy-Belegnummer
    key_data-sdaufnr = char$.
  ENDIF.

* Read the Serialnumbers of a Position.
  REFRESH: tkomser,
           tkomser_print.
  CALL FUNCTION 'GET_SERNOS_OF_DOCUMENT'
    EXPORTING
      key_data            = key_data
    TABLES
      sernos              = sernos
    EXCEPTIONS
      key_parameter_error = 1
      no_supported_access = 2
      no_data_found       = 3
      OTHERS              = 4.
  IF sy-subrc NE 0 AND
     sy-subrc NE 3.
    PERFORM protocol_update.
  ENDIF.

  CHECK sy-subrc EQ 0.
* Serialnummern übergeben
  tkomser-vbeln = sernos-sdaufnr.
  tkomser-posnr = sernos-posnr.
  LOOP AT sernos.
    tkomser-sernr = sernos-sernr.
    APPEND tkomser.
  ENDLOOP.

* Process the stringtable for Printing.
  CALL FUNCTION 'PROCESS_SERIALS_FOR_PRINT'
    EXPORTING
      i_boundary_left             = '(_'
      i_boundary_right            = '_)'
      i_sep_char_strings          = ',_'
      i_sep_char_interval         = '_-_'
      i_use_interval              = 'X'
      i_boundary_method           = 'C'
      i_line_length               = 50
      i_no_zero                   = 'X'
      i_alphabet                  = sy-abcde
      i_digits                    = '0123456789'
      i_special_chars             = '-'
      i_with_second_digit         = ' '
    TABLES
      serials                     = tkomser
      serials_print               = tkomser_print
    EXCEPTIONS
      boundary_missing            = 01
      interval_separation_missing = 02
      length_to_small             = 03
      internal_error              = 04
      wrong_method                = 05
      wrong_serial                = 06
      two_equal_serials           = 07
      serial_with_wrong_char      = 08
      serial_separation_missing   = 09.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.


ENDFORM.                               " GET_ITEM_SERIALS
*eject


*&---------------------------------------------------------------------*
*&      Form  ITEM_SERIALS_PRINT
*&---------------------------------------------------------------------*
*       This routine prints the serialnumbers of a salesdocument       *
*       position.                                                      *
*----------------------------------------------------------------------*
FORM item_serials_print.

  DATA: first_line(1) TYPE c.

  first_line = charx.
  LOOP AT tkomser_print.
    komser = tkomser_print.
    IF NOT first_line IS INITIAL.
*     Output of the Headerline
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_LINE_SERIAL_HEADER'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
      CLEAR first_line.
    ELSE.
*     Output of the following printlines
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_LINE_SERIAL'
        EXCEPTIONS
          element = 1
          window  = 2.
      IF sy-subrc NE 0.
        PERFORM protocol_update.
      ENDIF.
    ENDIF.
  ENDLOOP.
* If serialnumbers exists a empty line is printed.
  IF first_line IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'EMPTY_LINE'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ENDIF.

ENDFORM.                               " ITEM_SERIALS_PRINT
*eject


*&---------------------------------------------------------------------*
*&      Form  HEADER_INTER_PRINT
*&---------------------------------------------------------------------*
*       Prints the message that if other condition for the positions   *
*       exists they are printed there.                                 *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM header_inter_print.

  CHECK NOT steu-vdkex IS INITIAL.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'HEADER_TERMS_OF_TXTEND'
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                               " HEADER_INTER_PRINT

*&---------------------------------------------------------------------*
*&      Form  GET_CONTROLL_DATA
*&---------------------------------------------------------------------*
*       Checks if servicedata for the header exists.                   *
*       Checks if servicedata for the position exists.                 *
*       Checks if noticedata for the header exists.                    *
*       Checks if noticedata for the position exists.                  *
*----------------------------------------------------------------------*
FORM get_controll_data.

  DATA: lines TYPE i.

* Exists servicedata for the header?
  DESCRIBE TABLE tkomservh LINES lines.
  IF lines GT 0.
    steu-vdkex = 'X'.
  ENDIF.

* Exists servicedata for the position?
  DESCRIBE TABLE tkomservp LINES lines.
  IF lines GT 0.
    steu-vdpex = 'X'.
  ENDIF.

* Exists noticedata for the header?
  DESCRIBE TABLE tkomservhn LINES lines.
  IF lines GT 0.
    steu-kbkex = 'X'.
  ENDIF.

* Exists noticedata for the position?
  DESCRIBE TABLE tkomservpn LINES lines.
  IF lines GT 0.
    steu-kbpex = 'X'.
  ENDIF.

ENDFORM.                               " GET_CONTROLL_DATA
*eject


*&---------------------------------------------------------------------*
*&      Form  HEADER_SERV_PRINT
*&---------------------------------------------------------------------*
*       Output of the validity of a service-contract.                  *
*----------------------------------------------------------------------*
FORM header_serv_print.

  CHECK NOT steu-vdkex IS INITIAL.
  READ TABLE tkomservh INDEX 1.
  MOVE tkomservh TO vedka.

* Output of the validity.
  IF NOT vedka-venddat IS INITIAL OR
     vedka-venddat EQ space.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER_TERMS_OF_SERV1'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ELSEIF vedka-vbegdat NE space AND
         NOT vedka-vbegdat IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER_TERMS_OF_SERV2'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ELSE.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER_TERMS_OF_SERV3'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
  ENDIF.

ENDFORM.                               " HEADER_SERV_PRINT

*&---------------------------------------------------------------------*
*&      Form  get_fax_land
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAST_TLAND  text
*----------------------------------------------------------------------*
FORM get_fax_land USING   p_nast_land LIKE nast-tland.

  DATA  l_land    LIKE nast-tland .
  CLEAR l_land.


  IF NOT addr_key-addrnumber IS INITIAL.
    CALL FUNCTION 'WFMC_FAXNUMBER_FOR_ADDRESS'
      EXPORTING
        adrnr          = addr_key-addrnumber
      IMPORTING
        tland          = l_land
      EXCEPTIONS
        addr_not_exist = 1
        OTHERS         = 2.
    IF sy-subrc = 0 AND NOT l_land IS INITIAL.
      p_nast_land = l_land.
    ENDIF.

  ENDIF.
ENDFORM.                    " get_fax_land
*ENHANCEMENT-POINT RVADOFOI_01 SPOTS ES_RVADOR01 STATIC.

*ENHANCEMENT-POINT RVADOR01_03 SPOTS ES_RVADOR01 STATIC.

INCLUDE zdor01_f01.
*INCLUDE dor01_f01.

*----------------------------------------------------------------------*
*       Form  FORM_OPEN_NEW
*----------------------------------------------------------------------*
*       Start of printing the form
*       Output Control Email-Enhancement - New Form open
*----------------------------------------------------------------------*
*  -->  US_SCREEN  Output on screen
*                  ' ' = printer
*                  'X' = screen
*  -->  US_COUNTRY Country for telecommunication and SET COUNTRY
*----------------------------------------------------------------------*
FORM form_open_new USING us_screen us_country.

  DATA: lvs_recipient  LIKE  swotobjid,
        lvs_sender     LIKE  swotobjid,
        lvf_device(30) TYPE  c,
        ls_addr_key    TYPE  addr_key,
        lvs_itcpo      TYPE  itcpo.

* set address key data for FM call
  ls_addr_key-addr_type  = vbdka-address_type.
  ls_addr_key-addrnumber = vbdka-adrnr.
  ls_addr_key-persnumber = vbdka-adrnp.

* calling FM to get address data
  CALL FUNCTION 'SD_PDF_HELPER'
    EXPORTING
      is_addr_key         = ls_addr_key
      is_nast             = nast
      iv_prog_id          = sy-repid
      iv_country          = vbdka-land1
      iv_screen           = us_screen
    IMPORTING
      es_address          = gv_address
      es_recipient        = lvs_recipient
      es_sender           = lvs_sender
      es_itcpo            = lvs_itcpo
      ev_device           = lvf_device
    EXCEPTIONS
      otf_data_exception  = 1
      address_exception   = 2
      comm_type_exception = 3
      OTHERS              = 4.

  IF sy-subrc <> 0.
* Implement suitable error handling here
    PERFORM protocol_update.
    retcode = 1.
  ENDIF.

* Before calling open_form - set device to printer which will not trigger email send
* and request OTF data by setting itcpo table - tdgetotf
  lvf_device = 'PRINTER'.
  lvs_itcpo-tdgetotf = 'X'.

* open form
* open form
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      archive_index  = toa_dara
      archive_params = arc_params
      device         = lvf_device
      dialog         = ' '
      form           = tnapr-fonam
      language       = nast-spras
      options        = lvs_itcpo
      mail_sender    = lvs_sender
      mail_recipient = lvs_recipient
    EXCEPTIONS
      canceled       = 1
      device         = 2
      form           = 3
      options        = 4
      unclosed       = 5
      mail_options   = 6
      archive_error  = 7
      OTHERS         = 8.

  IF sy-subrc NE 0.
    CASE sy-subrc.
      WHEN 7.
        retcode = sy-subrc.
        syst-msgid = 'VN'.
        syst-msgno = '096'.
        syst-msgty = 'E'.
        syst-msgv1 = nast-kschl.
        syst-msgv2 = nast-kappl.
        PERFORM protocol_update.
      WHEN OTHERS.
        retcode = sy-subrc.
        PERFORM protocol_update.
    ENDCASE.
  ENDIF.

  SET COUNTRY us_country.

ENDFORM.                               " FORM_OPEN_NEW

*----------------------------------------------------------------------*
*       Form  FORM_CLOSE_NEW
*----------------------------------------------------------------------*
*       End of printing the form
*       Output Control Email-Enhancement - New Form close
*----------------------------------------------------------------------*
FORM form_close_new.

  DATA: i_itcpp LIKE itcpp,
        t_lines LIKE tline OCCURS 100 WITH HEADER LINE.

  CALL FUNCTION 'CLOSE_FORM'
    IMPORTING
      result  = i_itcpp
    TABLES
      otfdata = otf_data.

  SET COUNTRY space.

* calling FM to get otf data
  CALL FUNCTION 'SD_PDF_HELPER'
    EXPORTING
      is_otf_data         = otf_data
      is_nast             = nast
    IMPORTING
      es_pdf_file         = gv_pdf
    EXCEPTIONS
      otf_data_exception  = 1
      address_exception   = 2
      comm_type_exception = 3
      OTHERS              = 4.

  IF sy-subrc <> 0.
* Implement suitable error handling here
    PERFORM protocol_update.
    retcode = 1.
  ENDIF.

  PERFORM send_data.
  CHECK retcode = 0.

ENDFORM.                               " FORM_CLOSE_NEW

*----------------------------------------------------------------------*
*       Form  SEND_DATA
*----------------------------------------------------------------------*
*       Output Control Email-Enhancement - send_data
*----------------------------------------------------------------------*
FORM send_data.

* Setting invoice data to structure for use in badi.
  DATA:
    ls_inv_head_detail   TYPE invoice_s_prt_head_detail,
    ls_inv_item_detail   TYPE invoice_s_prt_item_detail,
    lt_inv_item_detail   TYPE invoice_t_prt_item_detail,
    lv_dummy             TYPE char1,
    ls_email_rcp         TYPE smtp_sd_sls_addr_s,
    ls_email_sendr       TYPE smtp_sd_sls_addr_s,
    lo_cl_bcs            TYPE REF TO cl_bcs,
    ls_file_attribs      TYPE file_attributes_s,
    i_itcpp              LIKE itcpp,
    otf_data             LIKE itcoo OCCURS 0 WITH HEADER LINE,
    us_pdf_file          TYPE fpformoutput,
    lv_send_to_all       TYPE os_boolean,
    lo_badi_mapper       TYPE REF TO badi_sd_obj_mapper,
    lo_badi_sd_sls_email TYPE REF TO badi_sd_sls_email,
    lv_mail_subject      TYPE so_obj_des.


  us_pdf_file-pdf = gv_pdf.
  lv_mail_subject = nast-tdcovtitle.

  TRY.

* get Badi handle for obj mapper
* filters  any filters implement here
      GET BADI lo_badi_mapper
        FILTERS
          sd_process_filter = if_sd_email_process_constant=>rvador01.

* Catch not implemented exception
    CATCH cx_badi_not_implemented.
      CLEAR lo_badi_mapper.

  ENDTRY.

  TRY.

      GET BADI lo_badi_sd_sls_email
        FILTERS
          sd_email_progs = if_sd_email_process_constant=>rvador01.

* Catch not implemented exception
    CATCH cx_badi_not_implemented.
      CLEAR lo_badi_sd_sls_email.

  ENDTRY.

  ls_email_rcp-email_addr = gv_address-smtp_addr.

  IF lo_badi_sd_sls_email IS BOUND.

    IF lo_badi_mapper IS BOUND.

      IF lo_badi_mapper->imps IS NOT INITIAL.

        DATA: is_item_vbdpa TYPE vbdpa,
              it_item       TYPE vbdpa_tt.

* Move all items to item table type for Badi use
        LOOP AT tvbdpa.
          CLEAR is_item_vbdpa.
          is_item_vbdpa = tvbdpa.
          APPEND is_item_vbdpa TO it_item.
        ENDLOOP.

        CALL BADI lo_badi_mapper->set_so_to_generic
          EXPORTING
            is_header_vbdka = vbdka
            it_item_vbdpa   = it_item.

* Call BAdI for modify email details
        CALL BADI lo_badi_sd_sls_email->set_mapper
          EXPORTING
            io_mapper = lo_badi_mapper->imp.

        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

      ENDIF.
    ENDIF.
    INCLUDE zrvador01_remove_default.
*    include RVADOR01_REMOVE_DEFAULT if found.
    CALL BADI lo_badi_sd_sls_email->modify_email
      EXPORTING
        iv_language      = nast-spras
        is_email_rcp     = ls_email_rcp
        is_email_sendr   = ls_email_sendr
      CHANGING
        io_cl_bcs        = lo_cl_bcs
      EXCEPTIONS
        exc_send_req_bcs = 1
        exc_address_bcs  = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      MESSAGE e000 WITH vbdka-vbeln
              INTO lv_dummy.
      PERFORM protocol_update.
      retcode = 99.
      RETURN.
    ENDIF.
    INCLUDE zrvador01_add_email.
*    include RVADOR01_ADD_EMAIL if found.
* add Exceptions for process document method and check response.
    ls_file_attribs-pdf_file = us_pdf_file.
    CALL BADI lo_badi_sd_sls_email->process_document
      EXPORTING
        iv_language      = nast-spras
        is_file_attribs  = ls_file_attribs
*       iv_text          = --change text in impl class
        iv_subject       = lv_mail_subject
      CHANGING
        io_cl_bcs        = lo_cl_bcs
      EXCEPTIONS
        exc_send_req_bcs = 1
        exc_document_bcs = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      MESSAGE e000 WITH vbdka-vbeln
              INTO lv_dummy.
      PERFORM protocol_update.
      retcode = 99.
      RETURN.
    ENDIF.

* Send document
    CALL BADI lo_badi_sd_sls_email->send_document
      EXPORTING
        io_cl_bcs        = lo_cl_bcs
      CHANGING
        ev_send_to_all   = lv_send_to_all
      EXCEPTIONS
        exc_send_req_bcs = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      MESSAGE e000 WITH vbdka-vbeln
              INTO lv_dummy.
      PERFORM protocol_update.
      retcode = 99.
      RETURN.
    ENDIF.

    IF lv_send_to_all = abap_true.
* Write success message into log
      MESSAGE i022(so)
              INTO lv_dummy.
      PERFORM protocol_update.
    ELSE.
* Write fail message into log
      MESSAGE i023(so)
              WITH vbdka-vbeln
              INTO lv_dummy.
      PERFORM protocol_update.
    ENDIF.

  ENDIF.
*end email badi bound
ENDFORM.                               " SEND_DATA

*----------------------------------------------------------------------*
*       Form  CHECK_BADI_EMAIL
*----------------------------------------------------------------------*
*       Output Control Email-Enhancement - Check BADI implementation
*       and set flags
*----------------------------------------------------------------------*
FORM check_badi_email.

  gv_email_enh_flag = abap_false.

  TRY.

      GET BADI go_badi_sls_email
        FILTERS
          sd_email_progs = if_sd_email_process_constant=>rvador01.

      IF go_badi_sls_email IS BOUND.
        IF go_badi_sls_email->imps IS NOT INITIAL.
          gv_email_enh_flag = abap_true.
        ENDIF.
      ENDIF.

*  Catch not implemented exception
    CATCH cx_badi_not_implemented.
      gv_email_enh_flag = abap_false.
      CLEAR go_badi_sls_email.

  ENDTRY.

ENDFORM.                               " CHECK_BADI_EMAIL

*----------------------------------------------------------------------*
*       Form  ARCHIVE_DATA
*----------------------------------------------------------------------*
*       Output Control Email-Enhancement - Archiving
*----------------------------------------------------------------------*
FORM archive_data.

  FIELD-SYMBOLS: <archive_params> TYPE arc_params.
  FIELD-SYMBOLS: <index>.
  DATA: lv_arc TYPE string VALUE '(SAPLSTXC)ARC_PARAMS'.
  DATA: BEGIN OF archive_otf OCCURS 0.
          INCLUDE STRUCTURE itcoo.
  DATA: END OF archive_otf.

  DATA: archive_params TYPE arc_params.

  DATA: length(2) TYPE n.
  DATA: offset    TYPE i.
  DATA: new_form  TYPE abap_bool.

  CHECK ( gv_email_enh_flag = abap_true ).
  CHECK ( otf_data[] IS NOT INITIAL ).

  CHECK ( ( nast-tdarmod = 2 ) OR ( nast-tdarmod = 3 ) ).

  ASSIGN (lv_arc) TO <archive_params>.
  CHECK sy-subrc = 0.
  archive_params = <archive_params>.
  IF <archive_params> IS ASSIGNED.
    UNASSIGN <archive_params>.
  ENDIF.

  LOOP AT otf_data ASSIGNING FIELD-SYMBOL(<otf>).
    archive_otf = <otf>.
    IF archive_otf-tdprintcom = 'MC'.
      IF new_form = abap_true.
*       next form starts, archive current form
        PERFORM otf_archive(saplstxc) TABLES archive_otf USING archive_params toa_dara.
        REFRESH archive_otf.
        archive_otf = <otf>.
        CLEAR: toa_dara, length, offset, new_form.
      ENDIF.
*     collect data from MC... cmds into TOA_DARA structure
      length = archive_otf-tdprintpar+1(2).     "length of MC_xx... data
      IF length > 0.
        ASSIGN toa_dara+offset(length) TO <index>.
        <index> = archive_otf-tdprintpar+3.
        ADD length TO offset.
      ENDIF.
      IF archive_otf-tdprintpar(1) = 'X'.
*       this is the end of MC... archive index data
        new_form = abap_true.
      ENDIF.
    ENDIF.
    APPEND archive_otf.
  ENDLOOP.

  IF new_form = abap_true.
*   archive last form
    PERFORM otf_archive(saplstxc) TABLES archive_otf USING archive_params toa_dara.
    FREE archive_otf.
    CLEAR: toa_dara, length, offset, new_form.
  ENDIF.

ENDFORM.                               " ARCHIVE_DATA
