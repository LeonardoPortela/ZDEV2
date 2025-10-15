*&---------------------------------------------------------------------*
*& Modulpool         RKKBMLMAT                                         *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*SL 19.04.2011: OSS note 1580559
* RHU 23.03.2009 Wrong material texts                       Note 1320867
*SLP6BK000895 29.05.2002 Hinweis 519552 Parameter Curtp optional und mit
*                                     Wertehilfe
*AL0K020500 25.05.01 Korrektur: Lesebaustein CKMS_PERIOD_READ_ONLY statt
*                               CKMS_PERIOD_READ_WITH_ITAB
*P9CK078355  29.09.00
*ALRK261429  01.12.99 Anpassung Status, CURTP als Parameter (no display)
*ALRK238664  21.09.99 Detailbild: Drucktaste ohne Aktion
PROGRAM zcor011 MESSAGE-ID c+ NO STANDARD PAGE HEADING.
*ENHANCEMENT-POINT RKKBMLMAT_G4 SPOTS ES_RKKBMLMAT STATIC.
*ENHANCEMENT-POINT RKKBMLMAT_G5 SPOTS ES_RKKBMLMAT.
*ENHANCEMENT-POINT RKKBMLMAT_G6 SPOTS ES_RKKBMLMAT STATIC.
*ENHANCEMENT-POINT RKKBMLMAT_G7 SPOTS ES_RKKBMLMAT.

* global data
*INCLUDE RKKBMLMATTOP.
TYPE-POOLS: ckmv0,
            kkbml,
            vrm,
            ckm0.
INCLUDE lckm0top_vorgaenge.         " Konstanten für Vorgangsschlüssel

INCLUDE: rkkbeq30.

CONSTANTS:
* number of rows for cursor-fetch
  c_package_size TYPE i VALUE 200,
  y_curtp_lc(2)  VALUE '10'.

CONSTANTS:
* Status
  y_undefiniert
    LIKE ckmlpp-status VALUE ckm0_status-undefiniert,
  y_neu_angelegt
    LIKE ckmlpp-status VALUE ckm0_status-neu_angelegt,
  y_ohne_bestand_eroeffnet
    LIKE ckmlpp-status VALUE ckm0_status-ohne_bestand_eroeffnet,
  y_periode_eroeffnet
    LIKE ckmlpp-status VALUE ckm0_status-periode_eroeffnet,
  y_preisaenderung_erfolgt
    LIKE ckmlpp-status VALUE ckm0_status-preisaenderung_erfolgt,
  y_mengen_und_werte_erfasst
    LIKE ckmlpp-status VALUE ckm0_status-mengen_und_werte_erfasst,
  y_nur_werte_erfasst
    LIKE ckmlpp-status VALUE ckm0_status-nur_werte_erfasst,
  y_einstufig_abgerechnet
    LIKE ckmlpp-status VALUE ckm0_status-einstufig_abgerechnet,
  y_mehrstufig_abgerechnet
    LIKE ckmlpp-status VALUE ckm0_status-mehrstufig_abgerechnet,
  y_abschlussbuchung_storniert
    LIKE ckmlpp-status VALUE ckm0_status-abschlussbuchung_storniert,
  y_abschlussbuchung_erfolgt
    LIKE ckmlpp-status VALUE ckm0_status-abschlussbuchung_erfolgt.

TYPES:
*  Material/valuated sales order/project stock data
  BEGIN OF s_stock,
    kalnr TYPE ckmlhd-kalnr,
    mlast TYPE ckmlhd-mlast,
    matnr TYPE ckmlhd-matnr,
    bwkey TYPE ckmlhd-bwkey,
    bwtar TYPE ckmlhd-bwtar,
    sobkz TYPE ckmlhd-sobkz,
    vbeln TYPE ckmlhd-vbeln,
    posnr TYPE ckmlhd-posnr,
    pspnr TYPE ckmlhd-pspnr,
    bklas TYPE mbew-bklas,
    mtart TYPE mara-mtart,
    matkl TYPE mara-matkl,
    spart TYPE mara-spart,
    meins TYPE mara-meins,
  END OF s_stock,
  t_stock  TYPE STANDARD TABLE OF s_stock
           WITH KEY kalnr,
* total records
  t_ckmlpp TYPE STANDARD TABLE OF ckmlpp
           WITH KEY kalnr bdatj poper,
  t_ckmlcr TYPE STANDARD TABLE OF ckmlcr
           WITH KEY kalnr bdatj poper curtp,
* output-table
  BEGIN OF s_out,
    kalnr       TYPE ckmlhd-kalnr,
    mlast       TYPE ckmlhd-mlast,
    matnr       TYPE ckmlhd-matnr,
    bwkey       TYPE ckmlhd-bwkey,
    bwtar       TYPE ckmlhd-bwtar,
    sobkz       TYPE ckmlhd-sobkz,
    vbeln       TYPE ckmlhd-vbeln,
    posnr       TYPE ckmlhd-posnr,
    pspnr       TYPE ckmlhd-pspnr,
    bdatj       TYPE ckmlpp-bdatj,
    poper       TYPE ckmlpp-poper,
    status      TYPE ckmlpp-status,
    status_text TYPE dd07v-ddtext,
    konts       TYPE t030-konts,  "Conta razão
    curtp       TYPE ckmlcr-curtp,
    bklas       TYPE mbew-bklas,
    werks       TYPE t001w-werks,
    mtart       TYPE mara-mtart,
    matkl       TYPE mara-matkl,
    spart       TYPE mara-spart,
    ktext       TYPE makt-maktx,
    vprsv       TYPE ckmlcr-vprsv,
    lbkum       TYPE ckmlpp-lbkum,
    meins       TYPE mara-meins,
    salk3       TYPE ckmlcr-salk3,
    salk3_u     TYPE ckmlcr-salk3,
    salkv       TYPE ckmlcr-salkv,
    eb_dif      TYPE cki_doc_ml-eb_dif,
    stprs       TYPE ckmlcr-stprs,
    pvprs       TYPE ckmlcr-pvprs,
    prabw_prz   TYPE ck_prabw_prz,
    peinh       TYPE ckmlcr-peinh,
    waers       TYPE waers,
    gsber       TYPE t134g-gsber,
    salkv_u     TYPE ckmlcr-salk3, "PBI - 72568 - CBRAND
  END OF s_out,
  t_out TYPE STANDARD TABLE OF s_out WITH KEY kalnr,
*  informations according to curtp (for one val. area)
  BEGIN OF s_curr_info,
    bwkey   TYPE ckmlhd-bwkey,
    curtp   TYPE ckmlcr-curtp,
    waers   TYPE cki_ml_cty-waers,
    text    TYPE cki_ml_cty-text,
    currtyp TYPE cki_ml_cty-currtyp,
    valutyp TYPE cki_ml_cty-valutyp,
  END OF s_curr_info,
  t_curr_info TYPE STANDARD TABLE OF s_curr_info
            WITH KEY bwkey curtp,
  BEGIN OF s_status,
    status TYPE ckmlpp-status,
    text   TYPE dd07v-ddtext,
  END OF s_status,
  t_status TYPE STANDARD TABLE OF s_status
         WITH KEY status.
*----------------------------------------------------------------------
DATA:
* output table with all currency
  g_t_out       TYPE t_out,
  g_out         TYPE s_out,
* output table with one currency
  g_t_alv_list  TYPE t_out,
*  Buffer for currency informations
  g_curr_info   TYPE s_curr_info,       "Current bwkey/currency type
  g_t_curr_info TYPE t_curr_info,     "All read bwkey/currency type
*
  g_t_status    TYPE t_status,
  gh_old_bwkey  TYPE mlkey-bwkey.

*----------------------------------------------------------------------
TABLES: ckmlhd,
        ckmlpp,
        mara,
        mbew,
        marv,
        t001k,
        t030,
        t134g,
        sscrfields.

*Tabelle für Wertehilfe Curtp
DATA: BEGIN OF t_curtp_f4 OCCURS 0,
        curtp LIKE mlkey-curtp,
        text  LIKE ckmlcur-ddtext,
      END OF t_curtp_f4.

DATA: t_curtp_vrm  TYPE vrm_values,
      gs_curtp_vrm TYPE vrm_value.
DATA: t_curtp LIKE cki_ml_cty OCCURS 0 WITH HEADER LINE.

DATA: lh_bwkey   TYPE mlkey-bwkey,
      lh_bukrs   TYPE mlkey-bukrs,
      ls_ckmlcur TYPE ckmlcur.



DATA: t_dd07v LIKE dd07v OCCURS 20 WITH HEADER LINE.

* Variablen für den ALV.
DATA: exit.

TYPE-POOLS slis.
DATA: fieldcat      TYPE slis_t_fieldcat_alv.
DATA: fieldcathead  TYPE slis_fieldcat_alv.
DATA: is_variant    LIKE disvariant.
DATA: it_events     TYPE slis_t_event.
DATA: it_eventhead  TYPE slis_alv_event.


* Extakte
DATA: gs_extract1     LIKE disextract.
DATA: gs_extract2     LIKE disextract.
DATA: gs_admin        LIKE ltexadmin.

* Selektionsbild Extraktverwaltung
* Ausgabe als Popup
SELECTION-SCREEN BEGIN OF SCREEN 4500 AS WINDOW TITLE TEXT-ext.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_noex RADIOBUTTON GROUP extr.
    SELECTION-SCREEN COMMENT 3(50) TEXT-ex1 FOR FIELD p_noex.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_save RADIOBUTTON GROUP extr.
    SELECTION-SCREEN COMMENT 3(50) TEXT-ex3 FOR FIELD p_save.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 3(29) TEXT-ex2 FOR FIELD p_ex1.
    PARAMETERS: p_ex1 LIKE ltex-exname.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 3(29) TEXT-ex5 FOR FIELD p_ext1.
    PARAMETERS: p_ext1 LIKE ltex-text.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_load RADIOBUTTON GROUP extr.
    SELECTION-SCREEN COMMENT 3(50) TEXT-ex4 FOR FIELD p_load.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 3(29) TEXT-ex2 FOR FIELD p_ex2.
    PARAMETERS: p_ex2 LIKE ltex-exname.
    SELECTION-SCREEN COMMENT 47(40) p_ext2 FOR FIELD p_ex2.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF SCREEN 4500.

TABLES: tcurm, mlkey.
DATA: p_bwkey  LIKE ckmlhd-bwkey.      "Hilfsfeld Bewertungskreis

********************** Selektionsbildschirm ****************************
* Objekteingrenzung
SELECTION-SCREEN SKIP 2.
SELECT-OPTIONS: r_matnr FOR  ckmlhd-matnr.
*INCLUDE rckm_par_bukrs_werks_ml_prod.  "Parameter BUKRS/WERKS
PARAMETERS: p_bukrs LIKE mlkey-bukrs_ml_productive
                    OBLIGATORY
                    MEMORY ID buk.
SELECT-OPTIONS:    s_hkont FOR t030-konts,
                   s_werks FOR mlkey-werks_ml_productive NO INTERVALS MEMORY ID wrk.

SELECT-OPTIONS: s_gsber FOR  t134g-gsber NO INTERVALS. "NO-EXTENSION.
SELECT-OPTIONS: r_bwtar FOR  ckmlhd-bwtar  MEMORY ID bwt.
PARAMETERS:
  r_st_d  LIKE bsid-umskz AS CHECKBOX  DEFAULT ' '.
PARAMETERS:
  r_fisco LIKE bsid-umskz DEFAULT ' ' NO-DISPLAY.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: r_mtart FOR  mara-mtart    MEMORY ID mta MODIF ID puk,
                r_bklas FOR  mbew-bklas    MODIF ID puk,
                r_matkl FOR  mara-matkl    MEMORY ID mtl MODIF ID puk,
                r_spart FOR  mara-spart    MEMORY ID spa MODIF ID puk,
                r_mlast FOR  ckmlhd-mlast  NO-EXTENSION NO INTERVALS
                                           MODIF ID puk,
                r_status FOR ckmlpp-status MODIF ID puk,
                r_vbeln FOR  ckmlhd-vbeln  MODIF ID puk,
                r_posnr FOR  ckmlhd-posnr  MODIF ID puk,
                r_pspnr FOR  ckmlhd-pspnr  MODIF ID puk.
PARAMETERS:     p_ex_sel TYPE c            DEFAULT space
                                           NO-DISPLAY.
SELECTION-SCREEN SKIP 1.
* Periode
SELECTION-SCREEN BEGIN OF BLOCK parameter WITH FRAME TITLE TEXT-101.
  PARAMETERS: p_poper LIKE cki_doc_ml-sl_periode
                      MEMORY ID mlp,
              p_bdatj LIKE ckmlrunplant-gjahr MEMORY ID mlj.
SELECTION-SCREEN END OF BLOCK parameter.

SELECTION-SCREEN BEGIN OF BLOCK currency WITH FRAME TITLE TEXT-008.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (31) TEXT-007 FOR FIELD p_curtp.
    PARAMETERS p_curtp LIKE ckmlcr-curtp OBLIGATORY AS LISTBOX VISIBLE
    LENGTH 30.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK currency.

SELECTION-SCREEN BEGIN OF BLOCK extract WITH FRAME TITLE TEXT-104.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (31) TEXT-ex2 FOR FIELD p_pex2
      MODIF ID ext.
    PARAMETERS: p_pex2 LIKE ltex-exname
                                                      MODIF ID ext.
    SELECTION-SCREEN COMMENT 47(40) p_pext2 FOR FIELD p_pex2
      MODIF ID ext.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK extract.

SELECTION-SCREEN BEGIN OF BLOCK variant WITH FRAME TITLE TEXT-102.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (31) TEXT-103 FOR FIELD p_varian
      MODIF ID var.
    PARAMETERS p_varian LIKE disvariant-variant MODIF ID var.
    SELECTION-SCREEN COMMENT 47(40) p_vartxt FOR FIELD p_varian
      MODIF ID var.

  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK variant.
************************************************************************

************************************************************************
INITIALIZATION.

***

  CLEAR: ls_ckmlcur.
  SET PF-STATUS 'SELECTION'.

  CLEAR is_variant.
  is_variant-report = sy-repid.
  is_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = is_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_varian = is_variant-variant.
    p_vartxt = is_variant-text.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_EXTRACT_AT_INIT'
    CHANGING
      cs_extract1 = gs_extract1
      cs_extract2 = gs_extract2.


************************************************************************
AT SELECTION-SCREEN OUTPUT.
************************************************************************
  PERFORM display_bukrs_or_werks.
  IF s_gsber[] IS INITIAL AND s_werks[] IS INITIAL.
    MESSAGE 'Informe Centro e/ou Divisão.' TYPE 'I'.
    SET CURSOR FIELD 'S_WERKS-LOW' .
  ENDIF.
*** modify screen
  CHECK sy-dynnr NE '4500'.
  IF p_load = 'X'.
    p_pex2 = p_ex2.
    p_pext2 = p_ext2.
  ELSEIF p_save = 'X'.
    p_pex2 = p_ex1.
    p_pext2 = p_ext1.
  ELSE.
    CLEAR: p_pex2, p_pext2.
  ENDIF.

  REFRESH: t_curtp_vrm.
  CLEAR: lh_bwkey, lh_bukrs, ls_ckmlcur.


  IF NOT s_werks[] IS INITIAL.
    LOOP AT s_werks.
      CALL FUNCTION 'DETERMIN_BWKEY_BUKRS_FOR_PLANT'
        EXPORTING
          werk  = s_werks-low
        IMPORTING
          bwkey = lh_bwkey
          bukrs = lh_bukrs.

      IF lh_bwkey <> gh_old_bwkey.
        gh_old_bwkey = lh_bwkey.

        CALL FUNCTION 'GET_BWKEY_CURRENCY_INFO'
          EXPORTING
            bwkey               = lh_bwkey
            call_by_init_prog   = ' '
            i_customizing       = ' '
          TABLES
            t_curtp_for_va      = t_curtp
          EXCEPTIONS
            bwkey_not_found     = 1
            bwkey_not_active    = 2
            matled_not_found    = 3
            internal_error      = 4
            more_than_3_curtp   = 5
            customizing_changed = 6
            OTHERS              = 7.

        IF sy-subrc <> 0.
* Keine messages in F4-Hilfen!
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.

      LOOP AT t_curtp.
        CLEAR: gs_curtp_vrm.
        gs_curtp_vrm-key = t_curtp-curtp.
        gs_curtp_vrm-text = t_curtp-text.
        APPEND gs_curtp_vrm TO t_curtp_vrm.
      ENDLOOP.
    ENDLOOP.
  ELSEIF s_werks[] IS INITIAL.

    SELECT SINGLE * INTO ls_ckmlcur FROM ckmlcur
         WHERE sprsl = sy-langu
         AND   curtp = y_curtp_lc.
    CLEAR: gs_curtp_vrm, t_curtp_vrm.
    gs_curtp_vrm-key = y_curtp_lc.
    gs_curtp_vrm-text = ls_ckmlcur-ddtext.
    APPEND gs_curtp_vrm TO t_curtp_vrm.
    p_curtp = y_curtp_lc.
  ENDIF.

  READ TABLE t_curtp_vrm WITH KEY key = p_curtp
                              TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0 OR
     p_curtp IS INITIAL.
    p_curtp = y_curtp_lc.
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_CURTP'
      values = t_curtp_vrm
* EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS = 2
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*modifications for the extracts
  PERFORM modify_selection_screen.
* modifications for extended selections
  PERFORM modify_object_selection TABLES r_vbeln
                                         r_posnr
                                         r_pspnr
                                         r_mtart
                                         r_bklas
                                         r_matkl
                                         r_spart
                                  USING  p_ex_sel.
* set default period if necessary
  PERFORM set_period CHANGING p_poper p_bdatj.

************************************************************************
AT SELECTION-SCREEN.
************************************************************************
  IF NOT p_bukrs IS INITIAL.
    p_bwkey = p_bukrs.
  ELSEIF s_werks IS NOT INITIAL.
    p_bwkey = s_werks-low.
  ELSE.
    p_bwkey = s_gsber-low.
  ENDIF.

*** Zugriffserlaubnis überprüfen
  IF sy-ucomm NE 'EXTR'.
    CALL FUNCTION 'REUSE_ALV_EXTRACT_AT_SELSCREEN'
      EXPORTING
        i_p_save    = p_save
        i_p_load    = p_load
      CHANGING
        c_p_ex1     = p_ex1
        c_p_ex2     = p_ex2
        c_p_ext1    = p_ext1
        c_p_ext2    = p_ext2
        cs_extract1 = gs_extract1
        cs_extract2 = gs_extract2.
  ENDIF.
  CHECK sy-dynnr NE '4500'.
  PERFORM variant_check_existence.
  PERFORM user_command.

************************************************************************
AT SELECTION-SCREEN ON p_bukrs.
************************************************************************
  IF NOT p_bukrs IS INITIAL.
    PERFORM check_bwkey USING p_bukrs.
  ENDIF.

************************************************************************
AT SELECTION-SCREEN ON s_werks.
************************************************************************
  IF NOT s_werks[] IS INITIAL.
    PERFORM check_bwkey_werks.
  ENDIF.

************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varian.
  PERFORM f4_for_variant.

************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ex1.
  CALL FUNCTION 'REUSE_ALV_EXTRACT_AT_F4_P_EX1'
    CHANGING
      c_p_ex1     = p_ex1
      c_p_ext1    = p_ext1
      cs_extract1 = gs_extract1.

************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ex2.
  CALL FUNCTION 'REUSE_ALV_EXTRACT_AT_F4_P_EX2'
    CHANGING
      c_p_ex2     = p_ex2
      c_p_ext2    = p_ext2
      cs_extract2 = gs_extract2.
***********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_curtp.

  CLEAR: lh_bwkey, lh_bukrs, ls_ckmlcur.
  REFRESH: t_curtp_f4.

  IF NOT s_werks[] IS INITIAL.
    LOOP AT s_werks.
      CALL FUNCTION 'DETERMIN_BWKEY_BUKRS_FOR_PLANT'
        EXPORTING
          werk  = s_werks-low
        IMPORTING
          bwkey = lh_bwkey
          bukrs = lh_bukrs.

      IF lh_bwkey <> gh_old_bwkey.
        gh_old_bwkey = lh_bwkey.


        CALL FUNCTION 'GET_BWKEY_CURRENCY_INFO'
          EXPORTING
            bwkey               = lh_bwkey
*           CALL_BY_INIT_PROG   = ' '
*           I_CUSTOMIZING       = ' '
          TABLES
            t_curtp_for_va      = t_curtp
          EXCEPTIONS
            bwkey_not_found     = 1
            bwkey_not_active    = 2
            matled_not_found    = 3
            internal_error      = 4
            more_than_3_curtp   = 5
            customizing_changed = 6
            OTHERS              = 7.

        IF sy-subrc <> 0.
* Keine messages in F4-Hilfen!
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.

      LOOP AT t_curtp.
        CLEAR t_curtp_f4.
        t_curtp_f4-curtp = t_curtp-curtp.
        t_curtp_f4-text = t_curtp-text.
        APPEND t_curtp_f4.
      ENDLOOP.
    ENDLOOP.

  ELSEIF s_werks[] IS INITIAL.

    SELECT SINGLE * INTO ls_ckmlcur FROM ckmlcur
    WHERE sprsl = sy-langu
    AND   curtp = y_curtp_lc.
    CLEAR: t_curtp_f4.
    t_curtp_f4-curtp = y_curtp_lc.
    t_curtp_f4-text = ls_ckmlcur-ddtext.
    APPEND t_curtp_f4.
    p_curtp = y_curtp_lc.
  ENDIF.

  READ TABLE t_curtp_f4 WITH KEY curtp = p_curtp
                      TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0 OR

     p_curtp IS INITIAL.

    p_curtp = y_curtp_lc.

  ENDIF.



  IF NOT t_curtp_f4[] IS INITIAL.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'CURTP'
        dynpnr      = '1000'
        value_org   = 'S'
        dynprofield = 'P_CURTP'
      TABLES
        value_tab   = t_curtp_f4.
  ENDIF.

************************************************************************


************************************************************************
START-OF-SELECTION.
***
  IF s_gsber[] IS INITIAL AND s_werks[] IS INITIAL.
    EXIT.
  ENDIF.

* set default period if necessary
  PERFORM set_period CHANGING p_poper p_bdatj.

* get output table by reading from DB or loading an extract
  IF p_load IS INITIAL.
*   read currencies of bwkey
    PERFORM read_curr_info USING p_bwkey.
    READ TABLE g_t_curr_info INTO g_curr_info
                             WITH KEY curtp = p_curtp.
    IF sy-subrc <> 0.
      READ TABLE g_t_curr_info INDEX 1 INTO g_curr_info.
    ENDIF.
*   read texts of all states
    PERFORM read_texts_of_states.
*   read ML-data from DB
    PERFORM read_data CHANGING g_t_out.
*   filter currency
    PERFORM filter_currency USING g_t_out g_curr_info-curtp
                            CHANGING g_t_alv_list.
  ELSE.
*   load extract
    CALL FUNCTION 'REUSE_ALV_EXTRACT_LOAD'
      EXPORTING
        is_extract = gs_extract2
      IMPORTING
        es_admin   = gs_admin
      TABLES
        et_exp01   = g_t_out.

*   set global parameters
    READ TABLE g_t_out INDEX 1 INTO g_out.
    p_bwkey = g_out-bwkey.
    p_bdatj = g_out-bdatj.
    p_poper = g_out-poper.
*   read currencies of bwkey
    PERFORM read_curr_info USING p_bwkey.
*   filter currency
    PERFORM filter_currency USING g_t_out g_curr_info-curtp
                            CHANGING g_t_alv_list.
  ENDIF.

* display or save output table
  IF NOT g_t_out IS INITIAL.
*   display the list
    IF p_save IS INITIAL.
      PERFORM display_list.
*   save extract
    ELSE.
      CALL FUNCTION 'REUSE_ALV_EXTRACT_SAVE'
        EXPORTING
          is_extract = gs_extract1
        TABLES
          it_exp01   = g_t_out.
    ENDIF.
  ELSE.
    MESSAGE i114.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*&      Select materials, read ML-data and build output-table
*&---------------------------------------------------------------------*
FORM read_data
  CHANGING e_t_out TYPE t_out.

  DATA: l_cursor            TYPE cursor,
        l_xflag_end_of_data,
        l_t_stock           TYPE t_stock,
        l_t_ckmlpp          TYPE t_ckmlpp,
        l_t_ckmlcr          TYPE t_ckmlcr,
        t_t030              TYPE TABLE OF t030 WITH HEADER LINE,
        vgsber              TYPE t134g-gsber,
        wl_t134g            TYPE t134g.

  IF s_werks[] IS INITIAL.
    SELECT *
      FROM t134g
      INTO wl_t134g
      WHERE gsber IN s_gsber
      AND   spart EQ '01'.
      s_werks-low    = wl_t134g-werks.
      CLEAR s_werks-high.
      s_werks-sign   = 'I'.
      s_werks-option = 'EQ'.
      APPEND s_werks.
    ENDSELECT.

  ENDIF.

  IF s_hkont[] IS NOT INITIAL.
    SELECT *
      FROM t030
      INTO TABLE t_t030
      WHERE konts IN s_hkont.
  ENDIF.
  SORT t_t030 BY  bklas.
  DELETE ADJACENT DUPLICATES FROM t_t030 COMPARING  bklas.
  LOOP AT t_t030.
    r_bklas-low    = t_t030-bklas.
    CLEAR r_bklas-high.
    r_bklas-sign   = 'I'.
    r_bklas-option = 'EQ'.
    APPEND r_bklas.
  ENDLOOP.

* open cursor to get all materials (CKMLHD, MARA) which satisfy the
* given selection criteria
*ENHANCEMENT-SECTION RKKBMLMAT_01 SPOTS ES_RKKBMLMAT .
  OPEN CURSOR WITH HOLD l_cursor FOR
  SELECT h~bwkey h~kalnr h~mlast h~matnr h~bwtar
         h~sobkz h~vbeln h~posnr h~pspnr
         m~mtart m~matkl m~spart m~meins
     FROM ckmlhd AS h JOIN mara AS m
          ON h~matnr = m~matnr
     WHERE h~bwkey IN s_werks" P_BWKEY
           AND h~matnr IN r_matnr
           AND h~bwtar IN r_bwtar
           AND h~vbeln IN r_vbeln
           AND h~posnr IN r_posnr
           AND h~pspnr IN r_pspnr
           AND h~mlast IN r_mlast
           AND m~mtart IN r_mtart
           AND m~matkl IN r_matkl
           AND m~spart IN r_spart.
*END-ENHANCEMENT-SECTION.

  WHILE l_xflag_end_of_data = space.
    FETCH NEXT CURSOR l_cursor
          INTO CORRESPONDING FIELDS OF TABLE l_t_stock
          PACKAGE SIZE c_package_size.
    IF sy-subrc <> 0.
      l_xflag_end_of_data = 'X'.
    ELSE.
*     read val. classes for all material/sales order/project stocks
      PERFORM get_bklas CHANGING l_t_stock.
*     delete materials which do not satisfy sel.-crit. for bklas
      IF NOT r_bklas IS INITIAL.
        DELETE l_t_stock WHERE NOT bklas IN r_bklas.
      ENDIF.
*     read total records
      PERFORM read_total_records USING l_t_stock
                                 CHANGING l_t_ckmlpp l_t_ckmlcr.
*     build output table
      PERFORM build_output CHANGING l_t_stock l_t_ckmlpp l_t_ckmlcr
                                    e_t_out.
    ENDIF.
  ENDWHILE.
ENDFORM.                    "read_data

*----------------------------------------------------------------------*
*       FORM read_curr_info
*----------------------------------------------------------------------*
*       read currency informations for a val. area
*       check authority for each currency
*----------------------------------------------------------------------*
FORM read_curr_info
   USING i_bwkey           TYPE ckmlhd-bwkey.

  DATA: l_curtp_for_va   TYPE cki_ml_cty,
        l_t_curtp_for_va TYPE cki_ml_cty OCCURS 0.
  DATA: l_xauth.

  CALL FUNCTION 'GET_BWKEY_CURRENCY_INFO'
    EXPORTING
      bwkey             = i_bwkey
    TABLES
      t_curtp_for_va    = l_t_curtp_for_va
    EXCEPTIONS
      bwkey_not_found   = 1
      bwkey_not_active  = 2
      matled_not_found  = 3
      more_than_3_curtp = 4
      internal_error    = 5
      OTHERS            = 6.

* write infos into buffer
  CLEAR g_t_curr_info.
  CLEAR g_curr_info.
  g_curr_info-bwkey = i_bwkey.
  LOOP AT l_t_curtp_for_va INTO l_curtp_for_va.
*   check authority and insert currency into buffer if possible
    CALL FUNCTION 'TP_VALUATION_AUTHORITY'
      EXPORTING
        i_bwkey = i_bwkey
        i_cvtyp = l_curtp_for_va-curtp
      IMPORTING
        e_xauth = l_xauth
      EXCEPTIONS
        OTHERS  = 1.
    IF l_xauth <> space.
      MOVE-CORRESPONDING l_curtp_for_va TO g_curr_info.
      APPEND g_curr_info TO g_t_curr_info.
    ENDIF.
  ENDLOOP.

* set current currency which will be displayed first
  SORT g_t_curr_info BY bwkey curtp.
  READ TABLE g_t_curr_info INDEX 1 INTO g_curr_info.
ENDFORM.                    "read_curr_info

*----------------------------------------------------------------------*
*       FORM read_texts_of_states
*----------------------------------------------------------------------*
*       read text for each state und write this information into buffer
*----------------------------------------------------------------------*
FORM read_texts_of_states.
  DATA: l_dfies   TYPE dfies,
        l_dd07v   TYPE dd07v,
        l_t_dd07v TYPE dd07v OCCURS 0,
        l_status  TYPE s_status.

  l_dfies-fieldname = 'STATUS'.
  l_dfies-tabname = 'CKMLPP'.
  CALL FUNCTION 'G_FIELD_READ'
    EXPORTING
      fieldname  = l_dfies-fieldname
      table      = l_dfies-tabname
      text_flag  = 'X'
    IMPORTING
      field_attr = l_dfies
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  CALL FUNCTION 'DDUT_DOMVALUES_GET'
    EXPORTING
      name          = l_dfies-domname
      langu         = sy-langu
      texts_only    = 'X'
    TABLES
      dd07v_tab     = l_t_dd07v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* write states/texts into buffer
  LOOP AT l_t_dd07v INTO l_dd07v.
    l_status-status = l_dd07v-domvalue_l.
    l_status-text   = l_dd07v-ddtext.
    APPEND l_status TO g_t_status.
  ENDLOOP.
ENDFORM.                    "read_texts_of_states

*----------------------------------------------------------------------*
*       FORM read_curr_info
*----------------------------------------------------------------------*
*       filter currency
*----------------------------------------------------------------------*
FORM filter_currency
   USING i_t_out TYPE t_out
         i_curtp TYPE curtp
   CHANGING e_t_alv_list TYPE t_out.

  DATA: l_out TYPE s_out.
  DATA: w_out TYPE s_out.
  DATA  it_out TYPE STANDARD TABLE OF s_out WITH KEY kalnr.

  it_out[] = i_t_out[].
  CLEAR e_t_alv_list.
  LOOP AT i_t_out INTO l_out
          WHERE curtp = i_curtp.
    IF r_st_d = 'X'.
      READ TABLE  it_out INTO w_out WITH KEY  kalnr =  l_out-kalnr
                                              curtp = '40'.
      IF sy-subrc = 0.
        l_out-salk3_u = w_out-salk3.
        l_out-salkv_u = w_out-salkv. "PBI - 72568 - CBRAND
      ENDIF.
    ENDIF.
    APPEND l_out TO e_t_alv_list.
  ENDLOOP.
ENDFORM.                    "filter_currency

*----------------------------------------------------------------------*
*       FORM get_bklas
*----------------------------------------------------------------------*
*       read val. classes for all material/sales order/project stocks
*----------------------------------------------------------------------*
FORM get_bklas
      CHANGING et_stock       TYPE t_stock.

  FIELD-SYMBOLS: <stock> TYPE s_stock.
  DATA: lt_bklas TYPE kkbml_t_bklas,
        ls_bklas TYPE kkbml_s_bklas.


  LOOP AT et_stock ASSIGNING <stock>.
    MOVE-CORRESPONDING <stock> TO ls_bklas.
    APPEND ls_bklas TO lt_bklas.
  ENDLOOP.

  CALL FUNCTION 'K_KKB_READ_BKLAS'
    CHANGING
      ct_bklas = lt_bklas.

  SORT et_stock BY bwkey matnr bwtar sobkz vbeln posnr pspnr.
  LOOP AT et_stock ASSIGNING <stock>.
    READ TABLE lt_bklas INDEX sy-tabix
                               INTO ls_bklas.
    <stock>-bklas = ls_bklas-bklas.
  ENDLOOP.
ENDFORM.                               " GET_BKLAS

*&---------------------------------------------------------------------*
*&      Form read_total_records
*&---------------------------------------------------------------------*
FORM read_total_records
   USING    i_t_stock                TYPE t_stock
   CHANGING e_t_ckmlpp               TYPE t_ckmlpp
            e_t_ckmlcr               TYPE t_ckmlcr.

 DATA: lt_ckmlhd LIKE ckmlhd OCCURS 0,
        lt_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE,
        l_ckmlhd  LIKE ckmlhd.
  DATA: l_stock   TYPE s_stock.

  DATA: ls_ckmlhd TYPE ckmlhd,
        ls_ckmlpp TYPE ckmlpp,
        ls_ckmlcr TYPE ckmlcr,
        f_untper  TYPE poper VALUE '000',
        l_bdatj   TYPE bdatj,
        l_poper   TYPE poper.

  DATA: l_lfgja TYPE mbew-lfgja,
        l_lfmon TYPE mbew-lfmon,
        h_bdatj TYPE bdatj,
        h_poper TYPE poper.

  FIELD-SYMBOLS: <s_ckmlhd> TYPE ckmlhd,
                 <s_ckmlpp> TYPE ckmlpp,
                 <s_ckmlcr> TYPE ckmlcr.

  CLEAR: e_t_ckmlpp, e_t_ckmlcr.

* Prepare interface parameters
  LOOP AT i_t_stock INTO l_stock.
    MOVE: l_stock-kalnr TO l_ckmlhd-kalnr,
          l_stock-bwkey TO l_ckmlhd-bwkey,
          l_stock-mlast TO l_ckmlhd-mlast,
          l_stock-matnr TO l_ckmlhd-matnr,
          l_stock-bwtar TO l_ckmlhd-bwtar,
          l_stock-vbeln TO l_ckmlhd-vbeln,
          l_stock-posnr TO l_ckmlhd-posnr,
          l_stock-pspnr TO l_ckmlhd-pspnr.
    APPEND l_ckmlhd TO lt_ckmlhd.
  ENDLOOP.

* ---> S4 Migration - 06/07/2023 - DG
  DATA: wa_kalnr      TYPE ckmv0_matobj_str,
        lt_kalnr      TYPE ckmv0_matobj_tbl.


  DATA: lv_bdatj_1 TYPE  ckmlpp-bdatj,
        lv_poper_1 TYPE  ckmlpp-poper.


  lv_bdatj_1 = p_bdatj.
  lv_poper_1 = p_poper.

  LOOP AT lt_ckmlhd INTO DATA(wa_ckmlhd).
    wa_kalnr-kalnr = wa_ckmlhd-kalnr.
    APPEND wa_kalnr TO lt_kalnr.
  ENDLOOP.


  CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
    EXPORTING
      i_bdatj_1               = lv_bdatj_1
      i_poper_1               = lv_poper_1
    TABLES
      t_kalnr                 = lt_kalnr
      t_ckmlpp                = e_t_ckmlpp
      t_ckmlcr                = e_t_ckmlcr
    EXCEPTIONS
      no_data_found           = 1
      input_data_inconsistent = 2
      buffer_inconsistent     = 3
      OTHERS                  = 4.


  ls_ckmlpp-bdatj = ls_ckmlcr-bdatj = p_bdatj.
  ls_ckmlpp-poper = ls_ckmlcr-poper = p_poper.
  MODIFY e_t_ckmlpp FROM ls_ckmlpp TRANSPORTING poper bdatj WHERE kalnr NE space.
  MODIFY e_t_ckmlcr FROM ls_ckmlcr TRANSPORTING poper bdatj WHERE kalnr NE space.

  LOOP AT lt_ckmlhd ASSIGNING <s_ckmlhd> WHERE mlast = '3'.
    LOOP AT e_t_ckmlpp ASSIGNING <s_ckmlpp>
                     WHERE kalnr = <s_ckmlhd>-kalnr AND
                           status NE y_mehrstufig_abgerechnet AND
                           status NE y_abschlussbuchung_storniert AND
                           status NE y_abschlussbuchung_erfolgt.
      LOOP AT e_t_ckmlcr ASSIGNING <s_ckmlcr>
                   WHERE kalnr  EQ <s_ckmlpp>-kalnr AND
                         bdatj  EQ <s_ckmlpp>-bdatj AND
                         poper  EQ <s_ckmlpp>-poper AND
                         untper EQ <s_ckmlpp>-untper.
        IF <s_ckmlpp>-status <> y_einstufig_abgerechnet.
          CLEAR <s_ckmlcr>-ebprd_ea.
          CLEAR <s_ckmlcr>-ebkdm_ea.
          CLEAR <s_ckmlcr>-ebprd_ma.
          CLEAR <s_ckmlcr>-ebkdm_ma.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    "read_total_records

*&---------------------------------------------------------------------*
*&      Form  build_output
*&---------------------------------------------------------------------*
FORM build_output
  CHANGING i_t_stock  TYPE t_stock
           i_t_ckmlpp TYPE t_ckmlpp
           i_t_ckmlcr TYPE t_ckmlcr
           e_t_out TYPE t_out.

  DATA: l_out        TYPE s_out,
        l_stock      TYPE s_stock,
        l_ckmlpp     TYPE ckmlpp,
        l_ckmlcr     TYPE ckmlcr,
        l_makt       TYPE makt,              "note 763745
        l_indx_pp    TYPE i VALUE 1,
        l_indx_cr    TYPE i VALUE 1,
        l_status     TYPE s_status,
        l_indx_curtp TYPE i,
        l_curr_info  TYPE s_curr_info,
        l_waers_1    TYPE waers,
        l_waers_2    TYPE waers,
        l_waers_3    TYPE waers,
        vgsber       TYPE t134g-gsber.

  DATA: l_text(60).
* (at most) 3 currencies
  READ TABLE g_t_curr_info INDEX 1 INTO l_curr_info.
  l_waers_1 = l_curr_info-waers.
  READ TABLE g_t_curr_info INDEX 2 INTO l_curr_info.
  l_waers_2 = l_curr_info-waers.
  READ TABLE g_t_curr_info INDEX 3 INTO l_curr_info.
  l_waers_3 = l_curr_info-waers.

* merge the given tables together
  SORT i_t_stock BY kalnr.
  SORT i_t_ckmlpp BY kalnr.
  SORT i_t_ckmlcr BY kalnr curtp.

  LOOP AT i_t_stock INTO l_stock.
*   there is at most one ckmlpp-entry for one material
    LOOP AT i_t_ckmlpp INTO l_ckmlpp FROM l_indx_pp.
      IF l_ckmlpp-kalnr > l_stock-kalnr.
        l_indx_pp = sy-tabix.
        EXIT.
      ELSEIF l_ckmlpp-kalnr < l_stock-kalnr.
        CONTINUE.
      ENDIF.
*     check if state satisfy the given sel.-crit.
      IF NOT l_ckmlpp-status IN r_status.
        CONTINUE.
      ENDIF.
*     fill output-line so far
      MOVE-CORRESPONDING l_stock TO l_out.
      l_out-werks   = l_stock-bwkey.
      "divisão ALRS
      CLEAR vgsber.
      SELECT SINGLE gsber
        FROM t134g
        INTO vgsber
        WHERE werks = l_out-werks.
      IF s_gsber[] IS NOT INITIAL.
        IF vgsber NOT IN s_gsber.
          CONTINUE.
        ENDIF.
      ENDIF.
      l_out-gsber   = vgsber.
      l_out-lbkum   = l_ckmlpp-lbkum.
      l_out-bdatj   = l_ckmlpp-bdatj.
      l_out-poper   = l_ckmlpp-poper.
      l_out-status  = l_ckmlpp-status.
      PERFORM get_text USING 'STATUS' l_out-status
                       CHANGING l_out-status_text.
      l_indx_curtp = 0.
*     Process all currencies/valuations for one material
      LOOP AT i_t_ckmlcr INTO l_ckmlcr FROM l_indx_cr.
        ADD 1 TO l_indx_curtp.
        IF l_ckmlcr-kalnr > l_ckmlpp-kalnr.
          l_indx_cr = sy-tabix.
          EXIT.
        ELSEIF l_ckmlcr-kalnr < l_ckmlpp-kalnr.
          CLEAR: l_indx_curtp.                              "note881341
          CONTINUE.
        ENDIF.
*       fill in the keyfigures
        l_out-curtp   = l_ckmlcr-curtp.
        l_out-salk3   = l_ckmlcr-salk3.
        l_out-salkv   = l_ckmlcr-salkv.
        l_out-eb_dif   = l_ckmlcr-ebprd_ea + l_ckmlcr-ebprd_ma +
                        l_ckmlcr-ebkdm_ea + l_ckmlcr-ebkdm_ma.
        l_out-vprsv   = l_ckmlcr-vprsv.
        l_out-stprs   = l_ckmlcr-stprs.
        l_out-pvprs   = l_ckmlcr-pvprs.
        PERFORM calc_prabw_prz USING l_out-stprs l_out-pvprs
                               CHANGING l_out-prabw_prz.
        l_out-peinh   = l_ckmlcr-peinh.
        IF l_indx_curtp = 1.
          l_out-waers = l_waers_1.
        ELSEIF l_indx_curtp = 2.
          l_out-waers = l_waers_2.
        ELSEIF l_indx_curtp = 3.
          l_out-waers = l_waers_3.
        ENDIF.
*       get texts for the materials                        "note 763745
        IF l_out-matnr = l_makt-matnr.                     "note 763745
          l_out-ktext = l_makt-maktx.                     "note 763745
        ELSE.                                              "note 763745
          SELECT SINGLE matnr maktx FROM makt              "note 763745
            INTO CORRESPONDING FIELDS OF l_makt            "note 763745
            WHERE matnr = l_out-matnr                      "note 763745
            AND   spras = sy-langu.                        "note 763745
          IF sy-subrc = 0.                                 "note 763745
            l_out-ktext = l_makt-maktx.                    "note 763745
          ELSE.                                            "note 763745
            CLEAR l_makt.                                  "note 763745
            CLEAR l_out-ktext.
          ENDIF.                                           "note 763745
        ENDIF.                                             "note 763745

        "--Incluir conta razão
        SELECT SINGLE bklas
          FROM mbew
          INTO @DATA(_class) "//Classe
         WHERE matnr = @l_out-matnr.

        DATA(_plano_ctas)   = '0050'. "//Plano contas: Quadro geral
        DATA(_key_operacao) = 'BSX'.  "//Chave de operação

        SELECT SINGLE konts
          FROM t030
          INTO l_out-konts
         WHERE ktopl = _plano_ctas
           AND ktosl = _key_operacao
           AND bklas = _class.
        "--

*       append the output-line to the output-table
        APPEND l_out TO e_t_out.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

** get texts for materials               "deleted here with note 763745
*  CALL METHOD cl_kkb_texts_get=>get_matnr_text_tab
*    CHANGING ct_tab = e_t_out
*    EXCEPTIONS OTHERS = 1.

ENDFORM.                    "build_output

*&---------------------------------------------------------------------*
*&      Form  get_text
*&---------------------------------------------------------------------*
FORM get_text
  USING i_fieldname
        i_value
  CHANGING e_text.

  DATA: l_s_status TYPE s_status,
        l_status   TYPE s_status-status.

  CLEAR e_text.

  CASE i_fieldname.
    WHEN 'STATUS'.
      l_status = i_value.
      READ TABLE g_t_status INTO l_s_status
                            WITH KEY status = l_status.
      e_text = l_s_status-text.
  ENDCASE.
ENDFORM.                    "get_text

*&---------------------------------------------------------------------*
*&      Form  calc_prabw_prz
*&---------------------------------------------------------------------*
FORM calc_prabw_prz
  USING i_stprs TYPE s_out-stprs
        i_pvprs TYPE s_out-pvprs
  CHANGING e_prabw_prz TYPE s_out-prabw_prz.

  DATA: l_diff TYPE s_out-stprs.
  DATA: l_float TYPE float.

  CLEAR e_prabw_prz.
  l_diff = i_pvprs - i_stprs.
  IF l_diff <> 0.
    e_prabw_prz = 999999.
    IF i_stprs <> 0.
      l_float = ( l_diff / i_stprs ) * 100.
      IF l_float <= 999999.
        e_prabw_prz = l_float.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "calc_prabw_prz

*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
FORM display_list.
  DATA: lt_sp_groups TYPE slis_t_sp_group_alv,
        l_t_fieldcat TYPE slis_t_fieldcat_alv,
        l_layout     TYPE slis_layout_alv,
        l_t_sort     TYPE slis_t_sortinfo_alv,
        l_sort       TYPE slis_sortinfo_alv,
        l_t_event    TYPE slis_t_event,
        l_event      TYPE slis_alv_event.
  DATA: l_repid      LIKE sy-repid.

  DATA wa_zmmt0097 TYPE zmmt0097.
  DATA it_zmmt0097 TYPE TABLE OF zmmt0097.
* if sales order/project stocks do not exist => do not show the key
* fields
  FIELD-SYMBOLS: <out> TYPE s_out.
  DATA: l_xbwtar,
        l_xvbeln,
        l_xpspnr.
  LOOP AT g_t_alv_list ASSIGNING <out>.
    IF NOT <out>-bwtar IS INITIAL.
      l_xbwtar = 'X'.
    ENDIF.
    IF NOT <out>-vbeln IS INITIAL.
      l_xvbeln = 'X'.
    ENDIF.
    IF NOT <out>-pspnr IS INITIAL.
      l_xpspnr = 'X'.
    ENDIF.
  ENDLOOP.

* field groups
  PERFORM set_fieldgroups CHANGING lt_sp_groups.

* fieldcat
  PERFORM build_fieldcat USING l_xbwtar l_xvbeln l_xpspnr
                         CHANGING l_t_fieldcat.

* layout
  l_layout-colwidth_optimize = 'X'.
  l_layout-get_selinfos = 'X'.
  l_layout-f2code = 'DETAIL'.

* Events
  l_event-name = slis_ev_top_of_page.
  l_event-form = 'ALV_TOP_OF_PAGE'.
  APPEND l_event TO l_t_event.

* Sort
  l_sort-fieldname = 'WERKS'.
  l_sort-spos = 0.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  APPEND l_sort TO l_t_sort.

  l_sort-fieldname = 'MTART'.
  l_sort-spos = 1.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  APPEND l_sort TO l_t_sort.

  CLEAR l_sort.
  l_sort-fieldname = 'BKLAS'.
  l_sort-spos = 2.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  APPEND l_sort TO l_t_sort.

  CLEAR l_sort.
  l_sort-fieldname = 'MATNR'.
  l_sort-spos = 3.
  l_sort-up = 'X'.
  APPEND l_sort TO l_t_sort.

  CLEAR l_sort.
  l_sort-fieldname = 'BWTAR'.
  l_sort-spos = 4.
  l_sort-up = 'X'.
  APPEND l_sort TO l_t_sort.

  CLEAR l_sort.
  l_sort-fieldname = 'VBELN'.
  l_sort-spos = 5.
  l_sort-up = 'X'.
  APPEND l_sort TO l_t_sort.

  CLEAR l_sort.
  l_sort-fieldname = 'POSNR'.
  l_sort-spos = 6.
  l_sort-up = 'X'.
  APPEND l_sort TO l_t_sort.

  CLEAR l_sort.
  l_sort-fieldname = 'PSPNR'.
  l_sort-spos = 7.
  l_sort-up = 'X'.
  APPEND l_sort TO l_t_sort.

* call ALV
  l_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     i_interface_check        = 'X'
      i_callback_program       = l_repid
      i_callback_pf_status_set = 'ALV_PF_STATUS_SET'
      i_callback_user_command  = 'ALV_USER_COMMAND'
*     I_STRUCTURE_NAME         =
      is_layout                = l_layout
      it_fieldcat              = l_t_fieldcat
*     IT_EXCLUDING             =
      it_special_groups        = lt_sp_groups
      it_sort                  = l_t_sort
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
      is_variant               = is_variant
      it_events                = l_t_event
    TABLES
      t_outtab                 = g_t_alv_list
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF sy-batch = 'X' AND r_fisco = 'X'.
    DELETE FROM zmmt0097.
    COMMIT WORK.
    REFRESH it_zmmt0097.
    LOOP AT g_t_alv_list INTO DATA(w_t_alv_list).
      SELECT SINGLE j_1bbranch
        FROM t001w
        INTO wa_zmmt0097-bukrs
       WHERE werks = w_t_alv_list-werks.

      SELECT SINGLE vkorg
        FROM t001w
        INTO wa_zmmt0097-bukrs
       WHERE werks = wa_zmmt0097-bukrs.
      wa_zmmt0097-werks = w_t_alv_list-werks.
      wa_zmmt0097-matnr = w_t_alv_list-matnr.
      wa_zmmt0097-meins = w_t_alv_list-meins.
      wa_zmmt0097-clabs = w_t_alv_list-lbkum.
      wa_zmmt0097-data  = sy-datum.
      wa_zmmt0097-hora  = sy-uzeit.
      APPEND wa_zmmt0097 TO it_zmmt0097.
    ENDLOOP.
    MODIFY zmmt0097 FROM TABLE it_zmmt0097.
    COMMIT WORK.

  ENDIF.
ENDFORM.                    "display_list

*&---------------------------------------------------------------------*
*&      Form  set_fieldgroups
*&---------------------------------------------------------------------*
FORM set_fieldgroups
  CHANGING ct_sp_groups TYPE slis_t_sp_group_alv.

  DATA: lf_sp_group TYPE slis_sp_group_alv.

  lf_sp_group-sp_group = '0001'.
  APPEND lf_sp_group TO ct_sp_groups.
  lf_sp_group-sp_group = '0002'.
  APPEND lf_sp_group TO ct_sp_groups.
  lf_sp_group-sp_group = '0003'.
  APPEND lf_sp_group TO ct_sp_groups.
  lf_sp_group-sp_group = '0004'.
  APPEND lf_sp_group TO ct_sp_groups.
  lf_sp_group-sp_group = '0005'.
  APPEND lf_sp_group TO ct_sp_groups.
  lf_sp_group-sp_group = '0006'.
  APPEND lf_sp_group TO ct_sp_groups.

  CALL FUNCTION 'SET_FIELDGROUP_TEXTS'
    CHANGING
      ct_special_groups = ct_sp_groups.
ENDFORM.                    "set_fieldgroups

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
FORM build_fieldcat
  USING i_xbwtar i_xvbeln i_xpspnr
  CHANGING e_t_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: l_fieldcat TYPE slis_fieldcat_alv,
        l_col_pos  TYPE i.

  CLEAR l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'WERKS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'WERKS'.
  l_fieldcat-ref_tabname = 'T001W'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-sp_group = '0005'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'GSBER'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'GSBER'.
  l_fieldcat-ref_tabname = 'T134G'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-sp_group = '0005'.
  APPEND l_fieldcat TO e_t_fieldcat.


  CLEAR l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'MTART'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MTART'.
  l_fieldcat-ref_tabname = 'MARA'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-sp_group = '0005'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'SPART'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'SPART'.
  l_fieldcat-ref_tabname = 'MARA'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0005'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'MATKL'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MATKL'.
  l_fieldcat-ref_tabname = 'MARA'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0005'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'BKLAS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'BKLAS'.
  l_fieldcat-ref_tabname = 'MBEW'.
  l_fieldcat-key = ' '.
  l_fieldcat-sp_group = '0005'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'MATNR'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MATNR'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  l_fieldcat-sp_group = '0005'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'KTEXT'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MAKTX'.
  l_fieldcat-ref_tabname = 'MAKT'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0005'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'BWTAR'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'BWTAR'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  IF i_xbwtar IS INITIAL.
    l_fieldcat-no_out = 'X'.
  ENDIF.
  l_fieldcat-sp_group = '0005'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'VBELN'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'VBELN'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  l_fieldcat-ddictxt = 'L'.
  IF i_xvbeln IS INITIAL.
    l_fieldcat-no_out = 'X'.
  ENDIF.
  l_fieldcat-sp_group = '0004'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'POSNR'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'POSNR'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  IF i_xvbeln IS INITIAL.
    l_fieldcat-no_out = 'X'.
  ENDIF.
  l_fieldcat-sp_group = '0004'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'PSPNR'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'PSPNR'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  l_fieldcat-ddictxt = 'L'.
  IF i_xpspnr IS INITIAL.
    l_fieldcat-no_out = 'X'.
  ENDIF.
  l_fieldcat-sp_group = '0004'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'LBKUM'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'LBKUM'.
  l_fieldcat-ref_tabname = 'CKMLPP'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-qfieldname = 'MEINS'.
  l_fieldcat-sp_group = '0003'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'MEINS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MEINS'.
  l_fieldcat-ref_tabname = 'MARA'.
  l_fieldcat-sp_group = '0005'.
*ENHANCEMENT-POINT RKKBMLMAT_02 SPOTS ES_RKKBMLMAT .
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'SALK3'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'SALK3'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-cfieldname = 'WAERS'.
  l_fieldcat-do_sum = 'X'.
  l_fieldcat-sp_group = '0001'.
*  L_FIELDCAT-OUTPUTLEN = 15.
  CONCATENATE 'Vlr Total' g_curr_info-waers INTO  l_fieldcat-seltext_l SEPARATED BY space.
  l_fieldcat-seltext_s = l_fieldcat-seltext_l.
  l_fieldcat-seltext_m = l_fieldcat-seltext_l.
  APPEND l_fieldcat TO e_t_fieldcat.

  "SALK3_U
  IF r_st_d = 'X'.
    CLEAR l_fieldcat.
    l_col_pos = l_col_pos + 1.
    l_fieldcat-fieldname = 'SALK3_U'.
    l_fieldcat-tabname = 'G_T_ALV_LIST'.
    l_fieldcat-ref_fieldname = 'SALK3'.
    l_fieldcat-ref_tabname = 'CKMLCR'.
    l_fieldcat-cfieldname = 'WAERS'.
    l_fieldcat-do_sum = 'X'.
    l_fieldcat-sp_group = '0001'.
*    L_FIELDCAT-OUTPUTLEN = 15.
    CONCATENATE 'Vlr Total' 'USD' INTO  l_fieldcat-seltext_l SEPARATED BY space.
    l_fieldcat-seltext_s = l_fieldcat-seltext_l.
    l_fieldcat-seltext_m = l_fieldcat-seltext_l.
    APPEND l_fieldcat TO e_t_fieldcat.
  ENDIF.

  IF r_st_d = 'X'.
    CLEAR l_fieldcat.
    l_col_pos = l_col_pos + 1.
    l_fieldcat-fieldname = 'SALKV_U'.
    l_fieldcat-tabname = 'G_T_ALV_LIST'.
    l_fieldcat-ref_fieldname = 'SALK3'.
    l_fieldcat-ref_tabname = 'CKMLCR'.
    l_fieldcat-cfieldname = 'WAERS'.
    l_fieldcat-do_sum = 'X'.
    l_fieldcat-sp_group = '0001'.
*    L_FIELDCAT-OUTPUTLEN = 15.
    CONCATENATE 'Valor/Preço PIP' 'USD' INTO  l_fieldcat-seltext_l SEPARATED BY space.
    l_fieldcat-seltext_s = l_fieldcat-seltext_l.
    l_fieldcat-seltext_m = l_fieldcat-seltext_l.
    APPEND l_fieldcat TO e_t_fieldcat.
  ENDIF.



  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'SALKV'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'SALKV'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-cfieldname = 'WAERS'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0001'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'EB_DIF'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'EB_DIF'.
  l_fieldcat-ref_tabname = 'CKI_DOC_ML'.
  l_fieldcat-cfieldname = 'WAERS'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0002'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'MLAST'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MLAST'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0001'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'STATUS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'STATUS'.
  l_fieldcat-ref_tabname = 'CKMLPP'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0006'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'KONTS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'KONTS'.
  l_fieldcat-ref_tabname = 'T030'.
  l_fieldcat-sp_group = '0006'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'STATUS_TEXT'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'STATUS'.
  l_fieldcat-ref_tabname = 'CKMLPP'.
  l_fieldcat-seltext_l = 'Periodenstatus Text'(011).
  l_fieldcat-outputlen = 25.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0006'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'VPRSV'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'VPRSV'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-sp_group = '0001'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'STPRS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'STPRS'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-cfieldname = 'WAERS'.
  l_fieldcat-no_sum = 'X'.
  l_fieldcat-sp_group = '0001'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'PVPRS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'PVPRS'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-cfieldname = 'WAERS'.
  l_fieldcat-no_sum = 'X'.
  l_fieldcat-sp_group = '0001'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'PRABW_PRZ'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'PRABW_PRZ'.
  l_fieldcat-ref_tabname = 'CKML_RUN_ALV_LIST01 '.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-no_sum = 'X'.
  l_fieldcat-sp_group = '0001'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'PEINH'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'PEINH'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-no_sum = 'X'.
  l_fieldcat-sp_group = '0001'.
  APPEND l_fieldcat TO e_t_fieldcat.

  CLEAR l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'WAERS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'WAERS'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-sp_group = '0001'.
  APPEND l_fieldcat TO e_t_fieldcat.
ENDFORM.                    "build_fieldcat

*---------------------------------------------------------------------*
*       FORM ALV_PF_STATUS_SET
*---------------------------------------------------------------------*
FORM alv_pf_status_set
USING rt_extab TYPE slis_t_extab.
* perform alv_exclude_fcode using '&LFO' changing rt_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.                    "alv_pf_status_set

*---------------------------------------------------------------------*
*       FORM alv_exclude_fcode
*---------------------------------------------------------------------*
FORM alv_exclude_fcode
USING i_fcode
CHANGING e_t_extab TYPE slis_t_extab.
  DATA: l_extab TYPE slis_extab.
  l_extab-fcode = i_fcode.
  COLLECT l_extab INTO e_t_extab.
ENDFORM.                    "alv_exclude_fcode

*---------------------------------------------------------------------*
*       FORM ALV_USER_COMMAND
*---------------------------------------------------------------------*
FORM alv_user_command
USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN 'CURR'.
      PERFORM select_currency.
      rs_selfield-refresh = 'X'.
      rs_selfield-col_stable = 'X'.
      rs_selfield-row_stable = 'X'.
    WHEN 'DETAIL'.
      PERFORM display_detail USING rs_selfield.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "alv_user_command

*---------------------------------------------------------------------*
*       FORM select_currency
*---------------------------------------------------------------------*
*       (1) display popup to choose a currency
*       (2) modify alv-list according to this currency
*---------------------------------------------------------------------*
FORM select_currency.

  DATA: l_curr_info TYPE s_curr_info,
        l_curtp     TYPE curtp.
  DATA: BEGIN OF l_value,
          curtp TYPE cki_ml_cty-curtp,
          text  TYPE cki_ml_cty-text,
          waers TYPE cki_ml_cty-waers,
        END OF l_value,
        l_t_value  LIKE l_value OCCURS 0,
        l_return   TYPE ddshretval,
        l_t_return TYPE ddshretval OCCURS 0.

* (1) display popup to choose a currency
  LOOP AT g_t_curr_info INTO l_curr_info.
    MOVE-CORRESPONDING l_curr_info TO l_value.
    APPEND l_value TO l_t_value.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CURTP'
      value_org       = 'S'
    TABLES
      value_tab       = l_t_value
      return_tab      = l_t_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* (2) modify alv-list according to this currency
  IF sy-subrc = 0.
    READ TABLE l_t_return INDEX 1 INTO l_return.
    IF sy-subrc = 0.
      l_curtp = l_return-fieldval.
      READ TABLE g_t_curr_info INTO g_curr_info
                               WITH KEY curtp = l_curtp.
      PERFORM filter_currency USING g_t_out g_curr_info-curtp
                              CHANGING g_t_alv_list.
    ENDIF.
  ENDIF.
ENDFORM.                    "select_currency

*---------------------------------------------------------------------*
*       FORM display_detail
*---------------------------------------------------------------------*
*       show ML-data for the selected material
*---------------------------------------------------------------------*
FORM display_detail
  USING i_selfield TYPE slis_selfield.

  DATA: l_out TYPE s_out.

  IF i_selfield-tabindex > 0.
    READ TABLE g_t_alv_list INDEX i_selfield-tabindex INTO l_out.
    IF sy-subrc = 0.
      CALL FUNCTION 'CKM8_ML_DATA_DISPLAY'
        EXPORTING
          i_matnr = l_out-matnr
          i_bwkey = l_out-bwkey
          i_bwtar = l_out-bwtar
          i_vbeln = l_out-vbeln
          i_posnr = l_out-posnr
          i_pspnr = l_out-pspnr
          i_bdatj = l_out-bdatj
          i_poper = l_out-poper
          i_curtp = g_curr_info-curtp.
    ENDIF.
  ELSE.
  ENDIF.
ENDFORM.                    "display_detail

*---------------------------------------------------------------------*
*       FORM ALV_TOP_OF_page                                          *
*---------------------------------------------------------------------*
FORM alv_top_of_page.
  DATA: lf_comment TYPE slis_listheader,
        lt_comment TYPE slis_t_listheader,
        l_jahrper  TYPE jahrper.

* Bewertungsebene
  CLEAR: lf_comment.
  lf_comment-typ = 'S'.
  CASE tcurm-bwkrs_cus.                "Bewertungsebene
    WHEN 1.
*      WRITE 'Werk'(004) TO LF_COMMENT-KEY.
    WHEN 3.
      WRITE 'Buchungskreis'(005) TO lf_comment-key.
    WHEN OTHERS.
      WRITE '? ? ? ? ? ?'(006) TO lf_comment-key.
  ENDCASE.
*  WRITE P_BWKEY TO LF_COMMENT-INFO.
*  APPEND LF_COMMENT TO LT_COMMENT.

* Periode
  CLEAR: lf_comment.
  lf_comment-typ = 'S'.
  WRITE 'Periode'(002) TO lf_comment-key.
  CALL FUNCTION 'K_KKB_JAHRPER_SET'
    EXPORTING
      i_jahr    = p_bdatj
      i_periode = p_poper
    IMPORTING
      o_jahrper = l_jahrper.
  WRITE l_jahrper TO lf_comment-info.
  APPEND lf_comment TO lt_comment.

* Währung/Bewertung
  CLEAR: lf_comment.
  lf_comment-typ = 'S'.
  WRITE 'Währung/Bewertung'(007) TO lf_comment-key.
  WRITE g_curr_info-waers TO lf_comment-info.
  WRITE g_curr_info-text TO lf_comment-info+6.
  APPEND lf_comment TO lt_comment.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_comment
*     I_LOGO             =
    .
ENDFORM.                    "alv_top_of_page

*---------------------------------------------------------------------*
*       FORM alv_caller_exit_at_start
*---------------------------------------------------------------------*
FORM alv_caller_exit_at_start.

  CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_GET'
*      IMPORTING
*           ES_LAYOUT                  =
*           ET_FIELDCAT                =
*           ET_SORT                    =
*           ET_FILTER                  =
*           ES_LIST_SCROLL             =
*           ES_VARIANT                 =
*           E_WIDTH                    =
*           ET_MARKED_COLUMNS          =
*           ET_FILTERED_ENTRIES        =
*           ET_FILTERED_ENTRIES_HEADER =
*           ET_FILTERED_ENTRIES_ITEM   =
*      TABLES
*           ET_OUTTAB                  =
*           ET_OUTTAB_HEADER           =
*           ET_OUTTAB_ITEM             =
*           ET_COLLECT00               =
*           ET_COLLECT01               =
*           ET_COLLECT02               =
*           ET_COLLECT03               =
*           ET_COLLECT04               =
*           ET_COLLECT05               =
*           ET_COLLECT06               =
*           ET_COLLECT07               =
*           ET_COLLECT08               =
*           ET_COLLECT09               =
*      EXCEPTIONS
*           NO_INFOS                   = 1
*           PROGRAM_ERROR              = 2
*           OTHERS                     = 3
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "alv_caller_exit_at_start

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command.

  CASE sscrfields-ucomm.
    WHEN 'EXTR'.
* extracts for alv-list
      CALL SELECTION-SCREEN '4500' STARTING AT 1 1.
    WHEN 'EXON'.
* extended selections ON
      p_ex_sel = 'X'.
    WHEN 'EXOFF'.
* extended selections OFF
      CLEAR p_ex_sel.
    WHEN 'ONLI'.
      IF  sy-batch NE 'X'.
        AUTHORITY-CHECK OBJECT 'K_ML_VA'
        ID 'BWKEY' FIELD p_bwkey
        ID 'ACTVT' FIELD '03'.
        IF sy-subrc NE 0.
          MESSAGE e054(c+) WITH p_bwkey. "Keine Berechtigung zum
          "Ausführen
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                               " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
FORM f4_for_variant.
  DATA: locl_variant LIKE disvariant.

  CLEAR exit.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = is_variant
      i_save     = 'A'
    IMPORTING
      e_exit     = exit
      es_variant = locl_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'  NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF exit = space.
      p_varian = locl_variant-variant.
      p_vartxt = locl_variant-text.
      MOVE locl_variant TO is_variant.
    ENDIF.
  ENDIF.

ENDFORM.                               " F4_FOR_VARIANT
*&----------------------------------------------------------------------
*&      Form  VARIANT_CHECK_EXISTENCE
*&----------------------------------------------------------------------
FORM variant_check_existence.
  DATA: locl_variant LIKE disvariant.

  IF NOT p_varian IS INITIAL.
    MOVE is_variant TO locl_variant.
    MOVE p_varian TO locl_variant-variant.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = locl_variant.
    is_variant = locl_variant.
    p_vartxt = is_variant-text.
  ELSE.
    CLEAR is_variant. CLEAR p_vartxt.
    is_variant-report = sy-repid.
    is_variant-username = sy-uname.
  ENDIF.

ENDFORM.                               "VARTIANT_CHECK_EXISTENCE
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM modify_selection_screen.
  LOOP AT SCREEN.

    IF screen-name = 'P_CURTP'.
      IF s_werks[] IS INITIAL.
        screen-input     = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF p_load NE 'X' AND p_save NE 'X'.
      IF screen-group1(2) = 'EX'.
        screen-invisible = '1'.
        screen-input     = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ELSEIF p_load = 'X'.
      IF screen-group1 = 'EXT' .
        screen-invisible = '0'.
        screen-active    = '1'.
        screen-input     = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ELSEIF screen-group1 NE 'VAR' AND screen-group1(2) NE 'EX'
                                    AND screen-group3 NE 'BLK'.
        screen-invisible = '1'.
        screen-input     = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ELSEIF p_save = 'X'.
      IF screen-group1(2) = 'EX' .
        screen-invisible = '0'.
        screen-active    = '1'.
        screen-input     = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                               " MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  MODIFY_OBJECT_SELECTION
*&---------------------------------------------------------------------*
*      -->P_EX_SEL  text
*----------------------------------------------------------------------*
FORM modify_object_selection TABLES &r_vbeln STRUCTURE r_vbeln
                                    &r_posnr STRUCTURE r_posnr
                                    &r_pspnr STRUCTURE r_pspnr
                                    &r_mtart STRUCTURE r_mtart
                                    &r_bklas STRUCTURE r_bklas
                                    &r_matkl STRUCTURE r_matkl
                                    &r_spart STRUCTURE r_spart
*                                    &r_error STRUCTURE r_error
                             USING  display TYPE c.

  IF display EQ space.
* clear the select options, that are not displayed
    REFRESH: &r_vbeln,
             &r_posnr,
             &r_pspnr,
             &r_mtart,
             &r_bklas,
             &r_matkl,
             &r_spart.
*            &r_error.
    CLEAR: &r_vbeln,
           &r_posnr,
           &r_pspnr,
           &r_mtart,
           &r_bklas,
           &r_matkl,
           &r_spart.
*          &r_error.
    SET PARAMETER ID 'MTA' FIELD &r_mtart.
* the next one dosn't exist anymore!?
*    SET PARAMETER ID 'MTL' FIELD &r_matkl.
    SET PARAMETER ID 'SPA' FIELD &r_spart.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'PUK'.
        screen-invisible = '1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                               " MODIFY_OBJECT_SELECTION

*---------------------------------------------------------------------*
*       FORM CHECK_BWKEY                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_BWKEY                                                       *
*---------------------------------------------------------------------*
FORM check_bwkey USING p_bwkey.

  SELECT SINGLE * FROM t001k WHERE bwkey = p_bwkey.
  IF sy-subrc NE 0.
    MESSAGE e316(c+) WITH p_bwkey.     "bwkey not found
  ENDIF.
  IF t001k-mlbwa IS INITIAL.
    MESSAGE e012(c+) WITH p_bwkey.     "ML is not active
  ENDIF.

ENDFORM.                               "CHECK_BWKEY
*---------------------------------------------------------------------*
*       FORM CHECK_BWKEY                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_BWKEY                                                       *
*---------------------------------------------------------------------*
FORM check_bwkey_werks.

  SELECT SINGLE * FROM t001k WHERE bwkey IN s_werks.
  IF sy-subrc NE 0.
    MESSAGE e316(c+) WITH p_bwkey.     "bwkey not found
  ENDIF.
  IF t001k-mlbwa IS INITIAL.
    MESSAGE e012(c+) WITH p_bwkey.     "ML is not active
  ENDIF.

ENDFORM.                               "CHECK_BWKEY

*----------------------------------------------------------------------*
*      FORM  set_period
*----------------------------------------------------------------------*
*      check entered period
*----------------------------------------------------------------------*
FORM set_period
  CHANGING e_poper TYPE poper
           e_bdatj TYPE bdatj.

  DATA: l_cu_poper TYPE poper,
        l_cu_bdatj TYPE bdatj.

  IF e_poper IS INITIAL OR e_bdatj IS INITIAL.
    PERFORM get_current_period CHANGING l_cu_poper l_cu_bdatj.
    e_bdatj = l_cu_bdatj.
    IF e_poper IS INITIAL.
      e_poper = l_cu_poper.
    ENDIF.
  ENDIF.
ENDFORM.                    "set_period

*----------------------------------------------------------------------*
*      FORM  get_current_period
*----------------------------------------------------------------------*
*      determine current period of bukrs
*----------------------------------------------------------------------*
FORM  get_current_period
  CHANGING e_poper TYPE poper
           e_bdatj TYPE bdatj.

  IF marv-bukrs <> t001k-bukrs.
    SELECT SINGLE * FROM marv WHERE bukrs = t001k-bukrs.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDIF.
  MOVE: marv-lfmon TO e_poper,
        marv-lfgja TO e_bdatj.
ENDFORM.                    "get_current_period


INCLUDE rckm_va_bukrs_werks.
