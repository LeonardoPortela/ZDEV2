************************************************************************
* DOCUMENTATION BOX
************************************************************************
* PROGRAMA      : ZFIY0010
* MODULO SAP    : FI
* TITULO        : Aplicativo SIFERE
* TIPO          : R
* COPIA         : J_1AF016
************************************************************************
* MODIFICACIONES
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************
REPORT ZFIY0010 NO STANDARD PAGE HEADING
                LINE-SIZE  215
                LINE-COUNT 65
                MESSAGE-ID 8a.

*&---------------------------------------------------------------------*
*& Title       : Withholdings and Perceptions - RG 4110
*&---------------------------------------------------------------------*
*& History of earlier changes of the program.                          *
*&                                                                     *
*& Program description:The report informs the DGI about all            *
*& withholdings (only AP) and perceptions (AR and AP) applied by your  *
*& company. The list shows the data concerning the vendor/customer and *
*& the document. The amounts are printed in local currency and with    *
*& signs. A total is printed per regime and run.                       *
*&                                                                     *
*& Conversion from the classic list into ALV list.                     *
*&                                                                     *
*& ALV Funtion module used in program as follows                       *
*& 1. REUSE_ALV_HIERSEQ_LIST_DISPLAY                                   *
*& 2. REUSE_ALV_LIST_DISPLAY
*&---------------------------------------------------------------------*

INCLUDE ZFIY0010_top.
INCLUDE ZFIY0010_sel.
INCLUDE ZFIY0010_f01.

*&---------------------------------------------------------------------*
*         INITIALIZATION.
*&---------------------------------------------------------------------*
INITIALIZATION.

  gv_repid = sy-repid. " Initialization report name

  CLEAR gs_keyinfo.
  gs_keyinfo-header01 = 'BELNR'.
  gs_keyinfo-item01   = 'BELNR'.
  gs_keyinfo-header02 = 'GJAHR'.
  gs_keyinfo-item02   = 'GJAHR'.
  gs_keyinfo-header03 = 'HKONT'.
  gs_keyinfo-item03   = 'HKONT'.
  gs_keyinfo-header04 = 'NAME1'.
  gs_keyinfo-item04   = 'NAME1'.


*----------------------------------------------------------------------*
*  Checking the parameters                                             *
*----------------------------------------------------------------------*
* ----------------------------------------------------------------------
* AT SELECTION-SCREEN.
* ----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
*  Grisar campos
  PERFORM f_grisado_campos.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fila.
  PERFORM  file_f4  CHANGING p_fila.



AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ktosl-low.
  CALL FUNCTION 'J_1A_HELP_KTOSL'
    EXPORTING
      display = ' '
    IMPORTING
      e_ktosl = s_ktosl-low
    EXCEPTIONS
      OTHERS  = 1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ktosl-high.
  CALL FUNCTION 'J_1A_HELP_KTOSL'
    EXPORTING
      display = ' '
    IMPORTING
      e_ktosl = s_ktosl-high
    EXCEPTIONS
      OTHERS  = 1.


AT SELECTION-SCREEN.
  PERFORM check_selection_screen.

*  ALV Changes starts

* Get the possible values for the detail list
AT SELECTION-SCREEN ON VALUE-REQUEST FOR par_var1.

  PERFORM variant_f4_help USING      gc_handle1
                          CHANGING   par_var1.

* Get the possible values for the company code totals
AT SELECTION-SCREEN ON VALUE-REQUEST FOR par_var2.

  PERFORM variant_f4_help USING     gc_handle2
                          CHANGING  par_var2.

* Validate the variant given on the selection screen for detail list
AT SELECTION-SCREEN ON par_var1.

* Check for the variant ..
  PERFORM check_variant_existance USING gc_handle1
                                        par_var1.
* Fill output internal tables with dummy data ..
  IF sscrfields-ucomm = 'CON1'.

    REFRESH gt_header.
    DO 3 TIMES.
      CALL FUNCTION 'INITIALIZE_STRUCTURE'
        EXPORTING
          i_n_fill   = 0
          i_i_fill   = 0
        CHANGING
          c_workarea = gs_header.
      APPEND gs_header TO gt_header.
    ENDDO.

    REFRESH gt_item.
    DO 3 TIMES.
      CALL FUNCTION 'INITIALIZE_STRUCTURE'
        EXPORTING
          i_n_fill   = 0
          i_i_fill   = 0
        CHANGING
          c_workarea = gs_item.
      APPEND gs_item TO gt_item.
    ENDDO.

*   Field catalog for layout variant data
    PERFORM fieldcat_build_alv  USING  gc_struct1 gc_struct2
                                CHANGING gt_fieldcat.
*   Note # 970568 Begin ****
    gs_variant-handle = gc_handle1.
**Note 1021760 Begins
    gs_variant-report = sy-repid.
    gs_variant-variant = par_var1.
**Note 1021760 Ends
*   Note # 970568 End ****
    CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
      EXPORTING
        i_callback_program = gv_repid
        it_fieldcat        = gt_fieldcat
        i_save             = gc_save
        is_variant         = gs_variant
        i_tabname_header   = gt_tab_header
        i_tabname_item     = gt_tab_item
        is_keyinfo         = gs_keyinfo
      TABLES
        t_outtab_header    = gt_header
        t_outtab_item      = gt_item
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    REFRESH: gt_header,
             gt_item,
             gt_fieldcat.

  ENDIF.

* Validate the variant given on the selection screen for totals
AT SELECTION-SCREEN ON par_var2.

* Check for the variant ..
  PERFORM check_variant_existance USING gc_handle2
                                        par_var2.

* Fill output internal table with dummy data ..
  IF sscrfields-ucomm = 'CON2'.
    REFRESH gt_tot_code.
    DO 3 TIMES.
      CALL FUNCTION 'INITIALIZE_STRUCTURE'
        EXPORTING
          i_n_fill   = 0
          i_i_fill   = 0
        CHANGING
          c_workarea = gs_tot_code.
      APPEND gs_tot_code TO gt_tot_code.
    ENDDO.

*  ****Field catalog build for layout variant data
    PERFORM fieldcat_build_alv_1 CHANGING gt_fieldcat_1.
*   Note # 970568 Begin ****
    gs_variant-handle = gc_handle2.
*   Note # 970568 End ****

*  ****Field list display for layout variant data
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program = gv_repid
        it_fieldcat        = gt_fieldcat_1
        i_save             = gc_save
        is_variant         = gs_variant
        it_events          = gt_events_1
      TABLES
        t_outtab           = gt_tot_code
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  REFRESH: gt_fieldcat_1,
           gt_tot_code.


* -------------------------------------------------------------------- *
*   START-OF-SELECTION                                                 *
* -------------------------------------------------------------------- *
START-OF-SELECTION.
  IF rb_003 EQ 'X'.
    IF s_hkont IS INITIAL.
      MESSAGE s000(z_fi) WITH 'Completar la Cuenta ' DISPLAY LIKE 'E'..
      EXIT.
    ENDIF.
  ENDIF.

  IF p_baja EQ 'X'.
    IF p_fila IS INITIAL.
      MESSAGE s454(zerror) WITH 'Completar fichero Local' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  b0sg-xdocc = 'X'.          "Note 1063453
* table for listheader
  CLEAR bhdgd.
  bhdgd-inifl = 0.
  bhdgd-lines = 95.
  sy-title = text-h01.
  bhdgd-uname = sy-uname.
  bhdgd-repid = sy-repid.
  bhdgd-line1 = text-h01.
  bhdgd-line2 = text-h02.
  bhdgd-bukrs = br_bukrs-low.

  READ TABLE br_bukrs INDEX 1.

  SELECT SINGLE * FROM t007f WHERE umkrs = br_bukrs-low.

  IF sy-subrc = 0.
    holding_sel = 'X'.

    SELECT * FROM t001 WHERE umkrs = t007f-umkrs.
      CHECK t001-bukrs NE t001-umkrs.
      br_bukrs-low = br_bukrs-high = t001-bukrs.
      APPEND br_bukrs.
    ENDSELECT.
    SELECT SINGLE kalsm INTO t005-kalsm FROM t005
           WHERE land1 = t001-land1 .
    SELECT SINGLE * FROM t001z WHERE bukrs = bkpf-bukrs AND
                                         party = 'J1AIDN'.
  ENDIF.

  PERFORM set_dates USING from_date br_budat-low.

  IF NOT br_budat-high IS INITIAL.
    PERFORM set_dates USING to_date br_budat-high.
  ELSE.
    to_date = from_date.
  ENDIF.

  IF from_date IS INITIAL AND
     to_date   IS INITIAL.
    set_date = 'X'.
  ENDIF.

  SELECT SINGLE * INTO t001
         FROM t001 WHERE bukrs = br_bukrs-low .

  SELECT SINGLE kalsm INTO t005-kalsm
         FROM t005 WHERE land1 = t001-land1   .

* ---> Header info: Company Tax code
  SELECT SINGLE paval INTO hd_ccuit
         FROM t001z WHERE bukrs = br_bukrs-low
                      AND party = 'J1AIDN' .
  IF NOT sy-subrc IS INITIAL OR
     hd_ccuit IS INITIAL .
* ---> NO HAY CUIT PARA LA COMPANY
  ENDIF .

  IF t001-umkrs NE space AND
     t001-bukrs NE t001-umkrs.
    holding_act = 'X'.
  ENDIF.
* ---> Header info: Company Name
  hd_cname = t001-butxt .

* ---> Get all document values to local memory
  SELECT * FROM t003_i INTO TABLE t_doctyp
          WHERE land1 = t001-land1 .

  SELECT * FROM j_1aotdetr INTO TABLE t_doclas
          WHERE land1 = t001-land1
            AND id_report = sy-repid .

* ---> Get all official tax codes to local memory
  SELECT * FROM j_1atpkof
      INTO CORRESPONDING FIELDS OF TABLE t_offtyp
          WHERE land1 = t001-land1 .

  SELECT j_1ataxcod text40 FROM j_1atxoff
      INTO CORRESPONDING FIELDS OF t_offtyp
      FOR ALL ENTRIES IN t_offtyp
          WHERE land1 = t001-land1
            AND j_1ataxcod = t_offtyp-j_1ataxcod .
    MODIFY t_offtyp TRANSPORTING text40
                    WHERE j_1ataxcod = t_offtyp-j_1ataxcod .
  ENDSELECT .

* ---> Get all official condition codes to local memory
  SELECT * FROM j_1afitpvt
      INTO CORRESPONDING FIELDS OF TABLE t_concod
          WHERE spras = sy-langu   .

* ---> Get all tax code groups to local memory
  SELECT * FROM t007c
      INTO CORRESPONDING FIELDS OF TABLE t_txgrp
          WHERE koart = 'D'        .

*----------------------------------------------------------------------*
*  SELECTION                                                           *
*----------------------------------------------------------------------*
GET bkpf.

  APPEND bkpf TO t_bkpf.

  IF rb_003 NE 'X'.

    CLEAR :  detail,
              x_cm .

    CHECK: bkpf-tcode NE 'FBA3',
           bkpf-tcode NE 'FBA8',
           bkpf-tcode NE 'FBB1'.


    IF set_date NE space.
      IF bkpf-budat <  f_budat OR
         f_budat    IS INITIAL.
        f_budat = bkpf-budat.
      ENDIF.

      IF bkpf-budat > t_budat.
        t_budat = bkpf-budat.
      ENDIF.
    ENDIF.

    READ TABLE t_doctyp WITH KEY land1 = t001-land1
                                 blart = bkpf-blart .
    IF sy-subrc IS INITIAL .
      READ TABLE t_doclas WITH KEY land1 = t001-land1
                                   id_report = sy-repid
                                   doccls = t_doctyp-doccls .
    ENDIF .
    IF sy-subrc IS INITIAL .
      MOVE t_doclas-j_1aoftp TO detail-class .
    ENDIF .

* ---> Official Number Validation
    IF NOT bkpf-xblnr IS INITIAL .
      CLEAR x_number .
      x_number = STRLEN( bkpf-xblnr ) .

      IF x_number = 13.
        detail-offnr = bkpf-xblnr(4).
        CONCATENATE detail-offnr bkpf-xblnr+5 INTO detail-offnr.
      ELSE.
        detail-offnr = bkpf-belnr.
      ENDIF.
    ENDIF .
    IF bkpf-xblnr IS INITIAL.
      detail-offnr = bkpf-belnr.
    ENDIF.

  ENDIF.

* GET DOCUMET LINE ITEMS
GET bseg.

  APPEND bseg TO t_bseg.

  CHECK rb_003 NE 'X'.
* ---> Only Vendor or Customer items
  CHECK bseg-koart = 'K' OR
        bseg-koart = 'D'.

* ---> Get partner master data
  PERFORM read_master_data USING bseg-xcpdd.

* process documents from releases < 4.0 correctly
  IF bseg-xzahl IS INITIAL.
    CALL FUNCTION 'J_1A_READ_XZAHL'
      EXPORTING
        i_bschl = bseg-bschl
      IMPORTING
        e_xzahl = bseg-xzahl.
  ENDIF.

  PERFORM read_values .

  CHECK: bseg-koart EQ 'K' OR bseg-koart EQ 'D',
         bseg-qsskz NE space,
         bseg-xzahl NE space,
         bseg-rebzt NE 'U'.

  PERFORM read_withhld_data.


GET bset.

  APPEND bset TO t_bset.

  CHECK rb_003 NE 'X'.

  IF ktosl_sel NE space.
    CHECK: bset-ktosl IN s_ktosl.
  ENDIF.

  IF holding_sel NE space.
    CHECK bkpf-bukrs = t001-umkrs.
  ENDIF.

  SELECT SINGLE * FROM j_1ataxid
                   WHERE kalsm = t005-kalsm
                   AND ktosl = bset-ktosl .

  IF ( j_1ataxid-j_1ataxid <> 'VP01' )
  AND ( j_1ataxid-j_1ataxid <> 'GP00' ).

    REJECT.

  ELSE.
*   if perception amount is zero: Don't display line   " Note 700313
    IF bset-hwste EQ 0.
      REJECT.
    ENDIF.
    PERFORM fill_detail_header.
    PERFORM fill_detail_for_tax.

  ENDIF.

*&---------------------------------------------------------------------*
*&  END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  IF  rb_003 NE 'X'.

    PERFORM set_dates USING from_date f_budat.
    PERFORM set_dates USING to_date   t_budat.

* Begin of GB - 22.09.10
    SORT t_item BY  bukrs belnr gjahr buzei witht.

    DELETE ADJACENT DUPLICATES FROM t_item COMPARING ALL FIELDS.

* Eliminado en 30.08.2011 - Diego .......
*    LOOP AT detail.
*      v_tabix = sy-tabix.
*      CLEAR detail-hwste.
*      LOOP AT t_item INTO st_item
*        WHERE bukrs = detail-bukrs  AND
*              belnr = detail-belnr  AND
*              gjahr = detail-gjahr.
*        detail-hwste = st_item-wt_qbshh + detail-hwste.
*      ENDLOOP.
*      MODIFY detail INDEX v_tabix.
*    ENDLOOP.
*
**  calculo las retenciones
*    LOOP AT gt_item INTO gs_item.
*      v_tabix = sy-tabix.
*      CLEAR detail-hwste.
*      LOOP AT t_item INTO st_item
*        WHERE "bukrs = detail-bukrs  AND
*              belnr = gs_item-belnr  AND
*              gjahr = gs_item-gjahr.
*        gs_item-x_text = st_item-wt_qbshh + gs_item-x_text.
*      ENDLOOP.
*      MODIFY detail INDEX v_tabix.
*    ENDLOOP.
** End of - 22.09.10
* Fin 30.08.2011 ..........................

**Note 1064177 Begins
    DATA wa_bkpf TYPE bkpf.
    LOOP AT detail.
      SELECT SINGLE * FROM bkpf INTO wa_bkpf WHERE
                                      bukrs = detail-bukrs  AND
                                      belnr = detail-belnr  AND
                                      gjahr = detail-gjahr.
      IF sy-subrc = 0 AND wa_bkpf-xreversal = '1'.
        DELETE detail WHERE bukrs = detail-bukrs  AND
                            belnr = wa_bkpf-stblg AND
                            gjahr = wa_bkpf-stjah.
        IF sy-subrc = 0.
          DELETE detail WHERE bukrs = detail-bukrs  AND
                              belnr = detail-belnr AND
                              gjahr = detail-gjahr.
        ENDIF.
      ELSEIF wa_bkpf-xreversal = '2'.
        detail-xreversal = wa_bkpf-xreversal.
        MODIFY detail.
      ENDIF.
    ENDLOOP.
**Note 1064177 Ends

    PERFORM print_detail.
    PERFORM print_tot_code.
    IF p_baja EQ 'X'.
      PERFORM f_archivo_bajada.
    ENDIF.
* ALV CHANGES Start
* To display details data using hierarchical list display
    PERFORM output_hierarchical_alv.
* ALV CHANGES ends

    IF NOT p_lfile IS INITIAL.
      PERFORM transfer_local USING s_file.
    ENDIF.

**Note 1013585 Begins
    PERFORM print_withheld.
    IF NOT p_lwfile IS INITIAL.
      PERFORM withheld_local.
    ENDIF.
  ELSE.
    PERFORM f_archivo_bajada.
    PERFORM f_alv.
  ENDIF.
