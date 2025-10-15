*----------------------------------------------------------------------*
***INCLUDE LMB_BUS2017FOM .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  OM_ITEM_DUPLICATE
*----------------------------------------------------------------------*
* Note 1768662 - this form routine checks if any of the items contain
* outsource manufacturing subcontracting purchase orders and creates
* a IMSEG line item for a 521 goods receipt where necessary.
* Step 1 - the purchase orders contained in the items are collected, if
* the item contains an inbound delivery the order is read from LIPS.
* The identified purchase orders are checked to see if they are OM
* relevant, the routine is exited if this is not the case.
* Step 2 - if OM relevant purchase orders are identified, all the items
* receive a new LINE_ID, where necessary a 521 item is generated and
* linked to the corresponding 101 items via PARENT_ID. If serial numbers
* have been passed over to the BAPI, the item number (MBLPO) is also
* changed to reflect the new structure of IMSEG. It is possible that
* the items table will contain components for normal subcontracting
* orders where a PARENT_ID has already been externally provided. Here
* the link to the correct new LINE_ID also has to be considered.

FORM om_item_duplicate  TABLES   t_imseg
                                 t_iseri
                                 lt_imseg_om_out
                                 lt_iseri_om_out
                                 lt_emseg_om_out
                      CHANGING   lv_imseg_changed
                                 lv_iseri_changed
                                 lv_flag_om_error.

  TYPE-POOLS: abap.
  TYPES:  BEGIN OF ty_om_items,
          ebeln           TYPE ekpo-ebeln,
          ebelp           TYPE ekpo-ebeln,
          vlief_avis      TYPE imseg-vlief_avis,
          vbelp_avis      TYPE imseg-vlief_avis,
          itcons          TYPE ekpo-itcons,
          END   OF ty_om_items,

          BEGIN OF ty_mat_type,
          matnr           TYPE mara-matnr,
          mtart           TYPE mara-mtart,
          END   OF ty_mat_type,

          BEGIN OF ty_t001w,
          werks           TYPE t001w-werks,
          bwkey           TYPE t001w-bwkey,
          END OF ty_t001w,

          BEGIN OF ty_line_ids,
          parent_id_old  TYPE imseg-parent_id,
          parent_id_new  TYPE imseg-parent_id,
          END OF   ty_line_ids.

  DATA: lt_ekkn           TYPE STANDARD TABLE OF ekkn,
        ls_ekkn           TYPE ekkn,
        lf_autyp          TYPE auftyp,
        ls_t156           TYPE t156,
        ls_t156_om        TYPE t156,
        ls_imseg          TYPE imseg,
        lt_t_imseg        TYPE STANDARD TABLE OF imseg,
        lt_imseg_new      TYPE ty_t_imseg,
        lt_ekpo_key       TYPE STANDARD TABLE OF ekpo_key,
        ls_ekpo_key       LIKE LINE OF lt_ekpo_key,
        lt_ekpo           TYPE STANDARD TABLE OF ekpo,
        lv_flag_om_item   TYPE abap_bool,
        lv_flag_iseri     TYPE abap_bool,
        lv_flag_avis      TYPE abap_bool,
        lv_flag_pline_id  TYPE abap_bool,
        lv_flag_p_id      TYPE abap_bool,
        lf_ebelp          TYPE ebelp,
        ls_t134m          TYPE t134m,
        lt_t_iseri_out    TYPE STANDARD TABLE OF iseri,
        lt_t_iseri        TYPE STANDARD TABLE OF iseri,
        lv_tabix          TYPE sy-tabix,
        lv_p_id_tabix     TYPE sy-tabix,
        ls_om_items       TYPE ty_om_items,
        lt_om_items       TYPE STANDARD TABLE OF ty_om_items,
        lt_om_items_avis  TYPE STANDARD TABLE OF ty_om_items,
        ls_emseg_om_out   TYPE emseg,
        ls_mat_type       TYPE ty_mat_type,
        lt_mat_type       TYPE HASHED TABLE OF ty_mat_type
                                   WITH UNIQUE KEY matnr,
        ls_t001w          TYPE ty_t001w,
        lt_t001w          TYPE HASHED TABLE OF ty_t001w
                                   WITH UNIQUE KEY werks,
        ls_line_ids       TYPE ty_line_ids,
        lt_line_ids       TYPE STANDARD TABLE OF ty_line_ids.

  FIELD-SYMBOLS:
        <ls_imseg>        TYPE imseg,
        <ls_ekpo>         TYPE ekpo,
        <lt_om_items>     TYPE ty_om_items,
        <lt_iseri>        TYPE iseri.

  REFRESH: lt_t_imseg, lt_t_iseri.

  " Set control flags to false
  lv_flag_om_item    = abap_false.             "OM relevant item/s exist
  lv_flag_iseri      = abap_false.             "serial numbers exist
  lv_flag_avis       = abap_false.             "ind. deliveries exist
  lv_flag_pline_id   = abap_false.             "ext. parent ids provided
  lv_imseg_changed   = abap_false.             "IMSEG items changed
  lv_iseri_changed   = abap_false.             "serial numbers changed
  lv_flag_om_error   = abap_false.             "error during processing

  " Copy data passed over to the BAPI
  lt_t_imseg[] = t_imseg[].
  IF t_iseri[] IS NOT INITIAL.
    lt_t_iseri[] = t_iseri[].
    lv_flag_iseri = abap_true.
  ENDIF.


  " STEP 1
  " First check if any of the POs are OM relevant, if not
  " leave the routine and do not change anything.
  LOOP AT lt_t_imseg ASSIGNING <ls_imseg>.
    CLEAR: ls_line_ids, ls_om_items, ls_imseg.

    IF  <ls_imseg>-ebeln IS NOT INITIAL
      AND <ls_imseg>-ebelp IS NOT INITIAL.
      MOVE : <ls_imseg>-ebeln      TO  ls_ekpo_key-ebeln,
             <ls_imseg>-ebelp      TO  ls_ekpo_key-ebelp.
      COLLECT   ls_ekpo_key        INTO  lt_ekpo_key.

      MOVE : <ls_imseg>-ebeln      TO  ls_om_items-ebeln,
             <ls_imseg>-ebelp      TO  ls_om_items-ebelp.
      APPEND ls_om_items TO lt_om_items.
    ELSEIF  <ls_imseg>-vlief_avis IS NOT INITIAL
     AND    <ls_imseg>-vbelp_avis IS NOT INITIAL .
      SELECT SINGLE vgbel vgpos FROM lips
             INTO  (ls_imseg-ebeln, ls_imseg-ebelp)
             WHERE vbeln =  <ls_imseg>-vlief_avis
             AND   posnr =  <ls_imseg>-vbelp_avis.
      IF sy-subrc = 0.
        lv_flag_avis = abap_true.
        MOVE : ls_imseg-ebeln      TO  ls_ekpo_key-ebeln,
               ls_imseg-ebelp      TO  ls_ekpo_key-ebelp.
        COLLECT  ls_ekpo_key       INTO  lt_ekpo_key.

        MOVE : ls_imseg-ebeln      TO  ls_om_items-ebeln,
               ls_imseg-ebelp      TO  ls_om_items-ebelp,
             <ls_imseg>-vlief_avis TO  ls_om_items-vlief_avis,
             <ls_imseg>-vbelp_avis TO  ls_om_items-vbelp_avis.
        APPEND ls_om_items TO lt_om_items.
      ENDIF.
    ENDIF.

    " Save external parent ids passed over to BAPI
    IF <ls_imseg>-parent_id IS NOT INITIAL.
      ls_line_ids-parent_id_old    = <ls_imseg>-parent_id.
      APPEND ls_line_ids TO lt_line_ids.
    ENDIF.

  ENDLOOP.

  IF lt_ekpo_key[] IS INITIAL. "no POs (will be auto. generated)
    RETURN.
  ELSE.
    CALL FUNCTION 'ME_EKPO_ARRAY_READ'
      TABLES
        pti_ekpo_key     = lt_ekpo_key
        pto_ekpo         = lt_ekpo
      EXCEPTIONS
        no_records_found = 1.
  ENDIF.

  FREE lt_ekpo_key. UNASSIGN <ls_imseg>. CLEAR ls_line_ids.

  SORT : lt_ekpo BY  ebeln ebelp.
  SORT lt_om_items BY ebeln ebelp vlief_avis vbelp_avis.
  DELETE ADJACENT DUPLICATES FROM lt_om_items COMPARING ALL FIELDS.

  " check for OM purchase orders
  LOOP AT lt_ekpo  ASSIGNING <ls_ekpo>.
    IF       <ls_ekpo>-pstyp  EQ '3'                           "n1893959
       AND   <ls_ekpo>-knttp  NE space
       AND   <ls_ekpo>-itcons NE space.
      lv_flag_om_item = abap_true.   "at least one item is OM relevant
      READ TABLE lt_om_items TRANSPORTING NO FIELDS WITH KEY
        ebeln = <ls_ekpo>-ebeln
        ebelp = <ls_ekpo>-ebelp BINARY SEARCH.
      LOOP AT lt_om_items ASSIGNING <lt_om_items> FROM sy-tabix
         WHERE ebeln = <ls_ekpo>-ebeln
          AND  ebelp = <ls_ekpo>-ebelp.
        <lt_om_items>-itcons = 'X'.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  IF lv_flag_om_item = abap_false.  " No PO is relevant for OM
    RETURN.
  ENDIF.

  " STEP 2
  " The table lt_om_items contains outsource manuf. PO items and we
  " now continue the processing and add an additional 521/522 item
  " where necessary.

  "Clear the LINE_ID buffer in case the BAPI is called multiple
  "times in the same roll area and an error is issued before the
  "buffer is re-set in MB_MOVEMENTS_REFRESH.
  CALL METHOD cl_mmim_line_id_manager=>reset.

  "Sort the serial numbers
  IF lv_flag_iseri = abap_true.
    SORT lt_t_iseri BY mblpo.
  ENDIF.

  "Sort and compress external parent ids
  IF lt_line_ids[] IS NOT INITIAL.
    lv_flag_pline_id = abap_true.
    SORT lt_line_ids BY parent_id_old.
    DELETE ADJACENT DUPLICATES FROM lt_line_ids COMPARING parent_id_old.
  ENDIF.

  "Create sorted table for inbound deliveries
  IF lv_flag_avis = abap_true.
    lt_om_items_avis[] = lt_om_items[].
    SORT lt_om_items_avis BY vlief_avis vbelp_avis.
  ENDIF.


  LOOP AT lt_t_imseg ASSIGNING <ls_imseg>.

    CLEAR: lv_tabix, lv_p_id_tabix, ls_line_ids.
    MOVE sy-tabix TO lv_tabix .

    " Assign a new LINE_ID & global counter
    IF lv_flag_pline_id = abap_true.           "ext. parent ids provided
      IF <ls_imseg>-line_id IS NOT INITIAL.
        READ TABLE lt_line_ids INTO ls_line_ids WITH KEY
           parent_id_old = <ls_imseg>-line_id BINARY SEARCH.
        IF sy-subrc = 0.        " this item is a parent for another item
          lv_p_id_tabix = sy-tabix.
          lv_flag_p_id = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR <ls_imseg>-line_id.
    CALL METHOD cl_mmim_line_id_manager=>line_id_get
      CHANGING
        c_line_id = <ls_imseg>-line_id.
    <ls_imseg>-global_counter = <ls_imseg>-line_id.
    IF lv_flag_p_id = abap_true.
      ls_line_ids-parent_id_new = <ls_imseg>-line_id.
      MODIFY lt_line_ids FROM ls_line_ids INDEX lv_p_id_tabix.
    ENDIF.
    CLEAR ls_line_ids. lv_flag_p_id = abap_false.

    " Change the current parent id to the new line_id
    IF <ls_imseg>-parent_id IS NOT INITIAL.
      READ TABLE lt_line_ids INTO ls_line_ids WITH KEY
            parent_id_old = <ls_imseg>-parent_id.
      IF sy-subrc = 0 AND ls_line_ids-parent_id_new IS NOT INITIAL.
        <ls_imseg>-parent_id = ls_line_ids-parent_id_new.
      ENDIF.
    ENDIF.

    " Change document item no. in serial number table if necessary
    IF lv_flag_iseri = abap_true.
      READ TABLE  lt_t_iseri
          TRANSPORTING NO FIELDS WITH KEY mblpo  = lv_tabix.
      IF sy-subrc = '0'.              "at least one serial number exists
        lv_iseri_changed = abap_true.        "numbering has been changed
        LOOP AT lt_t_iseri ASSIGNING <lt_iseri> FROM sy-tabix
              WHERE mblpo = lv_tabix.
          <lt_iseri>-mblpo = <ls_imseg>-line_id.
          APPEND <lt_iseri> TO lt_t_iseri_out.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " Check if the material is valuated
    READ TABLE lt_mat_type INTO ls_mat_type
      WITH TABLE KEY matnr =  <ls_imseg>-matnr.
    IF sy-subrc NE 0.
      SELECT SINGLE matnr mtart FROM mara INTO ls_mat_type
        WHERE matnr = <ls_imseg>-matnr.
      INSERT ls_mat_type INTO TABLE lt_mat_type.
    ENDIF.
    READ TABLE lt_t001w INTO ls_t001w
   WITH TABLE KEY werks =  <ls_imseg>-werks.
    IF sy-subrc NE 0.
      SELECT SINGLE werks bwkey FROM t001w INTO ls_t001w
        WHERE werks = <ls_imseg>-werks.
      INSERT ls_t001w INTO TABLE lt_t001w.
    ENDIF.
    SELECT SINGLE * FROM t134m INTO ls_t134m WHERE
        bwkey = ls_t001w-bwkey AND
        mtart = ls_mat_type-mtart.
    IF sy-subrc = 0.
      IF ls_t134m-wertu = 'X'
        AND ls_t134m-mengu = 'X'.
        " continue processing
      ELSE.
        " do not add a 521 line item even if relevant for OM
        " as this would lead to the stock being receiving twice
        APPEND <ls_imseg> TO lt_imseg_new.
        CONTINUE.
      ENDIF.
    ENDIF.
*
    " Check if the PO items is relevant for Outsource Manufacturing
    IF    <ls_imseg>-ebeln IS INITIAL  AND          "PO auto. created or
          <ls_imseg>-vlief_avis IS INITIAL.         "subcon component
      APPEND <ls_imseg> TO lt_imseg_new.
      CONTINUE.
    ELSEIF  <ls_imseg>-ebeln IS NOT INITIAL         " PO number is known
      AND   <ls_imseg>-ebelp IS NOT INITIAL.
      READ TABLE lt_om_items ASSIGNING <lt_om_items>
       WITH  KEY ebeln = <ls_imseg>-ebeln
                 ebelp = <ls_imseg>-ebelp BINARY SEARCH.
      IF <lt_om_items>-itcons IS INITIAL.            " not an OM PO item
        APPEND <ls_imseg> TO lt_imseg_new.
        CONTINUE.
      ENDIF.
    ELSEIF  <ls_imseg>-vlief_avis IS NOT INITIAL   " GR for inbound del.
      AND   <ls_imseg>-vbelp_avis IS NOT INITIAL .
      READ TABLE lt_om_items_avis ASSIGNING <lt_om_items>
       WITH  KEY vlief_avis = <ls_imseg>-vlief_avis
                 vbelp_avis = <ls_imseg>-vbelp_avis BINARY SEARCH.
      IF <lt_om_items>-itcons IS INITIAL.            " not an OM PO item
        APPEND <ls_imseg> TO lt_imseg_new.
        CONTINUE.
      ENDIF.
    ENDIF.

    " Check that the PO item account assignment is a CO prod. order
    CLEAR: ls_ekkn, lf_ebelp, ls_imseg, ls_t156, ls_t156_om,
           ls_emseg_om_out.
    REFRESH lt_ekkn.

    lf_ebelp = <lt_om_items>-ebelp.
    CALL FUNCTION 'MMPUR_EKKN_READ_EBELN_EBELP'
      EXPORTING
        pi_ebeln             = <lt_om_items>-ebeln
        pi_ebelp             = lf_ebelp
      TABLES
        pto_ekkn_po          = lt_ekkn
      EXCEPTIONS
        no_records_requested = 1
        OTHERS               = 2.
    IF sy-subrc NE 0.
      APPEND <ls_imseg> TO lt_imseg_new.
      CONTINUE.
    ENDIF.
    LOOP AT lt_ekkn INTO ls_ekkn
                    WHERE aufnr NE space.
      CALL FUNCTION 'K_ORDER_READ'
        EXPORTING
          aufnr     = ls_ekkn-aufnr
        IMPORTING
          autyp     = lf_autyp
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc NE 0.
        APPEND <ls_imseg> TO lt_imseg_new.
        EXIT.
      ENDIF.
      IF lf_autyp EQ '04'.    "CO order
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lf_autyp NE '04' OR
       ( sy-subrc IS INITIAL AND lf_autyp EQ '04' AND    "1825650
         NOT ls_ekkn-vbeln IS INITIAL ).                 "1825650
      APPEND <ls_imseg> TO lt_imseg_new.
      CONTINUE.
    ENDIF.

    " Duplicate current line and add new line with same parameters
    " and movement type 521 (see logic in LMIGOKL2, FORM SC_LINE_MODIFY
    " and method add_consumption_item, CLASS CL_SHP_LECOMP)
    APPEND <ls_imseg> TO lt_imseg_new.
    " Find the correct movement type for the new line and check the
    " leading movement type (103/105, 107/109 not supported)
    SELECT SINGLE * FROM t156 INTO ls_t156
           WHERE bwart EQ <ls_imseg>-bwart.
    IF ls_t156-kzwes IS NOT INITIAL.
      lv_flag_om_error      = abap_true.
      ls_emseg_om_out-tabix = lv_tabix.
      ls_emseg_om_out-msgty = 'E'.
      ls_emseg_om_out-msgid = 'M7'.
      ls_emseg_om_out-msgno = '096'.
      ls_emseg_om_out-msgv1 = <ls_imseg>-bwart.
      APPEND ls_emseg_om_out TO lt_emseg_om_out.
      RETURN.
    ENDIF.
    IF ls_t156-shkzg = 'S'.
      SELECT SINGLE * FROM t156 INTO ls_t156_om
             WHERE bustr EQ '501'
             AND shkzg EQ 'S'
             AND rstyp EQ 'F'.
    ELSEIF ls_t156-shkzg = 'H'.               "goods receipt rev./return
      SELECT SINGLE * FROM t156 INTO ls_t156_om
             WHERE bustr EQ '501'
             AND shkzg EQ 'H'
             AND rstyp EQ 'F'.
    ENDIF.
    " Update existing IMSEG entry with 521 information
    IF sy-subrc EQ 0.
      ls_imseg = <ls_imseg>.
      ls_imseg-aufnr = ls_ekkn-aufnr.
      ls_imseg-bwart = ls_t156_om-bwart.
      ls_imseg-parent_id = <ls_imseg>-line_id.
      CLEAR: ls_imseg-ebeln, ls_imseg-ebelp, ls_imseg-kzvbr,
             ls_imseg-vlief_avis, ls_imseg-vbelp_avis,
             ls_imseg-bpmng, ls_imseg-bprme, ls_imseg-grund,
             ls_imseg-kzbew, ls_imseg-elikz, ls_imseg-line_id.
      CALL METHOD cl_mmim_line_id_manager=>line_id_get
        CHANGING
          c_line_id = ls_imseg-line_id.
      ls_imseg-global_counter = ls_imseg-line_id.
      APPEND ls_imseg TO lt_imseg_new.
    ENDIF.

  ENDLOOP.

  REFRESH lt_imseg_om_out. REFRESH lt_iseri_om_out.

  " Pass back new IMSEG items and serial numbers table
  IF t_imseg[] NE lt_imseg_new[].
    lv_imseg_changed  = abap_true.
    lt_imseg_om_out[] = lt_imseg_new[].
    IF lv_iseri_changed = abap_true.
      lt_iseri_om_out[] = lt_t_iseri_out[].
    ENDIF.
  ENDIF.


ENDFORM.                    " OM_ITEM_DUPLICATE
