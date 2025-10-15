*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F28
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_C
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_c .

* Build the fieldcat according to DDIC structure J_1BNFE_ACTIVE:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'J_1BNFE_ACTIVE'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_c.

* Add an icon field for the Reason-Assigned Status
  wa_fieldcatalog_c-fieldname = 'STATUS'.
  wa_fieldcatalog_c-col_pos   = 1.
  wa_fieldcatalog_c-reptext   = text-190.
  wa_fieldcatalog_c-outputlen = 2.
  APPEND wa_fieldcatalog_c TO it_fieldcatalog_c.


* Modify field catalog
  LOOP AT it_fieldcatalog_c INTO wa_fieldcatalog_c.

*   Display only DOCNUM, Access-Key Fields, Cancellation Reason
    IF wa_fieldcatalog_c-fieldname <> 'STATUS'  AND
       wa_fieldcatalog_c-fieldname <> 'DOCNUM'  AND
       wa_fieldcatalog_c-fieldname <> 'REGIO'   AND
       wa_fieldcatalog_c-fieldname <> 'NFYEAR'  AND
       wa_fieldcatalog_c-fieldname <> 'NFMONTH' AND
       wa_fieldcatalog_c-fieldname <> 'STCD1'   AND
       wa_fieldcatalog_c-fieldname <> 'MODEL'   AND
       wa_fieldcatalog_c-fieldname <> 'SERIE'   AND
       wa_fieldcatalog_c-fieldname <> 'NFNUM9'  AND
       wa_fieldcatalog_c-fieldname <> 'DOCNUM9' AND
       wa_fieldcatalog_c-fieldname <> 'CDV'     AND
       wa_fieldcatalog_c-fieldname <> 'REASON'  AND
       wa_fieldcatalog_c-fieldname <> 'REASON1' AND
       wa_fieldcatalog_c-fieldname <> 'REASON2' AND
       wa_fieldcatalog_c-fieldname <> 'REASON3' AND
       wa_fieldcatalog_c-fieldname <> 'REASON4'.

      wa_fieldcatalog_c-no_out = c_x.
    ENDIF.

*   Set output length for cancel-reason1 description field
    IF wa_fieldcatalog_c-fieldname = 'REASON1'  OR
       wa_fieldcatalog_c-fieldname = 'REASON2'  OR
       wa_fieldcatalog_c-fieldname = 'REASON3'  OR
       wa_fieldcatalog_c-fieldname = 'REASON4'.
      wa_fieldcatalog_c-outputlen = 30.
    ENDIF.
*   Set output length for other cancel-reason description fields
    IF wa_fieldcatalog_c-fieldname = 'REASON2'  OR
       wa_fieldcatalog_c-fieldname = 'REASON3'  OR
       wa_fieldcatalog_c-fieldname = 'REASON4'.
      wa_fieldcatalog_c-outputlen = 10.
    ENDIF.

*   Highlight Access-Key fields
    IF wa_fieldcatalog_c-fieldname = 'REGIO'.
*     START: field list of Access Key
      indic = c_x.
      wa_fieldcatalog_c-emphasize = c_grid_color_c300.
    ELSEIF indic = c_x AND wa_fieldcatalog_c-fieldname = 'CDV'.
*     END  : field list of Access Key
      wa_fieldcatalog_c-emphasize = c_grid_color_c300.
      CLEAR indic.
    ELSEIF indic = c_x.
      wa_fieldcatalog_c-emphasize = c_grid_color_c300.
    ENDIF.

*   Activate single-click sensitivity (--> hotspot)
    IF wa_fieldcatalog_c-fieldname = 'DOCNUM'.
      wa_fieldcatalog_c-hotspot = 'X'.
    ENDIF.

    MODIFY it_fieldcatalog_c FROM wa_fieldcatalog_c INDEX sy-tabix.

  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_C
*&---------------------------------------------------------------------*
*&      Form  CHECK_REASONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_reasons .

  DATA: answer TYPE c LENGTH 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel     = 'Atenção!'
      textline1 = 'Será cancelado o documento.'
      textline2 = 'Deseja cancelar?'
    IMPORTING
      answer    = answer.

* Check if each NF-e has a cancellation reason assigned
  CLEAR check_negative.
  LOOP AT it_cancel_alv INTO wa_cancel_alv.
    IF wa_cancel_alv-reason IS INITIAL OR
       wa_cancel_alv-reason1 IS INITIAL.
      check_negative = c_x.
    ENDIF.
  ENDLOOP.

  IF ( check_negative EQ c_x ) AND ( answer EQ 'J' ).
    CLEAR ok_code.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '076'.
  ELSE.
    IF answer EQ 'J'.
*   Transfer assigned cancellation reasons
      CLEAR it_alv_selection_mod.                           "1151112
      LOOP AT it_alv_selection INTO wa_alv_selection.
        index = sy-tabix.

        READ TABLE it_cancel_alv
           WITH KEY docnum = wa_alv_selection-docnum
           INTO wa_cancel_alv.

        IF sy-subrc IS INITIAL.
          wa_alv_selection-reason   = wa_cancel_alv-reason.
          wa_alv_selection-reason1  = wa_cancel_alv-reason1.
          wa_alv_selection-reason2  = wa_cancel_alv-reason2.
          wa_alv_selection-reason3  = wa_cancel_alv-reason3.
          wa_alv_selection-reason4  = wa_cancel_alv-reason4.
          APPEND wa_alv_selection TO it_alv_selection_mod.  "1151112
*        MODIFY it_alv_selection FROM wa_alv_selection INDEX index.
        ELSE.
          DELETE it_alv_selection INDEX index.
        ENDIF.
      ENDLOOP.

      CLEAR ok_code.
      gf_first_display_0102 = 'R'.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_REASONS
*&---------------------------------------------------------------------*
*&      Form  GRID_UPDATE_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grid_update_0102 .

  DATA lv_mode TYPE c.

* Set status icon
  PERFORM set_status_icon_0102.
* Update display of ALV
  CALL METHOD ctl_cancel_alv->refresh_table_display
    EXPORTING
      is_stable      = gs_alv_refres_cond
      i_soft_refresh = c_x.
  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " GRID_UPDATE_0102
*&---------------------------------------------------------------------*
*&      Form  SET_STATUS_ICON_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_status_icon_0102 .

* Set status icon that indicates if a cancellation reason is assigned
  LOOP AT it_cancel_alv INTO wa_cancel_alv.
    index = sy-tabix.

    IF wa_cancel_alv-reason  IS INITIAL OR
       wa_cancel_alv-reason1 IS INITIAL.
      wa_cancel_alv-status = icon_cancel.
    ELSE.
      wa_cancel_alv-status = icon_okay.
    ENDIF.

    MODIFY it_cancel_alv FROM wa_cancel_alv INDEX index.
  ENDLOOP.

ENDFORM.                    " SET_STATUS_ICON_0102
*&---------------------------------------------------------------------*
*&      Form  ROW_SELECTION_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM row_selection_0102 .

  CALL METHOD ctl_cancel_alv->get_current_cell
    IMPORTING
      es_row_id = wa_selected_rows_0102.

  APPEND wa_selected_rows_0102 TO it_selected_rows_0102.

  CALL METHOD ctl_cancel_alv->set_selected_rows
    EXPORTING
      it_index_rows = it_selected_rows_0102.

ENDFORM.                    " ROW_SELECTION_0102
*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0102 .

  gs_variant_c-report      = sy-repid.
  gs_variant_c-handle      = space.
  gs_variant_c-log_group   = space.
  gs_variant_c-username    = space.
  gs_variant_c-variant     = space.
  gs_variant_c-text        = space.
  gs_variant_c-dependvars  = space.

ENDFORM.                    " FILL_GS_VARIANT_0102
*&---------------------------------------------------------------------*
*&      Form  DELETE_FROM_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_from_list .

* Delete selected documents from the ALV list
  SORT it_selected_rows_0102 BY index DESCENDING.
  LOOP AT it_selected_rows_0102 INTO wa_selected_rows_0102.
    DELETE it_cancel_alv INDEX wa_selected_rows_0102-index.
    dynp_0102_no_nfe = dynp_0102_no_nfe - 1.
  ENDLOOP.

* Update ALV display
  PERFORM grid_update_0102.

ENDFORM.                    " DELETE_FROM_LIST
*&---------------------------------------------------------------------*
*&      Form  SET_REASON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_reason .

  IF it_selected_rows_0102 IS INITIAL.
    CLEAR ok_code.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
  ENDIF.

* Copy the selected cancellation reason to all selected documents
  LOOP AT it_selected_rows_0102 INTO wa_selected_rows_0102.

    READ TABLE it_cancel_alv
      INTO  wa_cancel_alv
      INDEX wa_selected_rows_0102-index.

    MOVE-CORRESPONDING wa_cancel_reason TO wa_cancel_alv.

    MODIFY it_cancel_alv
      FROM wa_cancel_alv
      INDEX wa_selected_rows_0102-index.

  ENDLOOP.

* Update ALV display
  PERFORM grid_update_0102.

ENDFORM.                    " SET_REASON
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_E_COLUMN_ID_FIELDNAME  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click_0102
       USING value(row_id)    LIKE lvc_s_roid-row_id
             value(fieldname) LIKE lvc_s_col-fieldname.

  CASE fieldname.
    WHEN 'DOCNUM'.
*   Copy the selected cancellation reason to the document

      READ TABLE it_cancel_alv INDEX row_id INTO wa_cancel_alv.
      MOVE-CORRESPONDING wa_cancel_reason TO wa_cancel_alv.

      MODIFY it_cancel_alv
        FROM wa_cancel_alv
        INDEX row_id.

  ENDCASE.

* Update ALV display
  PERFORM grid_update_0102.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK_0102
