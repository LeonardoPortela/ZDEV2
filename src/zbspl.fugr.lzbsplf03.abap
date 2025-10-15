*----------------------------------------------------------------------*
***INCLUDE LBSPLF03 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  BSPL_LIST_EVENTS_INIT
*&---------------------------------------------------------------------*
FORM bspl_list_events_init TABLES pt_events TYPE slis_t_event.
* local data declarations
  DATA: ls_events TYPE slis_alv_event.

  CLEAR ls_events.
  ls_events-name       = 'TOP_OF_PAGE'.
  ls_events-form       = 'BSPL_LIST_TOP_OF_PAGE'.
  APPEND ls_events TO  pt_events.

  CLEAR ls_events.
  ls_events-name       = 'TOP_OF_LIST'.
  ls_events-form       = 'BSPL_LIST_TOP_OF_LIST'.
  APPEND ls_events TO  pt_events.

ENDFORM.                    " BSPL_LIST_EVENTS_INIT

*&---------------------------------------------------------------------*
*&      Form  BSPL_LIST_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM bspl_list_top_of_page.
* local data declarations
  DATA: ls_list_commentary TYPE slis_listheader.

* write standard page header
  PERFORM batch-heading(rsbtchh0).
* write list comentary without first line
  LOOP AT it_list_commentary FROM 2 "TO 5                   "n1489749
                             INTO ls_list_commentary.
    CHECK ls_list_commentary-info <> is_settings-allgline.  "n1489749
    WRITE: / ls_list_commentary-key,
             ls_list_commentary-info.
  ENDLOOP.
ENDFORM.                    " BSPL_LIST_TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  BSPL_LIST_TOP_OF_LIST
*&---------------------------------------------------------------------*
FORM bspl_list_top_of_list.
* local data declarations
  DATA: ls_list_commentary TYPE slis_listheader.

* fill standard page header
  bhdgd-inifl = con_0.
  bhdgd-lines = sy-linsz.
  bhdgd-uname = sy-uname.
  bhdgd-repid = is_settings-repid.
  READ TABLE it_list_commentary INDEX 1
                                 INTO ls_list_commentary.
  bhdgd-line1 = ls_list_commentary-info.
  bhdgd-line2 = is_settings-allgline.
  bhdgd-bukrs = is_settings-bukrs.
  bhdgd-domai = 'BUKRS'.
  bhdgd-start_pagno = is_settings-page_no.
ENDFORM.                    " BSPL_LIST_TOP_OF_LIST
*&---------------------------------------------------------------------*
*&      Form  BSPL_LIST_ALV_INTERFACE_SET
*&---------------------------------------------------------------------*
FORM bspl_list_alv_interface_set
              TABLES pt_fieldcat         TYPE      slis_t_fieldcat_alv
               USING value(ps_settings)  STRUCTURE rfbila_alv_settings
            CHANGING value(p_cb_program) LIKE      sy-repid
                     value(ps_variant)   STRUCTURE disvariant
                     value(ps_layout)    TYPE      slis_layout_alv.

* call back settings
  p_cb_program        = 'SAPLBSPL'.
* variant settings
  ps_variant-report   = ps_settings-repid.
  ps_variant-handle   = con_list.
  ps_variant-username = sy-uname.
  ps_variant-variant  = ps_settings-list_vari.
* layout settings
  ps_layout-info_fieldname   = 'LINE_COLOR'.
  ps_layout-coltab_fieldname = 'CELL_COLOR'.
* fieldcat settings
  LOOP AT pt_fieldcat ASSIGNING <fieldcat>.
    CASE <fieldcat>-fieldname.
      WHEN 'ID'
        OR 'SUBID'
        OR 'NPAGE'
        OR 'TLEVEL'
        OR 'LINE_COLOR'.
        <fieldcat>-no_out = con_x.

      WHEN 'TEXT'.
        PERFORM bspl_grid_column_color_set
                              CHANGING <fieldcat>-emphasize.
      WHEN 'RELVAR'.
        <fieldcat>-just = 'R'.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " BSPL_LIST_ALV_INTERFACE_SET
*&---------------------------------------------------------------------*
*&      Form  BSPL_LIST_SORTINFOS_SET
*&---------------------------------------------------------------------*
FORM bspl_list_sortinfos_set
                      TABLES pt_sort TYPE slis_t_sortinfo_alv.
* local data declaration
  DATA: ls_sort TYPE slis_sortinfo_alv.

  CLEAR: ls_sort.
  ls_sort-spos       = con_01.
  ls_sort-fieldname  = 'NPAGE'.
  ls_sort-tabname    = 'GT_GRIDOUTTAB'.
  ls_sort-up         = con_x.
  ls_sort-obligatory = con_x.
* set new page
  ls_sort-group      = '*'.
  APPEND ls_sort TO pt_sort.

  CLEAR: ls_sort.
  ls_sort-spos       = con_02.
  ls_sort-fieldname  = 'ID'.
  ls_sort-tabname    = 'GT_GRIDOUTTAB'.
  ls_sort-up         = con_x.
  ls_sort-obligatory = con_x.
  APPEND ls_sort TO pt_sort.

  CLEAR: ls_sort.
  ls_sort-spos       = con_03.
  ls_sort-fieldname  = 'SUBID'.
  ls_sort-tabname    = 'GT_GRIDOUTTAB'.
  ls_sort-up         = con_x.
  ls_sort-obligatory = con_x.
  APPEND ls_sort TO pt_sort.

ENDFORM.                    " BSPL_LIST_SORTINFOS_SET
*&---------------------------------------------------------------------*
*&      Form  BSPL_LIST_NEWPAGE_SET
*&---------------------------------------------------------------------*
FORM bspl_list_newpage_set TABLES pt_outtab TYPE tt_grid_outtab.
* local data declarations
  DATA: l_ergsl LIKE rf011p-ergsl.
  DATA: l_npage TYPE bspl_grid_fieldcat-npage.

  LOOP AT pt_outtab ASSIGNING <outtab>.
*.. skip to a new page on every level 02
    IF <outtab>-tlevel =  con_02
   AND <outtab>-ergsl <> l_ergsl.
      l_ergsl = <outtab>-ergsl.
      l_npage = l_npage + 1.
    ENDIF.
    <outtab>-npage = l_npage.
  ENDLOOP.

ENDFORM.                    " BSPL_LIST_NEWPAGE_SET

*&---------------------------------------------------------------------*
*&      Form CALL_BADI_EXIT
*&---------------------------------------------------------------------*
FORM call_badi_exit TABLES pt_outtab          TYPE tt_grid_outtab
                           pt_list_commentary TYPE slis_t_listheader.

  DATA: l_ref_to_exit  TYPE REF TO ZIF_EX_FI_BILA_OUTPUT,
        l_bukrs_land   TYPE land,
        l_outtab       TYPE ZIDFI_BSPL_GRID,
        l_outtab_line  TYPE zbspl_grid_fieldcat,
        l_bukrs_range  TYPE acc_t_ra_bukrs,
        l_bukrs_line   TYPE acc_s_ra_bukrs,
        l_comment_line TYPE idfi_grid_header,
        l_comment_tab  TYPE idfi_t_grid_header.

  CALL METHOD cl_exithandler=>get_instance
    EXPORTING
      exit_name = 'FI_BILA_OUTPUT'
    CHANGING
      instance  = l_ref_to_exit.

* Convert type of output table ----------------------------
  LOOP AT pt_outtab INTO l_outtab_line.
    APPEND l_outtab_line TO l_outtab.

    l_bukrs_line-sign   = 'I'.
    l_bukrs_line-option = 'EQ'.
    l_bukrs_line-low    = l_outtab_line-rbukrs.
    COLLECT l_bukrs_line INTO l_bukrs_range.

  ENDLOOP.

* Build up commentary list for BADI
  LOOP AT pt_list_commentary INTO l_comment_line.
    APPEND l_comment_line TO l_comment_tab.
  ENDLOOP.

* Determine country for BADI filter value -----------------
  CALL FUNCTION 'TAX_REP_CHECK_CC_RANGE'
    EXPORTING
      im_ran_bukrs = l_bukrs_range
    IMPORTING
      ex_land      = l_bukrs_land
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    CLEAR l_bukrs_land.
  ENDIF.

  CALL METHOD l_ref_to_exit->additional_actions
    EXPORTING
      flt_val       = l_bukrs_land
      i_outtab      = l_outtab
      i_comment_tab = l_comment_tab.

ENDFORM.                    "CALL_BADI_EXIT
*&---------------------------------------------------------------------*
*&      Form  BSPL_LIST_PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM bspl_list_pf_status_set USING t_exuc TYPE slis_t_extab.
* local data declarations
  DATA: lt_exuc TYPE slis_t_extab.

  APPEND '&ALL'      TO lt_exuc.
  APPEND '&SAL'      TO lt_exuc.
  APPEND '&NFO'      TO lt_exuc.
  APPEND '&EB9'      TO lt_exuc.

  CASE g_list_state.
    WHEN con_acct.
      APPEND 'COMP'          TO lt_exuc.
      APPEND 'COMP_BUSA'     TO lt_exuc.
      IF x_bukrs_unique <> space.
        APPEND 'EXPA'        TO lt_exuc.
        IF x_rbusa_unique <> space.
          APPEND 'EXPA_BUSA' TO lt_exuc.
        ENDIF.
      ELSE.
        APPEND 'EXPA_BUSA'   TO lt_exuc.
      ENDIF.

    WHEN con_ccod.
      APPEND 'EXPA'          TO lt_exuc.
      APPEND 'COMP_BUSA'     TO lt_exuc.
      IF x_bukrs_unique <> space.
        APPEND 'COMP'        TO lt_exuc.
      ENDIF.
      IF x_rbusa_unique <> space.
        APPEND 'EXPA_BUSA'   TO lt_exuc.
      ENDIF.

    WHEN con_leaf.
      IF x_bukrs_unique <> space.
        APPEND 'COMP'          TO lt_exuc.
      ENDIF.
      APPEND 'EXPA'          TO lt_exuc.
      APPEND 'EXPA_BUSA'     TO lt_exuc.
  ENDCASE.

  SET PF-STATUS 'LIST_FULLSCREEN'
                           EXCLUDING lt_exuc.


ENDFORM.                    " BSPL_LIST_PF_STATUS_SET

*&---------------------------------------------------------------------*
*&      Form  BSPL_LIST_USER_COMMAND
*&---------------------------------------------------------------------*
FORM bspl_list_user_command USING p_ucomm     LIKE sy-ucomm
                             ps_selfield TYPE slis_selfield.

  CASE p_ucomm.
    WHEN 'EXPA'.
      g_list_state        = con_ccod.
      ps_selfield-refresh = con_x.

    WHEN 'COMP'.
      g_list_state        = con_acct.
      ps_selfield-refresh = con_x.

    WHEN 'EXPA_BUSA'.
      g_list_state        = con_leaf.
      ps_selfield-refresh = con_x.

    WHEN 'COMP_BUSA'.
      g_list_state        = con_ccod.
      ps_selfield-refresh = con_x.

    WHEN 'SEL'.                                             "n1489749
      CALL FUNCTION 'FB_SELECTIONS_DISPLAY'                 "n1489749
        EXPORTING                                           "n1489749
          ed_program = sy-cprog                             "n1489749
          ed_mode    = '1'.                                 "n1489749

    WHEN OTHERS. EXIT.
  ENDCASE.

  REFRESH: gt_gridouttab.
  CLEAR:   gt_gridouttab.
  PERFORM bspl_grid_outtab_fill      TABLES gt_rsthie
                                            gt_bspldata
                                            gt_gridtotals
                                            gt_ergsl_text
                                            gt_edit_settings
                                            gt_gridouttab
                                      USING gs_settings.

  PERFORM bspl_list_newpage_set      TABLES gt_gridouttab.

ENDFORM.                    " BSPL_LIST_USER_COMMAND
