FUNCTION ZBSPL_LIST_CREATE.
*"--------------------------------------------------------------------
*"*"Interface global:
*"  IMPORTING
*"     VALUE(IS_SETTINGS) LIKE  RFBILA_ALV_SETTINGS
*"  STRUCTURE  RFBILA_ALV_SETTINGS
*"     VALUE(IT_LIST_COMMENTARY) TYPE  SLIS_T_LISTHEADER OPTIONAL
*"--------------------------------------------------------------------
* local data declarations
  DATA: l_cb_program LIKE sy-repid   VALUE 'SAPLBSPL'.
  DATA: ls_variant   LIKE disvariant.
  DATA: ls_layout    TYPE slis_layout_alv.
  DATA: lt_fieldcat  TYPE slis_t_fieldcat_alv.
  DATA: lt_events    TYPE slis_t_event.
  DATA: lt_sort      TYPE slis_t_sortinfo_alv .

  DATA  ld_tlevel    TYPE numc2.                            "n1595587
*----------------------------------------------------------------------*

  CALL FUNCTION 'RGRE_ERGSL_TEXT_GET'
    EXPORTING
      language        = is_settings-fs_language
      balance_version = is_settings-fs_version
      text_type       = ' '    "<<< means all texts
    TABLES
      text_tab        = gt_ergsl_text.

* save RFBILA ALV SETTINGS
  gs_settings = is_settings.

  PERFORM bspl_grid_totals_calculate TABLES gt_rsthie
                                            gt_bspldata
                                            gt_gridtotals
                                      USING is_settings.

  PERFORM bspl_grid_outtab_fill      TABLES gt_rsthie
                                            gt_bspldata
                                            gt_gridtotals
                                            gt_ergsl_text
                                            gt_edit_settings
                                            gt_gridouttab
                                      USING is_settings.

* begin "n1595587
  CASE is_settings-totals_level.
    WHEN space.
    WHEN 0.
      DELETE gt_gridouttab
        WHERE racct > space.
    WHEN OTHERS.
      ld_tlevel = is_settings-totals_level + 1.
      DELETE gt_gridouttab
        WHERE racct > space
        OR    totals_level >= ld_tlevel
        OR    tlevel       >  ld_tlevel.
  ENDCASE.
* end "n1595587

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name               = l_cb_program
*     I_INTERNAL_TABNAME     = LT_FIELDCAT
      i_structure_name       = 'BSPL_GRID_FIELDCAT'
      i_client_never_display = 'X'
*     I_INCLNAME             = 'LBSPLU04'
*     I_BYPASSING_BUFFER     =
*     I_BUFFER_ACTIVE        =
    CHANGING
      ct_fieldcat                  = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface       = 1
      program_error                = 2
      OTHERS                       = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM bspl_list_alv_interface_set
                               TABLES lt_fieldcat
                                USING is_settings
                             CHANGING l_cb_program
                                      ls_variant
                                      ls_layout.

  PERFORM bspl_list_events_init   TABLES lt_events[].
  PERFORM bspl_list_sortinfos_set TABLES lt_sort[].
  PERFORM bspl_list_newpage_set   TABLES gt_gridouttab.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
      i_callback_program             = l_cb_program
      i_callback_pf_status_set       = 'BSPL_LIST_PF_STATUS_SET'
      i_callback_user_command        = 'BSPL_LIST_USER_COMMAND'
*     I_STRUCTURE_NAME               =
      is_layout                      = ls_layout
      it_fieldcat                    = lt_fieldcat
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
      it_sort                        = lt_sort
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
      i_default                      = 'X'
      i_save                         = 'A'
      is_variant                     = ls_variant
      it_events                      = lt_events
*     IT_EVENT_EXIT                  =
*     IS_PRINT                       =
*     IS_REPREP_ID                   =
*     I_SCREEN_START_COLUMN          = 0
*     I_SCREEN_START_LINE            = 0
*     I_SCREEN_END_COLUMN            = 0
*     I_SCREEN_END_LINE              = 0
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                       = gt_gridouttab
    EXCEPTIONS
      program_error                  = 1
      OTHERS                         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFUNCTION.
