"Name: \PR:CORUAFFW\FO:LIST_BLOCKED_ENTRIES_ALV\SE:BEGIN\EI
ENHANCEMENT 0 ZCORUAFFW.

  types: begin of type_BLPK,
           PRTNR  type BLPK-PRTNR,
           ACHARG type BLPK-ACHARG,
         end   of type_BLPK.

  DATA: lt_BLPK  TYPE table of type_BLPK  ,
        lt_affw  type table of affw       ,
        ls_affw  TYPE affw                ,
        ls_BLPK  type type_BLPK           ,
        ls_alv   type AFFW_S_ALV          ,
        l_subrc2 LIKE sy-subrc            ,
        l_index  type i                   .

  refresh lt_BLPK.

IF 1 = 1.

  lt_affw[] = affw_tab[].
  sort lt_affw by WEBLNR  ascending
                  WEBLPOS ascending.

  if not lt_affw[] is initial.
  select PRTNR ACHARG
    from BLPK
    into table lt_BLPK
    for all entries in lt_affw
  where  PRTNR eq lt_affw-PRTNR.
  endif.

  sort lt_BLPK by PRTNR ascending.

  IF NOT l_change IS INITIAL.
    REFRESH gt_affw_alv.
    CLEAR   gt_affw_alv.
    REFRESH gt_fieldcat.
    REFRESH gt_exctab.
  ENDIF.

* Kumulierte Ausgabetabelle aufbauen
  IF flg_start = yx OR
*    Neuaufbau nach Bearbeitung aus kumulierter Sicht
     NOT flg_refresh IS INITIAL OR
     NOT s_pname IS INITIAL.
*   Neuaufbau nur nach Start bzw. Sichern oder bei ext. Sort.
    PERFORM create_affwb_out_tab.
  ELSE.
*   Aktualisiern
    PERFORM update_affwb_out_tab.
  ENDIF.

  IF s_pname IS INITIAL.
*   Erstsortierung nach Erfassungsdatum
*   nur nach Start bzw. Sichern
    IF flg_start = yx.
      SORT affwb_out_tab BY ersda erzet werks lgort matnr charg
                            bwart erfme msgid msgno weblnr weblpos.
    ENDIF.
  ELSE.
*   externe Sortierung
*   PERFORM DO_SORT IN PROGRAM (S_PNAME) TABLES AFFWB_OUT_TAB.
    CALL FUNCTION 'CNSG_SORT_TABLE'
      TABLES
        t_table  = affwb_out_tab
      CHANGING
        c_handle = g_sort_handle.
  ENDIF.

  LOOP AT affwb_out_tab.
    MOVE-CORRESPONDING affwb_out_tab TO gt_affw_alv.          "#EC ENHOK
*   Berechtigungsprüfung
    IF NOT gt_affw_alv-autyp IS INITIAL AND
       NOT gt_affw_alv-werks IS INITIAL.
      AUTHORITY-CHECK OBJECT 'C_AFFW_TWK'
           ID 'AUTYP' FIELD gt_affw_alv-autyp
           ID 'WERKS' FIELD gt_affw_alv-werks.
      l_subrc2 = sy-subrc.
    ENDIF.
    IF NOT l_subrc2 IS INITIAL.
      gt_affw_alv-vbkz = vbkz_shw.
    ENDIF.
*   Ampel setzen
    CASE gt_affw_alv-vbkz.
      WHEN vbkz_bu.
        CONTINUE.
      WHEN vbkz_del OR vbkz_dll.
        gt_affw_alv-status = icon_red_light.
      WHEN vbkz_ins OR vbkz_upd.
        gt_affw_alv-status = icon_green_light.
      WHEN vbkz_shw.
        gt_affw_alv-status = icon_yellow_light.
      WHEN OTHERS.
        gt_affw_alv-status = icon_light_out.
    ENDCASE.
*   Schlüssel für Lesebaustein füllen
    mtcom-kenng = kenng_makt.
    mtcom-spras = sy-langu.
    mtcom-matnr = gt_affw_alv-matnr.
*   Materialstamm lesen
    CALL FUNCTION 'MATERIAL_READ'
      EXPORTING
        schluessel         = mtcom
      IMPORTING
        matdaten           = makt
      TABLES
        seqmat01           = dummy_tab
      EXCEPTIONS
        material_not_found = 04
        plant_not_found    = 08
        account_not_found  = 12.
    IF sy-subrc <> 0.
      CLEAR makt.
    ELSE.
      gt_affw_alv-maktx = makt-maktx.
    ENDIF.
    CLEAR gt_affw_alv-box.
    APPEND gt_affw_alv.
  ENDLOOP.

  IF NOT l_change IS INITIAL.
    gt_affw_alv-box = space.
    MODIFY gt_affw_alv TRANSPORTING box WHERE box = yx.
    REFRESH gt_fieldcat.
    REFRESH gt_exctab.
  ENDIF.

* Fehlertexte hinzufuegen
  CALL FUNCTION 'CO_WB_MESSAGE_TRANSFORM_MULTI'
    EXPORTING
      i_langu                   = sy-langu
    TABLES
      t_msg                     = gt_affw_alv
    EXCEPTIONS
      structure_error_in_t_msg  = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*----> build up fieldcatalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
      i_structure_name   = 'AFFW_S_ALV'
"$$
    CHANGING
      ct_fieldcat        = gt_fieldcat.
* define layout
  PERFORM layout_build CHANGING gs_layout.
* change field catalog
  PERFORM fieldcat_edit CHANGING gt_fieldcat[].
* create excluding tab
  PERFORM exctab_init CHANGING gt_exctab.
* create events table
  PERFORM set_events CHANGING gt_events.
* create title
  PERFORM title_build CHANGING g_title.
* create variant
  PERFORM variant_build CHANGING gs_variant.

  LOOP AT gt_affw_alv into ls_alv.
    l_index = sy-tabix.
    read table lt_affw into ls_affw
      with key WEBLNR  = ls_alv-WEBLNR
               WEBLPOS = ls_alv-WEBLPOS
      binary search.
     read table lt_BLPK into ls_blpk
       with key PRTNR = ls_affw-PRTNR
       binary search.
    if sy-subrc is initial.
      ls_alv-acharg = ls_blpk-acharg.
      modify gt_affw_alv from ls_alv
        index l_index
        transporting acharg.
    endif.
    clear: ls_alv ,
           ls_affw,
           ls_blpk.
  ENDLOOP.

* Ausgabe als ALV-Grid
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
      i_callback_pf_status_set = g_status_set
      i_callback_user_command  = g_user_command
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat[]
      it_except_qinfo          = gt_exctab
      i_grid_title             = g_title
      i_default                = 'X'
      i_save                   = 'A'
      is_variant               = gs_variant
      it_events                = gt_events
    TABLES
      t_outtab                 = gt_affw_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDIF.

check 1 = 2.

ENDENHANCEMENT.
