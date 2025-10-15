*&---------------------------------------------------------------------*
*&  Include           YHBRCVT08
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       FORM FILL_ALV_TAB
*----------------------------------------------------------------------*
FORM fill_alv_tab.

  DATA: l_bukrs_text LIKE hrca_company-comp_name,
        l_btrtl_text LIKE t001p-btext,
        l_trans_text LIKE q0410-tdesc,
        w_btrtl      LIKE coll_tab-btrtl.

  SORT coll_tab.

  LOOP AT coll_tab.
    w_btrtl = coll_tab-btrtl.   " to be used at 'at new werks' ...
    AT NEW bukrs.
      PERFORM companycode_text_get USING coll_tab-bukrs
                                   CHANGING l_bukrs_text.
      alv_tab-bukrst = l_bukrs_text.
    ENDAT.
    AT NEW btrtl.
      PERFORM re001p USING coll_tab-werks coll_tab-btrtl
                     CHANGING l_btrtl_text.
      alv_tab-btrtlt = l_btrtl_text.
    ENDAT.
    AT NEW persg.
      SELECT SINGLE ptext INTO alv_tab-persgt FROM t501t WHERE persg EQ coll_tab-persg AND sprsl EQ 'PT'.

    ENDAT.
    AT NEW persk.
      SELECT SINGLE ptext INTO alv_tab-perskt FROM t503t WHERE persk EQ coll_tab-persk AND sprsl EQ 'PT'.

    ENDAT.


*   move rest
    MOVE-CORRESPONDING coll_tab TO alv_tab.
    "clear alv_tab-sign.
    APPEND alv_tab.
  ENDLOOP.

ENDFORM.                               " FILL_ALV_TAB

*----------------------------------------------------------------------*
*       FORM CREATE_FIELDCAT
*----------------------------------------------------------------------*
FORM create_fieldcat.

  DATA: repid         LIKE sy-repid,
        func_name(28).

  repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_buffer_active        = ''
      i_program_name         = repid
      i_internal_tabname     = 'ALV_TAB'
*     I_STRUCTURE_NAME       =
      i_inclname             = 'YHBRCVTR0TOP'
    CHANGING
      ct_fieldcat            = fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MOVE 'REUSE_ALV_FIELDCATALOG_MERGE' TO func_name.
    PERFORM append_error
                   USING space
                         'E'
                         '005'
                         sy-subrc
                         func_name
                         space
                         space.
  ENDIF.

ENDFORM.                               " CREATE_FIELDCAT

*----------------------------------------------------------------------*
*       FORM MODIFY_FIELDCAT
*----------------------------------------------------------------------*
FORM modify_fieldcat.

  LOOP AT fieldcat.
    CASE fieldcat-fieldname.
      WHEN 'PERNR'.
        fieldcat-seltext_s = 'Pernr  '(056).
        fieldcat-seltext_m = 'Pernr '(057).
        fieldcat-seltext_l = 'Pernr      '(058).
        fieldcat-outputlen = 8.
        fieldcat-ddictxt = 'S'.
        fieldcat-no_sum = 'X'.
      WHEN 'ENAME'.
        fieldcat-seltext_s = 'Nome  '(059).
        fieldcat-seltext_m = 'Nome '(060).
        fieldcat-seltext_l = 'Nome      '(061).
        fieldcat-outputlen = 40.
        fieldcat-ddictxt = 'L'.
      WHEN 'PERSG'.
        fieldcat-seltext_s = 'Grupo'(062).
        fieldcat-seltext_m = 'Grupo'(063).
        fieldcat-seltext_l = 'Grupo'(064).
        fieldcat-outputlen = 5.
        fieldcat-ddictxt = 'L'.
      WHEN 'PERSGT'.
        fieldcat-seltext_s = 'Descrição Grupo'(065).
        fieldcat-seltext_m = 'Descrição Grupo'(066).
        fieldcat-seltext_l = 'Descrição Grupo'(067).
        fieldcat-outputlen = 30.
        fieldcat-ddictxt = 'L'.
      WHEN 'PERSK'.
        fieldcat-seltext_s = 'Sub Grupo'(066).
        fieldcat-seltext_m = 'Sub Grupo'(067).
        fieldcat-seltext_l = 'Sub Grupo'(068).
        fieldcat-outputlen = 5.
        fieldcat-ddictxt = 'L'.
      WHEN 'PERSKT'.
        fieldcat-seltext_s = 'Desc Sub Grupo'(069).
        fieldcat-seltext_m = 'Desc Sub Grupo'(070).
        fieldcat-seltext_l = 'Desc Sub Grupo'(071).
        fieldcat-outputlen = 30.
        fieldcat-ddictxt = 'L'.
      WHEN 'DAYS'.
        fieldcat-seltext_s = 'Nro.Dias  '(017).
        fieldcat-seltext_m = 'Número de Dias '(018).
        fieldcat-seltext_l = 'Número de Dias      '(019).
        CLEAR fieldcat-ref_fieldname.
        CLEAR fieldcat-ref_tabname.
      WHEN 'AUSE'.
        fieldcat-seltext_s = 'Nro.Ause  '(050).
        fieldcat-seltext_m = 'Número de Ausen '(051).
        fieldcat-seltext_l = 'Número de Ausencia   '(052).
        CLEAR fieldcat-ref_fieldname.
        CLEAR fieldcat-ref_tabname.
      WHEN 'FERI'.
        fieldcat-seltext_s = 'Nro.Fer  '(053).
        fieldcat-seltext_m = 'Número de Féri '(054).
        fieldcat-seltext_l = 'Número de Férias    '(055).
        CLEAR fieldcat-ref_fieldname.
        CLEAR fieldcat-ref_tabname.
      WHEN 'LANCHE'.
        fieldcat-seltext_s = 'Lanche '(020).
        fieldcat-seltext_m = 'Lanche'(021).
        fieldcat-seltext_l = 'Lanche'(022).
        fieldcat-ddictxt = 'L'.
        fieldcat-no_sum = 'X'.
        CLEAR fieldcat-ref_fieldname.
        CLEAR fieldcat-ref_tabname.
      WHEN 'ALIM'.
        fieldcat-seltext_s = 'Alimento'(026).
        fieldcat-seltext_m = 'Alimento'(027).
        fieldcat-seltext_l = 'Alimento'(028).
        fieldcat-ddictxt = 'L'.
        fieldcat-no_sum = 'X'.
        CLEAR fieldcat-ref_fieldname.
        CLEAR fieldcat-ref_tabname.
      WHEN 'TOTAL'.
        fieldcat-do_sum = 'X'.
        fieldcat-seltext_s = 'Total'(023).
        fieldcat-seltext_m = 'Total'(024).
        fieldcat-seltext_l = 'Total'(025).
        fieldcat-ddictxt = 'L'.
        fieldcat-no_sum = 'X'.
        CLEAR fieldcat-ref_fieldname.
        CLEAR fieldcat-ref_tabname.
      WHEN 'ENTRYDATE'.
        fieldcat-seltext_m = text-068.
        fieldcat-seltext_l = text-068.
        fieldcat-ddictxt = 'L'.
      WHEN 'LEAVINGDATE'.
        fieldcat-seltext_m = text-069.
        fieldcat-seltext_l = text-069.
        fieldcat-ddictxt = 'L'.
      WHEN 'KOSTL'.
        fieldcat-ddictxt = 'L'.
    ENDCASE.
    MODIFY fieldcat.
  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM DISPLAY_LIST
*----------------------------------------------------------------------*
FORM display_list.

  DATA: is_layout            TYPE slis_layout_alv,
        func_name(22),
        l_i_save(1)          TYPE c,
        l_i_callback_program LIKE sy-repid.

  l_i_callback_program = 'ZHCM_HRST_48_PA_VA'.
  l_i_save = 'A'.

  is_layout-zebra = 'X'.
  is_layout-numc_sum = 'X'.
  is_layout-subtotals_text = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_buffer_active    = ''
      i_callback_program = l_i_callback_program
      i_save             = l_i_save
      is_layout          = is_layout
      it_fieldcat        = fieldcat[]
    TABLES
      t_outtab           = alv_tab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MOVE 'REUSE_ALV_GRID_DISPLAY' TO func_name.
    PERFORM append_error
                   USING space
                         'E'
                         '005'
                         sy-subrc
                         func_name
                         space
                         space.
  ENDIF.

ENDFORM.                               " FORM DISPLAY_LIST
