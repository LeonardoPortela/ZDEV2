*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
********************************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                                     *
* Data desenv ...: 14.03.2024                                                              *
* Objetivo    ...: Relatório Consulta de Vendedores                                        *
* Transação   ...: ZSDT026                                                                 *
* Autor       ...: ITSOUZA                                                                 *
********************************************************************************************
REPORT zsdr0210.

TYPES: BEGIN OF ty_saida,
         bukrs TYPE t001k-bukrs,
         vkbur TYPE tvbvk-vkbur,
         name1 TYPE t001w-name1,
         vkgrp TYPE tvbvk-vkgrp,
         bezei TYPE tvgrt-bezei,
       END OF ty_saida,

       BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
         vkorg TYPE t001w-vkorg,
       END OF ty_t001w.

DATA: t_saida  TYPE TABLE OF ty_saida,
      t_tvkgr      TYPE TABLE OF tvkgr,
      t_tvbvk      TYPE TABLE OF tvbvk,
      t_tvgrt      TYPE TABLE OF tvgrt,
      t_t001w      TYPE TABLE OF ty_t001w.

* Declara AVL GRID
DATA: t_fcat   TYPE slis_t_fieldcat_alv,
      s_fcat   LIKE LINE OF t_fcat,
      s_layout TYPE slis_layout_alv.

DATA: wa_saida TYPE ty_saida.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME .
    PARAMETERS: p_ven RADIOBUTTON GROUP g1 USER-COMMAND hide DEFAULT 'X',
                p_esc RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
  SELECT-OPTIONS: p_bukrs   FOR wa_saida-bukrs MODIF ID sc3 NO INTERVALS,
                  p_vkbur   FOR wa_saida-vkbur MODIF ID sc3 NO INTERVALS,
                  p_vkgrp   FOR wa_saida-vkgrp MODIF ID sc3 NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'SC3'.
      IF p_ven IS NOT INITIAL.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


START-OF-SELECTION.
  PERFORM f_select_data.
  PERFORM f_process_data.
  PERFORM f_check_data.
  PERFORM f_alv.


*&---------------------------------------------------------------------*
*& Form f_select_data
*&---------------------------------------------------------------------*
FORM f_select_data .
  IF p_ven IS NOT INITIAL.
    SELECT * FROM tvkgr
      INTO TABLE t_tvkgr.

    IF sy-subrc EQ 0.
      "Descrição do grupo de vendedores
      SELECT * FROM tvgrt
        INTO TABLE t_tvgrt
        FOR ALL ENTRIES IN t_tvkgr
        WHERE vkgrp EQ t_tvkgr-vkgrp
          AND spras EQ sy-langu.
    ENDIF.

  ELSE.
    "Busca pela equipe de vendas e grupo vendedores
    SELECT * FROM tvbvk
      INTO TABLE t_tvbvk
      WHERE vkbur IN p_vkbur
        AND vkgrp IN p_vkgrp.

    IF sy-subrc EQ 0.
      "Descrição do grupo de vendedores
      SELECT * FROM tvgrt
      INTO TABLE t_tvgrt
      FOR ALL ENTRIES IN t_tvbvk
      WHERE vkgrp EQ t_tvbvk-vkgrp
        AND spras EQ sy-langu.

      SELECT werks name1 vkorg FROM t001w
        INTO TABLE t_t001w
        FOR ALL ENTRIES IN t_tvbvk
        WHERE werks EQ t_tvbvk-vkbur
          AND vkorg IN p_bukrs.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_process_data
*&---------------------------------------------------------------------*
FORM f_process_data .
  IF p_ven IS NOT INITIAL.
    LOOP AT t_tvkgr INTO DATA(wa_tvkgr).
      wa_saida-vkgrp = wa_tvkgr-vkgrp.
      READ TABLE t_tvgrt INTO DATA(wa_tvgrt) WITH KEY vkgrp = wa_tvkgr-vkgrp.
      IF sy-subrc EQ 0.
        wa_saida-bezei = wa_tvgrt-bezei.
      ENDIF.
      APPEND WA_saida TO t_saida.
    ENDLOOP.
  ELSE.
    LOOP AT t_tvbvk INTO DATA(wa_tvbvk).
      READ TABLE t_t001w INTO DATA(wa_t001w) WITH KEY werks = wa_tvbvk-vkbur.
      IF sy-subrc EQ 0.
        wa_saida-bukrs = wa_t001w-vkorg.
        wa_saida-name1 = wa_t001w-name1.
      ELSEIF p_bukrs IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE t_tvgrt INTO wa_tvgrt WITH KEY vkgrp = wa_tvbvk-vkgrp.
      IF sy-subrc EQ 0.
        wa_saida-bezei = wa_tvgrt-bezei.
      ENDIF.

      wa_saida-vkbur = wa_tvbvk-vkbur.
      wa_saida-vkgrp = wa_tvbvk-vkgrp.
      APPEND WA_saida TO t_saida.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv
*&---------------------------------------------------------------------*
FORM f_alv .

  PERFORM f_alv_layout.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     IS_VARIANT         = GS_VARIANT_C
      i_callback_pf_status_set = 'SET_PF_STATUS'
*     I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
      it_fieldcat        = t_fcat[]
      is_layout          = s_layout
      i_save             = 'A'
*     IT_EVENTS          = EVENTS
*     IS_PRINT           = T_PRINT
*     IT_SORT            = T_SORT[]
    TABLES
      t_outtab           = t_saida.


ENDFORM.

*----------------------------------------------------------*
*       FORM SET_PF_STATUS                                 *
*----------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_alv_layout
*&---------------------------------------------------------------------*
FORM f_alv_layout .

  s_layout-zebra             = abap_true.
  s_layout-colwidth_optimize = abap_true.

  IF p_ven IS NOT INITIAL.
    PERFORM f_set_layout USING:
        1  'TVGRT'       'VKGRP'             'T_SAIDA' 'VKGRP'           ' '    ' ' ,      "   Grupo de vendas
        2  'TVGRT'       'BEZEI'             'T_SAIDA' 'BEZEI'           ' '    ' ' .      "   Descrição
  ELSE.
    PERFORM f_set_layout USING:
        1  'T001K'       'BUKRS'             'T_SAIDA' 'BUKRS'           ' '    ' ' ,      "   Empresa
        2  'TVBVK'       'VKBUR'             'T_SAIDA' 'VKBUR'           ' '    ' ' ,      "   Escritório de vendas
        3  'T001W'       'NAME1'             'T_SAIDA' 'NAME1'           ' '    ' ' ,      "   Descrição
        4  'TVBVK'       'VKGRP'             'T_SAIDA' 'VKGRP'           ' '    ' ' ,      "   Equipe de Venda
        5  'TVGRT'       'BEZEI'             'T_SAIDA' 'BEZEI'           ' '    ' ' .      "   Descrição
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SET_LAYOUT
*&---------------------------------------------------------------------*
FORM f_set_layout USING VALUE(p_col_pos)       TYPE i
                        VALUE(p_ref_tabname)   LIKE dd02d-tabname
                        VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                        VALUE(p_tabname)       LIKE dd02d-tabname
                        VALUE(p_field)         LIKE dd03d-fieldname
                        VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                        VALUE(p_outputlen).

  CLEAR: s_fcat.


  s_fcat-fieldname     = p_field.
  s_fcat-tabname       = p_tabname.
  s_fcat-ref_tabname   = p_ref_tabname.
  s_fcat-ref_fieldname = p_ref_fieldname.
  s_fcat-key           = ' '.
  s_fcat-key_sel       = 'X'.
  s_fcat-col_pos       = p_col_pos.
  s_fcat-no_out        = ' '.
  s_fcat-seltext_s     = p_scrtext_l.
  s_fcat-seltext_m     = p_scrtext_l.
  s_fcat-seltext_l     = p_scrtext_l.

  IF p_scrtext_l IS NOT INITIAL.
    s_fcat-reptext_ddic  = p_scrtext_l.
  ENDIF.

  APPEND s_fcat TO t_fcat.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_check_data
*&---------------------------------------------------------------------*
FORM f_check_data .
  IF t_saida IS INITIAL.
    MESSAGE text-e01 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.
