*&---------------------------------------------------------------------*
*& Include ZAA01TOP                                          PoolMóds.        ZAA01
*&
*&---------------------------------------------------------------------*

PROGRAM  zaa01.

TABLES: zaa_controle_hip,
        zaa_controle_doc,
        anla.

data: BEGIN OF ct_docu OCCURS 0.
  INCLUDE STRUCTURE TLINE.
  data: end of ct_docu.

DATA: BEGIN OF t_hip OCCURS 0.
        INCLUDE STRUCTURE zaa_controle_hip.
DATA: END OF t_hip.

DATA: BEGIN OF t_hiped OCCURS 0.
        INCLUDE STRUCTURE zaa_controle_hip.
DATA: END OF t_hiped.

DATA: w_txt50   TYPE anla-txt50,
      w_txa50   TYPE anla-txa50,
      w_anln1   TYPE anla-anln1,
      w_anln2   TYPE anla-anln2,
      w_bukrs   TYPE anla-bukrs,
      w_anlhtxt TYPE anlh-anlhtxt,
      w_buzei   type ZAA_CONTROLE_HIP-BUZEI,
      w_ucomm   TYPE sy-ucomm,
      w_mais    TYPE sy-ucomm,
      w_clear   TYPE sy-ucomm,
      w_up.

DATA:   BEGIN OF htheader.
        INCLUDE STRUCTURE thead.
DATA:   END OF htheader.

*---dynpro 113 (docu)---------------------------------------------------
data: g_custom_container  type ref to cl_gui_custom_container,
      g_docu_editor       type ref to cl_gui_textedit,
      gt_lines            type tywf_tline,
      g_get_template      type c.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TABSTRIP_110'
CONSTANTS: BEGIN OF c_tabstrip_110,
             tab1 LIKE sy-ucomm VALUE 'TABSTRIP_110_FC1',
             tab2 LIKE sy-ucomm VALUE 'TABSTRIP_110_FC2',
             tab3 LIKE sy-ucomm VALUE 'TABSTRIP_110_FC3',
           END OF c_tabstrip_110.
*&SPWIZARD: DATA FOR TABSTRIP 'TABSTRIP_110'
CONTROLS:  tabstrip_110 TYPE TABSTRIP.
DATA:      BEGIN OF g_tabstrip_110,
             subscreen   LIKE sy-dynnr,
             prog        LIKE sy-repid VALUE 'ZAA01',
             pressed_tab LIKE sy-ucomm VALUE c_tabstrip_110-tab1,
           END OF g_tabstrip_110.
DATA:      ok_code LIKE sy-ucomm.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_HIP' ITSELF
CONTROLS: TC_HIP TYPE TABLEVIEW USING SCREEN 0112.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_HIP'
DATA:     G_TC_HIP_LINES  LIKE SY-LOOPC.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_HIP_EDIT' ITSELF
CONTROLS: TC_HIP_EDIT TYPE TABLEVIEW USING SCREEN 0117.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_HIP_EDIT'
DATA:     G_TC_HIP_EDIT_LINES  LIKE SY-LOOPC.

data: wt_pais    type char50,
      wt_estado  type char50,
      wt_estado2 type char50,
      wt_medida  type char50,
      wt_butxt   type char50,
      wt_gsber   type char50,
      w_gsber    TYPE anlz-gsber,
      w_save.
