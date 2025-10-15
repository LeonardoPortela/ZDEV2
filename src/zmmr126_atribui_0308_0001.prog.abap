*----------------------------------------------------------------------*
***INCLUDE ZMMR126_ATRIBUI_0308.
*----------------------------------------------------------------------*

DATA: lc_0308_nr_perc_ava TYPE zde_nr_perc_ava_arq,
      ck_0308_nr_perc_ava TYPE char01.

DATA: vl_nr_perc_ava_arq TYPE c LENGTH 6,
      vl_nr_perc_ava_que TYPE c LENGTH 6,
      vl_nr_perc_ava_mof TYPE c LENGTH 6,
      vl_nr_perc_ava_pic TYPE c LENGTH 6,
      vl_nr_perc_ava_fer TYPE c LENGTH 6,
      vl_nr_perc_ava_ger TYPE c LENGTH 6,
      vl_nr_perc_ava_ard TYPE c LENGTH 6,
      vl_nr_perc_ava_ges TYPE c LENGTH 6,
      lc_field_set_0308  TYPE c LENGTH 50.

*&---------------------------------------------------------------------*
*&      Form  ATRIBUI_PERC_SUB_AVARIADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM atribui_perc_sub_avariado  USING p_nr_perc_ava TYPE zde_nr_perc_ava_arq CHANGING ok TYPE char01.

*-CS2021000183-#71105-29.03.2022-JT-inicio
  IF zde_zsdt0001cg_alv-nr_perc_ava_arq <> zde_zsdt0001cg_alv-nr_perc_ava_arq_old.
    objeto->ck_digitado_ava_arq = abap_false.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_ava_que <> zde_zsdt0001cg_alv-nr_perc_ava_que_old.
    objeto->ck_digitado_ava_que = abap_false.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_ava_mof <> zde_zsdt0001cg_alv-nr_perc_ava_mof_old.
    objeto->ck_digitado_ava_mof = abap_false.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_ava_pic <> zde_zsdt0001cg_alv-nr_perc_ava_pic_old.
    objeto->ck_digitado_ava_pic = abap_false.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_ava_fer <> zde_zsdt0001cg_alv-nr_perc_ava_fer_old.
    objeto->ck_digitado_ava_fer = abap_false.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_ava_ger <> zde_zsdt0001cg_alv-nr_perc_ava_ger_old.
    objeto->ck_digitado_ava_ger = abap_false.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_ava_ard <> zde_zsdt0001cg_alv-nr_perc_ava_ard_old.
    objeto->ck_digitado_ava_ard = abap_false.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_ava_ges <> zde_zsdt0001cg_alv-nr_perc_ava_ges_old.
    objeto->ck_digitado_ava_ges = abap_false.
  ENDIF.
*-CS2021000183-#71105-29.03.2022-JT-fim

  lc_0308_nr_perc_ava = p_nr_perc_ava.
  ck_0308_nr_perc_ava = abap_false.
  CLEAR: lc_field_set_0308.

  CALL SCREEN 0308 STARTING AT 30 15.
  ok = ck_0308_nr_perc_ava.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0308  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0308 INPUT.

  CASE ok_code.
    WHEN 'CONFIRMAR'.
      PERFORM verifica_subtotal_avariado CHANGING sy-subrc.
      IF sy-subrc IS INITIAL.
        ck_0308_nr_perc_ava = abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0308  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0308 OUTPUT.

  SET PF-STATUS 'PF0308'.
  SET TITLEBAR 'TL0308'.

  LOOP AT SCREEN.

    TRY .
        CASE screen-name.
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_ARQ'.
            objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_arq IMPORTING e_ck_carac = DATA(e_ck_c) e_ck_obrigatorio = DATA(e_ck_o) ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_QUE'.
            objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_que IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_MOF'.
            objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_mof IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_PIC'.
            objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_pic IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_FER'.
            objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_fer IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_GER'.
            objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ger IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_ARD'.
            objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ard IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
          WHEN 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_GES'.
            objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ges IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
        ENDCASE.
      CATCH zcx_carga.    "
    ENDTRY.

    IF e_ck_c EQ abap_true.
      screen-input = '1'.
    ELSE.
      screen-input = '0'.
    ENDIF.

    IF e_ck_o EQ abap_true.
      screen-required = '1'.
    ELSE.
      screen-required = '0'.
    ENDIF.
    MODIFY SCREEN.

  ENDLOOP.

  IF lc_field_set_0308 IS NOT INITIAL.
    SET CURSOR FIELD lc_field_set_0308 OFFSET pos.
    CLEAR: lc_field_set_0308.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SUBTOTAL_AVARIADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM verifica_subtotal_avariado CHANGING p_subrc TYPE sy-subrc.

  "Verifica Obrigatoriedade de Digitar

  DATA(lc_subrc) = p_subrc.

  lc_subrc = 1.

  TRY .

      objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_arq IMPORTING e_ck_carac = DATA(e_ck_c) e_ck_obrigatorio = DATA(e_ck_o) ).
      IF e_ck_o EQ abap_true AND objeto->ck_digitado_ava_arq EQ abap_false.
        lc_field_set_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_ARQ'.
        MESSAGE s238 DISPLAY LIKE 'E'.
        p_subrc = lc_subrc.
        EXIT.
      ENDIF.

      objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_que IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
      IF e_ck_o EQ abap_true AND objeto->ck_digitado_ava_que EQ abap_false.
        lc_field_set_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_QUE'.
        MESSAGE s239 DISPLAY LIKE 'E'.
        p_subrc = lc_subrc.
        EXIT.
      ENDIF.

      objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_mof IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
      IF e_ck_o EQ abap_true AND objeto->ck_digitado_ava_mof EQ abap_false.
        lc_field_set_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_MOF'.
        MESSAGE s240 DISPLAY LIKE 'E'.
        p_subrc = lc_subrc.
        EXIT.
      ENDIF.

      objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_pic IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
      IF e_ck_o EQ abap_true AND objeto->ck_digitado_ava_pic EQ abap_false.
        lc_field_set_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_PIC'.
        MESSAGE s241 DISPLAY LIKE 'E'.
        p_subrc = lc_subrc.
        EXIT.
      ENDIF.

      objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_fer IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
      IF e_ck_o EQ abap_true AND objeto->ck_digitado_ava_fer EQ abap_false.
        lc_field_set_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_FER'.
        MESSAGE s242 DISPLAY LIKE 'E'.
        p_subrc = lc_subrc.
        EXIT.
      ENDIF.

      objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ger IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
      IF e_ck_o EQ abap_true AND objeto->ck_digitado_ava_ger EQ abap_false.
        lc_field_set_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_GER'.
        MESSAGE s243 DISPLAY LIKE 'E'.
        p_subrc = lc_subrc.
        EXIT.
      ENDIF.

      objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ard IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
      IF e_ck_o EQ abap_true AND objeto->ck_digitado_ava_ard EQ abap_false.
        lc_field_set_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_ARD'.
        MESSAGE s244 DISPLAY LIKE 'E'.
        p_subrc = lc_subrc.
        EXIT.
      ENDIF.

      objeto->get_ck_sub_carac_ava_material( EXPORTING i_sub_carac_ava  = zif_carga=>st_tp_caract_sub_ges IMPORTING e_ck_carac = e_ck_c e_ck_obrigatorio = e_ck_o ).
      IF e_ck_o EQ abap_true AND objeto->ck_digitado_ava_ges EQ abap_false.
        lc_field_set_0308 = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA_GES'.
        MESSAGE s245 DISPLAY LIKE 'E'.
        p_subrc = lc_subrc.
        EXIT.
      ENDIF.

    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      p_subrc = lc_subrc.
      EXIT.
  ENDTRY.

  DATA: lc_total TYPE zde_nr_perc_ava_arq.

  lc_total = zde_zsdt0001cg_alv-nr_perc_ava_arq + zde_zsdt0001cg_alv-nr_perc_ava_que +
             zde_zsdt0001cg_alv-nr_perc_ava_mof + zde_zsdt0001cg_alv-nr_perc_ava_pic +
             zde_zsdt0001cg_alv-nr_perc_ava_fer + zde_zsdt0001cg_alv-nr_perc_ava_ger +
             zde_zsdt0001cg_alv-nr_perc_ava_ard + zde_zsdt0001cg_alv-nr_perc_ava_ges.

  IF lc_total NE lc_0308_nr_perc_ava.
    MESSAGE s236 DISPLAY LIKE 'E'.
  ELSE.
    lc_subrc = 0.
  ENDIF.

  p_subrc = lc_subrc.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0308_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0308_exit INPUT.

  ck_0308_nr_perc_ava = abap_false.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_01  INPUT
*&---------------------------------------------------------------------*
MODULE sub_perc_01 INPUT.
  objeto->ck_digitado_ava_arq            = abap_true.
  zde_zsdt0001cg_alv-nr_perc_ava_arq_old = zde_zsdt0001cg_alv-nr_perc_ava_arq. "*-CS2021000183-#71105-29.03.2022-JT
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_02  INPUT
*&---------------------------------------------------------------------*
MODULE sub_perc_02 INPUT.
  objeto->ck_digitado_ava_que            = abap_true.
  zde_zsdt0001cg_alv-nr_perc_ava_que_old = zde_zsdt0001cg_alv-nr_perc_ava_que. "*-CS2021000183-#71105-29.03.2022-JT
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_03  INPUT
*&---------------------------------------------------------------------*
MODULE sub_perc_03 INPUT.
  objeto->ck_digitado_ava_mof            = abap_true.
  zde_zsdt0001cg_alv-nr_perc_ava_mof_old = zde_zsdt0001cg_alv-nr_perc_ava_mof. "*-CS2021000183-#71105-29.03.2022-JT
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_04  INPUT
*&---------------------------------------------------------------------*
MODULE sub_perc_04 INPUT.
  objeto->ck_digitado_ava_pic            = abap_true.
  zde_zsdt0001cg_alv-nr_perc_ava_pic_old = zde_zsdt0001cg_alv-nr_perc_ava_pic. "*-CS2021000183-#71105-29.03.2022-JT
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_05  INPUT
*&---------------------------------------------------------------------*
MODULE sub_perc_05 INPUT.
  objeto->ck_digitado_ava_fer            = abap_true.
  zde_zsdt0001cg_alv-nr_perc_ava_fer_old = zde_zsdt0001cg_alv-nr_perc_ava_fer. "*-CS2021000183-#71105-29.03.2022-JT
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_06  INPUT
*&---------------------------------------------------------------------*
MODULE sub_perc_06 INPUT.
  objeto->ck_digitado_ava_ger            = abap_true.
  zde_zsdt0001cg_alv-nr_perc_ava_ger_old = zde_zsdt0001cg_alv-nr_perc_ava_ger. "*-CS2021000183-#71105-29.03.2022-JT
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_07  INPUT
*&---------------------------------------------------------------------*
MODULE sub_perc_07 INPUT.
  objeto->ck_digitado_ava_ard            = abap_true.
  zde_zsdt0001cg_alv-nr_perc_ava_ard_old = zde_zsdt0001cg_alv-nr_perc_ava_ard. "*-CS2021000183-#71105-29.03.2022-JT
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SUB_PERC_08  INPUT
*&---------------------------------------------------------------------*
MODULE sub_perc_08 INPUT.
  objeto->ck_digitado_ava_ges            = abap_true.
  zde_zsdt0001cg_alv-nr_perc_ava_ges_old = zde_zsdt0001cg_alv-nr_perc_ava_ges. "*-CS2021000183-#71105-29.03.2022-JT
ENDMODULE.
