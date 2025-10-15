FUNCTION z_retorna_seg_terc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_MONAT) TYPE  MONAT OPTIONAL
*"     REFERENCE(P_GJAHR) TYPE  GJAHR OPTIONAL
*"     REFERENCE(P_MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(RT_INICIO) TYPE  ZLXHME_RANGE_DT_T OPTIONAL
*"     REFERENCE(RT_FINAL) TYPE  ZLXHME_RANGE_DT_T OPTIONAL
*"     REFERENCE(P_CD_INI) TYPE  J_1BTXJCD OPTIONAL
*"     REFERENCE(P_CD_FIM) TYPE  J_1BTXJCD OPTIONAL
*"  TABLES
*"      IT_SEG_TERC STRUCTURE  ZVALOR_SEG_TERC OPTIONAL
*"  CHANGING
*"     VALUE(WA_VELOR_SEGURO) TYPE  ZVALOR_SEG_TERC OPTIONAL
*"  EXCEPTIONS
*"      SEM_REGISTRO
*"----------------------------------------------------------------------

  DATA: BEGIN OF gs_monat,
          sign        TYPE c,
          option(2)   TYPE c,
          low         TYPE c LENGTH 2,
          high        TYPE c LENGTH 2,
        END OF gs_monat.

  DATA: gt_monat LIKE TABLE OF gs_monat.

  DATA: BEGIN OF gs_gjahr,
          sign        TYPE c,
          option(2)   TYPE c,
          low         TYPE c LENGTH 4,
          high        TYPE c LENGTH 4,
        END OF gs_gjahr.
  DATA: gt_gjahr LIKE TABLE OF gs_gjahr.

  DATA: BEGIN OF gs_matnr,
          sign        TYPE c,
          option(2)   TYPE c,
* ---> S4 Migration - 10/06/2023 - DG
*          low         TYPE c LENGTH 18,
*          high        TYPE c LENGTH 18,
          low         TYPE c LENGTH 40,
          high        TYPE c LENGTH 40,
* <--- S4 Migration - 10/06/2023 - DG

        END OF gs_matnr.
  DATA: gt_matnr LIKE TABLE OF gs_matnr.

  DATA: BEGIN OF gs_cd_ini,
          sign        TYPE c,
          option(2)   TYPE c,
          low         TYPE c LENGTH 15,
          high        TYPE c LENGTH 15,
        END OF gs_cd_ini.
  DATA: gt_cd_ini LIKE TABLE OF gs_cd_ini.

  DATA: BEGIN OF gs_cd_fim,
          sign        TYPE c,
          option(2)   TYPE c,
          low         TYPE c LENGTH 15,
          high        TYPE c LENGTH 15,
        END OF gs_cd_fim.
  DATA: gt_cd_fim LIKE TABLE OF gs_cd_fim.

  CLEAR: it_seg_terc[], wa_velor_seguro.

  IF p_monat IS NOT INITIAL.
    gs_monat-sign   = 'I'.
    gs_monat-option = 'EQ'.
    gs_monat-low    = p_monat.
    APPEND gs_monat TO gt_monat.
  ENDIF.

  IF p_gjahr IS NOT INITIAL.
    gs_gjahr-sign   = 'I'.
    gs_gjahr-option = 'EQ'.
    gs_gjahr-low    = p_gjahr.
    APPEND gs_gjahr TO gt_gjahr.
  ENDIF.

  IF p_matnr IS NOT INITIAL.
    gs_matnr-sign   = 'I'.
    gs_matnr-option = 'EQ'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_matnr
      IMPORTING
        output = gs_matnr-low.

    APPEND gs_matnr TO gt_matnr.
  ENDIF.

  IF p_cd_ini IS NOT INITIAL.
    gs_cd_ini-sign   = 'I'.
    gs_cd_ini-option = 'EQ'.
    gs_cd_ini-low    = p_cd_ini.
    APPEND gs_cd_ini TO gt_cd_ini.
  ENDIF.

  IF p_cd_fim IS NOT INITIAL.
    gs_cd_fim-sign   = 'I'.
    gs_cd_fim-option = 'EQ'.
    gs_cd_fim-low    = p_cd_fim.
    APPEND gs_cd_fim TO gt_cd_fim.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_seg_terc
    FROM zvalor_seg_terc
   WHERE monat IN gt_monat
     AND gjahr IN gt_gjahr
     AND cd_material IN gt_matnr
     AND dt_inicio IN rt_inicio
     AND dt_final IN rt_final
     AND cd_cidade_ini IN gt_cd_ini
     AND cd_cidade_fim IN gt_cd_fim.

  IF sy-subrc NE 0.
    MESSAGE 'Não encontrado registros de seguro de frete!' TYPE 'E' RAISING sem_registro.
  ELSE.
    READ TABLE it_seg_terc INTO wa_velor_seguro INDEX 1.
  ENDIF.

ENDFUNCTION.
