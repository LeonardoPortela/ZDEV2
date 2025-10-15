*&---------------------------------------------------------------------*
*& Report  ZOB_COST_CENTER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zob_cost_center.

CONSTANTS: lc_name TYPE tvarvc-name VALUE 'ZOB_COST_CENTER_DIAS'.

DATA: lv_data TYPE erdat.

DATA: lv_lines TYPE sy-tabix,
      lv_qtd   TYPE sy-tabix.

DATA: lt_centro_aux   TYPE zfit_ob_centro_custo.
DATA: lt_centros_cust TYPE zfit_ob_centro_custo.

IF sy-batch EQ abap_true.
  TRY.
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.
  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.
ENDIF.

SELECT *
  FROM tvarvc
  INTO @DATA(lw_tvarv)
  UP TO 1 ROWS
  WHERE name = @lc_name.
ENDSELECT.
IF sy-subrc IS INITIAL.
  lv_data = sy-datum - lw_tvarv-low.
ENDIF.


SELECT t~ersda,
       t~bukrs,
       t~gsber,
       c~spras,
       c~kokrs,
       c~kostl,
       t~datbi,
       t~datab,
       c~ktext,
       c~ltext,
       c~mctxt,
       t~verak,
       t~abtei,
       t~bkzkp
     FROM cskt AS c
     INNER JOIN csks AS t
     ON c~kostl = t~kostl
    AND c~kokrs = t~kokrs
    AND c~datbi = t~datbi
   INTO CORRESPONDING FIELDS OF TABLE @lt_centros_cust
  WHERE t~ersda GE @lv_data.

"Buscar Centro Custos alterados
SELECT objectid, udate, utime
  FROM cdhdr
  INTO TABLE @DATA(lt_cdhdr)
  WHERE udate >= @lv_data
    AND objectclas = 'KOSTL'.
IF sy-subrc IS INITIAL.

  DATA(lt_cdhdr_aux) = lt_cdhdr.
  SORT lt_cdhdr_aux BY objectid.
  DELETE ADJACENT DUPLICATES FROM lt_cdhdr_aux COMPARING objectid.

  SELECT t~ersda,
         t~bukrs,
         t~gsber,
         c~spras,
         c~kokrs,
         c~kostl,
         t~datbi,
         t~datab,
         c~ktext,
         c~ltext,
         c~mctxt,
         t~verak,
         t~abtei,
         t~bkzkp
       FROM cskt AS c
       INNER JOIN csks AS t
       ON c~kostl = t~kostl
      AND c~kokrs = t~kokrs
      AND c~datbi = t~datbi
     APPENDING CORRESPONDING FIELDS OF TABLE @lt_centros_cust
     FOR ALL ENTRIES IN @lt_cdhdr_aux
    WHERE t~kokrs = @lt_cdhdr_aux-objectid(4)
      AND t~kostl = @lt_cdhdr_aux-objectid+4(10).

ENDIF.

SORT lt_centros_cust BY kokrs kostl datbi spras.
DELETE ADJACENT DUPLICATES FROM lt_centros_cust COMPARING kokrs kostl datbi spras.

LOOP AT lt_centros_cust ASSIGNING FIELD-SYMBOL(<fs_centro_custo>).

  <fs_centro_custo>-ktext = zcl_string=>tira_acentos( i_texto = CONV #( <fs_centro_custo>-ktext ) ).
  <fs_centro_custo>-ltext = zcl_string=>tira_acentos( i_texto = CONV #( <fs_centro_custo>-ltext ) ).
  <fs_centro_custo>-mctxt = zcl_string=>tira_acentos( i_texto = CONV #( <fs_centro_custo>-mctxt ) ).
  <fs_centro_custo>-verak = zcl_string=>tira_acentos( i_texto = CONV #( <fs_centro_custo>-verak ) ).

ENDLOOP.

CHECK lt_centros_cust[] IS NOT INITIAL.


lv_lines = lines( lt_centros_cust ).

LOOP AT lt_centros_cust ASSIGNING <fs_centro_custo>.

  ADD 1 TO lv_qtd.

  APPEND <fs_centro_custo> TO lt_centro_aux.

  CHECK  lv_qtd = 500 OR lv_qtd = lv_lines.

  TRY .
      zcl_int_ob_centro_custo=>zif_integracao_outbound~get_instance(
        )->execute_request( i_info_request = lt_centro_aux
        ).

    CATCH zcx_integracao INTO DATA(ex_integra).    "
      ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    CATCH zcx_error INTO DATA(ex_error).    "  "
      ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

  lv_lines = lv_lines - lv_qtd.
  CLEAR lv_qtd.
  REFRESH: lt_centro_aux.

ENDLOOP.
