*&---------------------------------------------------------------------*
*&  Include           ZFTPM_NOTA_F01*&
*&---------------------------------------------------------------------*
*
DATA: lt_zetpm_imp_nota TYPE zetpm_imp_nota.
DATA: lw_zetpm_imp_nota TYPE zetpm_imp_nota_t.
DATA: lt_zcatalogo TYPE zcatalogo_t.
DATA: lw_viqmfe TYPE viqmfe.
DATA: lt_viqmur TYPE zt_viqmur.
DATA: lw_viqmur TYPE zcatviqmur.
DATA:  vl_fm_name TYPE rs38l_fnam.

DATA:      l_langu      LIKE sy-langu.
DATA:      l_anz_lines  LIKE sy-tfill.
DATA:      l_uebernehmen.
DATA:      l_change     LIKE viqmel-indtx.
DATA:      l_level(10).
DATA:      l_textobjekt LIKE thead-tdname.
DATA:      l_tline_tab  LIKE tline OCCURS 2 WITH HEADER LINE.


DATA: BEGIN OF l_line_tab OCCURS 5.
        INCLUDE STRUCTURE tline.
DATA: END OF l_line_tab.

DATA: ltx_head     LIKE thead.

DATA ltxttab TYPE tline OCCURS 100 WITH HEADER LINE.
FREE: lw_zetpm_imp_nota, ltxttab[], lt_zcatalogo, lt_viqmur..
CLEAR: lt_zetpm_imp_nota.

MOVE-CORRESPONDING viqmel TO lt_zetpm_imp_nota.

CHECK lt_zetpm_imp_nota IS NOT INITIAL.

lt_zetpm_imp_nota-edate = sy-datum.

IF lt_zetpm_imp_nota-equnr IS NOT INITIAL.
  SELECT SINGLE *
  FROM eqkt
  INTO @DATA(_eqkt)
    WHERE equnr EQ @lt_zetpm_imp_nota-equnr.
  lt_zetpm_imp_nota-eqktx = _eqkt-eqktx.
ENDIF.

SELECT SINGLE *
FROM tq80_t
INTO @DATA(_tq80_t)
  WHERE qmart EQ @lt_zetpm_imp_nota-qmart
   AND  spras EQ 'P'.
lt_zetpm_imp_nota-qmartx = _tq80_t-qmartx.

SELECT SINGLE *
FROM crhd
INTO @DATA(_crhd)
  WHERE objid EQ @lt_zetpm_imp_nota-arbpl.
lt_zetpm_imp_nota-objid = _crhd-objid.
lt_zetpm_imp_nota-arbpl = _crhd-arbpl.

SELECT SINGLE *
FROM ihpa
INTO @DATA(_ihpa)
  WHERE objnr EQ @lt_zetpm_imp_nota-objnr.
lt_zetpm_imp_nota-parnr_vera = _ihpa-parnr.

IF lt_zetpm_imp_nota-parnr_vera IS NOT INITIAL.
  SELECT SINGLE *
  FROM pa0002
  INTO @DATA(_pa0002)
    WHERE pernr EQ @lt_zetpm_imp_nota-parnr_vera.
  lt_zetpm_imp_nota-cname = _pa0002-cname.
ENDIF.

IF  lt_zetpm_imp_nota-qmnam IS NOT INITIAL.
  CLEAR _pa0002.
  SELECT SINGLE *
  FROM pa0002
  INTO _pa0002
    WHERE pernr EQ lt_zetpm_imp_nota-qmnam.
  lt_zetpm_imp_nota-c_name = _pa0002-cname.
ENDIF.

SELECT SINGLE *
FROM iflo
INTO @DATA(_iflo)
  WHERE tplnr EQ @lt_zetpm_imp_nota-tplnr.
lt_zetpm_imp_nota-pltxt = _iflo-pltxt.

SELECT SINGLE *
  FROM cskt
  INTO @DATA(_cskt)
    WHERE kostl EQ @lt_zetpm_imp_nota-kostl.
lt_zetpm_imp_nota-ktext = _cskt-ktext.

SELECT SINGLE *
FROM t001w
INTO @DATA(_t001w)
WHERE werks EQ @lt_zetpm_imp_nota-swerk.
lt_zetpm_imp_nota-name1 = _t001w-name1.

SELECT SINGLE *
FROM crtx
INTO @DATA(_crtx)
WHERE objid EQ @lt_zetpm_imp_nota-objid.
lt_zetpm_imp_nota-cr_ktext = _crtx-ktext.

APPEND lt_zetpm_imp_nota TO lw_zetpm_imp_nota.

*Preenchendo dados analise de falha.
SELECT *
FROM viqmfe
INTO CORRESPONDING FIELDS OF TABLE lt_zcatalogo
  WHERE qmnum EQ lt_zetpm_imp_nota-qmnum.

LOOP AT lt_zcatalogo ASSIGNING FIELD-SYMBOL(<zw_catalogo>).

  SELECT SINGLE *
  FROM qpct
  INTO @DATA(_qpct)
    WHERE katalogart EQ @<zw_catalogo>-fekat
     AND codegruppe  EQ @<zw_catalogo>-fegrp
     AND code        EQ @<zw_catalogo>-fecod.
  IF _qpct IS INITIAL.
    <zw_catalogo>-ztext = space.
  ELSE.
    <zw_catalogo>-ztext = _qpct-kurztext.
  ENDIF.

  CLEAR _qpct.
  SELECT SINGLE *
  FROM qpct
  INTO _qpct
    WHERE katalogart EQ <zw_catalogo>-otkat
     AND codegruppe  EQ <zw_catalogo>-otgrp
     AND code        EQ <zw_catalogo>-oteil.
  IF _qpct IS INITIAL.
    <zw_catalogo>-kurztext = space.
  ELSE.
    <zw_catalogo>-kurztext = _qpct-kurztext.
  ENDIF.
ENDLOOP.

SELECT *
FROM viqmur
INTO CORRESPONDING FIELDS OF TABLE lt_viqmur
  WHERE qmnum EQ lt_zetpm_imp_nota-qmnum.

LOOP AT lt_viqmur ASSIGNING FIELD-SYMBOL(<ls_viqmur>).
  CLEAR: _qpct.
  SELECT SINGLE *
  FROM qpct
  INTO _qpct
    WHERE katalogart EQ <ls_viqmur>-urkat
     AND codegruppe  EQ <ls_viqmur>-urgrp
     AND code        EQ <ls_viqmur>-urcod.
  IF _qpct IS INITIAL.
    <ls_viqmur>-kurztext = space.
  ELSE.
    <ls_viqmur>-kurztext = _qpct-kurztext.
  ENDIF.
ENDLOOP.

IF viqmel-indtx EQ 'X'.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id        = headltx-tdid
      language  = headltx-tdspras
      name      = headltx-tdname
      object    = headltx-tdobject
    IMPORTING
      header    = headltx
    TABLES
      lines     = ltxttab
    EXCEPTIONS
      not_found = 1.
ENDIF.

"verificar se ordem pertence a empresa tgg.
CLEAR: ls_t001k.
CALL FUNCTION 'AIP01_PLANT_DETERMINE'
  EXPORTING
    i_werks  = lt_zetpm_imp_nota-swerk
  IMPORTING
    es_t001k = ls_t001k
  EXCEPTIONS
    OTHERS   = 1.


SELECT SINGLE *
  FROM setleaf
  INTO @DATA(i_data)
    WHERE setname EQ 'MAGI_PM_IW32'
      AND valfrom EQ @ls_t001k-bukrs.

CLEAR: s_name, s_name1.
IF i_data IS INITIAL.
  s_name = 'ZPMF0004'.
  s_name1 = 'ZPMF0005'.
ELSE.
  s_name = 'ZPMF0013'.
  s_name1 = 'ZPMF0012'.
ENDIF.

v_control-preview     = abap_true.
v_control-no_dialog   = abap_true.
v_output-tddest       = 'LOCL'.
v_output-tdnoprint    = abap_true.

CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname           = s_name
  IMPORTING
    fm_name            = vl_fm_name
  EXCEPTIONS
    no_form            = 1
    no_function_module = 2
    OTHERS             = 3.

"FF - inicio 11/12/2023 - Solic. Guilherme Ferreira. Erro ao imprimir texto longo maior que 28 linhas
LOOP AT ltxttab ASSIGNING FIELD-SYMBOL(<fs_txt>).
  IF sy-tabix > 28.
    DELETE ltxttab INDEX sy-tabix .
  ENDIF.
ENDLOOP.
"FF - fim 11/12/2023


CALL FUNCTION vl_fm_name
  EXPORTING
*   CONTROL_PARAMETERS = V_CONTROL
*   OUTPUT_OPTIONS    = V_OUTPUT
    gt_zetpm_imp_nota = lt_zetpm_imp_nota
*   USER_SETTINGS     = 'X'
  TABLES
    gt_ltxttab        = ltxttab[]
    gt_viqmfe         = lt_zcatalogo
    gt_viqmur         = lt_viqmur
    gt_nota           = lw_zetpm_imp_nota
  EXCEPTIONS
    formatting_error  = 1
    internal_error    = 2
    send_error        = 3
    user_canceled     = 4
    OTHERS            = 5.

IF sy-subrc EQ 0.

  DATA: p_respo TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING        "TITLEBAR = 'Confirmar'
      text_question         = 'Deseja imprimir formulario para anotações de materiais?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      display_cancel_button = ' '
    IMPORTING
      answer                = p_respo.


  IF p_respo = 1.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = s_name1
      IMPORTING
        fm_name            = vl_fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    CALL FUNCTION vl_fm_name
      EXPORTING
        gt_zetpm_imp_nota = lt_zetpm_imp_nota
      TABLES
        gt_ltxttab        = ltxttab[]
        gt_viqmfe         = lt_zcatalogo
        gt_viqmur         = lt_viqmur
        gt_nota           = lw_zetpm_imp_nota
      EXCEPTIONS
        formatting_error  = 1
        internal_error    = 2
        send_error        = 3
        user_canceled     = 4
        OTHERS            = 5.
  ENDIF.
ENDIF.
