*&---------------------------------------------------------------------*
*& Report  ZRD_zpmt0074_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zpmt0074_exit.

FIELD-SYMBOLS: <fs_wa_registro_manter> TYPE zpmt0074.


FORM f_exit_zpmt0074_0001 USING p_registro_manter TYPE any.

  DATA: wl_zpmt0074 TYPE zpmt0074.

  CLEAR: wl_zpmt0074.

  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0074.

  IF wl_zpmt0074-us_criacao IS INITIAL.
    wl_zpmt0074-dt_criacao      = sy-datum.
    wl_zpmt0074-hr_criacao      = sy-uzeit.
    wl_zpmt0074-us_criacao      = sy-uname.
  ENDIF.



  MOVE-CORRESPONDING wl_zpmt0074 TO p_registro_manter.

ENDFORM.

FORM f_exit_zpmt0074_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zpmt0074 TYPE zpmt0074.

  CLEAR: wl_zpmt0074.

  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0074.

  IF wl_zpmt0074-eqtyp IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha a Ctg.equipamento!'.
    EXIT.
  ELSE.
    SELECT SINGLE * FROM t370u INTO @DATA(ls_t370u) WHERE eqtyp EQ @wl_zpmt0074-eqtyp.
    IF sy-subrc NE 0.
      MESSAGE i024(sd) WITH 'Ctg.equipamento invalida !'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.
  CLEAR: ls_t370u.

  IF wl_zpmt0074-klasse IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha a classe!'.
    EXIT.
  ELSE.

    SELECT SINGLE * FROM klah INTO @DATA(ls_klah) WHERE class EQ @wl_zpmt0074-klasse.
    IF sy-subrc NE 0.
      MESSAGE i024(sd) WITH 'Classe invalida !'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.
  CLEAR: ls_klah.

  IF wl_zpmt0074-fleet_cat IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha o tipo de veiculo!'.
    EXIT.
  ELSE.

    SELECT SINGLE * FROM t370flt INTO @DATA(ls_t370flt) WHERE fleet_cat EQ @wl_zpmt0074-fleet_cat.
    IF sy-subrc NE 0.
      MESSAGE i024(sd) WITH 'Tipo de veiculo invalido !'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.
  CLEAR: ls_t370flt.

  IF wl_zpmt0074-herst IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha o fabricante!'.
    EXIT.
  ELSE.
    SELECT SINGLE * FROM zpmt0069 INTO @DATA(ls_zpmt0069) WHERE herst EQ @wl_zpmt0074-herst.
    IF sy-subrc NE 0.
      MESSAGE i024(sd) WITH 'Fabricante invalido !'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.
  CLEAR: ls_zpmt0069.

  IF wl_zpmt0074-typbz IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha a denomin.tipo!'.
    EXIT.
  ELSE.
    SELECT SINGLE * FROM zpmt0070 INTO @DATA(ls_zpmt0070) WHERE typbz EQ @wl_zpmt0074-typbz.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM zpmt0070 INTO ls_zpmt0070 WHERE zdesc = wl_zpmt0074-typbz. "FF Solic Guilherme Ferreira via teams. Sem chamado.  11.07.2023
      IF sy-subrc <> 0.
        MESSAGE i024(sd) WITH 'Modelo invalido !'.
        p_error = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR: ls_zpmt0070.

  IF wl_zpmt0074-mptyp IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha a  ctg.pto.medição!'.
    EXIT.
  ELSE.
    SELECT SINGLE * FROM t370p_t INTO @DATA(ls_t370p_t) WHERE mptyp EQ @wl_zpmt0074-mptyp AND spras EQ @sy-langu.
    IF sy-subrc NE 0.
      MESSAGE i024(sd) WITH 'Categoria do ponto de medição invalido!'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.
  CLEAR: ls_t370p_t.

  IF wl_zpmt0074-psort IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha o item de medição!'.
    EXIT.
  ENDIF.

  IF wl_zpmt0074-pttxt IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha a denominação!'.
    EXIT.
  ENDIF.

  IF wl_zpmt0074-atnam IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha a caracteristica!'.
    EXIT.
  ELSE.
    SELECT SINGLE * FROM m_merkb INTO @DATA(ls_m_merkb) WHERE atnam EQ @wl_zpmt0074-atnam.
    IF sy-subrc NE 0.
      MESSAGE i024(sd) WITH 'Característica invalida!'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.
  CLEAR: ls_m_merkb.


  IF wl_zpmt0074-casas_decimais IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha a quantidade de casas decimais!'.
    EXIT.
  ENDIF.

  IF wl_zpmt0074-codgr IS NOT INITIAL.
    SELECT SINGLE * FROM qpgr INTO @DATA(ls_qpgr) WHERE codegruppe EQ @wl_zpmt0074-codgr.
    IF sy-subrc NE 0.
      MESSAGE i024(sd) WITH 'GrpCódigos invalido!'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.
  CLEAR: ls_qpgr.

  IF wl_zpmt0074-begru IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha o GrpAutorizações!'.
    EXIT.
  ELSE.
    SELECT SINGLE * FROM t370b INTO @DATA(ls_t370b) WHERE begru EQ @wl_zpmt0074-begru.
    IF sy-subrc NE 0.
      MESSAGE i024(sd) WITH 'GrpAutorizações invalido!'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.
  CLEAR: ls_t370b.

  IF wl_zpmt0074-psort IS NOT INITIAL.
    TRANSLATE wl_zpmt0074-psort TO UPPER CASE.
  ENDIF.
*
  IF wl_zpmt0074-pttxt IS NOT INITIAL.
    TRANSLATE wl_zpmt0074-pttxt TO UPPER CASE.
  ENDIF.
*
  IF wl_zpmt0074-atnam IS NOT INITIAL.
    TRANSLATE wl_zpmt0074-atnam TO UPPER CASE.
  ENDIF.

  "Verificar se ja existe cadastro horometro ou odometro.
  IF wl_zpmt0074-atnam EQ 'ODOMETRO'.
    SELECT SINGLE * FROM zpmt0074 INTO @DATA(ls_zpmt0074)
      WHERE atnam EQ 'HORIMETRO'
        AND klasse  EQ @wl_zpmt0074-klasse
        AND eqtyp   EQ @wl_zpmt0074-eqtyp
        AND fleet_cat EQ @wl_zpmt0074-fleet_cat
        AND herst  EQ @wl_zpmt0074-herst
        AND typbz  EQ @wl_zpmt0074-typbz.

    IF sy-subrc EQ 0.
      MESSAGE i024(sd) WITH 'Ja existe contador do tipo'
                            'HORIMETRO para este parametro!'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF wl_zpmt0074-atnam EQ 'HORIMETRO'.
    SELECT SINGLE * FROM zpmt0074 INTO ls_zpmt0074
      WHERE atnam EQ 'ODOMETRO'
        AND klasse  EQ wl_zpmt0074-klasse
        AND eqtyp   EQ wl_zpmt0074-eqtyp
        AND fleet_cat EQ wl_zpmt0074-fleet_cat
        AND herst  EQ wl_zpmt0074-herst
        AND typbz  EQ wl_zpmt0074-typbz.

    IF sy-subrc EQ 0.
      MESSAGE i024(sd) WITH 'Ja existe contador do tipo'
                            'ODOMETRO para este parametro!'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.


  MOVE-CORRESPONDING wl_zpmt0074 TO p_registro_manter.

ENDFORM.


FORM f_exit_zpmt0074_0005 CHANGING p_saida TYPE any.

  DATA: wl_zpmt0074 TYPE zpmt0074.

  CLEAR: wl_zpmt0074.

  MOVE-CORRESPONDING p_saida TO wl_zpmt0074.

  IF wl_zpmt0074-us_criacao IS INITIAL.
    wl_zpmt0074-dt_criacao      = sy-datum.
    wl_zpmt0074-hr_criacao      = sy-uzeit.
    wl_zpmt0074-us_criacao      = sy-uname.
  ELSE.
    wl_zpmt0074-dt_modif      = sy-datum.
    wl_zpmt0074-hr_modif      = sy-uzeit.
    wl_zpmt0074-us_modif      = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zpmt0074 TO p_saida.


ENDFORM.

FORM f_exit_zpmt0074_0010 TABLES t_saida.

  DATA: it_zpmt0074 TYPE TABLE OF zpmt0074.

  IF t_saida[] IS NOT INITIAL.
    MOVE-CORRESPONDING t_saida[] TO it_zpmt0074.
    SORT it_zpmt0074 BY typbz.
    FREE: t_saida[].
    MOVE-CORRESPONDING it_zpmt0074 TO t_saida[].
  ENDIF.
ENDFORM.

FORM  f_exit_zpmt0074_0016 USING p_ucomm  TYPE sy-ucomm CHANGING p_registro_manter TYPE any p_saida TYPE any.

  DATA: wl_zpmt0074 TYPE zpmt0074.
*
  CLEAR: wl_zpmt0074.
*
  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0074.


  IF wl_zpmt0074-psort IS NOT INITIAL.
    TRANSLATE wl_zpmt0074-psort TO UPPER CASE.
  ENDIF.
*
  IF wl_zpmt0074-pttxt IS NOT INITIAL.
    TRANSLATE wl_zpmt0074-pttxt TO UPPER CASE.
  ENDIF.
*
  IF wl_zpmt0074-atnam IS NOT INITIAL.
    TRANSLATE wl_zpmt0074-atnam TO UPPER CASE.
  ENDIF.

  MOVE-CORRESPONDING wl_zpmt0074 TO p_registro_manter.

ENDFORM.

FORM f_exit_zpmt0074_0017 USING p_tipo.

  TYPES: BEGIN OF y_herst,
           herst      TYPE  herst,
           class_oper TYPE  eqart,
         END OF  y_herst.

  TYPES: BEGIN OF y_typbz,
           typbz      TYPE  typbz,
           herst      TYPE  herst,
           class_oper TYPE  eqart,
         END OF  y_typbz.

  TYPES: BEGIN OF y_fleet_cat,
           eqart TYPE  eqart,
           eartx TYPE  eartx,
         END OF  y_fleet_cat.


  DATA: l_shlp          TYPE shlp_descr,
        l_wa            TYPE ddshiface,
        l_rc            LIKE sy-subrc,
        l_vtweg         TYPE vtweg,
        l_spart         TYPE spart,
        l_campo         TYPE char50,
        l_campo_aux     TYPE char50,
        t_return_values LIKE ddshretval OCCURS 0 WITH HEADER LINE.

  DATA: t_zpmr0001  TYPE TABLE OF y_herst,
        it_zpmr0001 TYPE TABLE OF y_typbz,
        it_t370k    TYPE TABLE OF y_fleet_cat.

  FIELD-SYMBOLS: <p_fleet_cat> TYPE fleet_cat,
                 <p_typbz>     TYPE typbz,
                 <p_herst>     TYPE herst.

  IF p_tipo = '0001'.
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = 'Z_SH_HERST'
        shlptype = 'SH'
      IMPORTING
        shlp     = l_shlp.

    l_campo = '(ZREGISTER_DATA)<fs_wa_registro_manter>-fleet_cat'.
    ASSIGN (l_campo) TO  <p_fleet_cat>.

    FREE:t_zpmr0001.
    SELECT  *
      FROM zpmr0001
      INTO CORRESPONDING FIELDS OF TABLE t_zpmr0001
     WHERE class_oper = <p_fleet_cat>.

    CHECK t_zpmr0001 IS NOT INITIAL.

    SORT t_zpmr0001 BY herst.
    DELETE ADJACENT DUPLICATES FROM t_zpmr0001 COMPARING herst.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        pvalkey          = ' '
        retfield         = 'HERST'
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = '<FS_WA_REGISTRO_MANTER>-HERST'
        callback_program = sy-repid
        value_org        = 'S'
      TABLES
        value_tab        = t_zpmr0001
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.

  ELSEIF p_tipo = '0002'.

    "==========================================================================
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = 'Z_SH_TYPBZ'
        shlptype = 'SH'
      IMPORTING
        shlp     = l_shlp.

    l_campo = '(ZREGISTER_DATA)<fs_wa_registro_manter>-fleet_cat'.
    ASSIGN (l_campo) TO  <p_fleet_cat>.

    l_campo_aux = '(ZREGISTER_DATA)<fs_wa_registro_manter>-herst'.
    ASSIGN (l_campo_aux) TO  <p_herst>.

    FREE: it_zpmr0001.
    SELECT  *
      FROM zpmr0001
      INTO CORRESPONDING FIELDS OF TABLE it_zpmr0001
     WHERE class_oper = <p_fleet_cat>
       AND herst      = <p_herst>.



    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        pvalkey          = ' '
        retfield         = 'TYPBZ'
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = '<FS_WA_REGISTRO_MANTER>-TYPBZ'
        callback_program = sy-repid
        value_org        = 'S'
      TABLES
        value_tab        = it_zpmr0001
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.

  ELSEIF p_tipo = '0003'.

*    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
*      EXPORTING
*        shlpname = 'Z_SH_HERST'
*        shlptype = 'SH'
*      IMPORTING
*        shlp     = l_shlp.
*
*    l_campo = '(ZREGISTER_DATA)<fs_wa_registro_manter>-fleet_cat'.
*    ASSIGN (l_campo) TO  <p_fleet_cat>.

    FREE:it_t370k.
    SELECT  *
      FROM t370k_t
      INTO CORRESPONDING FIELDS OF TABLE it_t370k.

    CHECK it_t370k IS NOT INITIAL.

    SORT it_t370k BY eqart.
*    DELETE ADJACENT DUPLICATES FROM it_t370k COMPARING eqart.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        pvalkey          = ' '
        retfield         = 'EQART'
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = '<FS_WA_REGISTRO_MANTER>-FLEET_CAT'
        callback_program = sy-repid
        value_org        = 'S'
      TABLES
        value_tab        = it_t370k
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.


  ENDIF.



ENDFORM.
