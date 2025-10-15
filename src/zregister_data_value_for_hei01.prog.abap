*----------------------------------------------------------------------*
***INCLUDE ZREGISTER_DATA_VALUE_FOR_HEI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  VALUE_FOR_HERST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_for_herst INPUT.


  DATA: l_shlp          TYPE shlp_descr,
        l_wa            TYPE ddshiface,
        l_rc            LIKE sy-subrc,
        l_vtweg         TYPE vtweg,
        l_spart         TYPE spart,

        l_campo         TYPE char50,
        t_return_values LIKE ddshretval OCCURS 0 WITH HEADER LINE.

  FIELD-SYMBOLS: <p_fleet_cat> TYPE fleet_cat,
                 <p_herst>     TYPE herst.


  CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
    EXPORTING
      shlpname = 'Z_SH_ZPMT0069'
      shlptype = 'SH'
    IMPORTING
      shlp     = l_shlp.

*  SELECT SINGLE *
*    FROM zpmr0001
*    INTO @DATA(w_zpmr0001)
*   WHERE class_oper = @<fs_wa_registro_manter>-fleet_cat.
*
*  IF sy-subrc = 0.
*    l_vtweg = w_tvkbz-vtweg.
*    l_spart = w_tvkbz-spart.
*  ELSE.
*    l_vtweg = '*'.
*    l_spart = '*'.
*  ENDIF.
  l_campo = '<fs_wa_registro_manter>-fleet_cat>'.
  ASSIGN (l_campo) TO  <p_fleet_cat>.




  LOOP AT l_shlp-interface INTO l_wa.
    IF     l_wa-shlpfield  EQ 'FLEET_CAT'.
      l_wa-value = <p_fleet_cat>.
*    ELSEIF l_wa-shlpfield  EQ 'VTWEG'.
*      l_wa-value = l_vtweg.
*    ELSEIF l_wa-shlpfield  EQ 'SPART'.
*      l_wa-value = l_spart.
*    ELSEIF l_wa-shlpfield  EQ 'VKBUR'.
*      l_wa-valfield = 'X'.
    ENDIF.
    MODIFY l_shlp-interface FROM l_wa INDEX sy-tabix.
  ENDLOOP.

  CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
    EXPORTING
      shlp          = l_shlp
    IMPORTING
      rc            = l_rc
    TABLES
      return_values = t_return_values.

  READ TABLE t_return_values INDEX 1.

  IF sy-subrc = 0.
    CLEAR: l_campo.
    l_campo = '<fs_wa_registro_manter>-herst>'.
    ASSIGN (l_campo) TO  <p_herst>.
    <p_herst> = t_return_values-fieldval.
  ENDIF.


ENDMODULE.
