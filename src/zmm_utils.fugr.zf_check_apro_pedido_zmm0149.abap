FUNCTION zf_check_apro_pedido_zmm0149.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EBELN) TYPE  EBELN
*"  EXPORTING
*"     REFERENCE(E_APROVADO) TYPE  CHAR01
*"     VALUE(E_DATA) TYPE  CDDATUM
*"     VALUE(E_HORA) TYPE  CDUZEIT
*"----------------------------------------------------------------------

  DATA vobjectid TYPE cdhdr-objectid.

  CHECK i_ebeln IS NOT INITIAL.

  vobjectid = i_ebeln.
  SELECT *
          FROM cdhdr AS a
            INTO TABLE @DATA(t_cdhdr)
               WHERE objectclas EQ 'EINKBELEG'
                 AND objectid   EQ @vobjectid
                 AND ( tcode EQ 'ME29N' OR  tcode EQ '' ).


  IF t_cdhdr IS NOT INITIAL.
    SORT t_cdhdr ASCENDING BY udate utime.
    READ TABLE t_cdhdr INTO DATA(ws_cdhdr) INDEX 1.

    SELECT  SINGLE *
      FROM cdpos
      INTO @DATA(ws_cdpos)
      WHERE objectclas EQ @ws_cdhdr-objectclas
      AND   objectid   EQ @ws_cdhdr-objectid
      AND   changenr   EQ @ws_cdhdr-changenr
      AND   fname      EQ 'FRGKE'
      AND   value_new  EQ '2'.

    IF ws_cdpos IS NOT INITIAL.
      e_aprovado = abap_true. "Não esta aprovado.
      e_data = ws_cdhdr-udate.
      e_hora = ws_cdhdr-utime.
    ENDIF.
  ENDIF.

  CLEAR: ws_cdhdr, ws_cdpos.


ENDFUNCTION.
