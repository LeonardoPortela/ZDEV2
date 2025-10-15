FUNCTION z_sd_busca_suframa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_PARID) TYPE  J_1BPARID
*"     REFERENCE(P_EMPRESA) TYPE  BUKRS
*"  EXPORTING
*"     REFERENCE(R_SUFRAMA) TYPE  KVERM
*"----------------------------------------------------------------------

  DATA: v_knb1        TYPE knb1,
        vg_kverm      TYPE knb1,
        p_zona_franca TYPE takld.

  CHECK NOT p_parid IS INITIAL.
  CHECK NOT p_empresa IS INITIAL.

  SELECT SINGLE * INTO v_knb1
    FROM knb1
   WHERE kunnr EQ p_parid
     AND bukrs EQ p_empresa.

  CHECK ( sy-subrc IS INITIAL ) AND ( NOT v_knb1-kverm IS INITIAL ).

  SELECT SINGLE taxkd
    INTO p_zona_franca
    FROM knvi
   WHERE kunnr EQ p_parid
     AND taxkd EQ '2'
     AND tatyp EQ 'IBRX'
     AND aland EQ 'BR '.

  IF sy-subrc EQ 0.
    IF p_zona_franca EQ '2'.
      r_suframa = v_knb1-kverm.
    ENDIF.
  ENDIF.

ENDFUNCTION.
